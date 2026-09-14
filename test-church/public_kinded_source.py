"""Public ground-kind regressions with exact-output GHC replay and False controls.

Run after building djex, with a fresh output directory. Reference declarations
supply only a transparent alias and a methodless class/instance, never the
implementations to be synthesized. Receipts retain source and runtime hashes.
"""
from pathlib import Path
import argparse
import os
import re
import subprocess
import sys

import behavior_runtime as runtime

CASES = [
    ('identity', 'forall (f :: * -> *) a. f a -> f a', 'probe @[] [7 :: Int] == [7]'),
    ('callback', 'forall b. ((forall (f :: * -> *) a. f a -> f a) -> b) -> b',
     r'probe (\h -> h @Maybe (Just (7 :: Int))) == Just 7'),
    ('vacuous_root', 'forall (f :: * -> *) a. a -> a', 'probe @[] (7 :: Int) == 7'),
    ('vacuous_local', 'forall a. (forall (f :: * -> *). a) -> a',
     'probe @Int (7 :: forall (f :: * -> *). Int) == 7'),
    ('vacuous_callback', 'forall b. ((forall (f :: * -> *) a. a -> a) -> b) -> b',
     r'probe (\h -> h @[] (7 :: Int)) == 7'),
    ('shadowed_callback', 'forall (f :: * -> *) b. ((forall (f :: *) a. a -> a) -> b) -> b',
     r'probe @[] (\h -> h @Int (7 :: Int)) == 7'),
    ('vacuous_result', 'forall a. a -> (forall (f :: * -> *). a)', 'probe @Int 7 @[] == 7'),
    ('adjacent_kinds', 'forall a. (forall (f :: * -> *). a) -> (forall (f :: *). a) -> (a,a)',
     'probe @Int (7 :: forall (f :: * -> *). Int) (7 :: forall (f :: *). Int) == (7,7)'),
    ('alias_argument', 'forall a. Fixture.Identity (forall (f :: * -> *). a) -> a',
     'probe @Int (7 :: forall (f :: * -> *). Int) == 7'),
    ('constrained_callback', 'forall b. ((forall (f :: * -> *) a. Fixture.Marker a => a -> a) -> b) -> b',
     r'probe (\h -> h @[] (7 :: Int)) == 7'),
    ('impredicative_pair', 'forall (f :: * -> *). ((forall a. a -> a), (forall a. a -> a))',
     'case probe @[] of (left, right) -> left (7 :: Int) == 7 && right True'),
]
HEADER = """{-# LANGUAGE RankNTypes, ImpredicativeTypes, KindSignatures, ScopedTypeVariables, TypeApplications, TypeAbstractions, FlexibleContexts, LiberalTypeSynonyms, NoPolyKinds #-}
module Main where
import Fixture
"""
FIXTURE = """module Fixture where
type Identity x = x
class Marker a
instance Marker Int
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--exe', type=Path, required=True)
    parser.add_argument('--backend', choices=['djinn', 'exference'], default='djinn')
    parser.add_argument('--case', action='append', dest='cases')
    parser.add_argument('--choice-budget', type=int,
                        help='Djinn choice budget; omitted retains the frontend default')
    args = parser.parse_args()
    if args.choice_budget is not None and (args.choice_budget < 0 or args.backend != 'djinn'):
        parser.error('--choice-budget requires Djinn and a non-negative value')
    root = Path(__file__).resolve().parent.parent
    exe = args.exe.resolve()
    out = runtime.prepare_output_directory(args.output.resolve())
    environment = out / 'environment'
    environment.mkdir()
    (environment / 'Fixture.hs').write_text(FIXTURE, encoding='utf-8')
    paths = subprocess.check_output(
        ['git', 'ls-files', '-z', '--cached', '--others', '--exclude-standard'],
        cwd=root).decode().split('\0')
    paths = sorted({p for p in paths if p and (root/p).is_file()
                    and Path(p).suffix in ['.hs', '.cabal', '.py']})
    report = dict(status='running', backend=args.backend, queries=[], choice_budget=args.choice_budget,
                  source_sha256={p:runtime.sha256(root/p) for p in paths},
                  runtime_sha256={str(exe):runtime.sha256(exe)})
    for p in paths:
        dest = out/'source'/p
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes((root/p).read_bytes())
    runner = runtime.Processes(out, 90)
    env = dict(os.environ, djex_datadir=str(root), PYTHONDONTWRITEBYTECODE='1')

    def save():
        report['processes'] = runner.rows
        runtime.write_json(out/'results.json', report)

    def replay(label, signature, implementation, observation):
        folder = out/label
        folder.mkdir()
        source = folder/'Main.hs'
        source.write_text(HEADER+'probe :: '+signature+'\n'+implementation
                          +'\nmain :: IO ()\nmain = print ('+observation+')\n',
                          encoding='utf-8')
        binary = folder/'replay.exe'
        compiled = runner.run(label+'-compile',
            ['ghc', '-fforce-recomp', '-Wno-star-is-type', '-i'+str(environment),
             str(source), '-odir', str(folder), '-hidir', str(folder), '-o', str(binary)],
            cwd=root, env=env)
        result = dict(fixture_sha256=runtime.sha256(source),
                      compile_exit_code=compiled.returncode)
        if compiled.returncode == 0:
            executed = runner.run(label+'-replay', [str(binary)], cwd=root, env=env)
            result.update(execution_exit_code=executed.returncode, output=executed.stdout,
                          passed=executed.returncode == 0 and executed.stdout.strip() == 'True')
        else:
            result.update(passed=False, compiler_error=compiled.stderr)
        return result

    try:
        save()
        selected = [case for case in CASES if not args.cases or case[0] in args.cases]
        if args.cases and set(args.cases) != {case[0] for case in selected}:
            raise ValueError('unknown case filter')
        for label, signature, observation in selected:
            row = dict(label=label, signature=signature, observation=observation)
            report['queries'].append(row)
            shot = runner.run(label+'-one-shot',
                [str(exe), args.backend, '--environment', str(environment),
                 '--render', 'expression', '--select', 'first'] +
                ([] if args.choice_budget is None else ['--choice-budget', str(args.choice_budget)]) +
                [signature], cwd=root, env=env)
            row['one_shot'] = dict(exit_code=shot.returncode, stdout=shot.stdout, stderr=shot.stderr)
            if shot.returncode == 0 and shot.stdout.strip():
                row['one_shot']['replay'] = replay(
                    label+'-one-shot', signature, 'probe = '+shot.stdout.strip(), observation)
            save()
            # The named pair query distinguishes the differently kinded inputs.
            predicate = observation
            if label == 'adjacent_kinds':
                predicate = predicate.replace(
                    '(7 :: forall (f :: *). Int)', '(9 :: forall (f :: *). Int)'
                ).replace('== (7,7)', '== (7,9)')
            command = [str(exe), 'repl', '--ignore-startup', '--environment', str(environment)]
            prefix = ':set prompt ""\n:backend '+args.backend+'\n:set allow-unused on\n:set djinn-axioms off\n'
            if args.choice_budget is not None:
                prefix += ':set choice-budget ' + str(args.choice_budget) + '\n'
            query = ':synth probe :: '+signature+' where '+predicate+'\n:quit\n'
            named = runner.run(label+'-named', command, source=prefix+query, cwd=root, env=env)
            row['named'] = dict(exit_code=named.returncode, stdout=named.stdout, stderr=named.stderr)
            binding = re.search(r'(?m)^probe(?:\s|=)', named.stdout)
            observations = re.search(
                r'checked=(\d+), true=(\d+), false=(\d+), error=(\d+), timeout=(\d+)', named.stderr)
            row['named']['accepted'] = bool(
                named.returncode == 0 and binding and observations and int(observations[2]) > 0)
            if row['named']['accepted']:
                row['named']['replay'] = replay(
                    label+'-named', signature, named.stdout[binding.start():].strip(), predicate)
            false = runner.run(label+'-false', command,
                source=prefix+':synth probe :: '+signature+' where Prelude.False\n:quit\n',
                cwd=root, env=env)
            observed = re.search(
                r'checked=(\d+), true=(\d+), false=(\d+), error=(\d+), timeout=(\d+)', false.stderr)
            row['false_control'] = dict(
                exit_code=false.returncode, stdout=false.stdout, stderr=false.stderr,
                passed=bool(false.returncode == 0 and observed and int(observed[2]) == 0
                    and int(observed[3]) > 0 and int(observed[4]) == 0 and int(observed[5]) == 0
                    and 'BEHAVIORAL_NO_MATCH' in false.stderr
                    and not re.search(r'(?m)^probe(?:\s|=)', false.stdout)))
            row['passed'] = bool(row['one_shot'].get('replay',{}).get('passed')
                and row['named'].get('replay',{}).get('passed') and row['false_control']['passed'])
            save()
            print(label, 'PASS' if row['passed'] else 'FAIL', flush=True)
        bad = 'forall (f :: *) a. f a -> f a'
        invalid = runner.run('invalid-kind',
            [str(exe), args.backend, '--environment', str(environment),
             '--render', 'expression', '--select', 'first', bad], cwd=root, env=env)
        report['invalid_kind'] = dict(
            exit_code=invalid.returncode, stdout=invalid.stdout, stderr=invalid.stderr,
            passed=invalid.returncode != 0 and not invalid.stdout.strip()
                and 'kind' in invalid.stderr.lower())
        report['status'] = 'passed' if (
            all(row['passed'] for row in report['queries']) and report['invalid_kind']['passed']
        ) else 'failed'
    except BaseException as failure:
        report.update(status='failed', failure=repr(failure))
    finally:
        report['sources_unchanged'] = all(
            runtime.sha256(root/p) == h for p,h in report['source_sha256'].items())
        report['runtimes_unchanged'] = all(
            runtime.sha256(Path(p)) == h for p,h in report['runtime_sha256'].items())
        if not report['sources_unchanged'] or not report['runtimes_unchanged']:
            report['status'] = 'failed'
        save()
    print(report['status'], flush=True)
    return 0 if report['status'] == 'passed' else 1


if __name__ == '__main__':
    sys.exit(main())
