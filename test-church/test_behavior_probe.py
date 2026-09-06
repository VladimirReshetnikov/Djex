"""Focused corpus guards; root runs these separately from compiler acceptance."""
import ctypes
import json
import os
from pathlib import Path
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest import mock

from behavior_probe import (behavioral_observations, commands, displayed_definition,
                            isolated_replay_sources, main, replay_source, validate_settings)
from behavior_runtime import Processes, prepare_output_directory, render_type


class TranscriptTests(unittest.TestCase):
    def test_output_guard_accepts_new_or_empty_paths_and_preserves_prior_files(self):
        with tempfile.TemporaryDirectory(prefix="behavior-output-guard-") as directory:
            output = Path(directory) / "attempt"
            self.assertEqual(prepare_output_directory(output), output)
            self.assertEqual(prepare_output_directory(output), output)
            marker = output / "results.json"
            marker.write_bytes(b"previous immutable receipt\r\n")
            with self.assertRaisesRegex(ValueError, "fresh path"):
                prepare_output_directory(output)
            self.assertEqual(marker.read_bytes(), b"previous immutable receipt\r\n")
            with self.assertRaisesRegex(ValueError, "fresh path"):
                prepare_output_directory(marker)

    def test_haskell_runner_rejects_existing_receipt_before_preparation(self):
        with tempfile.TemporaryDirectory(prefix="behavior-existing-receipt-") as directory:
            marker = Path(directory) / "results.json"
            marker.write_bytes(b"previous receipt")
            with mock.patch.object(sys, "argv", ["behavior_probe.py", "--prepare-only", "--output", directory]), \
                    mock.patch("behavior_probe.source_provenance", side_effect=AssertionError("guard ran too late")):
                with self.assertRaisesRegex(ValueError, "fresh path"):
                    main()
            self.assertEqual(list(Path(directory).iterdir()), [marker])
            self.assertEqual(marker.read_bytes(), b"previous receipt")

    def test_exact_multiline_equation(self):
        source = "djex[djinn]> answer a =\n  case a of\n    x -> x\n[notice]\n"
        self.assertEqual(displayed_definition(source, "answer"), "answer a =\n  case a of\n    x -> x")

    def test_missing_duplicate_or_partial_definition_fails(self):
        for text in ("", "answer :: a -> a\n", "answer x = x\nanswer x = x\n", "answer = undefined\n"):
            with self.subTest(text=text), self.assertRaises(ValueError):
                displayed_definition(text, "answer")

    def test_accounted_success_and_false_control(self):
        summary = "[DJEX_REPL_BEHAVIORAL_OBSERVATIONS] checked=3,true=1,false=2,error=0,timeout=0,window=8"
        self.assertEqual(behavioral_observations(summary, expect_success=True, window=8)["false"], 2)
        summary = "[DJEX_REPL_BEHAVIORAL_OBSERVATIONS] checked=3,true=0,false=3,error=0,timeout=0,window=8\n[DJEX_REPL_BEHAVIORAL_NO_MATCH] no candidate"
        self.assertEqual(behavioral_observations(summary, expect_success=False, window=8)["false"], 3)

    def test_no_output_is_not_a_false_predicate_receipt(self):
        prefix = "[DJEX_REPL_BEHAVIORAL_OBSERVATIONS] "
        suffix = "\n[DJEX_REPL_BEHAVIORAL_NO_MATCH] no candidate"
        failures = ["", "[DJEX_REPL_BEHAVIORAL_PREFLIGHT] invalid",
                    prefix + "checked=0,true=0,false=0,error=0,timeout=0,window=8" + suffix,
                    prefix + "checked=1,true=0,false=0,error=1,timeout=0,window=8" + suffix,
                    prefix + "checked=1,true=0,false=0,error=0,timeout=1,window=8" + suffix,
                    prefix + "checked=2,true=0,false=1,error=0,timeout=0,window=8" + suffix,
                    prefix + "checked=1,true=0,false=1,error=0,timeout=0,window=7" + suffix]
        for text in failures:
            with self.subTest(text=text), self.assertRaises(ValueError):
                behavioral_observations(text, expect_success=False, window=8)

    def test_manifest_renderer_preserves_quantifier_location(self):
        variable = {"tag": "name", "name": "a"}
        scheme = {"tag": "forall", "binders": ["a"], "body": {"tag": "arrow", "domain": variable, "codomain": variable}}
        goal = {"tag": "arrow", "domain": scheme, "codomain": scheme}
        self.assertEqual(render_type(goal), "((forall a. (a -> a)) -> (forall a. (a -> a)))")
        self.assertEqual(render_type(goal, lean=True), "((∀ (a : Type), (a → a)) → (∀ (a : Type), (a → a)))")

    def test_backend_and_settings_snapshot_are_required(self):
        args = SimpleNamespace(window=8, budget=100, steps=200, djinn_strategy="depth-first")
        settings = {"backend": "exference", "ranking": "balanced", "select": "first",
                    "render": "definition", "prompt": '\"\"', "allow-unused": "on",
                    "djinn-axioms": "off", "djinn-strategy": "depth-first",
                    "candidate-limit": "8", "quality-window": "8",
                    "choice-budget": "100", "max-steps": "200"}
        good = "Active backend: exference\n" + "\n".join(key + " = " + value for key, value in settings.items())
        self.assertEqual(validate_settings(good, "exference", args), settings)
        bad = [good.replace("Active backend: exference", "Active backend: djinn"),
               good.replace("backend = exference", "backend = djinn"),
               good.replace("choice-budget = 100", "choice-budget = 0"),
               good.replace("djinn-strategy = depth-first", "djinn-strategy = interleave"),
               good + "\nbackend = exference", good + "\n[DJEX_REPL_BACKEND] failure",
               good + "\n[DJEX_REPL_COMMAND] failure"]
        for text in bad:
            with self.subTest(text=text), self.assertRaises(ValueError):
                validate_settings(text, "exference", args)

    def test_selected_djinn_strategy_is_emitted_without_changing_other_limits(self):
        for strategy in ("depth-first", "interleave"):
            args = SimpleNamespace(window=4096, budget=100000, steps=100000, djinn_strategy=strategy)
            source, cases = commands("djinn", ["not"], args, {"not": "a -> a"})
            self.assertEqual(source.splitlines().count(":set djinn-strategy " + strategy), 1)
            for setting in (":set candidate-limit 4096", ":set quality-window 4096",
                            ":set choice-budget 100000", ":set max-steps 100000", ":set select first"):
                self.assertIn(setting, source.splitlines())
            self.assertEqual(len(cases), 1)
            self.assertIn("where Prelude.False", source)

    def test_candidate_modules_do_not_gain_control_or_other_candidate_scope(self):
        first = {"name": "first", "operation": "reverse", "type": "forall a. a -> a",
                 "definition": "first = oracleWitness_reverse"}
        second = {"name": "second", "operation": "reverse", "type": "forall a. a -> a",
                  "definition": "second = first"}
        sources, labels = isolated_replay_sources([first, second], 2)
        self.assertEqual(labels[:2], ["first", "second"])
        first_source, second_source = sources["BehaviorCandidate0.hs"], sources["BehaviorCandidate1.hs"]
        self.assertIn(first["definition"], first_source)
        self.assertIn(second["definition"], second_source)
        for source in (first_source, second_source):
            self.assertNotRegex(source, r"(?m)^oracle(?:Witness|Wrong)_.*(?:=|::)")
            self.assertNotRegex(source, r"(?m)^import .*Behavior(?:Candidate|Oracle)")
        self.assertNotRegex(first_source, r"(?m)^second(?:\s|=)")
        self.assertNotRegex(second_source, r"(?m)^first(?:\s|=)")
        self.assertIn("oracleWitness_reverse =", sources["BehaviorOracleControls.hs"])
        self.assertNotIn(first["definition"], sources["BehaviorCandidates.hs"])
        with self.assertRaises(ValueError):
            replay_source([first, second], 2)


def still_running(pid):
    if os.name == "nt":
        from ctypes import wintypes
        kernel = ctypes.WinDLL("kernel32", use_last_error=True)
        kernel.OpenProcess.argtypes = (wintypes.DWORD, wintypes.BOOL, wintypes.DWORD)
        kernel.OpenProcess.restype = wintypes.HANDLE
        kernel.WaitForSingleObject.argtypes = (wintypes.HANDLE, wintypes.DWORD)
        kernel.WaitForSingleObject.restype = wintypes.DWORD
        kernel.CloseHandle.argtypes = (wintypes.HANDLE,)
        handle = kernel.OpenProcess(0x00100000, False, pid)
        if not handle:
            return False
        try:
            return kernel.WaitForSingleObject(handle, 0) == 258
        finally:
            kernel.CloseHandle(handle)
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    stat = Path(f"/proc/{pid}/stat")
    return not (stat.exists() and stat.read_text().split(")", 1)[1].strip().startswith("Z"))


class ProcessTreeTests(unittest.TestCase):
    def test_short_owned_process_success(self):
        with tempfile.TemporaryDirectory(prefix="behavior-process-success-") as directory:
            result = Processes(directory, 10).run("success", [sys.executable, "-c", "print('owned success')"])
            self.assertEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "owned success\n")
            receipt = json.loads((Path(directory) / "processes.json").read_text())
            self.assertEqual(receipt[0]["exit_code"], 0)
            self.assertFalse(receipt[0]["timed_out"])

    def test_timeout_terminates_sleeping_descendant(self):
        with tempfile.TemporaryDirectory(prefix="behavior-process-timeout-") as directory:
            program = "import subprocess,sys,time; child=subprocess.Popen([sys.executable,'-c','import time; time.sleep(60)']); print(child.pid,flush=True); time.sleep(60)"
            with self.assertRaises(TimeoutError):
                Processes(directory, 2).run("timeout", [sys.executable, "-c", program])
            receipt = json.loads((Path(directory) / "processes.json").read_text())[0]
            child = int(Path(receipt["stdout_path"]).read_text().strip())
            self.assertTrue(receipt["timed_out"])
            self.assertIsNone(receipt["exit_code"])
            self.assertFalse(still_running(child), "owned descendant escaped timeout cleanup")
            self.assertFalse(still_running(receipt["owned_root_pid"]))


if __name__ == "__main__":
    unittest.main()
