#!/usr/bin/env python3
"""Extract Church signatures, using GHC only to resolve their type holes.

No source expression is exported as a synthesis candidate.  The manifest is
deliberately independent of either search engine and records the original
signature alongside the type GHC inferred for it.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess


PARTIAL = {
    "head", "last", "at", "foldl1", "foldr1", "fromJust", "fromLeft",
    "fromRight", "maximumBy", "maximumOn", "minimumBy", "minimumOn",
    "minMaxBy", "minmaxElement", "atKey", "reduce", "maximum", "minimum",
    "minMax",
}


class TypeParser:
    def __init__(self, text):
        normalized = text.replace("∀", "forall ")
        token_pattern = r"\w[\w']*|->|[().`{}]"
        residue = re.sub(token_pattern, "", normalized)
        if residue.strip():
            raise ValueError(f"unsupported type syntax {residue!r} in {text!r}")
        self.tokens = re.findall(token_pattern, normalized)
        self.position = 0

    def peek(self):
        return self.tokens[self.position] if self.position < len(self.tokens) else None

    def take(self, expected=None):
        value = self.peek()
        if value is None or (expected is not None and value != expected):
            raise ValueError(f"expected {expected!r}, got {value!r}: {self.tokens}")
        self.position += 1
        return value

    def parse(self):
        value = self.arrow()
        if self.peek() is not None:
            raise ValueError(f"unconsumed type tokens: {self.tokens[self.position:]}")
        return value

    def arrow(self):
        if self.peek() == "forall":
            self.take()
            binders = []
            while self.peek() != ".":
                if self.peek() == "{":
                    self.take()
                    binders.append(self.take())
                    self.take("}")
                else:
                    binders.append(self.take())
            self.take(".")
            return {"tag": "forall", "binders": binders, "body": self.arrow()}
        value = self.application()
        if self.peek() == "`":
            self.take()
            constructor = self.take()
            self.take("`")
            value = app(app(name(constructor), value), self.application())
        if self.peek() == "->":
            self.take()
            value = {"tag": "arrow", "domain": value, "codomain": self.arrow()}
        return value

    def application(self):
        value = self.atom()
        while self.peek() not in (None, ")", "->", "`", "."):
            value = app(value, self.atom())
        return value

    def atom(self):
        if self.peek() == "(":
            self.take()
            value = self.arrow()
            self.take(")")
            return value
        return name(self.take())


def name(value):
    return {"tag": "name", "name": value}


def app(function, argument):
    return {"tag": "app", "function": function, "argument": argument}


def render(tree):
    tag = tree["tag"]
    if tag == "name":
        return tree["name"]
    if tag == "forall":
        return "forall " + " ".join(tree["binders"]) + ". " + render(tree["body"])
    if tag == "arrow":
        left = render(tree["domain"])
        if tree["domain"]["tag"] in ("forall", "arrow"):
            left = "(" + left + ")"
        return left + " -> " + render(tree["codomain"])
    if tag == "app":
        left, right = render(tree["function"]), render(tree["argument"])
        if tree["function"]["tag"] in ("forall", "arrow"):
            left = "(" + left + ")"
        if tree["argument"]["tag"] != "name":
            right = "(" + right + ")"
        return left + " " + right
    raise ValueError(tag)


def expand(tree, aliases):
    """Capture-avoiding expansion; give every forall an ASCII fresh binder."""
    counter = 0

    def go(node, variables):
        nonlocal counter
        tag = node["tag"]
        if tag == "forall":
            updated = dict(variables)
            binders = []
            for binder in node["binders"]:
                fresh = "church" + str(counter)
                counter += 1
                updated[binder] = name(fresh)
                binders.append(fresh)
            return {"tag": tag, "binders": binders, "body": go(node["body"], updated)}
        if tag == "arrow":
            return {"tag": tag, "domain": go(node["domain"], variables),
                    "codomain": go(node["codomain"], variables)}
        arguments = []
        head = node
        while head["tag"] == "app":
            arguments.insert(0, head["argument"])
            head = head["function"]
        if head["tag"] == "name" and head["name"] in aliases:
            alias = aliases[head["name"]]
            parameters = alias["parameters"]
            if len(arguments) != len(parameters):
                raise ValueError(f"unsaturated or oversaturated alias: {render(node)}")
            substituted = {p: go(a, variables) for p, a in zip(parameters, arguments)}
            return go(alias["type"], substituted)
        if tag == "name":
            return variables.get(node["name"], node)
        if tag == "app":
            return app(go(node["function"], variables), go(node["argument"], variables))
        raise ValueError(tag)

    return go(tree, {})


def final_result(tree):
    while tree["tag"] in ("forall", "arrow"):
        tree = tree["body"] if tree["tag"] == "forall" else tree["codomain"]
    return tree


def outer_arguments(tree):
    result = []
    while tree["tag"] in ("forall", "arrow"):
        if tree["tag"] == "forall":
            tree = tree["body"]
        else:
            result.append(tree["domain"])
            tree = tree["codomain"]
    return result


def classify(case):
    if case["name"] in PARTIAL:
        return "requires_partiality", "No total closed inhabitant: choose the returned element type empty and supply an empty Church container. Requires an explicitly supplied element default."
    parsed = TypeParser(case["resolved_signature"]).parse()
    result = final_result(parsed)
    result_text = render(result)
    if (result_text == "Int" or result_text == "Pair Int Int") and not any(
        render(argument) == "Int" for argument in outer_arguments(parsed)
    ):
        return "needs_int_provider", "Total with one concrete provider, churchZero :: Int. No integer constructor occurs in the pure function-only search vocabulary."
    return "total", "Synthesize with no named source implementations and no bottom/default provider."


def source_inventory(text):
    aliases, signatures, locals_ = {}, [], []
    enclosing = None
    lines = text.splitlines()
    for line_number, line in enumerate(lines, 1):
        alias = re.match(r"^type\s+(\w+)\s*(.*?)\s*=\s*(.*)$", line)
        if alias:
            alias_name, parameters, body = alias.groups()
            aliases[alias_name] = {"line": line_number, "parameters": parameters.split(),
                                   "source": line, "type": TypeParser(body).parse()}
        signature = re.match(r"^(\s*)([\w][\w']*)\s*(?:::|∷)\s*(.*)$", line)
        if not signature:
            continue
        indentation, function, body = signature.groups()
        # Continue signatures until the first non-indented declaration/body.
        index = line_number
        while index < len(lines):
            continuation = lines[index]
            if not continuation.strip() or continuation.lstrip().startswith("--"):
                break
            if len(continuation) - len(continuation.lstrip()) <= len(indentation):
                break
            if "=" in continuation or "::" in continuation or "∷" in continuation:
                break
            body += " " + continuation.strip()
            index += 1
        entry = {"name": function, "line": line_number, "source_signature": body,
                 "has_holes": bool(re.search(r"\b_\b", body))}
        entry["scope"] = "local" if indentation else "top_level"
        if indentation:
            entry["enclosing_definition"] = enclosing
        else:
            enclosing = function
        (locals_ if indentation else signatures).append(entry)
    return aliases, signatures, locals_


def ghc_signatures(output):
    block = output.split("TYPE SIGNATURES\n", 1)[1].split("TYPE CONSTRUCTORS\n", 1)[0]
    result = {}
    current = None
    for line in block.splitlines():
        match = re.match(r"^  ([\w][\w']*)\s*::\s*(.*)$", line)
        if match:
            current, body = match.groups()
            result[current] = body
        elif current and line.strip():
            result[current] += " " + line.strip()
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path,
                        default=Path(__file__).resolve().parents[1] / "docs/examples/Church.hs")
    parser.add_argument("--output", type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument("--ghc", default="ghc")
    parser.add_argument("--check", action="store_true", help="fail if checked-in generated files are stale")
    args = parser.parse_args()
    source = args.source.read_text(encoding="utf-8-sig")
    aliases, cases, local_signatures = source_inventory(source)
    command = [args.ghc, "-v0", "-fno-code", "-fno-write-interface", "-ddump-types",
               "-fprint-explicit-foralls", "-dppr-cols=10000", str(args.source)]
    compiled = subprocess.run(command, capture_output=True, text=True, encoding="utf-8", check=True,
                              env={**os.environ, "GHC_CHARENC": "UTF-8"})
    inferred = ghc_signatures(compiled.stdout)
    if set(inferred) != {case["name"] for case in cases}:
        raise ValueError("GHC and source inventories disagree")
    if not PARTIAL <= set(inferred):
        raise ValueError("partiality classification contains stale names")
    top_level_count = len(cases)
    for local in local_signatures:
        body = local["source_signature"]
        variables = sorted({token for token in re.findall(r"\w[\w']*", body)
                            if token[0].islower() and token != "forall"})
        local["resolved_signature"] = "forall " + " ".join(variables) + ". " + body
        local["scope_note"] = "Generalized standalone query for the annotated local binding; enclosing lexical type variables remain recorded by name. No captured source value is admitted."
    cases += local_signatures
    for index, case in enumerate(cases):
        case["id"] = f"church_case_{index:03d}"
        if case["scope"] == "top_level":
            case["resolved_signature"] = inferred[case["name"]]
        case["expanded_type"] = expand(TypeParser(case["resolved_signature"]).parse(), aliases)
        case["expanded_signature"] = render(case["expanded_type"])
        case["classification"], case["classification_reason"] = classify(case)
        case["query_signature"] = case["expanded_signature"]
        case["acceptance_signature"] = case["resolved_signature"]
        if case["classification"] == "requires_partiality":
            resolved_tree = TypeParser(case["resolved_signature"]).parse()
            returned = final_result(resolved_tree)
            while returned["tag"] == "app":
                returned = returned["argument"]
            variable = returned["name"]
            binder_index = resolved_tree["binders"].index(variable)
            default_type = name(case["expanded_type"]["binders"][binder_index])
            augmented = dict(case["expanded_type"])
            augmented["body"] = {"tag": "arrow", "domain": default_type,
                                 "codomain": augmented["body"]}
            case["default_type"] = default_type
            case["query_signature"] = render(augmented)
            resolved_tree["body"] = {"tag": "arrow", "domain": name(variable),
                                     "codomain": resolved_tree["body"]}
            case["acceptance_signature"] = render(resolved_tree)
    counts = {category: sum(c["classification"] == category for c in cases)
              for category in ("total", "needs_int_provider", "requires_partiality")}
    version = subprocess.run([args.ghc, "--numeric-version"], capture_output=True,
                             text=True, check=True).stdout.strip()
    manifest = {
        "schema_version": 1,
        "source": "docs/examples/Church.hs",
        "source_sha256": hashlib.sha256(source.encode("utf-8")).hexdigest(),
        "source_hash_encoding": "UTF-8 without BOM; CRLF and CR line endings normalized to LF",
        "ghc_version": version,
        "extraction": "GHC -fno-code -fno-write-interface -ddump-types -fprint-explicit-foralls; implementations are used only by GHC to resolve partial signature holes, never as candidates or providers.",
        "counts": {"top_level": top_level_count, "all_signatures": len(cases), "partial_signatures": sum(c["has_holes"] for c in cases),
                   "local_signatures": len(local_signatures), **counts},
        "aliases": aliases,
        "local_signatures": local_signatures,
        "cases": cases,
    }
    generated = {
        "manifest.json": json.dumps(manifest, indent=2, ensure_ascii=False) + "\n",
        "cases.tsv": "".join("\t".join([case["id"], case["name"], case["classification"],
                                           case["query_signature"], case["acceptance_signature"],
                                           case["resolved_signature"]]) + "\n"
                               for case in cases),
        "aliases.hs.inc": "\n".join(alias["source"] for alias in aliases.values()) + "\n",
    }
    if args.check:
        for filename, expected in generated.items():
            path = args.output / filename
            if not path.exists() or path.read_text(encoding="utf-8") != expected:
                raise ValueError(f"stale generated corpus file: {path}; rerun extract_corpus.py")
    else:
        args.output.mkdir(parents=True, exist_ok=True)
        for filename, content in generated.items():
            (args.output / filename).write_text(content, encoding="utf-8")
    print(json.dumps(manifest["counts"], sort_keys=True))


if __name__ == "__main__":
    main()
