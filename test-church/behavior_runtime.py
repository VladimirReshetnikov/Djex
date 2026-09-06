"""Small provenance/process helpers for the six-operation behavioral corpus."""
from __future__ import annotations

import hashlib
import json
import os
from contextlib import ExitStack
from pathlib import Path
import re
import signal
import subprocess
import time

from behavior_spec import OPERATIONS, SOURCE_LINES, SOURCE_SIGNATURES


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def write_json(path, value):
    Path(path).write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")


def prepare_output_directory(path):
    """A new attempt must never replace earlier captures or receipts."""
    path = Path(path)
    if path.exists():
        if not path.is_dir() or any(path.iterdir()):
            raise ValueError("output directory must be empty; choose a fresh path to preserve earlier acceptance receipts: " + str(path))
    else:
        path.mkdir(parents=True)
    return path


def render_type(node, *, lean=False):
    """Render the actual manifest tree, rather than a second signature table."""
    tag = node["tag"]
    if tag == "name":
        name = node["name"]
        if not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9']*", name):
            raise ValueError("unsupported manifest type name")
        return name
    if tag == "arrow":
        arrow = " → " if lean else " -> "
        return "(" + render_type(node["domain"], lean=lean) + arrow + render_type(node["codomain"], lean=lean) + ")"
    if tag == "app":
        return "(" + render_type(node["function"], lean=lean) + " " + render_type(node["argument"], lean=lean) + ")"
    if tag == "forall":
        binders = node["binders"]
        if not binders or len(binders) != len(set(binders)) or any(not re.fullmatch(r"[a-z][A-Za-z_0-9']*", x) for x in binders):
            raise ValueError("unsupported manifest binder group")
        prefix = ("∀ " + " ".join(f"({x} : Type)" for x in binders) + ", "
                  if lean else "forall " + " ".join(binders) + ". ")
        return "(" + prefix + render_type(node["body"], lean=lean) + ")"
    raise ValueError("unsupported manifest type node: " + str(tag))


def source_provenance(manifest_path):
    manifest_path = Path(manifest_path).resolve()
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    source = manifest_path.parent.parent / manifest["source"]
    normalized_hash = hashlib.sha256(source.read_text(encoding="utf-8").encode("utf-8")).hexdigest()
    if normalized_hash != manifest["source_sha256"]:
        raise ValueError("Church source differs from its canonical-LF manifest hash")
    selected = []
    for operation in OPERATIONS:
        matches = [row for row in manifest["cases"] if row["name"] == operation]
        if len(matches) != 1:
            raise ValueError("missing or ambiguous Church operation: " + operation)
        row = matches[0]
        if (row["line"] != SOURCE_LINES[operation] or row["classification"] != "total"
                or row["source_signature"] != SOURCE_SIGNATURES[operation]):
            raise ValueError("source operation signature/classification changed: " + operation)
        selected.append({**{key: row[key] for key in ("id", "name", "line", "source_signature", "expanded_type")},
                         "haskell_target": render_type(row["expanded_type"]),
                         "lean_target": render_type(row["expanded_type"], lean=True)})
    return {"manifest_path": str(manifest_path), "manifest_sha256": sha256(manifest_path),
            "church_source_path": str(source), "church_source_raw_sha256": sha256(source),
            "church_source_canonical_lf_sha256": normalized_hash, "operations": selected}


class Processes:
    def __init__(self, directory, timeout):
        self.directory = Path(directory)
        self.timeout = timeout
        self.rows = []

    def run(self, label, command, *, source=None, cwd=None, env=None):
        started = time.monotonic()
        row = {"label": label, "command": list(map(str, command)),
               "process_timeout_seconds": self.timeout, "cwd": str(cwd) if cwd else None,
               "status": "running", "started_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
               "capture_format": "raw child bytes; returned text decodes UTF-8 with universal newlines"}
        input_path = None
        if source is not None:
            input_path = self.directory / (label + ".input.txt")
            input_path.write_text(source, encoding="utf-8")
            row.update(input_path=str(input_path.resolve()), input_sha256=sha256(input_path))
        paths = {suffix: self.directory / f"{label}.{suffix}.txt" for suffix in ("stdout", "stderr")}
        for suffix, path in paths.items():
            row[suffix + "_path"] = str(path.resolve())
        self.rows.append(row)
        write_json(self.directory / "processes.json", self.rows)
        print(f"[behavior process] started {label} at {row['started_utc']} (guard {self.timeout:g}s)", flush=True)
        process = None
        job = None
        captures = ExitStack()
        try:
            # Files expose progress while the child runs and cannot retain
            # inherited pipe handles that would delay a timeout drain. Stdin
            # uses the exact recorded input bytes, so a blocked synchronous
            # pipe write cannot postpone Windows' wait deadline either.
            stdin = captures.enter_context(input_path.open("rb")) if input_path else subprocess.DEVNULL
            stdout = captures.enter_context(paths["stdout"].open("wb", buffering=0))
            stderr = captures.enter_context(paths["stderr"].open("wb", buffering=0))
            job = WindowsJob() if os.name == "nt" else None
            # Windows starts suspended so no compiler/backend can escape the
            # Job before assignment. POSIX creates a dedicated process group.
            options = ({"creationflags": 0x00000004 | 0x08000000}
                       if job else {"start_new_session": True})
            process = subprocess.Popen(list(map(str, command)), stdin=stdin,
                                       stdout=stdout, stderr=stderr,
                                       cwd=cwd, env=env, **options)
            row["owned_root_pid"] = process.pid
            write_json(self.directory / "processes.json", self.rows)
            if job:
                job.assign_and_resume(process)
            process.wait(timeout=self.timeout)
            row.update(status="completed", exit_code=process.returncode, timed_out=False)
        except subprocess.TimeoutExpired:
            row.update(status="timed_out", exit_code=None, timed_out=True)
            terminate_tree(process, job)
            process.wait(timeout=5)
            row["exit_after_tree_cleanup"] = process.returncode
        except BaseException as failure:
            row.update(status="interrupted_or_failed", exit_code=None, interrupted_or_failed=str(failure))
            terminate_tree(process, job)
            if process is not None:
                process.wait(timeout=5)
            raise
        finally:
            # A successful root must not leave background descendants either.
            if job:
                job.close()
            elif process is not None:
                terminate_tree(process, None)
            captures.close()
            row["wall_seconds"] = time.monotonic() - started
            row["finished_utc"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
            for suffix, path in paths.items():
                if path.exists():
                    row[suffix + "_sha256"] = sha256(path)
            write_json(self.directory / "processes.json", self.rows)
            print(f"[behavior process] finished {label} at {row['finished_utc']}: {row['status']}, exit={row.get('exit_code')}, {row['wall_seconds']:.2f}s", flush=True)
        if row["timed_out"]:
            raise TimeoutError(f"owned process exceeded the separate wall-clock guard: {label}")
        # Match communicate(text=True)'s successful UTF-8/universal-newline
        # decoding, while leaving the original capture bytes untouched.
        return subprocess.CompletedProcess(command, process.returncode,
                                           paths["stdout"].read_text(encoding="utf-8"),
                                           paths["stderr"].read_text(encoding="utf-8"))


def terminate_tree(process, job):
    if process is None:
        return
    if job:
        job.terminate()
    else:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass


class WindowsJob:
    """An owned kill-on-close Job, assigned before the suspended root starts."""
    def __init__(self):
        import ctypes
        from ctypes import wintypes
        self.ctypes = ctypes
        self.kernel = ctypes.WinDLL("kernel32", use_last_error=True)
        self.kernel.CreateJobObjectW.argtypes = (ctypes.c_void_p, wintypes.LPCWSTR)
        self.kernel.CreateJobObjectW.restype = wintypes.HANDLE
        self.kernel.SetInformationJobObject.argtypes = (wintypes.HANDLE, ctypes.c_int, ctypes.c_void_p, wintypes.DWORD)
        self.kernel.SetInformationJobObject.restype = wintypes.BOOL
        self.kernel.AssignProcessToJobObject.argtypes = (wintypes.HANDLE, wintypes.HANDLE)
        self.kernel.AssignProcessToJobObject.restype = wintypes.BOOL
        self.kernel.TerminateJobObject.argtypes = (wintypes.HANDLE, wintypes.UINT)
        self.kernel.TerminateJobObject.restype = wintypes.BOOL
        self.kernel.CloseHandle.argtypes = (wintypes.HANDLE,)
        self.kernel.CloseHandle.restype = wintypes.BOOL

        class BasicLimits(ctypes.Structure):
            _fields_ = [("process_time", ctypes.c_int64), ("job_time", ctypes.c_int64),
                        ("flags", wintypes.DWORD), ("min_working", ctypes.c_size_t),
                        ("max_working", ctypes.c_size_t), ("active_processes", wintypes.DWORD),
                        ("affinity", ctypes.c_size_t), ("priority", wintypes.DWORD),
                        ("scheduling", wintypes.DWORD)]

        class ExtendedLimits(ctypes.Structure):
            _fields_ = [("basic", BasicLimits), ("io", ctypes.c_uint64 * 6),
                        ("process_memory", ctypes.c_size_t), ("job_memory", ctypes.c_size_t),
                        ("peak_process_memory", ctypes.c_size_t), ("peak_job_memory", ctypes.c_size_t)]

        self.handle = self.kernel.CreateJobObjectW(None, None)
        if not self.handle:
            raise ctypes.WinError(ctypes.get_last_error())
        limits = ExtendedLimits()
        limits.basic.flags = 0x00002000  # JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
        if not self.kernel.SetInformationJobObject(self.handle, 9, ctypes.byref(limits), ctypes.sizeof(limits)):
            error = ctypes.WinError(ctypes.get_last_error())
            self.close()
            raise error

    def assign_and_resume(self, process):
        if not self.kernel.AssignProcessToJobObject(self.handle, int(process._handle)):
            error = self.ctypes.WinError(self.ctypes.get_last_error())
            process.kill()  # Still suspended: it cannot have created children.
            raise error
        ntdll = self.ctypes.WinDLL("ntdll")
        ntdll.NtResumeProcess.argtypes = (self.ctypes.c_void_p,)
        ntdll.NtResumeProcess.restype = self.ctypes.c_long
        status = ntdll.NtResumeProcess(int(process._handle))
        if status < 0:
            self.terminate()
            raise OSError(f"NtResumeProcess failed: {status:#x}")

    def terminate(self):
        if self.handle and not self.kernel.TerminateJobObject(self.handle, 124):
            raise self.ctypes.WinError(self.ctypes.get_last_error())

    def close(self):
        if self.handle:
            self.kernel.CloseHandle(self.handle)
            self.handle = None


def validate_limits(args):
    import math
    for name in ("window", "steps", "budget", "process_timeout"):
        value = getattr(args, name)
        if not math.isfinite(value) or value <= 0:
            raise ValueError(name + " must be finite and positive")
