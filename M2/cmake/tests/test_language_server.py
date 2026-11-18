"""Smoke-test the built/installed LSP launcher and its sibling M2 selection."""
import argparse
import json
import os
from pathlib import Path
import selectors
import signal
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("launcher", type=Path)
args = parser.parse_args()

with tempfile.TemporaryDirectory(prefix="m2-lsp-") as directory, tempfile.TemporaryFile() as log:
    # A different M2 on PATH must not override the executable beside the launcher.
    wrong_m2 = Path(directory) / "M2"
    wrong_m2.write_text("#!/bin/sh\necho 'wrong M2 selected from PATH' >&2\nexit 99\n")
    wrong_m2.chmod(0o755)
    env = dict(os.environ, PATH=directory + os.pathsep + os.environ.get("PATH", ""))
    process = subprocess.Popen([str(args.launcher.resolve())], env=env,
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                               stderr=log, start_new_session=True)
    selector = selectors.DefaultSelector()
    selector.register(process.stdout, selectors.EVENT_READ)
    buffer = b""

    def send(method, params=None, request_id=None):
        message = dict(jsonrpc="2.0", method=method)
        if params is not None:
            message["params"] = params
        if request_id is not None:
            message["id"] = request_id
        data = json.dumps(message).encode()
        process.stdin.write(b"Content-Length: " + str(len(data)).encode() + b"\r\n\r\n" + data)
        process.stdin.flush()

    def receive(request_id):
        global buffer
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            if b"\r\n\r\n" in buffer:
                header, body = buffer.split(b"\r\n\r\n", 1)
                size = int(next(line.split(b":", 1)[1] for line in header.split(b"\r\n")
                                if line.lower().startswith(b"content-length:")))
                if len(body) >= size:
                    reply = json.loads(body[:size])
                    buffer = body[size:]
                    if reply.get("id") == request_id:
                        return reply
                    continue
            if selector.select(0.1):
                data = os.read(process.stdout.fileno(), 65536)
                if not data:
                    raise RuntimeError("language server exited before responding")
                buffer += data
        raise TimeoutError("language server did not respond")

    try:
        send("initialize", {}, 1)
        assert "completionProvider" in receive(1)["result"]["capabilities"]
        send("initialized", {})
        uri = (Path(directory) / "example.m2").as_uri()
        send("textDocument/didOpen", {"textDocument": {
            "uri": uri, "text": "ring\n", "languageId": "M2", "version": 1}})
        send("textDocument/completion", {"textDocument": {"uri": uri},
                                        "position": {"line": 0, "character": 2}}, 2)
        completions = receive(2)["result"]
        assert completions and all(item["label"].startswith("ring") for item in completions)
        send("shutdown", {}, 3)
        assert receive(3)["result"] is None
        send("exit")
        print("PASS: LSP initialization, completion, and sibling M2 selection")
    except Exception:
        log.seek(0)
        print(log.read().decode(errors="replace"))
        raise
    finally:
        # Own the process group, including the shell launcher's child.
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        selector.close()
