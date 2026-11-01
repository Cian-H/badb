#!/usr/bin/env python3
import os
import re

mapping = {
    "test-guardrail.scm": "policies-guardrail",
    "test-theme.scm": "surfaces-theme",
    "test-spec-tools.scm": "surfaces-spec-tools",
    "test-tui.scm": "surfaces-tui",
    "test-model.scm": "providers-model",
    "test-mcp.scm": "capabilities-mcp",
    "test-mcp-server.scm": "capabilities-mcp-server",
    "test-history-tools.scm": "capabilities-history-tools",
    "test-context.scm": "runtimes-context",
    "test-prompt.scm": "runtimes-prompt",
    "test-react.scm": "runtimes-react",
    "test-react-stream.scm": "runtimes-react-stream",
    "test-rollback.scm": "runtimes-rollback",
    "test-synthesis.scm": "runtimes-synthesis",
    "test-verify.scm": "runtimes-verify",
    "test-config.scm": "foundation-config",
    "test-util.scm": "foundation-util",
    "test-log.scm": "foundation-log",
    "test-srfi-253.scm": "foundation-srfi-253"
}

os.makedirs("test/specs", exist_ok=True)

for src_file, feature_id in mapping.items():
    src_path = os.path.join("test", src_file)
    dst_path = os.path.join("test", "specs", f"{feature_id}.geis.scm")
    with open(src_path, "r", encoding="utf-8") as f:
        content = f.read()

    # Ensure (badb spec) is imported
    if "#:use-module (badb spec)" not in content and "(badb spec)" not in content:
        content = re.sub(r"(#:use-module\s+\([^\)]+\))", r"\1\n  #:use-module (badb spec)", content, count=1)

    # In test-spec-tools, avoid replacing fixtures inside string literals
    if src_file == "test-spec-tools.scm":
        content = re.sub(r'\(test-begin\s+"spec-tools"\)', f'(spec-begin "{feature_id}")', content)
        content = re.sub(r'\(test-end\s+"spec-tools"\)', '(spec-end)\n(spec-exit)', content)
    else:
        # Replace first top-level test-begin
        content = re.sub(r'\(test-begin(?:\s+[^)]+)?\)', f'(spec-begin "{feature_id}")', content, count=1)
        # Replace last test-end
        parts = content.rsplit("(test-end", 1)
        if len(parts) == 2:
            after = parts[1]
            close_idx = after.find(")")
            content = parts[0] + "(spec-end)\n(spec-exit)" + after[close_idx+1:]

    with open(dst_path, "w", encoding="utf-8") as f:
        f.write(content)
    print(f"Generated {dst_path}")
