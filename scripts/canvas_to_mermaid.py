#!/usr/bin/env python3
"""Derive a GitHub-renderable Mermaid flowchart from an Obsidian .canvas file.

GitHub cannot render .canvas files (Obsidian's JSON Canvas format), but it
renders Mermaid natively in markdown. This script reads a canvas and emits a
simplified Mermaid flowchart: text nodes become boxes (labelled by their first
line, markdown stripped), file nodes become rounded boxes (basename), groups
become subgraphs (membership by geometric containment), and canvas edges
become arrows with their labels. Spatial layout, colours, node prose, and
embedded images are deliberately dropped - the canvas remains the full
version; this is the README-visible summary.

Usage:
  python3 scripts/canvas_to_mermaid.py canvases/runway_paper_pipeline.canvas
  python3 scripts/canvas_to_mermaid.py <canvas> --update-readme README.md

--update-readme replaces the block between the markers
<!-- pipeline-map:start --> and <!-- pipeline-map:end --> in the given file.
"""

import json
import os
import re
import sys

MAX_LABEL = 60
START_MARK = "<!-- pipeline-map:start -->"
END_MARK = "<!-- pipeline-map:end -->"


def mermaid_id(raw):
    return re.sub(r"[^A-Za-z0-9]", "_", raw)


def label_of(node):
    if node["type"] == "file":
        return os.path.basename(node["file"])
    first = node.get("text", "").strip().splitlines()[0] if node.get("text") else ""
    first = re.sub(r"[*`]", "", first).strip()
    if len(first) > MAX_LABEL:
        first = first[: MAX_LABEL - 1].rstrip() + "…"
    return first.replace('"', "'") or node["id"]


def center_in(node, group):
    cx = node["x"] + node["width"] / 2
    cy = node["y"] + node["height"] / 2
    return (group["x"] <= cx <= group["x"] + group["width"]
            and group["y"] <= cy <= group["y"] + group["height"])


def to_mermaid(canvas):
    nodes = canvas.get("nodes", [])
    edges = canvas.get("edges", [])
    groups = [n for n in nodes if n["type"] == "group"]
    plain = [n for n in nodes if n["type"] != "group"]

    member_of = {}
    for n in plain:
        # smallest containing group wins, so nested groups behave
        holders = [g for g in groups if center_in(n, g)]
        if holders:
            g = min(holders, key=lambda g: g["width"] * g["height"])
            member_of[n["id"]] = g["id"]

    lines = ["flowchart LR"]
    for g in groups:
        gl = (g.get("label") or g["id"]).replace('"', "'")
        lines.append(f'  subgraph {mermaid_id(g["id"])}["{gl}"]')
        for n in plain:
            if member_of.get(n["id"]) == g["id"]:
                shape = ("(", ")") if n["type"] == "file" else ("[", "]")
                lines.append(
                    f'    {mermaid_id(n["id"])}{shape[0]}"{label_of(n)}"{shape[1]}')
        lines.append("  end")
    for n in plain:
        if n["id"] not in member_of:
            shape = ("(", ")") if n["type"] == "file" else ("[", "]")
            lines.append(f'  {mermaid_id(n["id"])}{shape[0]}"{label_of(n)}"{shape[1]}')
    for e in edges:
        arrow = "-->"
        el = e.get("label", "").replace('"', "'")
        link = f'{arrow}|"{el}"|' if el else arrow
        lines.append(f'  {mermaid_id(e["fromNode"])} {link} {mermaid_id(e["toNode"])}')
    return "\n".join(lines)


def main():
    args = [a for a in sys.argv[1:]]
    if not args:
        sys.exit(__doc__)
    canvas_path = args[0]
    with open(canvas_path) as f:
        mermaid = to_mermaid(json.load(f))
    block = f"{START_MARK}\n```mermaid\n{mermaid}\n```\n{END_MARK}"

    if "--update-readme" in args:
        readme_path = args[args.index("--update-readme") + 1]
        with open(readme_path) as f:
            text = f.read()
        pattern = re.compile(re.escape(START_MARK) + r".*?" + re.escape(END_MARK),
                             re.DOTALL)
        if not pattern.search(text):
            sys.exit(f"markers not found in {readme_path}")
        with open(readme_path, "w") as f:
            f.write(pattern.sub(lambda _: block, text))
        print(f"updated {readme_path} from {canvas_path}")
    else:
        print(block)


if __name__ == "__main__":
    main()
