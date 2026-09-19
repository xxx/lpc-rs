#!/usr/bin/env python3
"""Record explicitly reviewed apply references after updating ulib code and prose."""

import argparse
import hashlib
import json
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="+", help="reviewed reference paths, relative to the repo root")
    args = parser.parse_args()
    root = Path(__file__).resolve().parent.parent
    record = root / "tests/fixtures/ulib_apply_review.json"
    reviewed = json.loads(record.read_text(encoding="utf-8"))

    for argument in args.paths:
        path = root / argument
        try:
            relative = path.resolve().relative_to(root).as_posix()
        except ValueError:
            parser.error(f"not a repository path: {argument}")
        if not (
            relative.startswith("doc/apply/") and relative.endswith(".md")
            or relative == "doc/efun/parse_add_rule.md"
        ):
            parser.error(f"not an apply reference: {argument}")
        if path.is_file():
            text = path.read_bytes().decode("utf-8").replace("\r\n", "\n")
            reviewed[relative] = hashlib.sha256(text.encode("utf-8")).hexdigest()
        elif relative in reviewed:
            del reviewed[relative]
        else:
            parser.error(f"reference does not exist and was not previously recorded: {argument}")

    record.write_text(json.dumps(reviewed, indent=2, sort_keys=True) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
