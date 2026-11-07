#!/usr/bin/env python3
"""Build and run the committed fuzz targets with curated and accumulated inputs."""

import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parent.parent
PROJECTS = {
    "core": (ROOT / "fuzz", ("compiler", "save_format", "save_round_trip", "grammar", "formatting", "stm")),
    "telnet": (ROOT / "lpc-rs-telnet/fuzz", ("feed", "session")),
}


def host_triple():
    return next(line.split(": ", 1)[1] for line in subprocess.check_output(
        ["rustc", "+nightly", "-vV"], text=True).splitlines() if line.startswith("host: "))


def run(command, environment, log):
    print("+", " ".join(map(str, command)), flush=True)
    with log.open("w") as output:
        result = subprocess.run(command, cwd=ROOT, env=environment, stdout=output, stderr=subprocess.STDOUT)
    lines = log.read_text(errors="replace").splitlines()
    print("\n".join(lines[-40:] if result.returncode else lines[-5:]), flush=True)
    result.check_returncode()


def coverage(target, directory, environment):
    build_directory = Path(environment["CARGO_TARGET_DIR"]) / "coverage"
    run(
        ["cargo", "+nightly", "fuzz", "coverage", "--fuzz-dir", directory,
         "--target-dir", build_directory,
         "--codegen-units", "16", target, directory / "corpus" / target, directory / "seeds" / target],
        environment, directory / "logs" / f"{target}-coverage.log",
    )
    sysroot = Path(subprocess.check_output(["rustc", "+nightly", "--print", "sysroot"], text=True).strip())
    host = host_triple()
    llvm = sysroot / "lib/rustlib" / host / "bin/llvm-cov"
    binary = build_directory / host / "release" / target
    output = directory / "coverage" / target
    sources = sorted((ROOT / "lpc-rs-telnet/src").glob("*.rs")) if target in PROJECTS["telnet"][1] else [
        file for parent in (ROOT / "src", ROOT / "lpc-rs-core/src", ROOT / "lpc-rs-utils/src")
        for file in sorted(parent.rglob("*.rs")) if "/fuzzing/" not in file.as_posix()
    ]
    with (output / "report.txt").open("w") as report:
        subprocess.run([llvm, "report", binary, f"-instr-profile={output / 'coverage.profdata'}", *sources],
                       cwd=ROOT, env=environment, stdout=report, check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", choices=PROJECTS, required=True)
    parser.add_argument("--seconds", type=int, default=20)
    parser.add_argument("--target", action="append")
    parser.add_argument("--coverage", action="store_true")
    args = parser.parse_args()
    if not 1 <= args.seconds <= 3600:
        parser.error("--seconds must be between 1 and 3600")
    directory, available = PROJECTS[args.project]
    targets = args.target or available
    if any(target not in available for target in targets):
        parser.error("--target must name a target in the selected project")
    (directory / "logs").mkdir(exist_ok=True)
    environment = os.environ.copy()
    environment["CARGO_TARGET_DIR"] = str(ROOT / "target/fuzz")
    run(["cargo", "+nightly", "fetch", "--locked", "--target", host_triple(),
         "--manifest-path", directory / "Cargo.toml"],
        environment, directory / "logs/fetch.log")
    environment["CARGO_NET_OFFLINE"] = "true"
    run(["cargo", "+nightly", "fmt", "--manifest-path", directory / "Cargo.toml", "--check"],
        environment, directory / "logs/fmt.log")
    run(["cargo", "+nightly", "clippy", "--manifest-path", directory / "Cargo.toml",
         "--all-targets", "--locked", "--", "-D", "warnings"],
        environment, directory / "logs/clippy.log")
    run(["cargo", "+nightly", "fuzz", "build", "--fuzz-dir", directory, "--codegen-units", "16"],
        environment, directory / "logs/build.log")
    for target in targets:
        corpus = directory / "corpus" / target
        corpus.mkdir(parents=True, exist_ok=True)
        run(["cargo", "+nightly", "fuzz", "run", "--fuzz-dir", directory, "--codegen-units", "16",
             target, corpus, directory / "seeds" / target, "--", f"-max_total_time={args.seconds}",
             "-max_len=32768", "-timeout=10", "-rss_limit_mb=2048"],
            environment, directory / "logs" / f"{target}.log")
    if args.coverage:
        for target in targets:
            coverage(target, directory, environment)


if __name__ == "__main__":
    main()
