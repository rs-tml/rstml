#!/usr/bin/env python3
"""Compare the library APIs and render an advisory next-version badge."""

import argparse
import json
from pathlib import Path
import subprocess


def predict_version():
    baseline = subprocess.check_output(
        ["git", "describe", "--tags", "--match", "v[0-9]*", "--abbrev=0", "--first-parent"],
        text=True,
    ).strip()
    breaking = False
    for features in ("--default-features", "--all-features"):
        command = [
            "cargo", "semver-checks", "--package", "rstml", "--package", "rstml-control-flow",
            "--baseline-rev", baseline, "--release-type", "patch", features,
        ]
        result = subprocess.run(command)
        if result.returncode == 100:
            breaking = True
        elif result.returncode != 0:
            raise subprocess.CalledProcessError(result.returncode, command)

    metadata = json.loads(subprocess.check_output(
        ["cargo", "metadata", "--no-deps", "--format-version", "1"], text=True,
    ))
    current = next(package["version"] for package in metadata["packages"] if package["name"] == "rstml")
    release = current.split("+", 1)[0]
    major, minor, patch = map(int, release.split("-", 1)[0].split("."))
    if breaking and major > 0:
        major, minor, patch = major + 1, 0, 0
    elif breaking and minor > 0:
        minor, patch = minor + 1, 0
    elif "-" not in release:
        patch += 1
    version = f"{major}.{minor}.{patch}"
    print(f"Future version: {version} (compared with {baseline})")
    return version


def write_badge(version, output):
    label = f"v{version}"
    value_width = len(label) * 7 + 16
    width = 50 + value_width
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(f'''<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="20" role="img" aria-label="future: {label}">
  <title>future: {label}</title>
  <clipPath id="rounded"><rect width="{width}" height="20" rx="3"/></clipPath>
  <g clip-path="url(#rounded)">
    <rect width="50" height="20" fill="#555"/>
    <rect x="50" width="{value_width}" height="20" fill="#007ec6"/>
  </g>
  <g fill="#fff" text-anchor="middle" font-family="Verdana, sans-serif" font-size="11">
    <text x="25" y="14">future</text>
    <text x="{50 + value_width / 2}" y="14">{label}</text>
  </g>
</svg>
''', encoding="utf-8")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="Path to the generated SVG badge")
    arguments = parser.parse_args()
    write_badge(predict_version(), arguments.output)
