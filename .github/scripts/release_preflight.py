#!/usr/bin/env python3

"""
Minimal release preflight for loadeR.

This script is meant to run from a manual GitHub Action before publishing a
draft release. It checks a few basic release invariants on the candidate
branch.

It also emits non-blocking warnings about the development branch, so the team
can keep a simple and visible gitflow:

- main carries the next public release
- devel should move ahead afterwards as a development version

The output is intentionally formatted as a release checklist, not as a raw log,
so the result is easy to review before publishing. The checklist currently
contains these checks:

- Release is being prepared from the main branch
- All tracked changes are committed
- Draft release tag matches the latest commit on the release branch
- DESCRIPTION does not use a development version
- DESCRIPTION date exists, is valid and matches today
- NEWS contains an entry with the release version and date
- Release version is greater than the previous release
- DESCRIPTION in devel is greater and uses a development version

Each checklist line uses one of these states:

- [OK] means the check passed
- [WARN] means the release can still proceed, but something should be reviewed
- [ERROR] means the release candidate is inconsistent and should be fixed first
"""

import datetime
import os
import re
import subprocess
import sys
from string import Template
from pathlib import Path


release_branch = os.environ["RELEASE_BRANCH"]
devel_branch = os.environ.get("DEVEL_BRANCH", "devel").strip()

errors = []
warnings = []
oks = []
checks = []


def run(*args, check=True):
    result = subprocess.run(args, capture_output=True, text=True)
    if check and result.returncode != 0:
        raise RuntimeError(
            f"Command failed ({' '.join(args)}):\n{result.stdout}\n{result.stderr}"
        )
    return result


def add_ok(label, message):
    checks.append(("OK", label, message))
    oks.append(message)


def add_warn(label, message):
    checks.append(("WARN", label, message))
    warnings.append(message)


def add_error(label, message):
    checks.append(("ERROR", label, message))
    errors.append(message)


def parse_dcf(path):
    data = {}
    current = None
    for raw_line in Path(path).read_text(encoding="utf-8").splitlines():
        if raw_line.startswith((" ", "\t")) and current:
            data[current] += "\n" + raw_line.strip()
            continue
        if ":" not in raw_line:
            continue
        key, value = raw_line.split(":", 1)
        current = key.strip()
        data[current] = value.strip()
    return data


def parse_version(version):
    base = re.sub(r"\.9000$", "", version)
    try:
        return tuple(int(part) for part in base.split("."))
    except ValueError:
        return None


def format_version(parts):
    return ".".join(str(part) for part in parts)


def format_news_date(date_iso):
    try:
        return datetime.date.fromisoformat(date_iso).strftime("%d %b %Y")
    except ValueError:
        return None


CHECK_RELEASE_BRANCH = f"Release is being prepared from the {release_branch} branch"
CHECK_WORKING_TREE = "All tracked changes are committed"
CHECK_RELEASE_TAG = (
    f"Draft release tag matches the latest commit on the {release_branch} branch"
)
CHECK_DESC_VERSION = "`DESCRIPTION` does not use a development version"
CHECK_DESC_DATE = "`DESCRIPTION` date exists, is valid and matches today"
CHECK_NEWS_ENTRY = "`NEWS` contains an entry with the release version and date"
CHECK_VERSION_ORDER = "Release version is greater than the previous release"
CHECK_DEVEL_VERSION = "`DESCRIPTION` in devel is greater and uses a development version"

SUMMARY_TEMPLATE = Template(
    """# 🚦 Release preflight for `${release_branch}`

- Expected release branch: `${release_branch}`
- Version in `DESCRIPTION`: `${version}`
- Development branch checked: `${devel_branch}`

## ✅ Checklist
${checklist}

## 🧾 Final summary

| Result | Count |
| --- | ---: |
| 🔴 Errors | `${error_count}` |
| 🟡 Warnings | `${warning_count}` |

${warning_note}"""
)


current_branch = run("git", "rev-parse", "--abbrev-ref", "HEAD").stdout.strip()
if current_branch != release_branch:
    add_error(
        CHECK_RELEASE_BRANCH,
        f"Checked out branch is `{current_branch}`, expected `{release_branch}`.",
    )
else:
    add_ok(CHECK_RELEASE_BRANCH, f"Checked out release branch `{release_branch}`.")

status = run("git", "status", "--porcelain", "--untracked-files=no").stdout.strip()
if status:
    add_error(CHECK_WORKING_TREE, "Repository has tracked changes after checkout.")
else:
    add_ok(CHECK_WORKING_TREE, "Working tree is clean for tracked files.")

desc = parse_dcf("DESCRIPTION")
version = desc.get("Version", "")
date_value = desc.get("Date", "")

if not version:
    add_error(CHECK_DESC_VERSION, "DESCRIPTION is missing Version.")
elif version.endswith(".9000"):
    add_error(
        CHECK_DESC_VERSION,
        f"DESCRIPTION version `{version}` still looks like a development version.",
    )
else:
    add_ok(CHECK_DESC_VERSION, f"DESCRIPTION version is `{version}`.")

tag_name = f"v{version}" if version else ""
if not tag_name:
    add_warn(
        CHECK_RELEASE_TAG,
        "Could not derive the draft release tag name from `DESCRIPTION` Version.",
    )
else:
    tag_ref = run("git", "rev-parse", "-q", "--verify", f"refs/tags/{tag_name}", check=False)
    if tag_ref.returncode != 0:
        add_warn(
            CHECK_RELEASE_TAG,
            f"Tag `{tag_name}` has not been created yet, so this check cannot be completed.",
        )
    else:
        tag_commit = run("git", "rev-list", "-n", "1", tag_name).stdout.strip()
        branch_head = run("git", "rev-parse", "HEAD").stdout.strip()
        if tag_commit == branch_head:
            add_ok(
                CHECK_RELEASE_TAG,
                f"Tag `{tag_name}` matches the latest commit on `{release_branch}`.",
            )
        else:
            short_tag_commit = run("git", "rev-parse", "--short", tag_commit).stdout.strip()
            short_branch_head = run("git", "rev-parse", "--short", branch_head).stdout.strip()
            add_error(
                CHECK_RELEASE_TAG,
                f"Tag `{tag_name}` points to `{short_tag_commit}`, "
                f"but the latest commit on `{release_branch}` is `{short_branch_head}`.",
            )

today = datetime.date.today().isoformat()
if not date_value:
    add_error(CHECK_DESC_DATE, "DESCRIPTION is missing Date.")
else:
    try:
        parsed_date = datetime.date.fromisoformat(date_value)
        if parsed_date.isoformat() != today:
            add_warn(
                CHECK_DESC_DATE,
                f"DESCRIPTION Date is `{parsed_date.isoformat()}`, while today is `{today}`."
            )
        else:
            add_ok(CHECK_DESC_DATE, f"DESCRIPTION Date matches today (`{today}`).")
    except ValueError:
        add_error(
            CHECK_DESC_DATE,
            f"DESCRIPTION Date `{date_value}` is not a valid ISO date.",
        )

news_text = Path("NEWS").read_text(encoding="utf-8")
news_date = format_news_date(date_value) if date_value else None
if news_date is None:
    add_error(
        CHECK_NEWS_ENTRY,
        "Could not derive the expected NEWS heading date from DESCRIPTION Date.",
    )
else:
    release_heading = f"## v{version} ({news_date})"
    if release_heading not in news_text:
        add_error(CHECK_NEWS_ENTRY, f"NEWS does not contain heading `{release_heading}`.")
    else:
        add_ok(CHECK_NEWS_ENTRY, f"NEWS contains heading `{release_heading}`.")

release_parts = parse_version(version)
if release_parts is None:
    add_error(
        CHECK_VERSION_ORDER,
        f"Could not parse DESCRIPTION version `{version}`.",
    )
else:
    tag_output = run("git", "tag", "--list", "v*").stdout.splitlines()
    previous_versions = []
    for tag in tag_output:
        match = re.fullmatch(r"v(\d+(?:\.\d+)*)", tag.strip())
        if not match:
            continue
        tag_parts = parse_version(match.group(1))
        if tag_parts is None:
            continue
        previous_versions.append(tag_parts)

    distinct_previous_versions = [parts for parts in previous_versions if parts != release_parts]
    if not distinct_previous_versions:
        add_warn(
            CHECK_VERSION_ORDER,
            "Could not find a previous release tag distinct from the current version.",
        )
    else:
        previous_release = max(distinct_previous_versions)
        if release_parts > previous_release:
            add_ok(
                CHECK_VERSION_ORDER,
                f"DESCRIPTION version `{version}` is greater than previous release `v{format_version(previous_release)}`."
            )
        else:
            add_error(
                CHECK_VERSION_ORDER,
                f"DESCRIPTION version `{version}` is not greater than previous release `v{format_version(previous_release)}`."
            )

remote_devel_check = run(
    "git", "show-ref", "--verify", f"refs/remotes/origin/{devel_branch}", check=False
)
if remote_devel_check.returncode != 0:
    add_warn(
        CHECK_DEVEL_VERSION,
        f"Could not inspect origin/{devel_branch}.",
    )
else:
    devel_description = run(
        "git", "show", f"origin/{devel_branch}:DESCRIPTION", check=False
    )
    if devel_description.returncode != 0:
        add_warn(
            CHECK_DEVEL_VERSION,
            f"Could not read DESCRIPTION from origin/{devel_branch}.",
        )
    else:
        devel_desc_path = Path(".github/.tmp_devel_DESCRIPTION")
        devel_desc_path.write_text(devel_description.stdout, encoding="utf-8")
        devel_desc = parse_dcf(devel_desc_path)
        devel_desc_path.unlink()

        devel_version = devel_desc.get("Version", "")
        if not devel_version:
            add_warn(
                CHECK_DEVEL_VERSION,
                f"{devel_branch} DESCRIPTION is missing Version.",
            )
        else:
            devel_parts = parse_version(devel_version)
            if devel_parts is None:
                add_warn(
                    CHECK_DEVEL_VERSION,
                    f"{devel_branch} version `{devel_version}` could not be parsed."
                )
            else:
                if devel_version == version:
                    add_warn(
                        CHECK_DEVEL_VERSION,
                        f"{devel_branch} version matches release version `{version}`. "
                        "It should normally move ahead after a release."
                    )
                elif devel_parts <= release_parts:
                    add_warn(
                        CHECK_DEVEL_VERSION,
                        f"{devel_branch} version `{devel_version}` is not ahead of "
                        f"release version `{version}`."
                    )
                else:
                    add_ok(
                        CHECK_DEVEL_VERSION,
                        f"{devel_branch} version `{devel_version}` is ahead of the "
                        f"release candidate."
                    )

                if not devel_version.endswith(".9000"):
                    add_warn(
                        CHECK_DEVEL_VERSION,
                        f"{devel_branch} version `{devel_version}` does not end in "
                        "`.9000`, so it may not be clearly marked as a development version."
                    )

check_order = [
    CHECK_RELEASE_BRANCH,
    CHECK_WORKING_TREE,
    CHECK_RELEASE_TAG,
    CHECK_DESC_VERSION,
    CHECK_DESC_DATE,
    CHECK_NEWS_ENTRY,
    CHECK_VERSION_ORDER,
    CHECK_DEVEL_VERSION,
]

ordered_checks = []
for expected_label in check_order:
    ordered_checks.extend([item for item in checks if item[1] == expected_label])

remaining_checks = [item for item in checks if item not in ordered_checks]
ordered_checks.extend(remaining_checks)

checklist_lines = []
for status, label, message in ordered_checks:
    if status == "OK":
        status_badge = "🟢"
    elif status == "WARN":
        status_badge = "🟡"
    else:
        status_badge = "🔴"
    checklist_lines.append(f"- {status_badge} **{label}**: {message}")

warning_note = ""
if warnings:
    warning_note = (
        "> [!WARNING]\n"
        "> Warnings are advisory, but they should be reviewed before publishing the release."
    )

summary = SUMMARY_TEMPLATE.substitute(
    release_branch=release_branch,
    version=version or "missing",
    devel_branch=devel_branch,
    checklist="\n".join(checklist_lines),
    error_count=len(errors),
    warning_count=len(warnings),
    warning_note=warning_note,
)
print(summary)

summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
if summary_path:
    with open(summary_path, "a", encoding="utf-8") as fh:
        fh.write(summary + "\n")

if errors:
    sys.exit(1)