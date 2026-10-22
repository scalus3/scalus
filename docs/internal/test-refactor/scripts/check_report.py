#!/usr/bin/env python3
"""Checks P2 cluster reports against report.schema.json and the repository ([P2-7]..[P2-9]).

Validates the JSON Schema subset the schema uses (type, required, additionalProperties, enum,
pattern, minItems, minimum, $ref), then checks that every file:line points into an existing file
and that ids are unique and every main-bug finding names its findings.md entry.

Usage: check_report.py reports/*.json   (run from the repository root)
"""
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SCHEMA = json.load(open(os.path.join(HERE, "..", "report.schema.json")))
TYPES = {"object": dict, "array": list, "string": str, "integer": int, "number": (int, float)}


def validate(value, schema, path, errors):
    if "$ref" in schema:
        schema = SCHEMA["$defs"][schema["$ref"].split("/")[-1]]
    t = schema.get("type")
    if t and (not isinstance(value, TYPES[t]) or (t in ("integer", "number") and isinstance(value, bool))):
        errors.append(f"{path}: expected {t}")
        return
    if "enum" in schema and value not in schema["enum"]:
        errors.append(f"{path}: {value!r} not in {schema['enum']}")
    if "pattern" in schema and not re.search(schema["pattern"], value):
        errors.append(f"{path}: {value!r} does not match {schema['pattern']}")
    if "minimum" in schema and value < schema["minimum"]:
        errors.append(f"{path}: {value} < {schema['minimum']}")
    if isinstance(value, dict):
        for key in schema.get("required", []):
            if key not in value:
                errors.append(f"{path}: missing {key}")
        props = schema.get("properties", {})
        for key, v in value.items():
            if key in props:
                validate(v, props[key], f"{path}.{key}", errors)
            elif schema.get("additionalProperties") is False:
                errors.append(f"{path}: unknown field {key}")
    if isinstance(value, list):
        if len(value) < schema.get("minItems", 0):
            errors.append(f"{path}: fewer than {schema['minItems']} items")
        for i, v in enumerate(value):
            validate(v, schema.get("items", {}), f"{path}[{i}]", errors)


def check_ref(ref, path, errors, line_counts):
    file, _, ranges = ref.partition(":")
    if not os.path.isfile(file):
        errors.append(f"{path}: no such file {file}")
        return
    if file not in line_counts:
        with open(file, "rb") as f:
            line_counts[file] = sum(1 for _ in f)
    top = max(int(n) for n in re.findall(r"[0-9]+", ranges))
    if top > line_counts[file]:
        errors.append(f"{path}: {ref} is past the end ({line_counts[file]} lines)")


def check(report_path):
    report = json.load(open(report_path))
    errors = []
    validate(report, SCHEMA, "$", errors)
    line_counts = {}
    for i, b in enumerate(report.get("behaviors", [])):
        for j, ref in enumerate(b.get("tests", [])):
            check_ref(ref, f"$.behaviors[{i}].tests[{j}]", errors, line_counts)
    ids = set()
    for i, f in enumerate(report.get("findings", [])):
        if f.get("id") in ids:
            errors.append(f"$.findings[{i}]: duplicate id {f.get('id')}")
        ids.add(f.get("id"))
        for j, ref in enumerate(f.get("evidence", [])):
            check_ref(ref, f"$.findings[{i}].evidence[{j}]", errors, line_counts)
        if f.get("scope") == "main-bug" and "findingsLog" not in f:
            errors.append(f"$.findings[{i}]: main-bug needs findingsLog")
        if f.get("verdict") == "NARROWED" and "verification" not in f:
            errors.append(f"$.findings[{i}]: NARROWED needs verification")
    for e in errors:
        print(f"{report_path}: {e}")
    return not errors


if __name__ == "__main__":
    results = [check(p) for p in sys.argv[1:]]
    sys.exit(0 if results and all(results) else 1)
