"""Harvest what the Python side shows off.

The same idea as harvest.R, in the language the other half of this audience
uses. rasterio, rioxarray and geopandas carry their expectations in docstrings,
in reStructuredText and notebook prose under docs/, and in their test suites.
Their users arrive at adgl with those habits, in different words: a rasterio
user asks for a Window, a vapour user asks for a window, and an sf user asks
for a wkt_filter.

Run it by hand:

    python3 expectations/harvest.py [source-directory] [output-directory]

where source-directory holds one checkout per package:

    for r in rasterio/rasterio corteva/rioxarray geopandas/geopandas; do
      git clone --depth 1 https://github.com/$r <source-directory>/$(basename $r)
    done
"""

import ast
import collections
import csv
import io
import os
import re
import sys
import tokenize

PACKAGES = ("rasterio", "rioxarray", "geopandas")

# Calls worth counting are the ones that reach the library, so a call is kept
# when its dotted name starts with one of these. Bare numpy, pytest and
# builtins are the bulk of any test file and say nothing about expectations.
PREFIXES = {
    "rasterio": ("rasterio", "rio", "src", "dst", "dataset", "ds", "windows",
                 "warp", "features", "mask", "merge", "transform"),
    "rioxarray": ("rioxarray", "rio", "xr", "rds", "xds", "raster"),
    "geopandas": ("geopandas", "gpd", "gdf", "df", "GeoDataFrame",
                  "GeoSeries", "read_file", "to_file"),
}

DOC_SUFFIXES = (".rst", ".md", ".ipynb", ".txt")


def dotted(node):
    """The call's name as written, so rasterio.open and src.read both keep
    the part that says which library is being asked."""
    parts = []
    while isinstance(node, ast.Attribute):
        parts.append(node.attr)
        node = node.value
    if isinstance(node, ast.Name):
        parts.append(node.id)
    elif isinstance(node, ast.Call):
        parts.append("()")
    else:
        return None
    return ".".join(reversed(parts))


def code_blocks_from_docs(path):
    """Fenced and directive-style code out of the prose. Sphinx uses
    `.. code-block:: python` and doctest `>>>`; notebooks keep source as a
    list of lines in JSON, which a regex is enough to reach."""
    try:
        text = open(path, encoding="utf-8", errors="replace").read()
    except OSError:
        return []

    blocks = []
    if path.endswith(".ipynb"):
        for cell in re.findall(r'"source":\s*\[(.*?)\]', text, re.S):
            lines = re.findall(r'"((?:[^"\\]|\\.)*)"', cell)
            blocks.append("".join(l.encode().decode("unicode_escape")
                                  for l in lines))
        return blocks

    # Doctest lines, which is how most of these docs are written.
    doctest = [m.group(1) for m in re.finditer(r"^\s*>>> (.*)$", text, re.M)]
    if doctest:
        blocks.append("\n".join(doctest))

    # Indented blocks under a python code-block directive.
    for match in re.finditer(r"\.\. (?:code-block|sourcecode):: python\n\n"
                             r"((?:[ \t]+.*\n|\n)+)", text):
        blocks.append("\n".join(line.strip() for line in
                                match.group(1).splitlines()))
    return blocks


def docstrings(tree):
    out = []
    for node in ast.walk(tree):
        if isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef,
                             ast.AsyncFunctionDef)):
            text = ast.get_docstring(node)
            if text and ">>>" in text:
                out.append("\n".join(m.group(1) for m in
                                     re.finditer(r"^\s*>>> (.*)$", text, re.M)))
    return out


def calls_in(code, prefixes, sink, package, source, path):
    try:
        tree = ast.parse(code)
    except (SyntaxError, ValueError):
        return
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        name = dotted(node.func)
        if not name:
            continue
        if name.split(".")[0] not in prefixes:
            continue
        kwargs = sorted(kw.arg for kw in node.keywords if kw.arg)
        sink.append((package, source, os.path.basename(path), name,
                     " ".join(kwargs)))


def harvest(package, root, sink):
    prefixes = set(PREFIXES[package])
    for base, dirs, files in os.walk(root):
        dirs[:] = [d for d in dirs if d not in (".git", "build", "dist")]
        for name in files:
            path = os.path.join(base, name)
            rel = os.path.relpath(path, root)
            if name.endswith(".py"):
                try:
                    code = open(path, encoding="utf-8",
                                errors="replace").read()
                except OSError:
                    continue
                source = "test" if "test" in rel else "source"
                calls_in(code, prefixes, sink, package, source, path)
                try:
                    tree = ast.parse(code)
                except (SyntaxError, ValueError):
                    continue
                for block in docstrings(tree):
                    calls_in(block, prefixes, sink, package, "docstring",
                             path)
            elif name.endswith(DOC_SUFFIXES) and "doc" in rel:
                for block in code_blocks_from_docs(path):
                    calls_in(block, prefixes, sink, package, "doc", path)


def main():
    source_dir = sys.argv[1] if len(sys.argv) > 1 else "../py"
    out_dir = sys.argv[2] if len(sys.argv) > 2 else "expectations"

    sink = []
    for package in PACKAGES:
        root = os.path.join(source_dir, package)
        if not os.path.isdir(root):
            print("  %s (missing, skipped)" % package)
            continue
        print("  %s" % package)
        harvest(package, root, sink)

    if not sink:
        raise SystemExit("nothing harvested: is source-directory right?")

    os.makedirs(out_dir, exist_ok=True)

    counts = collections.Counter((p, f) for p, _, _, f, _ in sink)
    files = collections.defaultdict(set)
    sources = collections.defaultdict(set)
    for package, source, path, fun, _ in sink:
        files[(package, fun)].add(path)
        sources[(package, fun)].add(source)

    inventory = os.path.join(out_dir, "inventory-python.csv")
    with open(inventory, "w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["package", "fun", "calls", "files", "sources"])
        for (package, fun), n in counts.most_common():
            writer.writerow([package, fun, n, len(files[(package, fun)]),
                             "+".join(sorted(sources[(package, fun)]))])

    args = collections.Counter((p, f, a) for p, _, _, f, a in sink if a)
    argfile = os.path.join(out_dir, "argument-use-python.csv")
    with open(argfile, "w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["package", "fun", "args", "calls"])
        for (package, fun, a), n in args.most_common():
            writer.writerow([package, fun, a, n])

    print("\n%d calls to %d distinct names" % (len(sink), len(counts)))
    print("wrote %s and %s" % (inventory, argfile))


if __name__ == "__main__":
    main()
