# Expectations

adgl is small on purpose, and the risk of a small API is that it is small in
the wrong places. This directory is the check on that: it reads what the
packages next door actually do in their own documentation, tests and
vignettes, and measures adgl against it.

The reasoning is that a package's worked examples are the closest thing there
is to a written record of what its users expect to be able to do. Nobody
writes an example for something nobody asks for.

Nothing here runs under `R CMD check`. The harvest needs the other packages'
sources and the network; the suite is meant to be read as much as run.

## The three pieces

`harvest.R` clones nothing and installs nothing. Point it at a directory of
package checkouts and it parses every `man/*.Rd` example, every file under
`tests/`, and the R chunks of every vignette, then records each call to one of
that package's own functions along with the argument names the caller used.

```sh
mkdir -p ../cran
for p in sf terra gdalraster stars vapour tmap s2; do
  git clone --depth 1 https://github.com/cran/$p ../cran/$p
done
Rscript expectations/harvest.R ../cran expectations
```

It writes `inventory.csv` (function, how many calls, how many files, which
kinds of source) and `argument-use.csv` (which argument combinations are
actually used). The arguments are the more useful half: `st_read(dsn)` and
`st_read(dsn, query = )` are different expectations wearing the same name.

`coverage.csv` is the curated part, and the only file here that is a
judgement rather than a measurement. One row per expectation, with where it
was seen, and what adgl does about it: `have`, `gap`, or `bug`.

`test-expectations.R` is the suite. Every `have` row has a test that does in
adgl what the harvested call does in its own package, with the original call
in a comment above it. Every `gap` and `bug` row is a named skip, so a run
prints the to-do list.

```sh
Rscript expectations/run.R
```

## What it is not

It is not a compatibility layer and it is not a plan to grow adgl until it
matches sf. A `gap` row is a question, not a commitment: several of them are
things adgl declines on purpose, and the point of writing them down is to
decline them once, in public, rather than repeatedly in private.

## Still to harvest

Issue trackers, where the expectation is usually a complaint and therefore
sharper than an example; and the Python side, `rasterio`, `rioxarray` and
`geopandas`, whose users arrive with the same expectations in different words.
