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
`harvest.py` does the same for the Python side, reading docstrings, the
doctest and code-block fragments under `docs/`, notebook cells and the test
suites.

```sh
mkdir -p ../cran
for p in sf terra gdalraster stars vapour tmap s2 geos geodata supercells; do
  git clone --depth 1 https://github.com/cran/$p ../cran/$p
done
Rscript expectations/harvest.R ../cran expectations
```

```sh
mkdir -p ../py
for r in rasterio/rasterio corteva/rioxarray geopandas/geopandas \
         geopandas/pyogrio; do
  git clone --depth 1 https://github.com/$r ../py/$(basename $r)
done
python3 expectations/harvest.py ../py expectations
```

They write `inventory.csv` (function, how many calls, how many files, which
kinds of source) and `argument-use.csv` (which argument combinations are
actually used), and the `-python` pair beside them. The arguments are the more useful half: `st_read(dsn)` and
`st_read(dsn, query = )` are different expectations wearing the same name.

`coverage.csv` is the curated part, and the only file here that is a
judgement rather than a measurement. One row per expectation, with where it
was seen, and what adgl does about it: `have`, `gap`, `bug`, `wont` for
something that belongs outside this package, or `elsewhere` for something a
neighbour already owns.

Where the two corpora agree, the expectation is worth more than its count.
Reading past the edge of a raster is `boundless` in rasterio and `extend` in
terra; narrowing fields in the driver is `ignore_fields` in geopandas and
`SetIgnoredFields` in GDAL's own API. Neither corpus alone made those look
like more than one package's habit.

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

## What a second pass taught us about choosing packages

The first pass took the obvious neighbours. The second took packages that
*consume* a spatial object rather than producing one, and it barely paid:
geos, geodata and supercells added 1,590 calls and not one new expectation.
geos round-trips WKB and does geometry, geodata's examples are downloads, and
supercells has one function. They validate that adgl's output is usable by a
wk-native package, which is worth knowing, and nothing else.

pyogrio in the same pass was worth all three several times over, because it is
an IO layer rather than a consumer of one: 520 calls whose argument names are
almost exactly adgl's vector surface. It corroborated the four biggest open
gaps, and its argument ranking is the clearest statement of priority in either
corpus.

So the rule is: harvest what *is* an IO layer, or what calls one heavily.
A package that receives an already-loaded object has nothing to say about
loading it.

## Still to harvest

Issue trackers, where the expectation is usually a complaint and therefore
sharper than an example. The GitHub API is not reachable from the sessions
this was built in, so that stage needs a different route.

Two other seams worth opening: the connection strings and driver names the
examples use, which say what kinds of source people expect to hand a reader
(`/vsicurl/`, `NETCDF:"f":var`, `PG:`, GeoParquet); and GDAL's own autotest
suite, which is the closest thing to a specification of what the library
promises.
