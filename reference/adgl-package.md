# adgl: Lazy Spatial Input and Output Through GDAL

Query, filter, read and transform raster and vector sources through GDAL
behind one small set of verbs. A source is opened once and held as a
plan rather than as data, so narrowing it costs nothing until a terminal
verb asks for the result. When a source's own metadata is deficient the
package says so, says what it will cost, and gives the text that would
fix it, and it never applies that fix on the caller's behalf.

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com>
