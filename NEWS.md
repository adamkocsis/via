# Change log of the R package 'via'

# via 0.2.0. 
### Added 
- Class: `SfcArray` between the inheritance order of `XArray` -> `SfArray`, to allow geometry-only arrays
- casting functions between `SfcArray`, `SfArray`, and `XArray`

### Changed
- The `paleocoastlines` demo data is now an `SfcArray`-class object. Column names changed to `c("margin", "coast")`. 

### Fixed
- alternative generic of `st_bbox()` changed to be a function of `obj` instead of `x` for generic-method consistency.
- removed deprecated documentation reference to 'raster'


# via 0.1.0. - 2023-03-15
### Added 
- Classes: XArray, SfArray, RasterArray
- Suggests: terra, sf

### Removed
- SpatialArray
- Some RasterArray methods as they require more development to make them compatible with terra

### Fixed
- Class organization
- Omitted dependencies on raster and sp/rgdal

* * *


# via 0.0.1 - 2022-08-13

### Initialize 
Material moved from the chronosphere package. 
