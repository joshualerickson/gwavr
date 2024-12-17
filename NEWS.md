# gwavr 0.3.2 (2024-12-17)  

* Added new functions:  `get_noaatlas()`, `get_noaatlas_png()`, `get_noaatlas_interactively()`.  

* Updated URL's in non-exported NLDI functions: `get_NLDI()` and `get_NLDI_catchments()`. Thanks to @dblodgett-usgs for the pull request with changes to the URL's.  

# gwavr 0.3.1  

* Updated `get_nhdplus_interactively()` to account for deprecation of functions in nhdplusTools package.  

* Updated `get_basin_interactively()` to allow for some deleting.  

# gwavr 0.3.0  

* Added new functions:  `get_streamnetwork_interactively()`, `get_usgs_iv_interactively()`, `get_usgs_dv_interactively()`.  

* Updated `get_basin_interactively()` to take a inputted DEM as an option.  

* Added a new dependency: 'htmlwidgets'.  

# gwavr 0.2.1  

* Added more hydrologic unit (HUC) options via nhdplusTools to function `get_nhdplus_interactively()`.

# gwavr 0.2.0  

* Added a new function `get_basin_interactively()` and new modules `basinModUI` and `basinMod`.
* Added three new dependencies: 'terra', 'elevatr', and 'whitebox'.

# gwavr 0.1.0  

* Added a `NEWS.md` file to track changes to the package.  
* First submission to CRAN
