# What is geoprocessoR?

A package with tools for climate data geoprocessing. 

This package is part of the [climate4R bundle](http://www.meteo.unican.es/climate4r), which is formed by the core packages `loadeR`, `transformeR`, `downscaleR` and `visualizeR`.

The recommended installation procedure is to use the `install_github` command from the devtools R package (see the installation info in the wiki):

```r
devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/geoprocessoR"))
```
**NOTE:** Note that `transformeR` is a dependency for `geoprocessoR`. It also requires rgdal: `install.packages("rgdal")`. Note that `transformeR` also includes illustrative datasets for the `climate4R` framework.

**EXAMPLE:** The following code shows an example of `climate4R` data projection for gridded data (see the [Wiki](https://github.com/SantanderMetGroup/geoprocessoR/wiki) for more worked examples).

```r
library(transformeR)
library(geoprocesoR)

data("EOBS_Iberia_pr")
plot(get2DmatCoordinates(EOBS_Iberia_pr))

grid <- projectGrid(EOBS_Iberia_pr,
                    original.CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                    new.CRS = "+init=epsg:28992")
plot(get2DmatCoordinates(grid))

# Use visualizeR to plot the mean climatology of the original and projected grids:
# devtools::install_github("SantanderMetGroup/visualizeR")
library(visualizeR)
spatialPlot(climatology(EOBS_Iberia_pr))
spatialPlot(climatology(grid))
```

---
References and further information: 

Iturbide et al. (2019) The R-based climate4R open framework for Reproducible Climate Data Access and Post-processing. **Environmental Modelling and Software** 111: 42-54.  https://doi.org/10.1016/j.envsoft.2018.09.009.


Cofiño et al. (2017) The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. **Climate Services**, http://dx.doi.org/10.1016/j.cliser.2017.07.001.
