# What is geoprocessoR?

A package with tools for climate data geoprocessing. It is part of the climate4R Bundle <http://www.meteo.unican.es/climate4R>).

The recommended installation procedure is to use the `install_github` command from the devtools R package (see the installation info in the wiki):

```r
devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/geoprocessoR"))
```
**NOTE:** Note that `transformeR` is a dependency for `geoprocessoR`. Note that `transformeR` also includes illustrative datasets for the `climate4r` framework.

**EXAMPLE:** The following code shows two examples of `climate4r` data projection, for station and gridded data:

```r
library(transformeR)
data("VALUE_Iberia_pr")
plot(getCoordinates(VALUE_Iberia_pr))
grid <- projectGrid(VALUE_Iberia_pr,
                    original.CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                    new.CRS = "+init=epsg:28992")
plot(getCoordinates(grid))

data("EOBS_Iberia_pr")
plot(get2DmatCoordinates(EOBS_Iberia_pr))
grid <- projectGrid(EOBS_Iberia_pr,
                    original.CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                    new.CRS = "+init=epsg:28992")
plot(get2DmatCoordinates(grid))
require(visualizeR)
spatialPlot(climatology(grid))
```

---
References and further information: 

Iturbide et al. (2018) climate4R: An R-based Framework for Climate Data Access, Post-processing and Bias Correction. Submitted to **Environmental Modeling and Software***, http://www.meteo.unican.es/climate4r_paper 

Cofiño et al. (2017) The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. **Climate Services**, http://dx.doi.org/10.1016/j.cliser.2017.07.001.
