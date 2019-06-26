#' @title Exemplary data for \code{spatialPlotStereo.R}
#'
#' @description This exemplary grid contains monthly mean data of geopotential height at 500hPa from NCEP renalysis, for the year 2000.
#' It has been obtained by using the \code{loadeR} command: 
#' 
#' ncep_hgt500_2000 <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/ncepReanalysis1/ncepReanalysis1_4xDaily.ncml", var = "hgt@500", latLim = c(20,90), years = 2000, time="DD", aggr.d="mean", aggr.m="mean") 
#' @docType data
#' @usage data(ncep_hgt500_2000)
#'
#' @format A grid object.
#' 
#' @source \url{https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html}.
#' @name ncep_hgt500_2000
