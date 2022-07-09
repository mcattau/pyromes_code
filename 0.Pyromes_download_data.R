# Title: "Pyromes"
# Author: Megan Cattau, Boise State University
# Manuscript authors: Megan Cattau, Adam Mahood, Jennifer Balch, Carol Wessman
## Contact info: megan.cattau@gmail.com or megan.cattau@boisestate.edu
## Project: US Pyromes
## Project description: This project aims to identify 'pyromes' in the contiguous US based on fire characteristics
## This code:
# 1. Downloads all of the data that you will need for this analysis directly/programatically (with the exception of one source that requires manual download)


setwd("/Users/megancattau/Dropbox/0_EarthLab/US_Pyromes/Pyromes/pyromes_code/Data")

# Load packages
library(assertthat)

# 1. Import all of the data that you will need for this analysis directly/programatically
# Data will include 
# US States boundaries
# MTBS fire perimeters
# MODIS active fire data (this requires a request and manual download)
# FPA-FOD fire dataset
# NLCD landcover type
# Climate Zone
# Landfire Fire Regime Group


## US States
States_download <- file.path('States', 'cb_2016_us_state_20m.shp')
if (!file.exists(States_download)) {
	from <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
	to <- paste0('States', ".zip")
	download.file(from, to)
	unzip(to, exdir = 'States')
	unlink(to)
	assert_that(file.exists(States_download))
}   

## MTBS fire perimeters, 1984-2020
# will be downloaded directly to Data folder
MTBS_download <- file.path('MTBS', 'mtbs_perims_DD.shp')
if (!file.exists(MTBS_download)) {
	from <-"https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
	to <- paste0('MTBS', ".zip")
	download.file(from, to)
	unzip(to, exdir = 'MTBS')
	unlink(to)
	assert_that(file.exists(MTBS_download))
}

## MODIS active fire data
# Submit a request via the link below for Country->United States from Jan 2003 - Dec 2021, MODIS C6.1, and download into Data folder in subfolder called "MODIS" and unzip
# https://firms.modaps.eosdis.nasa.gov/download/

## FPA-FOD
FOD_download <- file.path('FOD', 'Data/FPA_FOD_20210617.gdb')
if (!file.exists(FOD_download)) {
  from <-"https://www.fs.usda.gov/rds/archive/products/RDS-2013-0009.5/RDS-2013-0009.5_GDB.zip"
  to <- paste0('FOD', ".zip")
  download.file(from, to)
  unzip(to, exdir = 'FOD')
  unlink(to)
  assert_that(file.exists(FOD_download))
}

### EPA Level I Ecoregions 
# Available here: https://www.epa.gov/eco-research/ecoregions-north-america
# will be downloaded directly to Data folder
Ecoregion_download <- file.path('Ecoregion', 'NA_CEC_Eco_Level1.shp')
if (!file.exists(Ecoregion_download)) {
  from <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
  to <- paste0('Ecoregion', ".zip")
  download.file(from, to)
  unzip(to, exdir = 'Ecoregion')
  unlink(to)
  assert_that(file.exists(Ecoregion_download))
}


### NLCD
# will be downloaded directly to Data folder
NLCD_download <- file.path('NLCD', 'NLCD/nlcd_2016_land_cover_l48_20210604.img')
if (!file.exists(NLCD_download)) {
  from <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_land_cover_l48_20210604.zip"
  to <- paste0('NLCD', ".zip")
  download.file(from, to)
  unzip(to, exdir = 'NLCD')
  unlink(to)
  assert_that(file.exists(NLCD_download))
}

### Climate Zone
# will be downloaded directly to Data folder
Climate_download <- file.path('Climate', 'Beck_KG_V1_present_0p0083.tif')
if (!file.exists(Climate_download)) {
  from <- "https://figshare.com/ndownloader/files/12407516"
  to <- paste0('Climate', ".zip")
  download.file(from, to)
  unzip(to, exdir = 'Climate')
  unlink(to)
  assert_that(file.exists(Climate_download))
}

### Landfire Fire Regime Group (FRG)
# will be downloaded directly to Data folder
FRG_download <- file.path('FRG', 'Tif/us_105frg.tif')
if (!file.exists(FRG_download)) {
  from <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_105-US_105_FRG.zip&TYPE=landfire"
  to <- paste0('FRG', ".zip")
  download.file(from, to)
  unzip(to, exdir = 'FRG')
  unlink(to)
  assert_that(file.exists(FRG_download))
}

library(NCmisc)
list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)


