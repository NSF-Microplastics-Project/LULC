---
title: "NOAA C-CAP Tabulation and Summary"
author: "Skyler Elmstrom"
date: "3/7/2021"
output:
  html_document:
    theme: lumen
    code_download: true
    keep_md: true
---



<br>

Add description of C-CAP here and source links.

Add notes on project need and general method.

Add notes on output and how to access it.

<br>


<br>

<details>
  <summary><b>Required Libraries</b></summary>

```r
# devtools::install_github("WWU-IETC-R-Collab/IETC")
library(IETC)

library(rgdal) # R geoprocessing tools
library(raster) # Raster data manipulation
library(sf) # vector data manipulation
library(tidyverse)
```
</details>
<br>

<details>
  <summary><b>Functions and Class Key</b></summary>

```r
# Function to Tabulate raster::extract() Output by Polygon
# http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
tabFunc<-function(indx, extracted, region, regname) {
  dat<-as.data.frame(table(extracted[[indx]]))
  dat$name<-region[[regname]][[indx]]
  return(dat)
}

# Function for converting pixel count to square kilometers
pc2sqkm <- function(x, in_raster) {
  apc <- prod(raster::res(in_raster))
  x * apc / 1e+6
}

# Class Key For Fixing Class Names
# https://coast.noaa.gov/digitalcoast/training/ccap-land-cover-classifications.html
CCAP.class <- c('Developed - High Intensity' = '2',
                 'Developed - Medium Intensity' = '3',
                 'Developed - Low Intensity' = '4',
                 'Developed - Open Space' = '5',
                 'Cultivated Crops' = '6',
                 'Pasture/Hay' = '7',
                 'Grassland/Herbaceous' = '8',
                 'Deciduous Forest' = '9',
                 'Evergreen Forest' = '10',
                 'Mixed Forest' = '11',
                 'Scrub/Shrub' = '12',
                 'Palustrine Forested Wetland' = '13',
                 'Palustrine Scrub/Shrub Wetland' = '14',
                 'Palustrine Emergent Wetland' = '15',
                 'Estuarine Forested Wetland' = '16',
                 'Estuarine Scrub/Shrub Wetland' = '17',
                 'Estuarine Emergent Wetland' = '18',
                 'Unconsolidated Shore' = '19',
                 'Barren Land' = '20',
                 'Open Water' = '21',
                 'Palustrine Aquatic Bed' = '22',
                 'Estuarine Aquatic Bed' = '23',
                 'Tundra' = '24',
                 'Snow/Ice' = '25'
                  )
```
</details>
<br>

## Tabulating C-CAP by Risk Region in R


```r
# Load *.dat file from ArcGIS Export Raster
SFB.CCAP <- raster(
  (list.files('Data/', pattern='.dat$', full.names = T)[1])
  )

# Load Risk Regions
SFB.riskregions.z <- "https://github.com/NSF-Microplastics-Project/Risk_Region.shapefile/raw/main/Data/SFB_RiskRegions_20210304_SP.zip"
SFB.riskregions <- IETC::unzipShape(SFB.riskregions.z) # loads GitHub shapefile as an sf object

# Crop Raster to Risk Regions
SFB.CCAP.crop <- crop(SFB.CCAP, extent(SFB.riskregions)) # Crop to filtered risk region
SFB.CCAP.mask <- mask(SFB.CCAP.crop, SFB.riskregions)
apc <- prod(res(SFB.CCAP.mask)) # calculates the m^2 area per pixel based on original CCAP resolution
plot(SFB.CCAP.mask)

# Extract Raster Values
SFB.LULC <- raster::extract(SFB.CCAP.mask, SFB.riskregions) # be patient, this takes a while

# Tabulate raster::extract() lists
SFB.LULC.tabulated <- lapply(seq(SFB.LULC), tabFunc, SFB.LULC, SFB.riskregions, "name") %>% # tabulate result lists from raster::extract
  do.call("rbind", .) %>% # combine the tabulated raster::extract
  pivot_wider(names_from = Var1, # pivot classes wide
              values_from = Freq) %>% 
  dplyr::select(name, any_of(CCAP.class)) %>% # replace column class ID # with class name
  mutate(across(where(is.numeric), ~pc2sqkm(., SFB.CCAP.mask))) # convert pixel counts to sq km

# Write Final Table to Outputs
write_csv(SFB.LULC.tabulated, "Output/SFB_LULC.csv")
```
