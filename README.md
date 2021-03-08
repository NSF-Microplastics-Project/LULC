# Landuse/Landcover (LULC)

This repository contains the LULC summary table for the NSF Microplastics project area. LULC data was obtained from [NOAA C-CAP Regional Land Cover](https://coast.noaa.gov/digitalcoast/data/ccapregional.html).

## NOAA C-CAP

C-CAP (Coastal Change and Analysis Program) Regional Land Cover data are produced with a spatial resolution of 30 meters and are updated every 5 years. Additional information about this product can be found [here](https://coast.noaa.gov/data/digitalcoast/pdf/ccap-product-page.pdf).

## Usage

This table should be accessed directly from GitHub to ensure the latest version is used in analyses. Access the CSV file [here]("https://github.com/NSF-Microplastics-Project/LULC/Output") or with your favorite loading function in R:

#### Examples

```R
SFB_LULC <- data.table::fread("https://github.com/NSF-Microplastics-Project/LULC/raw/main/Output/SFB_LULC.csv") # data.table
SFB_LULC <- readr::read_csv("https://github.com/NSF-Microplastics-Project/LULC/raw/main/Output/SFB_LULC.csv") # readr
SFB_LULC <- read.csv("https://github.com/NSF-Microplastics-Project/LULC/raw/main/Output/SFB_LULC.csv") # base R
```

#### Issues/Problems?

Please create an [issue](https://github.com/NSF-Microplastics-Project/LULC/issues).

<br>

## Table Creation Method

C-CAP data  within the project extent were acquired August, 2020 as `*.tif` raster files. The C-CAP raster was exported from ArcGIS as an ENVI `*.dat` raster for convenience. The raster was then imported in R and tabulated by risk region. The full analysis in R can be found [here](https://github.com/NSF-Microplastics-Project/LULC/blob/main/CCAP.md). A general overview of the steps taken to tabulate the C-CAP data are as follows:

1. Load Raster and Risk Region Data
2. Crop and Mask Raster to Risk Regions
3. Use ArcGIS Colormap File to Update Raster Color Table
4. Extract Pixel Counts Within Risk Regions Using `raster::extract()`
5. Tabulate `raster::extract()` Output
6. Convert Pixel Counts to Square Kilometers

<br>

## Citation
>National Oceanic and Atmospheric Administration, Office for Coastal Management. Coastal Change Analysis Program (C-CAP) Regional Land Cover. Charleston, SC: NOAA Office for Coastal Management. Accessed August 2020 at www.coast.noaa.gov/htdata/raster1/landcover/bulkdownload/30m_lc/.
