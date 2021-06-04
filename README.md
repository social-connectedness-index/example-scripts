# Example code for the Social Connectedness Index

This repository provides a set of scripts and shapefiles to help make use of the Social Connectedness Index (SCI) data. The SCI data are downloadable at: <https://data.humdata.org/dataset/social-connectedness-index>.

It also includes replication code for [Kuchler, Russel, and Stroebel 2021](https://doi.org/10.1016/j.jue.2020.103314).

You can find the replication code for another paper that uses the SCI ([Bailey, Kuchler, Johnston, Russel, State, and Stroebel 2020](https://doi.org/10.1007/978-3-030-60975-7_1)) in [this separate repository](https://github.com/social-connectedness-index/euro_sci).

Separately, we host the zip file [gadm_based_shapefiles.zip](https://drive.google.com/file/d/1SfcY3Hpyws3o0Ukhq6smraAQ48zx90un/view?usp=sharing) which contains a set of shapefiles (in .shp format and, for R users, sf objects in .Rds files), built from the shapefiles for GADM version 2.8 and European NUTS 2016 (see sources and their relevant terms of use below). These can be matched to the Social Connectedness data for mapping. It also includes html files with interactive maps that can be used to explore the shapes.
 
***IMPORTANT NOTE***: This repository uses [git-lfs](https://git-lfs.github.com/) for versioning large files. You will need it installed to clone the repository.

  

## Repository Structure

  

The resources are split into 2 main directories:

  

1. `example_scripts` contains a set of example scripts in R that map the SCI data. It includes subfolders for each of the different SCI granularities (`country_country`, `county_county`, etc.). It also includes an `interactive_map` subfolder that provides example code to generate interactive html maps using the [Leaflet R package](https://rstudio.github.io/leaflet/). You can view the example maps by downloading the html file and opening it in any internet browser (e.g. Google Chrome). An example interactive SCI map is hosted [here](https://rpubs.com/domrussel/UKI41_SCI_map).

2. `covid19_exploration` contains a set of example scripts in R and Stata that produce the results in [Kuchler, Russel, and Stroebel 2020](https://arxiv.org/pdf/2004.03055.pdf). This folder contains a separate short readme. 

  
We also include `Relevant Literature + Bibtex.bib`, a list of papers that introduce and develop the Social Connectedness Index, as well as guidance on how to cite the prior literature when using the SCI data.
  

## Non-SCI data sources


To generate the results in `covid19_exploration`, we use a number of data from a number of sources:

1. `ACS_17_5YR_DPO5.csv` are county-level demographics from the [American Community Survey](https://www.census.gov/programs-surveys/acs).

2. `cty_covariates_oi.csv` are additional county-level demographics from [Opportunity Insights](https://opportunityinsights.org/data/).

3. `NCHSURCodes2013.csv` are National Center for Health Statistics [Urban-Rural County Classifications](https://www.cdc.gov/nchs/data_access/urban_rural.htm).

4. `sf12010countydistancemiles.csv` are county-to-county distances from the [National Bureau of Economic Research](https://data.nber.org/data/county-distance-database.html).

5. The `eurostat` folder contains European NUTS3 region demographics, made available by [Eurostat](https://ec.europa.eu/eurostat).

6. COVID data are pulled directly from Github repositories hosted by [Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) and [Dipartimento della Protezione Civile](https://github.com/pcm-dpc/COVID-19).


To generate the shapefiles in `gadm_based_shapefiles`, we bring together two sets of shapefiles:

  

1. For European NUTS2 and NUTS3 regions, we use (c) EuroGeographics for the administrative boundaries, available [here](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts).

2. For non-European countries, we use the version 2.8 Database of Global Administrative Areas (GADM) shapefiles, available [here](https://gadm.org/old_versions.html).

  

## Contact

  

This repository is managed by [Theresa Kuchler](http://pages.stern.nyu.edu/~tkuchler/) and [Johannes Stroebel](http://pages.stern.nyu.edu/~jstroebe/).
