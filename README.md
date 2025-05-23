# HAB-F: cellcount <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub:HABF-SDI-NCCOS-NOAA](https://img.shields.io/badge/GitHub-HABF--SDI--NCCOS--NOAA/cellcount-dodgerblue.svg)](https://github.com/HABF-SDI-NCCOS-NOAA/cellcount)
<!-- badges: end -->

*Developed by NOAA NCCOS - SDI - HAB Forecasting Branch*

## Overview:
The ***cellcount*** package provides a simple approach to quantifying bacteria and cyanobacteria *via* fluorescence microscopy. Microbial enumeration is an important tool for validating new methods and critical to assess potential ecological changes. However, standard manual microbial enumeration is time-consuming and notoriously difficult due to large colonies, filaments, and dense biomats. Conventional counting methods like microscopy require specialized expertise and are error prone due to variations between microscopists. Additionally, other methods of counting cells or particles (i.e. Coulter counter, flow cytometer, etc.) are not suitable for filamentous or colonial morphotypes that can clog flow cells and apertures. Here, we outline a new open-source package, ***cellcount***, a continuation of the report outlined in [@Pokrzywinski2019] and designed in the R computing language, to quickly and accurately count various microbial morphotypes. With this open source package, fluorescent microscope images can be analyzed to generate particle counts and quantify overall cell density from samples at a rapid speed with the accuracy of comparable conventional counting methods. Furthermore, additional tools are presented in this package using [R Shiny](https://cran.r-project.org/package=shiny) [@Chang2022] interfaces to demonstrate ease-of-use for researchers not familiar with the R environment. All materials outlined in this article, along with tutorials, can be found on NOAA's National Center for Coastal Ocean Science Harmful Algal Bloom Forecasting (NOAA NCCOS HAB-F) [GitHub.](https://github.com/orgs/HABF-SDI-NCCOS-NOAA/repositories)

Refer any errors or issues to: HAB@noaa.gov

# Getting started:

***cellcount*** is an open-source R package for enumerating particles in fluorescent microscopy images distributed by NOAA NCCOS HAB-F on GitHub and uses components in the RStudio IDE program. It is advised to use both the latest version of R and RStudio before beginning.

In addition, the following packages are required to use functions within ***cellcount***:

- ***RTools***
- ***EBImage*** [@Pau2010]
- ***tiff*** [@Urbanek2022]
- ***pixmap*** [@Bivand2021]
- ***raster*** [@Hijmans2023]
- ***beepr*** [@Baath2018]

Users that use the Windows OS system can access the download materials for ***RTools*** here: https://cran.rstudio.com/bin/windows/Rtools/rtools44/rtools.html

## Package installation using RStudio IDE:

To get started with cellcount, make a local clone of the repository using programs like GitHub Desktop or download the project file directly from this repository. Save the folder in an easily accessible location. Once cloned, open the R project file within the RStudio IDE and install the package locally by selecting *build*>*install package*, or use the shortcut Ctrl+Shift+B (Cmd+Shift+B for MacOS users). Once the cellcount package is installed, the following code can be used to bring cellcount into the R environment:

```{r}
library(cellcount)
```

## Package installation using devtools:

Download the zipped R package using the green "Clone or download" button. Unzip the file to the location of your choice. If you do not unzip the file, the following steps will not work!

In R studio, open the cellcount R project file in the folder of the same name and use the following code to install the package. You must be in the cellcount R project. If you aleady have devtools installed you can skip the first line.

```{r}
install.packages('devtools')

devtools::install()
```

Exit the cellcount R project and check your R library to confirm the package installed. At this point you can either delete the files from the zipped folder or retain them if you are interested in the analysis code. You can find these particular script files stored in the *cellcount_files_HABF* folder.

From your current project, use the following code to load and use the package.

```{r}
library(cellcount) 
```

Note: Requires R version 4.3.0 or higher.

## NOAA NCCOS HAB-Forecasting

The Harmful Algal Bloom Forecasting (HAB-F) branch within NOAA’s National Centers for Coastal Ocean Science (NCCOS) delivers near real-time harmful algal bloom (HAB) monitoring and forecasting products for predicting the intensity/severity, location, and respiratory health risk HABs pose in the Great Lakes and coastal regions of the U.S. While HAB-F’s monitoring, forecasting, and research efforts are national in scope, products often target specific regions based on localized needs and problems. Importantly, HAB monitoring and forecasting products require an understanding of the drivers for specific HAB species including weather, oceanographic, and environmental conditions, all of which vary greatly by region. Ultimately, HAB-F products serve as decision-support tools and provide early warning of HABs for local resource managers, public health officials, researchers, and the general public.

## Legal Disclaimer:
This repository is a scientific product and is not official communication of the National Oceanic and
Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is
provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of
Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial products, processes, or services by service
mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or
favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a
DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by
DOC or the United States Government.

## [Section 508](https://oceanservice.noaa.gov/accessibility-statement.html)

The National Ocean Service is committed to making its website accessible to the widest possible audience, including people with disabilities, in accordance with Section 508 of the Rehabilitation Act (29 U.S.C. 794d).

Section 508 is a federal law that requires agencies to provide individuals with disabilities equal access to electronic information and data comparable to those who do not have disabilities, unless an undue burden would be imposed on the agency.

The Section 508 standards are the technical requirements and criteria that are used to measure conformance within this law. More information on Section 508 and the technical standards can be found at Section508.gov.
