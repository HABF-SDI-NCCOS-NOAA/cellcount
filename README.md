# cellcount_main
R package "cellcount" for cell enumeration using fluorescent microscopy

*Developed by NOAA NCCOS - SDI - HAB Forecasting Branch*

## Overview:
The ***cellcount*** package provides a simple an automated approach to quantifying bacteria and cyanobacteria *via* flourescence microscopy. Cyanobacteria enumeration is an important tool to incorporate data results into mathematical models to assess bloom dynamics. However, standard manual cyanobacteria enumeration is considered time-consuming and can impact overall random error due to different inaccuracies from researcher to researcher. Here, we outline a new open-source tool, ***cellcount***, a package designed for the computing language R to assist with cyanobacteria enumeration. Fluorescent microscopy images are analyzed by this package to generate cyanobacteria counts to assess overall cell density from samples.

Please refer to the *"cellcount_intro.html"* documentation file to learn more about the package, analyze demo data, and explore how this tool can apply to your research.

Citation: Harman TE, Vandersea MW, Pokrzywinski K. 2024. cellcount (R package version 1.0.0.0002). [date accessed ]. https://github.com/HABF-SDI-NCCOS-NOAA/HABF_cellcount

Refer any errors or issues to: tyler.harman@noaa.gov

## Package installation:

To get started with cellcount, make a local clone of the repository using programs like GitHub Desktop or download the project file directly from this repository. Save the folder in an easily accessible location. Once cloned, open the R project file within the RStudio IDE and install the package locally by selecting *build*>*install package*, or use the shortcut Ctrl+Shift+B (Cmd+Shift+B for MacOS users). Once the cellcount package is installed, the following code can be used to bring cellcount into the R environment:

```{r}
library(cellcount)
```
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
