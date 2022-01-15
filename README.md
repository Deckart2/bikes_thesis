## Urban Bicycle Infrastructure and Gentrification: A Quantitative Assessment of 46 American Cities
### Gabe Morrison
#### Spring 2021

## Overview:
This repo represents the code I wrote in conjunction with my 2021 Geographical Sciences BA thesis entitled *Urban Bicycle Infrastructure and Gentrification: A Quantitative Assessment of 46 American Cities*. This research was advised by Professor Marynia Kolak. All mistakes in it, however, are mine alone.

All code was written in R and made use of the Tidyverse for data manipulation and processing, sf for spatial data handling, and ggplot and tmap for data visualization. 

A copy of the thesis can be found in the repo [here](https://github.com/Deckart2/bikes_thesis/blob/master/Thesis_final%20copy.docx). 

If you have any questions, please do not hestitate to reach out to me at gdmorrison [at] uchicago . edu. 

## Repo Organization:
Scripts are found in the scripts folder and written to be run in order of prefix ``01`` onwards. 

``01`` combines spatial bicycle lane data from the 46 cities in the study into one geojson file. 

``02`` combines other spatial files from different cities to create city boundary data.

``03`` creates MSA comparison data which is useful for to determine gentrification status (based on the definition provided by Freeman (2004)). 

``04`` reads and cleans census data and converts spatial bike lane data to tract level.

``05`` uses the Freeman definition to classify census tracts as gentrifying.

``06`` computes distance to city center.

``07`` adds categorical labels to cities based on US region and size.

``08`` computes summary statistics.

``09`` performs multivariate regression analysis

In ``coefficient_of_variation.R``, I explored the optimal spatial for the analysis and determined block and block group data to be too noisy and therefore opted for census tracts. 

Charts for the paper were created in ``method_chart.R``. 
