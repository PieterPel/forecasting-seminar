
# Introduction
- This directory contains the raw data and R scripts used by Team 8 (2023) of the Seminar in Forecasting at the Erasmus University of Rotterdam for the Econometrics bachelor programme. 
- For our report we have forecasted inflation in Spain, France, Italy, the Netherlands and the United Kingdom. We used the 'classical' methods: AR and PCR; and the machine learning methods: adaLASSO, random forest and BART. We especially focused on interpreting the random forest predictions using SHAP.

# Installation
- R version 4.2.3 was used and can be downloaded from https://www.r-project.org/
- The following CRAN R packages were used and can be installed using the install.packages() command in R:
--   bartMachine
--   caret
--   devtools
--   dplyr
--   forecast
--   ggplot2
--   glmnet
--   MCS
--   Metrics
--   MultiHorizonSPA
--   nipals
--   pls
--   plyr
--   ranger
--   rJava
--   shapr
--   tseries
--   usethis
- The following non-CRAN R packages were used and can be installed with the githubinstall() command:
-- HDeconometrics
-- treeshap

# Data sources
- The following data was used for our dataset:
-- OECD Main Economic Indicators from: https://stats.oecd.org/index.aspx?r=885300#
-- Oil price from: https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M
-- Gold price from: https://www.gold.org/goldhub/data/gold-prices
-- Long-term interest rates from: https://sdw.ecb.europa.eu/browse.do?node=9691124
-- Private consumption from: https://stats.oecd.org/Index.aspx?QueryId=21765#
-- Private final consumption expenditure from: https://fred.stlouisfed.org/searchresults?st=private+final+expenditure
-- Unit labour cost from: https://sdw.ecb.europa.eu/intelligentsearch/?searchTerm=unit%20labour%20cost&pageNo=1&itemPerPage=10&sortBy=relevance
-- M1, M2 and M3 for all countries except the UK from: https://sdw.ecb.europa.eu/home.do
-- M1, M2 and M3 for the UK from: https://www.bankofengland.co.uk/statistics/
-- Hourly earning for France from: https://fred.stlouisfed.org/series/LCEAPR01FRQ661S
-- Labour compensation for Spain from: https://fred.stlouisfed.org/series/LCEATT01ESQ189N
- The raw data is available under /data/raw
- The processed data is available under /data/processed
- The output is available under /output
-- The graphs are under /output/graphs
-- The tables are under /output/tables
-- The transformations used to make the variables stationary are in /output/transformations.csv

# Code structure
- All R code used is available under /src:
-- evaluating_results.R: script that can evaluate the forecasts
-- functions.R: script that contains various functions that are used like the forecasting methods
-- getting_results.R: script that makes the forecasts using the processed data
-- making_tables.R: script that creates informative tables from the forecasts and its evaluations
-- rolling_window.R: script that creates various rolling windows that can be used for forecasting
-- trimming_down_data.R: script that turns the raw data in processed data and outputs the transformations used for stationarity
-- shap.R: script that creates the various SHAP plots
- All R code was written ourselves, but functions.R and rolling_window.R were adapted from https://github.com/gabrielrvsc/ForecastingInflation/blob/main/functions/functions.R
-- Both functions are used in (Medeiros et al., 2021)
- How to use our code:
-- First run trimming_down_data to get the processed data
-- Then run getting_results to get the forecasts
-- Then run evaluating_results to evaluate the results
-- Then run making_tables to create tables with the results

# Results
- For an exhaustive discussion and conclusion of our results I refer to our final report. Our main conclusions are that:
-- Random gives the best predictions for most countries (when gauging with the RMSE)
-- SHAP gives roughly the same variables high importance for each country
-- The most important features for forecasting inflation have an S-curved effect on inflation

# Contact information
Merle Beerens: 537427@student.eur.nl
Milena Mustafajev: 525434@student.eur.nl
Pieter Pel: 525573pp@student.eur.nl
Chenyan Yang: 564312@student.eur.nl

# License
Creative Commons