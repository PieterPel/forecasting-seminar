#### Load packages
library(rJava)
library(bartMachine)
library(usethis)
library(devtools)
library(shapr)
library(dplyr)
library(HDeconometrics)
library(glmnet)
library(ranger)
library(pls)
library(nipals)
library(caret)
library(treeshap)

setwd('') # Set to where directory is located
source("/src/rolling_window.R") 
source("/src/functions.R")

#####

# Load the data
spain_data = read.csv(file='/data/processed/spain.csv')
france_data = read.csv(file='/data/processed/france.csv')
italy_data = read.csv(file='/data/processed/italy.csv')
netherlands_data = read.csv(file='/data/processed/netherlands.csv')
unitedkingdom_data = read.csv(file='/data/processed/unitedkingdom.csv')

countries = c("Spain, France", "Italy", "Netherlands", "United Kingdom")

country_dfs = list(spain_data, france_data, italy_data, netherlands_data, unitedkingdom_data)

# paramaters
nwindows = 1 # in months
max_correlation = 0.9
horizons = 3 # in months c(1,3,6,12)
predicting_var = "CPALTT01"

#Initialize
results = list()
counter <- 1

# Loop over all countries
for (data in country_dfs) {
  
  ### Make data frame in correct format (yes, should have done when making it)
  variables = data[1, ]
  colnames(data) = as.character(variables)
  
  dates = data$SUBJECT
  startdate <- which(data$SUBJECT == "X1996M03")
  enddate <- which(data$SUBJECT == "X2022M10")
  rownames(data) = as.character(dates)
  data = data%>%select(!c('SUBJECT'))
  data <- data [startdate:enddate, ]
  
  # Identify all character columns
  chars <- sapply(data, is.character)
  
  # Convert all character columns to numeric
  data[ , chars] <- as.data.frame(apply(data[ , chars], 2, as.numeric))
  
  ### Remove highly correlated features 
  # tmp <- cor(data)
  # tmp[!lower.tri(tmp)] <- 0
  # data.new <- data[, !apply(tmp, 2, function(x) any(abs(x) > max_correlation, na.rm = TRUE))]
  
  results_for_horizons = list()
  shap_results = list()
  # Loop over all horizons
  for (horizon in horizons) {
    
    ### Run the forecasts
    # adaLasso
    model_adaLASSO = rolling_window(runlasso, data, nwindows, horizon, predicting_var, adaptive=TRUE)
    
    # AR
    model_ar = rolling_window(runar, data, nwindows, horizon, variable = predicting_var, type = "bic")
    
    # Random forest
    model_rf = rolling_window(runrf, data, nwindows, horizon, predicting_var)
    
    # PCR
    model_pcr = rolling_window(runpcr, data, nwindows, horizon, predicting_var)
    
    # BART
    model_bart = rolling_window(runbart, data, nwindows, horizon, predicting_var)
    
    # Save models
    results_for_models = list( ar = model_ar, pcr = model_pcr, adaLasso = model_adaLASSO, rf = model_rf, bart = model_bart )
    results_for_horizons[[toString(horizon)]] <- results_for_models
    
  }
  
  # Add to results
  country_name <- countries[counter]
  counter <- counter + 1
  
  results[[country_name]] <- results_for_horizons
  
}
