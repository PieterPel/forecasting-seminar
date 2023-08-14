# set working directory
setwd('') # Set to where directory is located
source("src/rolling_window.R")
source("src/functions.R")


# Load the data
spain_data = read.csv(file='/data/processed/spain.csv')
france_data = read.csv(file='/data/processed/france.csv')
italy_data = read.csv(file='/data/processed/italy.csv')
netherlands_data = read.csv(file='/data/processed/netherlands.csv')
unitedkingdom_data = read.csv(file='/data/processed/unitedkingdom.csv')

countries = c("Spain", "France", "Italy", "Netherlands", "United Kingdom") ## add: "France", "Italy", "Netherlands", "United Kingdom")

country_dfs = list(spain_data, france_data, italy_data, netherlands_data, unitedkingdom_data) #add: france_data, italy_data, netherlands_data, unitedkingdom_data)

# paramaters
nwindows = 80 # in months
max_correlation = 0.9
horizon = 1 # in months c(1,3,6,12)
predicting_var = "CPALTT01"

#Initialize
shap_all = list()
importance_all = list()
forecasts_all = list()
counter <- 1

# Loop over all countries
for (data in country_dfs) {
  country = countries[counter]
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
  
  rf <- runrf_norw(data, predicting_var, horizon)
  
  #get shap values and importance
  shap = rf$shap
  shap_all[[country]] = shap
  importance = rf$outputs
  importance_all[[country]] = importance
  forecasts = rf$forecast
  forecasts_all[[country]] = forecasts
  
  ## decide on the most important features
  importance_asframe = as.matrix(importance)
  names = colnames(data)
  # Create a vector of lag numbers
  lags <- c(1, 2, 3)
  varnames_lag=names
  # Use a for loop to create lag variable names
  for (i in lags) {
    for(name in names){
      value=paste0(name, "_lag_", i)
      varnames_lag=append(varnames_lag, value)
    }
  }
  
  colnames = varnames_lag
  rownames(importance_asframe) = colnames

  #take averages over the rows
  averages <- rowMeans(importance_asframe)
  #Sort
  averages_order <- sort(averages, decreasing=TRUE, index.return =TRUE)
  averages_ordered <- averages_order[[1]]%>%as.matrix()
  names = c(nrow(averages_ordered))
  for(i in 1:nrow(averages_ordered)) {
    index = averages_order[["ix"]][i]
    names[i] = rownames(importance_asframe)[index]
  }
  rownames(averages_ordered) = names
  names(averages_order[["x"]]) = rownames(averages_ordered)
  
  # Plot
  x = 1:nrow(averages_ordered)
  plot(x, averages_ordered, title = country)
  
  counter = counter + 1
  
}  


plot_feature_importance(shap_all[["Spain"]], max_vars = 15, title = " " )
plot_feature_importance(shap_all[["France"]], max_vars = 15, title = "Feature Importance France")
plot_feature_importance(shap_all[["Italy"]], max_vars = 15, title = "Feature Importance Italy")
plot_feature_importance(shap_all[["Netherlands"]], max_vars = 15, title = "Feature Importance Netherlands")
plot_feature_importance(shap_all[["United Kingdom"]], max_vars = 15, title = "Feature Importance United Kingdom")

plot_feature_dependence(shap_all[["Spain"]], "CPALTT01_lag_1", title = "Feature dependence lagged CPI")
plot_feature_dependence(shap_all[["Spain"]], "CPGRLE01_lag_1", title = "Feature dependence")

plot_feature_dependence(shap_all[["France"]], "CPALTT01_lag_1", title = "Feature dependence lagged CPI")
plot_feature_dependence(shap_all[["France"]], "CPGRLE01_lag_1", title = "Feature dependence")

plot_feature_dependence(shap_all[["Italy"]], "CPALTT01_lag_1", title = "Feature dependence lagged CPI")
plot_feature_dependence(shap_all[["Italy"]], "CPGRLE01_lag_1", title = "Feature dependence")

plot_feature_dependence(shap_all[["Netherlands"]], "CPALTT01_lag_1", title = "Feature dependence lagged CPI")
plot_feature_dependence(shap_all[["Netherlands"]], "CPGRLE01_lag_1", title = "Feature dependence")

plot_feature_dependence(shap_all[["United Kingdom"]], "CPGRLE01_lag_1", title = "Feature dependence lagged CPGRLE")
plot_feature_dependence(shap_all[["United Kingdom"]], "CPHPLA01_lag_1", title = "Feature dependence lagged CPHPLA")
