# Load packages
library(Metrics)
library(dplyr)
library(MCS)
library(devtools)
library(MultiHorizonSPA)
library(forecast)
library(ggplot2)


# Initialize
results1 <- results
evaluation2 <- list()
cpi <-  data %>% pull(predicting_var) 
Total_Loss = list()
Loss = list()

# Loop over the results for all countries
for ( country in names( results1 ) ) {

  # Loop over the results for all horizons
  for ( horizon in names( results1[[country]] ) ) {
    
    # Loop over the results for all models
    for ( model in names( results1[[country]][[horizon]] ) ) {
      
     
      fc <- results1[[country]][[horizon]][[model]][["forecast"]]  # Extracts the forecasts
    
      # Fix formatting for random forest
      fc_rf = c()
      fc_rf_length <- length( results1[[country]][[horizon]][["rf"]][["forecast"]] ) / 5 - 1
      for (i in 0:fc_rf_length) {
        pr <- as.numeric( results1[[country]][[horizon]][["rf"]][["forecast"]][[5*i + 1]] ) # Extract prediction
        fc_rf <- append( fc_rf, pr ) # Add prediction
      }
      
      ## results1[[country]][[horizon]][[model]][["forecast"]] <- fc_rf
     
      if(model ==  "rf"){
        fc <- fc_rf
      } 
      
      cpi_tail <- tail( cpi, length(fc) ) # Extract the right actual values
      rmse <- rmse(cpi_tail, fc) # Calculate the rmse
      results1[[country]][[horizon]][[model]][["RMSE"]] <- rmse # Save rmse
      
      if(!model=="rf"){
        dm = dm.test(fc-cpi_tail, fc_rf-cpi_tail)
        results1[[country]][[horizon]][[model]][["DM"]] <- dm
      }
      Loss[[model]] = LossLevel(cpi_tail, fc)
    }
    ## Model Confidence Set test
    Total_Loss[[horizon]]<- Loss
    Loss_mcs = cbind(Loss[["ar"]]%>%as.vector(), Loss[["adaLasso"]]%>%as.vector(), Loss[["pcr"]]%>%as.vector(), Loss[["rf"]]%>%as.vector(), Loss[["bart"]]%>%as.vector() )
    MCS <- MCSprocedure(Loss=Loss_mcs,alpha=0.5,B=5000,statistic='Tmax',cl=NULL)
    evaluation2[[country]][[horizon]][["MCS"]] <- MCS
  }
  
  ##Multi-horizon SPA
  for ( model1 in names( results1[[country]][[horizon]] ) ) {
    tests = list()
    for (model2 in names( results1[[country]][[horizon]])){
      if(!model1==model2){
        LossDiff_aSPA = cbind(Total_Loss[["1"]][[model1]]-Total_Loss[["1"]][[model2]], Total_Loss[["3"]][[model1]]-Total_Loss[["3"]][[model2]], Total_Loss[["6"]][[model1]]-Total_Loss[["6"]][[model2]], Total_Loss[["12"]][[model1]]-Total_Loss[["12"]][[model2]])
        weights <- as.matrix(rep(1/ncol(LossDiff_aSPA), ncol(LossDiff_aSPA)))
        test = Test_aSPA(LossDiff=LossDiff_aSPA, weights=weights, L=3)
        tests[[model2]] <- test
      }
    }
    evaluation2[[country]][["1"]][[model1]][["aSPA"]] <- tests
  }
}

