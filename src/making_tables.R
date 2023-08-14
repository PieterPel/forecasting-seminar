################################################################
setwd('') # Set to where directory is located

### Put results in various tables

### RMSE tables (Note: entries are rounded to three digits)
spain_table <- make_rmse_table('Spain')
france_table <- make_rmse_table('France')
italy_table <- make_rmse_table('Italy')
netherlands_table <- make_rmse_table('Netherlands')
unitedkingdom_table <- make_rmse_table('UnitedKingdom')

### aSPA tables
# Note horizon is just where it is stored, does not have any economic significance
spain_aspa_table <- make_aspa_table('Spain', 1, pvalue = TRUE)
france_aspa_table <- make_aspa_table('France', 1, pvalue = TRUE)
italy_aspa_table <- make_aspa_table('Italy', 1, pvalue = TRUE)
netherlands_aspa_table <- make_aspa_table('Netherlands', 1, pvalue = TRUE)
unitedkingdom_aspa_table <- make_aspa_table('UnitedKingdom', 1, pvalue = TRUE)

average_aspa_table <- make_average_aspa_table()

### MCS tables
spain_mcs_table <- make_mcs_table('Spain')
france_mcs_table <- make_mcs_table('France')
italy_mcs_table <- make_mcs_table('Italy')
netherlands_mcs_table <- make_mcs_table('Netherlands')
unitedkingdom_mcs_table <- make_mcs_table('UnitedKingdom')



###############################################################
### Functions to create the various tables

# Useful in the functions
models <- c("rf", "bart", "adaLasso", "ar", "pcr")
horizons <- c("1", "3", "6", "12")
countries <- c("Spain", "France", "Italy", "Netherlands", "United Kingdom")

# Function that creates a table with normalized RMSE values and their significance from the results for a given country
make_rmse_table <- function(country_name) {
  # Initialize rows and columns
  table <- as.data.frame( matrix(nrow=5, ncol = 5) )
  rownames(table) <- models
  colnames(table) <- c("1", "3", "6", "12", "Average")
  
  # Loop over all rows
  for( rowname in rownames(table) ) {
    # Sum to eventually calculate the average
    sum <- 0
    
    # Loop over all 'horizon' columns
    for ( colname in colnames(table)[1:4] ) {
      
      # Obtain normalized RMSE
      rmse <- results1[[country_name]][[colname]][[rowname]][["RMSE"]]
      rmse_rf <- results1[[country_name]][[colname]][["rf"]][["RMSE"]]
      nrmse <- rmse / rmse_rf # Normalize compared to the random forest RMSE
      sum <- sum + nrmse # Used for calculating the average later
      nrmse <- round(nrmse, digits = 3) # Round the nrmse
      nrmse <- toString(nrmse) # Convert to a string
      
      sign <- ""
      
      # Obtain significance of Diebold-Mariano test
      if (rowname != "rf") {
        dm_pvalue <- results1[[country_name]][[colname]][[rowname]][["DM"]][["p.value"]]
        sign <- p_to_sign(dm_pvalue)
      }
      
      # Add entry to table
      entry <- paste(nrmse, sign, sep = "")
      
      table[rowname, colname] <- entry
    }
    
    table[rowname, "Average"] <- round( sum / (length( colnames(table) ) - 1 ), digits = 3 )
  }
  
  # Return completed table
  (table)
}

# Function that creates a table with aSPA test statistics and their significance from the results for a given country
# Note: horizon is just where it is stored, does not have economic significance on its own
make_aspa_table <- function(country_name, horizon, pvalue = FALSE) {
  horizon = toString(horizon)
  table <- as.data.frame(matrix(nrow=5, ncol=5))
  rownames(table) <- models
  colnames(table) <- models
  
  # Loop over all rows
  for ( rowname in rownames(table) ) {
    
    # Loop over all columns
    for ( colname in colnames(table) ) {
      
      # No aSPA test against itself ofcourse
      if(rowname == colname) {
        next
      }
      
      # Extract test statistic
      aspa_statistic <- as.numeric( evaluation[[country_name]][[horizon]][[rowname]][["aSPA"]][[colname]][["t_aSPA"]] )
      aspa_statistic <- round(aspa_statistic, digits = 3) # Round aSPA statistic
      
      # Extract p-value
      aspa_pvalue <- evaluation[[country_name]][[horizon]][[rowname]][["aSPA"]][[colname]][["p_value"]]
      if(aspa_statistic < 0 && !pvalue) {aspa_pvalue <- 1 - aspa_pvalue}
      
      # Get significance
      sign <- p_to_sign(aspa_pvalue)
      
      entry <- paste( toString(aspa_statistic), sign, sep = '')
      
      # Put entry in table
      table[rowname, colname] <- entry
      if( pvalue ) {
        table[rowname, colname] <- round(aspa_pvalue, 3)
      }
    }
  }
  
  (table)
}

# Function that creates a table with the average aSPA statistic or p-value
make_average_aspa_table <- function() {
  
  # Initialize table that will be returned
  table <- as.data.frame(matrix(nrow=5, ncol=5))
  rownames(table) <- models
  colnames(table) <- models
  
  # Loop over all rows
  for ( rowname in rownames(table) ) {
    
    # Loop over all columns
    for ( colname in colnames(table) ) {
      
      # No aSPA values against itself ofcourse
      if(rowname == colname) {
        next
      }
      
      # Calculate average aSPA statistic 
      sum <- 0
      for ( country in countries ) {
        value <- evaluation[[country]][["1"]][[rowname]][["aSPA"]][[colname]][["p_value"]]
        sum <- sum + value
      }
      average_aspa <- sum / length(countries)
      average_aspa <- round(average_aspa, digits = 3) # Round average aSPA
      
      # Save average to table
      table[rowname, colname] <- average_aspa
    }
  }
  
  # Return table
  (table)
}

# Function that creates a table with MCS test statistics
make_mcs_table <- function(country_name) {
  
  # Convert model_i to names of models we use
  models_convert_list <- list()
  models_convert_list[["model_1"]] <- "adaLasso"
  models_convert_list[["model_2"]] <- "ar"
  models_convert_list[["model_3"]] <- "rf"
  models_convert_list[["model_4"]] <- "pcr"
  models_convert_list[["model_5"]] <- "bart"
  
  # Initialize table that is returned
  table <- as.data.frame( matrix(nrow=4, ncol=6) )
  rownames(table) <- c( horizons )
  colnames(table) <- c( models, "p-value" )
  
  # Loop over all horizons
  for (horizon in horizons) {
    
    # Put p-value in table
    pvalue <- as.numeric( evaluation[[country_name]][[horizon]][["MCS"]]@Info[["mcs_pvalue"]] )
    table[horizon, "p-value"] <- pvalue
  
    # Put ranks in table
    showing <- as.data.frame( evaluation[[country_name]][[horizon]][["MCS"]]@show )

    # Obtain models that are included and their ranks
    included_models <- rownames(showing)
    ranks <- round(showing$Rank_M, digits = 0)

    # Put entries in the table
    for ( i in 1:length(included_models) ) {
      
      # Convert model to ones we use
      included_models[i] <- models_convert_list[[included_models[i]]]
      
      # Save
      table[horizon, included_models[i]] <- ranks[i]
    }
  }

  # Return table
  (table)
}

# Function to turn a p-value into the correct number of stars, in brackets
p_to_sign <- function(pval) {
  sign = '()'
  
  if (pval >= 0 && pval <= 0.01 ) {sign = '(***)'}
  if (pval >= 0.01 && pval <= 0.05) {sign = '(**)'}
  if (pval >= 0.05 && pval <= 0.10) {sign = '(*)'}
  
  sign
}