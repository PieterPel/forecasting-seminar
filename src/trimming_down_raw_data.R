# Load packages
setwd('') # Set to where directory is located
library(dplyr)
library(tseries)
library(plyr)

##########################################################################
### Opening the complete raw MEI file
all_mei <- read.csv(file = 'data/raw/main_economic_indicators0123_raw.csv') # Download from the text file that is located under data/raw
manuallyAddedFeatures <- read.csv(file = 'data/raw/manuallyAddedFeatures.csv') 

##########################################################################
### Only select countries that we are interested in
countries <- list("France", "Spain", "Italy", "Netherlands", "United Kingdom")
df <- all_mei[all_mei$Country %in% countries, ]

##########################################################################
### Only select quarterly and annual data

# Remove 'FLAGS' columns
df <- df[,!grepl( "FLAG", names(df), fixed = TRUE )]

# Split data frame in to 'info' columns and 'value' columns
df_info <- df[, 1:13]
df_values <- df[, -1:-13]

# df_yearly # Not really necessary right now
df_quarterly <- df_values[grepl( "Q", names(df_values), fixed = TRUE ) ]
df_monthly <- df_values[grepl( "M", names(df_values), fixed = TRUE ) ]

# Add info back to values
df_q <- cbind(df_info, df_quarterly)
df_m <- cbind(df_info, df_monthly)
df_total <- cbind(df_info, df_quarterly, df_monthly)

##########################################################################
### Save data

startDate = 'X1996M03' # Set the 'cut-off' point for the data

# Create data for each country
df_m_nd <- df_m[ !(is.na(df_m[startDate])), ] # Set from where you want the data to be available
df_nl <- df_m_nd[df_m_nd$Country == 'Netherlands', ]
df_fr <- df_m_nd[df_m_nd$Country == 'France', ]
df_es <- df_m_nd[df_m_nd$Country == 'Spain', ]
df_uk <- df_m_nd[df_m_nd$Country == 'United Kingdom', ]
df_it <- df_m_nd[df_m_nd$Country == 'Italy', ]

# Remove duplicates
df_nl <- df_nl[ !duplicated(df_nl$SUBJECT), ]
df_fr <- df_fr[ !duplicated(df_fr$SUBJECT), ]
df_es <- df_es[ !duplicated(df_es$SUBJECT), ]
df_uk <- df_uk[ !duplicated(df_uk$SUBJECT), ]
df_it <- df_it[ !duplicated(df_it$SUBJECT), ]

# Manually defined list of subjects that we don't want to have
bannedSubjects = list("LFHU24FE", # Level unemployment, while we also have the rate
                      "LFHUADFE", # ""
                      "LFHUTTFE", # ""
                      "MABMM301", # M1 of UK, but we have added it manually from BoE data
                      "MANMM101", # M3 of UK, but we have added it manually from BoE data
                      "LOCOSPNO", # Normalized share prices, but we have the total
                      "LOCOVRNO", # Normalized car registration sales, but we have the total
                      "LMUNRRTT") # Registered unemployment, but have the rate

# Remove duplicates of same underlying subject
countryDfs <- list(df_es, df_fr, df_it, df_nl, df_uk)
for (df in countryDfs) {
  goodVars = list()
  for (x in df$SUBJECT) {
    
    # Don't include banned subjects
    if (x %in% bannedSubjects) {
      next
    }
    
    subject_base <- gsub('.{2}$', '', x)
    if ( !(subject_base %in% gsub('.{2}$', '', goodVars)) ) {
      goodVars <- append(goodVars, x)
    }
  }
  # Only include good variables
  df <- df[df$SUBJECT %in% goodVars]
}

# For easy reference
df_es <- data.frame(countryDfs[1])
df_fr <- data.frame(countryDfs[2])
df_it <- data.frame(countryDfs[3])
df_nl <- data.frame(countryDfs[4])
df_uk <- data.frame(countryDfs[5])

#Combine the data for all countries and trim the subjects
df_m_trimmed <- rbind(df_es, df_fr, df_it, df_nl, df_uk)
df_m_trimmed <- df_m_trimmed[df_m_trimmed$SUBJECT %in% goodVars, ]

# Add the manually added features to the data
df_m_trimmed <- rbind(df_m_trimmed, manuallyAddedFeatures)

##########################################################################
### Check for stationarity
SUBJECT <- df_m_trimmed$SUBJECT
makeStationary <- data.frame( SUBJECT)
makeStationary$Country <- df_m_trimmed$Country
makeStationary['What makes it stationary'] <- NA
for (i in 1:nrow(df_m_trimmed)){
  
  # Select correct row and columns of data frame
  data <- df_m_trimmed[i,]
  subject <- data$SUBJECT
  country <- data$Country
  data <- data %>% select(startDate:'X2022M12')
  
  # Check if level data is stationary
  data <- (na.omit(as.numeric(data))) 
  if (adf.test(data)$p.value < 0.05) {
    makeStationary[i, ]['What makes it stationary'] <- 'Level'
    next
  }
  
  # Check if first difference data is stationary
  dataDiff <- diff(data) 
  if (adf.test(dataDiff)$p.value < 0.05) {
    makeStationary[i, ]['What makes it stationary'] <- 'First difference'
    next
  }
  
  # Check if second difference data is stationary
  dataSecondDiff <- diff(dataDiff) 
  if (adf.test(dataSecondDiff)$p.value < 0.05) {
    makeStationary[i, ]['What makes it stationary'] <- 'Second difference'
    next
  }
  
  
  # Check if log data is stationary
  dataLog <- log(data)
  if (adf.test(dataLog)$p.value < 0.05) {
    makeStationary[i, ]['What makes it stationary?'] <- 'Log'
    next
  }
  
  # Check if log difference data is stationary
  dataLogDiff <- diff(dataLog)
  if (adf.test(dataLogDiff)$p.value < 0.05) {
    makeStationary[i, ]['What makes it stationary'] <-  'Log-difference'
    next
  }
  
  # If nothing works:
  makeStationary[i, ]['What makes it stationary'] <-  'Nothing works'
  
}

# Also add to main dataframe
df_m_trimmed <- cbind(df_m_trimmed, makeStationary['What makes it stationary'])

### Transform data frame to put in appendix

# Initialize
SUBJECT = unique(df_m_trimmed$SUBJECT)
transformations <- as.data.frame( matrix(nrow=length(SUBJECT),ncol=5) )
colnames(transformations) <- c("Spain", "France", "Italy", "Netherlands", "United Kingdom")
row.names(transformations) <- SUBJECT

# Loop over all rows
for ( i in 1:length(df_m_trimmed) ) {
  # Find out info about the row
  row <- df_m_trimmed[i, ]
  subject <- row$SUBJECT
  country <- row$Country
  tf <- row['What makes it stationary']
  
  # Add it to the dataframe
  transformations[subject, country] <- tf
}

# Save 
write.csv(transformations, 'data/output/transformations.csv')

##########################################################################
### Actually create a data frame with stationary data

# Initialize data frame that contains the stationary data
stationaryMonthlyInfo <- df_m_trimmed[, 1:13]
df_m_trimmed_data <- df_m_trimmed[-1:-13]
df_m_trimmed_data <- df_m_trimmed_data[1:length(df_m_trimmed_data)-1]
stationaryMonthlyData <- df_m_trimmed_data

# Loop over all rows and transform them
for (i in 1:nrow(df_m_trimmed)) {
  wholeRow <- df_m_trimmed[i, ]
  row <- as.numeric(df_m_trimmed_data[i, ])
  
  if (wholeRow['What makes it stationary'] == 'Level') {
    stationaryMonthlyData[i, ] <- row
    next
  }
  
  if (wholeRow['What makes it stationary'] == 'First difference') {
    stationaryMonthlyData[i, ] <- diff(row)
    next
  }
  
  if (wholeRow['What makes it stationary'] == 'Second difference') {
    stationaryMonthlyData[i, ] <- diff(diff(row))
    next
  }
  
  if (wholeRow['What makes it stationary'] == 'Log') {
    stationaryMonthlyData[i, ] <- log(row)
    next
  }
  
  if (wholeRow['What makes it stationary'] == 'Log-difference') {
    stationaryMonthlyData[i, ] <- diff(log(row))
    next
  }
  
  if (wholeRow['What makes it stationary'] == 'Nothing works') {
    stationaryMonthlyData[i, ] <- diff(row) # Most data that get here at least appears to only have a trend
  }
}

# Stitch together the output
stationaryMonthlyDf <- cbind(stationaryMonthlyInfo, df_m_trimmed['What makes it stationary'], stationaryMonthlyData)

##########################################################################
### Creates dataframe that shows which countries the subject is available for

# Create dataframe
SUBJECT = unique(df_m_trimmed$SUBJECT)
countries = c('Spain', 'France', 'Italy', 'Netherlands', 'United Kingdom')
completelyAvailable <- data.frame( SUBJECT )
for (c in countries) {
  completelyAvailable[c] <- NA
}

# FIll dataframe (note that it does not work for manually added features yet)
for (i in 1:nrow(completelyAvailable)) {
  subject <- completelyAvailable[i,]$SUBJECT
  howToMark <- '#####'
  # Check for all countries
  for (cdf in countryDfs) {
    cdf <- data.frame(cdf)
    # Check if the subject is available for the country
    if ( !(any(cdf['SUBJECT'] == subject)) ) {
      next
    }
    # Check if the value is available at the start date
    if ( !( is.na( cdf[ which(cdf$SUBJECT == subject), ][startDate] ) ) ) {  
      # Add the checkmark to the data frame
      completelyAvailable[i, ][ toString(cdf[1, ]['Country'] ) ] = howToMark
    }
  }
}

#############################################################################
### Convert quarterly input to monthly data

# Read quarterly data and initialize monthly data frame
quarterlyData = read.csv('data/raw/quarterly_manual_data.csv')
quarterlyData <- quarterlyData[, -1:-3]
monthlyData = as.data.frame( matrix( nrow=nrow(quarterlyData), ncol=3*ncol(quarterlyData)-2 ) )

# Loop over every row of the quarterly data frame
for ( i in 1:nrow(quarterlyData) ) {
  row <- quarterlyData[i, ] 
  monthlyRow <- QtoM( row ) 
  monthlyData[i, ] = monthlyRow
}

# Save data, we added the results manually to manuallyAddedFeatures in MS Excel
write.csv(monthlyData, 'data/raw/monthly_data_from_quarterly.csv')


### Function that creates monthly data from quarterly data, linearly
QtoM <- function(input) {
  # Initialize the output vector and index
  input <- c( as.numeric(input) )
  output <- c()
  index <- 1
  for (x in input) {
    # Stop at last value of input
    if ( index == length(input) ) {
      output <- append(output, x)
      break
    }
    
    # If value of input is NA, add three NAs
    if(is.na(x)) {
      output <- append(output, NA)
      output <- append(output, NA)
      output <- append(output, NA)
      index <- index + 1
      next
    }
    # Add the value of the input to the output
    output <- append(output, x)
    
    # Calculate the difference
    diff <- ( input[index + 1] - x ) / 3
    
    # Add the interpolated values to the output
    output <- append(output, x + diff)
    output <- append(output, x + 2*diff)
    
    # Increment the index  
    index <- index + 1
    
  }
  
  output
}

######################################################################
### Check which data has gaps in the data

# Initialize
missingData = t(c( 'SUBJECT', 'Country', 'Date', 'Largest Gap') )

# Loop over all rows
for (i in 1:nrow(df_m_trimmed)) {
  # Initialize 
  dataHasStarted = FALSE
  dataHasStopped = FALSE
  missingDataBool = FALSE
  row = df_m_trimmed_data[i, ]
  wholeRow = df_m_trimmed[i, ]
  counter <- 1
  naCounter <- 0
  largestGap <- 0
  
  # Loop over all values in the row
  for (x in row) {
    
    # Check if the data has begun
    if ( !(is.na(x)) && !dataHasStarted) {
      dataHasStarted = TRUE
      next
    } 
    
    # Check if the data has stopped
    if (dataHasStarted && is.na(x) && !dataHasStopped) {
      dataHasStopped = TRUE
      next
    }
    
    # If the data begins again, there is a gap
    if (dataHasStarted && dataHasStopped && !( is.na(x) )) {
      missingDataBool = TRUE
      date = colnames(df_m_trimmed_data)[counter]
      if (naCounter > largestGap) {
        largestGap <- naCounter
      }
      naCounter <- 0
    }
    
    # Count how large the gap is
    if (dataHasStopped) {
      naCounter <- naCounter + 1
    }
    
    # Count where the gap ends
    counter <- counter + 1
  }
  
  if (missingDataBool) {
    missingData <- rbind(missingData, t( c(wholeRow$SUBJECT, wholeRow$Country, date, largestGap) ))
  }
  
}

# Make the missingData in to a data frame (Yes this is incredibly ugly code...)
if (TRUE) {
  SUBJECT <- missingData[,1][-1]
  missingDataFrame <- data.frame( SUBJECT )
  missingDataFrame$Country <- missingData[,2][-1]
  missingDataFrame$Date <- missingData[,3][-1]
  missingDataFrame$LargestGap <- missingData[,4][-1]
}

##########################################################################
### Interpolate the date

# Functino that linearly interpolates a dataframe
linear_interpolate <- function(data) {
  # Identify the start and end dates of each time series
  start_dates <- apply(data, 2, function(x) which(!is.na(x))[1])
  end_dates <- apply(data, 2, function(x) max(which(!is.na(x))))
  
  # Create a new dataframe to store the interpolated values
  interpolated_data <- data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  rownames(interpolated_data) <- rownames(data)
  colnames(interpolated_data) <- rownames(data)
  
  # Loop through each time series and interpolate missing values
  for (i in 1:nrow(data)) {
    series <- data[i, ]
    
    # Identify any missing values that need to be interpolated
    missing_indices <- which(is.na(series))
    
    # Interpolate the missing values using linear interpolation
    if (length(missing_indices) > 0) {
      non_missing_indices <- which(!is.na(series))
      interpolated_values <- approx(non_missing_indices, series[non_missing_indices], missing_indices)$y
      series[missing_indices] <- interpolated_values
    }
    
    interpolated_data[i, ] <- series
  }
  
  return(interpolated_data)
}

# Set last NA values at the end equal to the last non NA value
set_end_na_to_last <- function(data) {
  # Loop through each row of the data frame
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    # Find the index of the last non-NA value in the row
    last_non_na <- max(which(!is.na(row)))
    # Set any NA values at the end of the row to the last non-NA value
    row[last_non_na+1:ncol(data)] <- row[last_non_na]
    # Update the row in the original data frame
    data[i, ] <- row
  }
  return(data)
}

# Return interpolated data
df_m_trimmed_data_intr <- linear_interpolate(df_m_trimmed_data)
stationaryMonthlyData_intr <- linear_interpolate(stationaryMonthlyData)
stationaryMonthlyData_intr <- set_end_na_to_last(stationaryMonthlyData_intr)
rownames(stationaryMonthlyData_intr) <- rownames(stationaryMonthlyData)
stationaryMonthlyDf_intr <- cbind(stationaryMonthlyInfo, df_m_trimmed['What makes it stationary'], stationaryMonthlyData_intr)

##########################################################################
### Rewrite the data frame in the way that Merle wants it for the analysis

# Group by country
spain_data <- stationaryMonthlyDf_intr[stationaryMonthlyDf_intr$Country == 'Spain',]
france_data <- stationaryMonthlyDf_intr[stationaryMonthlyDf_intr$Country == 'France',]
italy_data <- stationaryMonthlyDf_intr[stationaryMonthlyDf_intr$Country == 'Italy',]
netherlands_data <- stationaryMonthlyDf_intr[stationaryMonthlyDf_intr$Country == 'Netherlands',]
unitedkingdom_data <- stationaryMonthlyDf_intr[stationaryMonthlyDf_intr$Country == 'United Kingdom',]

# Remove unnecessary columns and transpose
spain_data <- data.frame( t( cbind(spain_data$SUBJECT, spain_data[-1:-14]) ) )
france_data <- data.frame ( t( cbind(france_data$SUBJECT, france_data[-1:-14]) ) )
italy_data <- data.frame ( t( cbind(italy_data$SUBJECT, italy_data[-1:-14]) ) )
netherlands_data <- data.frame( t( cbind(netherlands_data$SUBJECT, netherlands_data[-1:-14]) ) )
unitedkingdom_data <- data.frame( t( cbind(unitedkingdom_data$SUBJECT, unitedkingdom_data[-1:-14]) ) )

cn <- c("SUBJECT", colnames(df_m_trimmed_data))
rownames(spain_data) <- cn
rownames(france_data) <- cn
rownames(italy_data) <- cn
rownames(netherlands_data) <- cn
rownames(unitedkingdom_data) <- cn

##########################################################################
### Save the data
write.csv(df_m_trimmed, 'data/processed/monthly_data_trimmed.csv')
write.csv(spain_data, 'data/processed/spain.csv')
write.csv(france_data, 'data/processed/france.csv')
write.csv(italy_data, 'data/processed/italy.csv')
write.csv(netherlands_data, 'data/processed/netherlands.csv')
write.csv(unitedkingdom_data, 'data/processed/unitedkingdom.csv')

