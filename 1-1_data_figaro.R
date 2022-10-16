# script:  1-1_data_figaro.R
# purpose: transform data for fractional logit model
# project: bmk oekostrom milliarde
# author:  lorenz
##########################################################################################
##########################################################################################


# define our countries of interest
countries <- c("AT", "BE", "BG", "CZ", "DE", "DK", "ES", "EE", "FI", "FR", "GB", "GR",
               "HR", "HU", "IE", "IT", "LT", "LU", "LV", "NL", "PL", "PT", "RO", "SK",
               "SI", "SE")


# import data frame with the sectoral aggregation 
sectors <- read_excel("Data/sectoral_aggregation.xlsx")


# vector of years to loop over
years <- as.character(2010:2020)


# create empty list for storing data
l_figaro <- list()


# loop over the years/files
for (i in 1:length(years)) {
  
  
  # import the tables
  figaro <- read.csv(paste0("./Data/matrix_eu-ic-io_ind-by-ind_", 
                            as.character(years[[i]]), ".csv"), header = FALSE)
  
  
  # change column names to first row
  colnames(figaro) <-lapply(figaro[1, ], as.character)
  
  
  # remove first row in df
  figaro <- figaro[-1,]
  
  
  # substring row labels
  figaro$rowLabels <- sub("^[^_]*", "", figaro$rowLabels)
  
  
  # remove first character
  figaro$rowLabels <- substr(figaro$rowLabels, 2, 1000)
  
  
  # merge with sectoral aggregation
  figaro <- left_join(sectors, figaro)
  
  
  # remove first row
  figaro <- as.data.frame(figaro[,-1])
  
  
  # change all except 1st column to numeric
  for (j in 2:(ncol(figaro))) {figaro[,j] <- as.numeric(as.character(figaro[,j]))}
  
  
  # aggregate rows by sectors
  figaro <- figaro %>% group_by(Sector) %>% summarise_all(sum, na.rm = TRUE)

  
  # transpose df
  figaro <- t(figaro)
  
  
  # change column names to first row
  colnames(figaro) <- figaro[1,]
  
  
  # remove first row in df
  figaro <- figaro[-1,]
  
  
  # change to dataframe
  figaro <- as.data.frame(figaro)
  
  
  # change to numeric
  for (k in 1:ncol(figaro)) {figaro[,k] <- as.numeric(as.character(figaro[,k]))}
  
  
  # calculate the technology matrix
  figaro <- as.data.frame(figaro/rowSums(figaro, na.rm = TRUE))
  
  
  # create country and sector variable from rownames
  figaro$Country <- substr(rownames(figaro),1,2)
  figaro$Sector  <- substr(rownames(figaro),4,10)
  
  
  # subset to energy sector and our country set
  figaro <- figaro %>% filter(Sector == "D35", Country %in% countries)
  
  
  # add year
  figaro$Year <- as.numeric(years[i])
  
  
  # store in list
  l_figaro[[i]] <- figaro
  
  
  
}




# rbind all dfs
figaro <- do.call(rbind, l_figaro)




##################end of script###########################################################
##########################################################################################
























