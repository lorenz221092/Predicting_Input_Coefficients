# script:  1-1_data_figaro.R
# purpose: transform data for fractional logit model
# project: predicting input coefficients
# author:  lorenz wimmer
##########################################################################################
##########################################################################################


# define our countries of interest (EU 27 + UK - MT - CY)
countries <- c("AT", "BE", "BG", "CZ", "DE", "DK", "ES", "EE", "FI", "FR", "UK", "EL",
               "HR", "HU", "IE", "IT", "LT", "LU", "LV", "NL", "PL", "PT", "RO", "SK",
               "SI", "SE")


# import data frame with the sectoral aggregation 
sectors <- read_excel("Data/sectoral_aggregation.xlsx")


# vector of years for our analysis
years <- as.character(2010:2017)


# create empty list for storing data
l_figaro <- list()


# loop over our years of interest (files)
for (i in 1:length(years)) {
  
  # import the tables
  figaro <- read.csv(paste0("./Data/ICIOI_64_30_", 
                                   as.character(years[[i]]), ".csv"), header = FALSE)
  
  # merge with sectoral aggregation
  figaro <- left_join(figaro, sectors)
  
  # replace empty sectors
  figaro$Sector <- ifelse(is.na(figaro$Sector), figaro$V2, figaro$Sector)
  
  # change column names
  colnames(figaro) <- c(paste0(figaro[1,1:ncol(figaro)-1],"_",figaro[2,1:ncol(figaro)-1]),
                        "Sectors")
  
  # crop file
  figaro <- figaro[3:nrow(figaro), 3:ncol(figaro)]
  
  # change to numeric
  for (j in 1:(ncol(figaro)-1)) {figaro[,j] <- as.numeric(figaro[,j])}
  
  # aggregate by sectors
  figaro <- figaro %>% group_by(Sectors) %>% summarise_all(sum, na.rm = TRUE)
  
  # subset to sectors and value added
  figaro <- figaro[figaro$Sectors %in% c(sectors$Sector, "B1G"),]
  
  # transpose
  figaro <- t(figaro)
  
  # change column names
  colnames(figaro) <- figaro[1,]
  
  # remove first row of matrix
  figaro <- figaro[-1,]
  
  # change to data frame
  figaro <- as.data.frame(figaro)
  
  # change to numeric
  for (k in 1:ncol(figaro)) {figaro[,k] <- as.numeric(figaro[,k])}
  
  # calculate the technology matrix
  figaro <- as.data.frame(figaro/rowSums(figaro, na.rm = TRUE))
  
  # create country and sector variable from the row names
  figaro$Country <- substr(rownames(figaro),1,2)
  figaro$Sector  <- substr(rownames(figaro),4,10)
  
  # subset to energy sector and our countries of interest
  figaro <- figaro %>% filter(Sector == "D", Country %in% countries)
  
  # add year
  figaro$Year <- as.numeric(years[i])
  
  # store in list
  l_figaro[[i]] <- figaro
 
}


# merge all data frames in the list
figaro <- do.call(rbind, l_figaro)




##################end of script###########################################################
##########################################################################################
























