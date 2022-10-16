# script:  2-0_merge_figaro_data.R
# purpose: transform data for fractional logit model
# project: predicting input coefficients
# author:  lorenz wimmer
##########################################################################################
##########################################################################################


# prepare for merging: change country codes
figaro$Country[figaro$Country=="GR"] <- "EL"
figaro$Country[figaro$Country=="GB"] <- "UK"


# join data frames
data  <- left_join(figaro, energy_perc)
data  <- left_join(data,   ngas)
data  <- left_join(data,   population)
data  <- left_join(data,   gdppc)


# replace NaNs with NAs
data[sapply(data, is.nan)] <- NA


# remove NAs
data <- data[complete.cases(data), ]


# add gas consumption per capita
data$Gas_FC_pC <- data$Gas_FC / data$Population


# change names
names(data) <- gsub("-", "to", names(data))



# aggregate energy shares to most important energy products###############################

# hydro
data$Hydro <- data$Hydro + data$Pumped_Hydro


# other
data$Other <- data$Other + data$Geothermal + data$Tide + data$Heat_Pumps +
  data$Waste


# coal
data$Coal <- data$Coal + data$Peat


# oil
data$Oil  <- data$Oil  + data$Shale 


# remove redundant variables
data$Pumped_Hydro <- data$Geothermal <- data$Tide <- data$Heat_Pumps <- data$Peat <-
  data$Waste <- data$Shale <- NULL



# export data ############################################################################

# bring in order first
data <- data[,c(35,37,1:34,38:52)]


# in csv format
write.csv(data, "data.csv")


# for further use in stata
write.dta(data, "data.dta")


# in excel format
write.xlsx(data, "data.xlsx", overwrite = TRUE)




##################end of script###########################################################
##########################################################################################