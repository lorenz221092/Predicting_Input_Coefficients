# script:  1-0_data_eurostat.R
# purpose: download data via eurostat api
# project: predicting input coefficients
# author:  lorenz wimmer
##########################################################################################
##########################################################################################




# download the energy shares from eurostat ###############################################
energy_mix1 <- get_eurostat(id = "nrg_ind_peh")


# subset and select variables (main producers, gross electricity and gross heat 
# production, all plants)
energy_mix1 <- energy_mix1 %>% filter(operator == "PRR_MAIN", nrg_bal %in% c("GEP","GHP"),
                                        plants == "TOTAL") %>% select(siec, geo, time, 
                                                                      values, unit, 
                                                                      nrg_bal)


# rename variables
names(energy_mix1) <- c("Type", "Country", "Year", "Value", "Unit", "Balance")


# transform year variable
energy_mix1$Year <- as.numeric(as.character(substr(energy_mix1$Year,1,4)))


# transform tera joule to gwh
energy_mix1[energy_mix1$Unit=="TJ","Value"] <- 
  energy_mix1[energy_mix1$Unit=="TJ","Value"] /3.6


# remove unit and balance columns
energy_mix1$Unit <- energy_mix1$Balance <- NULL


# aggregate fuel types
energy_mix1 <- energy_mix1 %>%  group_by(Country, Year, Type) %>% 
  summarise(Value = sum(Value))


# from long to wide
energy_mix1 <- energy_mix1 %>% spread(key = "Type", value = "Value")


# aggregate other fuels
energy_mix1$Other <- energy_mix1$X9900 + energy_mix1$X9900H + energy_mix1$E7000


# remove pumped hydro from hydro
energy_mix1$RA100 <- energy_mix1$RA100 - energy_mix1$RA130


# remove other fuels from data
energy_mix1$X9900 <- energy_mix1$X9900H <- energy_mix1$E7000 <- NULL


# rename variables
names(energy_mix1) <- c("Country", "Year", "Combustible", "Nuclear", "Hydro", 
                      "Pumped_Hydro", "Geothermal", "Wind", "Solar", "Tide", "Heat_Pumps",
                      "Total", "Other")



# same as above for more detailed data on combustible fuels###############################
energy_mix2 <- get_eurostat(id = "nrg_ind_pehcf")


# subset and select variables (same as above)
energy_mix2 <- energy_mix2 %>% filter(operator == "PRR_MAIN", nrg_bal %in% c("GEP","GHP"),
                                        plants == "TOTAL") %>% select(siec, geo, time, 
                                                                      values, unit, 
                                                                      nrg_bal)


# rename variables
names(energy_mix2) <- c("Type", "Country", "Year", "Value", "Unit", "Balance")


# transform year variable
energy_mix2$Year <- as.numeric(as.character(substr(energy_mix2$Year,1,4)))


# transform tera joule to gwh
energy_mix2[energy_mix2$Unit=="TJ","Value"] <- 
  energy_mix2[energy_mix2$Unit=="TJ","Value"] /3.6


# assign new name to bio gas (we do not want to aggregate it with natural gas)
energy_mix2[energy_mix2$Type=="R5300","Type"] <- "Z"


# transform type variable for aggregation by first character according to SIEC
energy_mix2$Type <- substr(energy_mix2$Type,1,1)


# aggregate fuel types
energy_mix2 <- energy_mix2 %>%  group_by(Country, Year, Type) %>% 
  summarise(Value = sum(Value))


# from long to wide
energy_mix2 <- energy_mix2 %>% spread(key = "Type", value = "Value")


# rename variables
names(energy_mix2) <- c("Country", "Year", "Coal", "Gas", "Oil", "Peat", 
                      "Bio_Rest", "Shale", "Waste", "Bio_Gas")



# calculate relative shares###############################################################
energy_mix <- left_join(energy_mix1, energy_mix2)


# remove redundant columns from data
energy_mix$Combustible <- energy_mix$Total <- NULL


# duplicate data frame
energy_perc <- energy_mix


# replace columns with shares
for (i in 3:ncol(energy_mix)) {
  
  energy_perc[,i] <- energy_mix[,i] / rowSums(energy_mix[,3:ncol(energy_mix)])
  
}


# get the totals in gwh
energy_perc$Sector_Size <- rowSums(energy_mix[,3:19])






# control variables ######################################################################



# download population data from eurostat #################################################
population <- get_eurostat(id = "demo_pjan")

# subset and select variables
population <- population %>% filter(age == "TOTAL", sex == "T") %>% select(geo, time, 
                                                                           values)

# rename variables
names(population) <- c("Country", "Year", "Population")

# transform year variable
population$Year <- as.numeric(as.character(substr(population$Year,1,4)))




# download natural gas consumption data from eurostat ####################################
ngas <- get_eurostat(id = "nrg_cb_gas")

# subset and select variables
ngas <- ngas %>% filter(nrg_bal == "FC_E", siec == "G3000") %>% select(geo, time, values)

# rename variables
names(ngas) <- c("Country", "Year", "Gas_FC")

# transform year variable
ngas$Year <- as.numeric(as.character(substr(ngas$Year,1,4)))




# download gdp per capita data from eurostat #############################################
gdppc <- get_eurostat(id = "nama_10_pc")

# subset and select variables
gdppc <- gdppc %>% filter(na_item == "B1GQ", unit == "CP_PPS_EU27_2020_HAB") %>% 
  select(geo, time, values)

# rename variables
names(gdppc) <- c("Country", "Year", "GDP_pC")

# transform year variable
gdppc$Year <- as.numeric(as.character(substr(gdppc$Year,1,4)))



##################end of script###########################################################
##########################################################################################