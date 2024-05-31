#read the file A#_Ammonia_EF_annual_by_Prov
#This is table from 
#"D:\1_InventoryContributions\1_Agriculture\2024\Data\Processing\2024_DB\5-Ammonia\NH3_NAESI_Synt_Fert.accdb"
# I'm using this to have the total NH3 emissions and applied fertilizer and IEF
#The IEF can be convert to N2O by applying EF4
library(tidyverse)

IEF_NH3 <- read.csv("../Input/A3_Ammonia_EF_Annual_by_Prov.csv")
EF4 <- read.csv("../Input/EF4.csv") #Ecod, Ecozone, Prov

#Assign EF4 for ecozones.
low.Ecozone <- c("Taiga Plain","Boreal Plains","Prairie","Montane Cordillera")
high.Ecozone <- c("Boreal Shield","Atlantic Maritime","MixedWood Plain","Pacific Maritime")
EF4 <- EF4 %>%
  mutate(EF4 = case_when(
    ZONE_NAME %in% low.Ecozone ~ 0.005,
    ZONE_NAME %in% high.Ecozone ~ 0.014,
    TRUE ~ 99
  )
  )

#Average EF4 for province
EF4.prov <- as.data.frame(tapply(EF4$EF4, EF4$Province_ID, mean))
colnames(EF4.prov) <-"Avg_EF4"
EF4.prov <- rownames_to_column(EF4.prov, var = "Province_ID")

#Obtain IEF for N2O
IEF_N2O <-IEF_NH3 %>%
  inner_join(EF4.prov,by = c("Province_ID" = "Province_ID")) %>%
  mutate(IEF_N2O = IEF * Avg_EF4)

#Take the average
IEF_N2O_prov <- IEF_N2O %>%
  group_by(Year,Province_ID) %>%
  summarise(Total_Fert = sum(Fertilizer_Applied,na.rm = TRUE),
            Total_Emissions = sum(Emissions, na.rm= TRUE),
            Avg_EF4 = mean(Avg_EF4)) %>%
  mutate(IEF_NH3 = Total_Emissions / Total_Fert,
         IEF_N2O = IEF_NH3*Avg_EF4)

#Make a table for region, i.e. put NL, NS, PE, and NB to Maritime
MT <- c("NL","NS","PE","NB")
IEF_N2O_region <- IEF_N2O_prov %>%
  mutate(Region = case_when(
    Province_ID %in% MT ~ "MT",
    TRUE ~Province_ID)) %>%
  group_by(Year,Region) %>%
  summarise(Total_Fert = sum(Total_Fert, na.rm = TRUE),
            Total_Emissions = sum(Total_Emissions, na.rm = TRUE),
            Avg_EF4 = mean(Avg_EF4)) %>%
  mutate(IEF_NH3 = Total_Emissions/Total_Fert,
         IEF_N2O = IEF_NH3*Avg_EF4)

#Calculate the GWP for the IEF, AR5 numbers
IEF_N2O_region_GWP <- IEF_N2O_region %>%
  mutate(CO2e = IEF_N2O *44/28 *265)

