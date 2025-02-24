#title: "Function for Indirect N2O_Leaching"
# author: "Dr. Chih-Yu Hung"
# date: "2024-05-01"
# output: html_document

library(tidyverse)



## Indirect N2O emissions

#Canada's National Inventory Report estimate indirect N2O emissions from volatilization and leaching/runoff. 

### 1.Volatilization
#The volatilization was estimated based on a series of coefficient that influence NH3 volatilization. The coefficients were developed by Bouwman (2002) and then validated by Sheppard et al.(2010), with Canada's data. The NH3 is converted to N2O by IPCC emission factor. 

### 2. Leaching/runoff
# The leaching/runoff may cause NH3 emissions and then being converted to N2O emissions. Canada's methodologies estimate leaching N proportion with the ratio of precipitation/ potential evapotranspiration. 
 

N2OEF_leaching <- function(SiteData, ProvinceCol = "ProvinceID", FertCol = "Fertilizer_Applied",
                           PCol = "P", PECol = "PE", CropCol = "CropID", YearCol = "Year" ,
                           RegionCol = "RegionID") {
  
  SiteData <- SiteData %>%
    rename(ProvinceID = !!sym(ProvinceCol), Fertilizer_Applied = !!sym(FertCol),
           P = !!sym(PCol), PE = !!sym(PECol), Year = !!sym(YearCol), RegionID = !!sym(RegionCol)
           ) %>%  # Dynamically rename the chosen column
    drop_na()
  
  N2OEF_leaching <- SiteData %>%
    mutate(Frac_leach = ifelse(P >= PE, 0.3, ifelse(P/PE <= 0.23,0.05,0.3247*(P/PE)-0.0247))) %>%
    mutate(Leach_factor = Frac_leach *0.0075) %>% #IPCC 2006, EF5 0.0075 kg N2O-N kg-1 N, it's 0.011 in the 2019 Refinement
mutate(N2O = Leach_factor*Fertilizer_Applied)

#Calculate the EF and total emissions based on CropID and ProvinceID, regardless of year
Coef_Prov_Crop <- N2OEF_leaching %>%
  group_by(ProvinceID,CropID) %>%
  summarise(Avg.N2O = mean(N2O),
            Tot.N2O = sum(N2O),
            Tot.Fert = sum(Fertilizer_Applied),
            N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))


#Calculate the EF and total emissions based on RegionID
Coef_RegionID <- N2OEF_leaching %>%
  group_by(RegionID) %>%
  summarise(Avg.N2O = mean(N2O),
            N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))

#Calculate the EF and total emissions based on province
Coef_province <-N2OEF_leaching %>%
  group_by(Year, ProvinceID) %>%
  summarise(Avg.N2O = mean(N2O),
            N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))

return(list(Coef_Prov_Crop = Coef_Prov_Crop, Coef_RegionID = Coef_RegionID, Coef_province = Coef_province, N2O_leaching =N2OEF_leaching))

}
