# title: "N2O emission factor"
# author: "Dr. Chih-Yu Hung"
# date: "2024-04-03"
# output: html_document
# 
#   ## Overview
#   This document create a function to calculate base emission factors and emission factors for the regions provide in the table
# 

library(tidyverse)

## N2OEF

# The function call `N2OEF` will calculate 
# 1. Base emission factors, considering precipitation, evaporation, topography, and soil texture
# 2. Emission factors, considering non-growing season, nitrogen source, cropping system

#The unit will be kg N2O-N kg-1 N 
N2OEF_direct <- function(SiteData) {
  
  #Create dataframe for results
  Result <- data.frame(
  RegionID = SiteData[, grepl("region", names(SiteData),ignore.case = TRUE)],
  Province = SiteData[, grepl("province", names(SiteData),ignore.case = TRUE)],
  Year = SiteData[, grepl("year", names(SiteData),ignore.case = TRUE)],
  Crop = SiteData[, grepl("cropID", names(SiteData),ignore.case = TRUE)],
  Fertilizer_Applied = SiteData$Fertilizer_Applied
  )
  
  #Check NA   
  if (any(is.na(SiteData))) {
    print("There are missing values (NA) in the data.")
    stop()  
  }
  if (any(SiteData < 0)) {
    print("There are negative values in the data.")
    stop()
  } 
  
  
  #assign the variables, I will provide an example for SiteData. This is in case I need to deal with different input
  Precip <- SiteData[, grepl("preci|^[Pp]$", names(SiteData),ignore.case = TRUE)]
  Evapo  <- SiteData[, grepl("evapotranspiration|potentail eva|PE|PET|evapo", names(SiteData),ignore.case = TRUE)]
  Topo   <- SiteData[, grepl("topo", names(SiteData),ignore.case = TRUE)]
  Crop_f <- SiteData[, grepl("Crop_f|Crop coef", names(SiteData),ignore.case = TRUE)]
  NSE    <- SiteData[, grepl("non-growing|growing|freeze-thaw|FTC|NSE", names(SiteData),ignore.case = TRUE)]
  NS     <- SiteData[, grepl("NSource", names(SiteData),ignore.case = TRUE)]
  Frac_C <- SiteData[, grepl("Coarse", names(SiteData),ignore.case = TRUE)]
  Frac_M <- SiteData[, grepl("Medium", names(SiteData),ignore.case = TRUE)]
  Frac_F <- SiteData[, grepl("Fine", names(SiteData),ignore.case = TRUE)]
  
  #Check the numbers
  #Precipitation (mm), in case there is a wrong unit (e.g. cm)   
  if (any(Precip < 80)) {
    print ("There are regions with low growing season precipitation (<80 mm) or unit is not correct, please check")
  }
  if (any(Precip > 1000)) {
    print ("There are regions with high growing season precipitation (> 1000 mm), please check")  
  } 
  
  #Check topography
  if (any(Topo > 1)) {
    stop("Topography are not between 0 and 1")
  }
  
  #Check the cropping system   
  if (is.numeric(Crop_f)){
    if(any(Crop_f > 4))
      stop("Cropping system codes should be between 1 to 4") #1 annual, 2 perennial, 3 impat, 4 unimpast 
  } 
  
  if (is.character(Crop_f)){
    allowed_values <- c("annual", "perennial", "impast", "unimpast")
    # Check if all elements in the vector are among the allowed values
    if (!all(tolower(Crop_f) %in% allowed_values)) {
      stop("Allowed values are 'annual', 'perennial', 'impast', 'unimpast'.")
    }
  }
  
  #Check the Nitrogen source (NS)
  if (is.numeric(NS)){
    if(any(!is.numeric(NS))){
      stop("Unidentified N source")
    }
    if(any(NS > 4))
      stop("Nitrogen source code should be between 1 to 4") #1 inorganic, 2 animal manure, 3 Crop residue, 4 biosolid    
  }

  
  #Check soil texture faction
  if (!all(sapply(list(Frac_C, Frac_M, Frac_C), is.numeric))) {
    stop("Not all vectors numeric in soil texture fraction")
  }
  # Check if vectors are between 0 and 1
  if (!all(Frac_C >= 0 & Frac_C <= 1) || 
      !all(Frac_M >= 0 & Frac_M <= 1) || 
      !all(Frac_F >= 0 & Frac_F <= 1)) {
    stop("Fraction of each soil texture must be between 0 and 1.")
  } 
  if (all(identical((Frac_C + Frac_M + Frac_F),1))) { 
    stop ("Sum of Frach of soil texture must be 1")
  }
  
  
  
  #Calculate the base emission factor applied with texture ratio factor in a region
  Result$EF_base <- ifelse(Precip > Evapo,exp(0.00558*Precip-7.7),
                           exp(0.00558*Evapo-7.7)*Topo + exp(0.00558*Precip-7.7)*(1-Topo))
  #Calculate the weighted ratio factor for soil texture
  Result$Wtd_RF_TX <- (Frac_C*0.49 + Frac_M*1 + Frac_F*2.55)
  
  #Introduce the Weighted ratio factor to calculate Base emission factor
  Result <- Result %>%
    mutate(EF_base = EF_base * Wtd_RF_TX)
  
  #Convert NSE, NS, and cropping system to their ratio factors
  Result$NSE <- ifelse(NSE == 1, 1/0.634, 1)
  Result$Crop_f <- ifelse(Crop_f == 1 | Crop_f == "annual", 1, 0.19)
  Result$NS[Result$Crop == 1 & NS == 1] <- 1
  Result$NS <- ifelse(NS == 1, NS, 0.84)
  #Calculate the EF 
  Result <- Result %>%
    mutate(EF = EF_base * NSE * Crop_f * NS) %>%
    mutate(N2O = EF * Fertilizer_Applied) %>%
    select(-NSE, -Crop_f, -NS)
  
  
  #Calculate the EF and total emissions based on CropID and ProvinceID regardless of year
  Result_Prov_Crop <- Result %>%
      group_by(Province,Crop) %>%
      summarise(Avg.N2O = mean(N2O),
                Tot.N2O = sum(N2O),
                Tot.Fert = sum(Fertilizer_Applied),
                N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))
    
  #Calculate the EF and total emissions based on RegionID
  Result_RegionID <- Result %>%
      group_by(RegionID) %>%
      summarise(Avg.N2O = mean(N2O),
                N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))
    
  #Calculate the EF and total emissions based on province and year
  Result_province <- Result %>%
      group_by(Year, Province) %>%
      summarise(Avg.N2O = mean(N2O),
                N2O.IEF = sum(N2O)/sum(Fertilizer_Applied))
    
    return(list(Result_Prov_Crop = Result_Prov_Crop, Result_RegionID = Result_RegionID, Result_province = Result_province, Result = Result))
    
  }
  
  


