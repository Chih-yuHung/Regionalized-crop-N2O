#"Function for Indirect N2O_Volatilization"
#author: "Dr. Chih-Yu Hung"
#date: "2024-05-01"

library(tidyverse)
#library(xlsx)


# ## Indirect N2O emissions
# 
# Canada's National Inventory Report estimate indirect N2O emissions from volatilization and leaching/runoff. 
# 
# ### 1.Volatilization
# The volatilization was estimated based on a series of coefficient that influence NH3 volatilization. The coefficients were developed by Bouwman (2002) and then validated by Sheppard et al.(2010), with Canada's data. The NH3 is converted to N2O by IPCC emission factor. 
# 
# ### 2. Leaching/runoff
# The leaching/runoff may cause NH3 emissions and then being converted to N2O emissions. Canada's methodologies estimate leaching N proportion with the ratio of precipitation/ potential evapotranspiration. 


## Volatilization function
Volatilization_prep <- function(SiteData) {
  
#Check NA   
  if (any(is.na(SiteData))) {
  print("There are missing values (NA) in the data.")
  stop()  
  }
  if (any(SiteData < -1)) {
  print("There are negative values in the data.")
  stop()
  } 
  
  
#Example for SiteData
#   RegionID:        suggest using HUP
#   ProvinceID:      Province or State ID
#   Year:            
#   CropID:          Name of crop
#   Crop_coef:       characteristic, Annual or Perennial
#   Fert_coef:       characteristic, Urea, Urea ammonia, Anhydrous ammonia, and Other
#   Method_ID_coef:  numeric, Broadcast (1) and Incorporated (2 or 3)
#   SoilpH:          numeric, -1 (<7.25) and 0 (>7.25)
#   SoilCEC:         numeric, or -1 (<250) and 0 (>250)
#   Climate          Temperate or Tropical
#   Percentage:      Percentage of soil condition (the combination of pH and CEC, sum of them must be 1)
#   Fraction:        Fraction of fertilizer
#   Proportion:      Proportion of application method
#   Fertilizer_Applied: The amount of applied fertilizer (kg)
#   Ecozone:         Ecozone for the Region


# 1. Crop type coefficient 
SiteData <- SiteData %>%
   mutate(Crop_coef = case_when(
          Crop_coef == "Annual" ~ -0.045,
          Crop_coef == "Perennial" ~ -0.158,
          TRUE ~ 99
  ))

#Check error
if (any(is.na(SiteData$Crop_coef))) {
    stop("Check Crop type results")
  } else {
  cat("All value converted")
  }
if(any(SiteData$Crop_coef == 99)) {
  stop("Check Crop results")
} else {
  cat("All value converted")
}

  
# 2. Fertilizer types
SiteData <- SiteData %>%
  mutate(Fert_coef = case_when(
        Fert_coef == "Urea" ~ 0.666,
        Fert_coef == "Urea ammonium nitrate" ~ 0.282,
        Fert_coef == "Anhydrous ammonia" ~ -1.151,
        Fert_coef == "Other" ~ -0.238,
        TRUE ~ 99
   ))

#Check error
if (any(is.na(SiteData$Fert_coef))) {
    stop("Check Fertilizer results")
  } else {
  cat("All value converted")
  }
if(any(SiteData$Fert_coef == 99)) {
  stop("Check Fertilizer results")
} else {
  cat("All value converted")
}


# 3. Application method ID0
SiteData <- SiteData %>%
    mutate(Method_ID_coef = case_when(
          Method_ID_coef == 1 ~ -1.305,
          Method_ID_coef %in% c(2,3) ~ -1.895,
          TRUE ~ 99
  ))

#The Coefs of application methods are identical for fertilization in double cropping regions. 


#Check error
if (any(is.na(SiteData$Method_ID_coef))) {
    stop("Check application results")
  } else {
  cat("All value converted")
  }
if(any(SiteData$Method_ID_coef == 99)) {
  stop("Check application results")
} else {
  cat("All value converted")
}

# 4. Convert Soil condition
SiteData <- SiteData %>%
  mutate(SoilpH = case_when(
        SoilpH == -1 ~ -1,
        SoilpH == 0  ~ -0.608,
        TRUE ~ 99
  ))

SiteData <- SiteData %>%
  mutate(SoilCEC = case_when(
        SoilCEC == -1 ~ 0.0507,
        SoilCEC == 0  ~ 0.0848,
        TRUE ~ 99
  ))

#Check error
if (any(is.na(SiteData$SoilpH)|is.na(SiteData$SoilCEC))) {
    stop("Check soil results")
  } else {
  cat("All value converted")
  }
if(any(SiteData$soilpH == 99|SiteData$soilCEC == 99)) {
  stop("Check soil results")
} else {
  cat("All value converted")
}

# 5. Climate condition
SiteData <- SiteData %>%
        mutate(Climate = case_when(
              Climate == "Temperate" ~ -0.402,
              Climate == "Tropical"  ~ 0,
              TRUE ~ 99
  ))


#Check error
if (any(is.na(SiteData$Climate))) {
    stop("Check climate results")
  } else {
  cat("All value converted")
  }
if(any(SiteData$Climate == 99)) {
  stop("Check climate results")
} else {
  cat("All value converted")
}

#Assign EF4 for Ecozones.
low.Ecozone <- c("Taiga Plain","Boreal Plains","Prairie","Montane Cordillera","Dry zone")
high.Ecozone <- c("Boreal Shield","Atlantic Maritime","MixedWood Plain","Pacific Maritime","Wet zone")
SiteData <- SiteData %>%
  mutate(Ecozone = case_when(
    Ecozone %in% low.Ecozone ~ 0.005,
    Ecozone %in% high.Ecozone ~ 0.014,
    TRUE ~ 99
  )
  )

#Check error
if (any(is.na(SiteData$Ecozone))) {
    stop("Check Ecozone results")
  } else {
  cat("All value converted")
  }
if(any(SiteData$Ecozone == 99)) {
  stop("Check Ecozone results")
} else {
  cat("All value converted")
}

return(SiteData)
}



## Calculate the NH3 and N2O
#When the data is ready, you can use this function to calculate the Fraction of volatilization and eventually calculate the Emission factors of N2O from volatilzation

#Coefficient calculation
#Calculate the sum of coefficient
#Sum of coefficient weighted by fraction of fertilizer, percentage of soil condition, and proportion of application method
N2OEF_volatilization <- function(SiteData) {

Coef_vola_ecodist<- SiteData %>%
  mutate(Coef_sum =exp( Crop_coef + Fert_coef + Method_ID_coef + SoilpH + SoilCEC + Climate) *Percentage*Fraction*Proportion) %>%
  group_by(RegionID) %>%
  mutate(Coef_sum = sum(Coef_sum))%>% #Since every Region is separated to 4 soil conditions, 4 fertilizer types, and 3 application method (Tot: 48), we need  to add them up. 
  mutate(NH3 = Coef_sum*Fertilizer_Applied,
         N2O = NH3 * Ecozone)

#Calculate the EF and total emissions based on CropID and ProvinceID
Coef_Prov_Crop <- Coef_vola_ecodist %>%
  group_by(ProvinceID,CropID) %>%
  summarise(Avg.NH3 = mean(NH3),
            Avg.N2O = mean(N2O),
            Tot.N2O = sum(N2O),
            Tot.Fert = sum(Fertilizer_Applied),
            NH3.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(NH3)/sum(Fertilizer_Applied),0),
            N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))


#Calculate the EF and total emissions based on RegionID
Coef_RegionID <- Coef_vola_ecodist %>%
  group_by(Year,ProvinceID,RegionID) %>%
  summarise(Avg.NH3 = mean(NH3),
            Avg.N2O = mean(N2O),
            NH3.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(NH3)/sum(Fertilizer_Applied),0),
            N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),0))

#Calculate the EF and total emissions based on province
Coef_province <-Coef_vola_ecodist %>%
  group_by(Year, ProvinceID) %>%
  summarise(Avg.NH3 = mean(NH3),
            Avg.N2O = mean(N2O),
            NH3.IEF = sum(NH3)/sum(Fertilizer_Applied),
            N2O.IEF = sum(N2O)/sum(Fertilizer_Applied))

return(list(Coef_Prov_Crop = Coef_Prov_Crop, Coef_RegionID = Coef_RegionID, Coef_province = Coef_province, Coef_vola_ecodist= Coef_vola_ecodist))

}




