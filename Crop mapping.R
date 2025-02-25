library(tidyverse)

# Define the target crops
target_crops <- c("Rye(spring)", "Barley", "Wheat(spring)", "Corn(grain)", "Canola", 
                  "Wheat(durum)", "Corn(silage)", "Peas", "Rye(fall)", "Soybeans", 
                  "Wheat(winter)", "Sorghum", "Camelina", "Triticale")

# Define the mapping rules
crop_mapping <- function(crop, class) {
  case_when(
    crop == "BARLEY" ~ "Barley",
    crop == "CORN" & class == "GRAIN" ~ "Corn(grain)",
    crop == "CORN" & class == "SILAGE" ~ "Corn(silage)",
    crop == "CORN" & class == "ALL CLASSES" ~ "Corn",
    crop == "RYE" & class == "SPRING" ~ "Rye(spring)",
    crop == "RYE" & class == "FALL" ~ "Rye(fall)",
    crop == "RYE" & class == "ALL CLASSES" ~ "Rye",
    crop == "WHEAT" & class == "WINTER" ~ "Wheat(winter)",
    crop == "WHEAT" & class == "SPRING, (EXCL DURUM)" ~ "Wheat(spring)",
    crop == "WHEAT" & class == "SPRING, DURUM" ~ "Wheat(durum)",
    crop == "SOYBEANS" ~ "Soybeans",
    crop == "SORGHUM" ~ "Sorghum",
    crop == "PEAS" ~ "Peas",
    crop == "TRITICALE" ~ "Triticale",
    crop == "CANOLA" ~ "Canola",
    crop == "CAMELINA" ~ "Camelina",
    TRUE ~ NA_character_  # Remove irrelevant crops
  )
}

