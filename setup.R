# R/setup.R -------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(soilDB)
  library(readxl)
  library(magick)
  library(here)
  library(scatterpie)
  library(ggrepel)
  library(scales)
  library(cowplot)     # for draw_plot() – installs with ggplot2 3.4+
  library(furrr)
  library(future)
})


# Define the target crops
target_crops <- c("Rye(spring)", "Barley", "Wheat(spring)", "Corn(grain)", "Canola", 
                  "Wheat(durum)", "Corn(silage)", "Peas", "Rye(fall)", "Soybeans", 
                  "Wheat(winter)", "Sorghum", "Camelina", "Triticale")

target_cropID <-  c(3, 4, 11, 14, 15, 16, 18, 19, 20, 21, 22, 24)

# Define the mapping rules
crop_mapping <- function(crop, class, UTIL_PRACTICE_DESC = NULL) {
  
  # If UTIL_PRACTICE_DESC is missing, replace with empty string
  if (is.null(UTIL_PRACTICE_DESC)) {
    UTIL_PRACTICE_DESC <- rep("", length(crop))
  }
  
  case_when(
    crop == "BARLEY" ~ "Barley",
    crop == "CORN" & UTIL_PRACTICE_DESC == "GRAIN" ~ "Corn(grain)",
    crop == "CORN" & UTIL_PRACTICE_DESC == "SILAGE" ~ "Corn(silage)",
    crop == "CORN" & class == "ALL CLASSES" ~ "Corn",
    crop == "RYE" & class == "SPRING" ~ "Rye(spring)",
    crop == "RYE" & class == "FALL" ~ "Rye(fall)",
    crop == "RYE" & class == "ALL CLASSES" ~ "Rye",
    crop == "WHEAT" & class == "WINTER" ~ "Wheat(winter)",
    crop == "WHEAT" & class == "SPRING, (EXCL DURUM)" ~ "Wheat(spring)",
    crop == "WHEAT" & class == "SPRING, DURUM" ~ "Wheat(durum)",
    crop == "SOYBEANS" ~ "Soybeans",
    crop == "SORGHUM" & UTIL_PRACTICE_DESC == "GRAIN" ~ "Sorghum(grain)",
    crop == "SORGHUM" & UTIL_PRACTICE_DESC == "SILAGE" ~ "Sorghum(silage)",
    crop == "SORGHUM" & class == "ALL CLASSES" ~ "Sorghum",
    crop == "PEAS" ~ "Peas",
    crop == "TRITICALE" ~ "Triticale",
    crop == "CANOLA" ~ "Canola",
    crop == "CAMELINA" ~ "Camelina",
    TRUE ~ NA_character_  # Remove irrelevant crops
  )
}


us_outputs <- function(...) {
  here("..", "..", "US data", "Outputs", ...)   # climb up twice, then across
}
