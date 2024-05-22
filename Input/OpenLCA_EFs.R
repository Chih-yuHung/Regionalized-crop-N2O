library(tidyverse)
#TO wrangle openLCA data for comparison
OpenLCA <- read.csv("Input/OpenLCA_EFs.csv")

#take an anverage for the regions
#Maritime provinces
MT <- c("NL","NS","PE","NB")

OpenLCA<- OpenLCA %>%
  mutate (Region = case_when(         #combine the region
    Province %in% MT ~ "MT",
    TRUE ~Province)) %>%
  group_by(Region) %>%
  summarise(EF1 = mean(EF1),
            FracGASF = mean(FracGASF),
            EF4 = mean(EF4),
            FracLEACH = mean(FracLEACH),
            EF5 = mean(EF5))
#Save the Data
save(OpenLCA, file = "Input/OpenLCA.RData")
