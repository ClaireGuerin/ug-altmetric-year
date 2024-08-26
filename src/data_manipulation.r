# Created by: Claire Guerin
# 26/08/2024

#install.packages("tidyverse")
library(tidyverse)

####---- DATA IMPORT ----####

setwd("/home/claire/Documents/git/ug-altmetric-year")
filepath <- "/home/claire/Documents/git/ug-altmetric-year/data/Altmetric-ResearchOutputs-UniversityofGroningen-2023.csv"

data <- read_csv(filepath, # Read input into tibble
                 col_names = TRUE)  %>% 
  rename_with(make.names)

# Remove outputs affiliated with UMCG = University Medical Center Groningen

str_detect(data$Affiliations..GRID., "University Medical Center Groningen")
data$Affiliations..GRID.[9]
data[9,]$Affiliations..GRID. # why are there no affiliations???
