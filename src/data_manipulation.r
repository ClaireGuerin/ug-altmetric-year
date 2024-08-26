# Created by: Claire Guerin
# 26/08/2024

#install.packages("tidyverse")
library(tidyverse)

####---- DATA IMPORT ----####

setwd("/home/claire/Documents/git/ug-altmetric-year")
filepath <- "/home/claire/Documents/git/ug-altmetric-year/data/Altmetric-ResearchOutputs-UniversityofGroningen-2023.csv"

dataset <- read_csv(filepath, # Read input into tibble
                 col_names = TRUE)  %>% 
  rename_with(make.names)

# Remove outputs affiliated with UMCG = University Medical Center Groningen

str_detect(dataset$Affiliations..GRID., "University Medical Center Groningen")
dataset$Affiliations..GRID.[9]
dataset[9,]$Affiliations..GRID. # why are there no affiliations??? What to do with NA affiliations? Look at departments instead?
dataset$Departments[9]

# Remove outputs in subject areas related to medicine
names(dataset)
non_medical_outputs <- !str_detect(dataset$Subjects..FoR., "Medic") # find which outputs have "Medic" in their subject area, 
                                                                    # reverse the matrix to know which ones do not
                                                                    # NB: there are also NAs in this. Maybe combine both methods?

dataset %>%
  filter(!grepl('Medic|Clinic', Subjects..FoR.)) %>% # 1,507 outputs out of 2,899
  select(Subjects..FoR.)

dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.)) %>% # 1,515 outputs out of 2,899
  filter(grepl('Medic|Clinic', Subjects..FoR.)) # out of which 324 outputs have medical topics

dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.)) %>%
  filter(!grepl('Medic|Clinic', Subjects..FoR.)) # we should get 1,515 - 324 = 1,191 outputs V

dataset %>%
  filter(!grepl('UMCG', Departments)) # 1,280 outputs out of 2,899

# Remove ALL

clean_outputs <- dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.) &
           !grepl('Medic|Clinic', Subjects..FoR.) &
           !grepl('UMCG', Departments)
         ) # 1,136 outputs

 clean_outputs %>% 
   select(contains("mentions")) # look at columns counting mentions
                                # next step: find out how many rows have a non-zero number in any of their columns
   

