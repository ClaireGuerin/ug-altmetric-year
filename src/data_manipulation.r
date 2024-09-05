# Created by: Claire Guerin
# 26/08/2024

#install.packages("tidyverse")
library(tidyverse)

####---- DATA IMPORT ----####

setwd("/home/claire/Documents/git/ug-altmetric-year")
filepath <- "/home/claire/Documents/git/ug-altmetric-year/data/AltmetricResearchOutputs-UniversityGroningen-2023.csv"

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
  filter(!grepl('Medic|Clinic', Subjects..FoR.)) %>% # 4,911 outputs out of 8,102 aren't Medic or Clinic topics
  select(Subjects..FoR.)

dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.)) %>% # 4,851 outputs out of 8,102 aren't affiliated to UMCG
  filter(grepl('Medic|Clinic', Subjects..FoR.)) # out of which 696 outputs have medical topics

dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.)) %>%
  filter(!grepl('Medic|Clinic', Subjects..FoR.)) # we should get 4,851 - 696 = 4,155 outputs (V)

dataset %>%
  filter(!grepl('UMCG', Departments)) # 4,137 outputs out of 2,899 do not belong to the UMCG department

# Remove ALL

clean_outputs <- dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.) &
           !grepl('Medic|Clinic', Subjects..FoR.) &
           !grepl('UMCG', Departments)
         ) # 3,817 outputs

n_outputs <- dim(clean_outputs)[1]

# Look at mentions 
names(clean_outputs)
head(clean_outputs$SSRN)
mentions <- clean_outputs %>% 
  select(ends_with("mentions"))

mention_sources <- str_replace(names(mentions),".mentions", "")

# Check data type for mentions is numeric
mentions %>% summarise_all(class)
n_variables <- dim(mentions)[2]
n_variables_numeric <- dim(mentions %>%
  filter(if_all(is.numeric)) # should return 17 (V)
  )[2]

n_variables == n_variables_numeric # all good if TRUE (V)
sum(mentions$News.mentions)
total_mentions <- mentions %>%
  summarise(across(News.mentions:Syllabi.mentions, sum))

n_mentions_total <- total_mentions %>%
  mutate(total_mentions = rowSums(.)) %>%
  select(total_mentions) %>%
  as.numeric


# find out how many rows are 0 in all of their columns

is_zero <- function(x) {
  x == 0
}

non_zero_mentions <- mentions %>%
  filter(if_any(News.mentions:Syllabi.mentions, ~ . != 0))

all_zero_mentions <- mentions %>%
  filter(if_all(News.mentions:Syllabi.mentions, ~ . == 0))

n_mentioned_outputs <- dim(non_zero_mentions)[1]
n_mentioned_outputs == n_outputs - dim(all_zero_mentions)[1] # check that we have the correct number (V)

n_mentioned_outputs*100/n_outputs

non_zero_mentions %>%
  summarise(across(News.mentions:Syllabi.mentions, list(mean= mean, sum = sum)))
