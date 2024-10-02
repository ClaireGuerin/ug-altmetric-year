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

# str_detect(dataset$Affiliations..GRID., "University Medical Center Groningen")
# dataset$Affiliations..GRID.[9]
# dataset[9,]$Affiliations..GRID. # why are there no affiliations??? What to do with NA affiliations? Look at departments instead?
# dataset$Departments[9]

# Remove outputs in subject areas related to medicine
#names(dataset)
#non_medical_outputs <- !str_detect(dataset$Subjects..FoR., "Medic") # find which outputs have "Medic" in their subject area, 
                                                                     # reverse the matrix to know which ones do not
                                                                     # NB: there are also NAs in this. Maybe combine both methods?

#dataset %>%
#  filter(!grepl('Medic|Clinic', Subjects..FoR.)) %>% # 4,911 outputs out of 8,102 aren't Medic or Clinic topics
#  select(Subjects..FoR.)

#dataset %>%
#  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.)) %>% # 4,851 outputs out of 8,102 aren't affiliated to UMCG
#  filter(grepl('Medic|Clinic', Subjects..FoR.)) # out of which 696 outputs have medical topics

#dataset %>%
#  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.)) %>%
#  filter(!grepl('Medic|Clinic', Subjects..FoR.)) # we should get 4,851 - 696 = 4,155 outputs (V)

#dataset %>%
#  filter(!grepl('UMCG', Departments)) # 4,137 outputs out of 2,899 do not belong to the UMCG department

# Remove ALL

clean_outputs <- dataset %>%
  filter(!grepl('University Medical Center Groningen', Affiliations..GRID.) &
           #!grepl('Medic|Clinic', Subjects..FoR.) &
           !grepl('UMCG', Departments)
         ) # 3,817 outputs

n_outputs <- dim(clean_outputs)[1]
cat(paste("Total outputs: ", n_outputs, "\n"))

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
  filter(if_all(where(is.numeric))) # should return 17 (V)
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

cat(paste("Out of which, ", n_mentioned_outputs, " were mentioned (", n_mentioned_outputs*100/n_outputs, "%).\n"))

non_zero_mentions %>%
  summarise(across(News.mentions:Syllabi.mentions, list(mean= mean, sum = sum)))

# Mentions zoom-in

important_mentions <- mentions %>%
  select(News.mentions, Blog.mentions, Policy.mentions, Patent.mentions) %>%
  summarize(across(News.mentions:Patent.mentions, sum)) %>%
  rowwise() %>% 
  mutate(t = sum(c(News.mentions, Blog.mentions, Policy.mentions, Patent.mentions))) 

cat(paste("Zooming in on", important_mentions$t, "mentions:\n"))
cat(paste("\t News:", important_mentions$News.mentions, "(", important_mentions$News.mentions*100/important_mentions$t, "%)\n"))
cat(paste("\t Blogs:", important_mentions$Blog.mentions, "(", important_mentions$Blog.mentions*100/important_mentions$t, "%)\n"))
cat(paste("\t Policies:", important_mentions$Policy.mentions, "(", important_mentions$Policy.mentions*100/important_mentions$t, "%)\n"))
cat(paste("\t Patents:", important_mentions$Patent.mentions, "(", important_mentions$Patent.mentions*100/important_mentions$t, ")\n"))

# Most mentioned topics

names(clean_outputs)

by_topic <- clean_outputs %>%
  rowwise() %>%
  mutate(All.Mentions = sum(News.mentions:Syllabi.mentions)) %>%
  group_by(Subjects..FoR.) %>%
  summarize(Total.Mentions = sum(All.Mentions))

ordered_topics_by_mentions <- by_topic %>%
  arrange(desc(Total.Mentions)) 

ordered_topics_by_mentions %>%
  print(n = 10)
 
# Problem: there can be (and often are) multiple topics associated with a single output!!!
# -> replicate rows by topic

ordered_topics_by_mentions[1,]

topics_mentions <- clean_outputs %>% 
  select(c(Subjects..FoR., ends_with("mentions"))) %>%
  rowwise() %>% 
  mutate(all_mentions = sum(News.mentions:Syllabi.mentions)) %>%
  select(c(Subjects..FoR., all_mentions))
  
  
topics_mentions %>%
  # 1. Separate the subject_area column into multiple rows, assuming subject areas are comma-separated
  separate_rows(Subjects..FoR., sep = ";\\s*") %>%
  
  # 2. Group by the subject_area column
  group_by(Subjects..FoR.) %>%
  
  # 3. Summarize the total mentions for each subject area
  summarize(total_mentions = sum(all_mentions, na.rm = TRUE)) %>%
  
  # 4. Arrange in descending order to see the most mentioned subject areas first
  arrange(desc(total_mentions))
