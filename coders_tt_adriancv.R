# Option 1: tidytuesdayR package 

install.packages("tidytuesdayR")

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, 
               here,
               visdat, 
               inspectdf,
               dplyr,
               janitor,
               esquisse,
               countrycode,
               ggplot2,
               rnaturalearth,
               ggspatial,
               mdsr,
               terra,
               spData, 
               readr)

tuesdata <- tidytuesdayR::tt_load('2024-09-03')


qname_levels_single_response_crosswalk <- tuesdata$qname_levels_single_response_crosswalk
stackoverflow_survey_questions <- tuesdata$stackoverflow_survey_questions
stackoverflow_survey_single_response <- tuesdata$stackoverflow_survey_single_response

View(qname_levels_single_response_crosswalk)
View(stackoverflow_survey_questions)
View(stackoverflow_survey_single_response)


branch <- 
stackoverflow_survey_single_response %>% 
  mutate(branch = case_when(main_branch == 1 ~ "Professional",
                            main_branch == 2 ~ "Learner",
                            main_branch == 3 ~ "Occasional",
                            main_branch == 4 ~ "Hobby",
                            main_branch == 5 ~ "Former pro"))



View(branch)

branch2 <-
  branch %>% 
  select(response_id, main_branch, branch, everything())

View(branch2)

branch3 <- 
  branch2 %>% 
  mutate(age_group = case_when(age <= 2 ~ "Under 35",
                               age > 2 & age <= 5 ~ "35 - 65",
                               TRUE ~ "65 and Over")) %>% 
  select(response_id, main_branch, branch, age, age_group, everything())



View(branch3)

esquisser(branch3)



library(ggplot2)

ggplot(branch3) +
 aes(x = branch, fill = age_group) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_minimal() +
  labs(x = "Profile",
       y = "Count",
       fill = "Age Group")



ggplot(branch3) +
 aes(x = branch, fill = branch) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_gray() +
  labs(x = "Type of profile",
       y = "Count",
       fill = "Profile")


install.packages("poliscidata")
data(world)
View(world)

setdiff(world$name_long, branch3$country)

branch4 <-
  branch3 %>% 
  mutate(countries = case_when(
    country == "United States of America" ~ "United States",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "Iran, Islamic Republic of..."  ~ "Iran",
    country == "Republic of North Macedonia" ~ "Macedonia",
    country == "United Republic of Tanzania" ~ "Tanzania",
    country == "Libyan Arab Jamahiriya" ~ "Libya",
    country == "Congo, Republic of the..." ~ "Republic of the Congo",
    country == "Venezuela, Bolivarian Republic of..." ~ "Venezuela",
    TRUE ~ country))

View(branch4)

country_data_joined <- 
inner_join(branch4, world, by = c("countries" = "name_long")) %>% 
  select(-c(remote_work:buildvs_buy), -c(currency:r_want_to_use))

View(country_data_joined)

country_data <- 
  country_data_joined %>% 
  group_by(countries, geom) %>% 
  summarise(total_respondents = n()) %>% 
  ungroup()

View(country_data)


regio_data <-
  country_data_joined %>% 
  group_by(subregion, geom) %>% 
  summarise(total_respondents = n()) %>% 
  ungroup()

View(regio_data)

ggplot(country_data) +
  geom_sf(mapping = aes(geometry = geom,
                        fill = total_respondents)) + 
  labs(fill = str_wrap("Number of respondents per country", width = 15))




