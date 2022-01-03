library(tidyverse)
library(lubridate)
library(glue)
library(jsonlite)

date <- Sys.Date()
date_long <- glue('{day(date)} de {month(date, label = T, abbr = F, locale = "pt_PT")} de {year(date)}')

df <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')

europe <- df %>% filter(continent == 'Europe') %>% 
  mutate(date = ymd(date)) %>% 
  filter(date >= as.Date('2021-09-01')) %>% 
  select(iso_code, location, date, new_cases_per_million, new_cases_smoothed_per_million, new_deaths_smoothed_per_million, people_fully_vaccinated_per_hundred, stringency_index) %>% 
  filter(!is.na(new_cases_smoothed_per_million)) %>% 
  filter(location != 'Vatican') %>% 
  filter(location != 'Faeroe Islands') %>% 
  filter(location != 'Isle of Man') %>% 
  filter(location != 'Gibraltar')


countries <- read_csv('countries.csv')

europe <- europe %>% 
  left_join(countries) %>% 
  select(-location) %>%
  rename('location' = 'location_pt')


#Find biggest increase in a week
top_changes <- europe %>% 
  select(iso_code, location, date, new_cases_smoothed_per_million) %>% 
  group_by(location) %>% 
  filter(date == max(date) | date == max(date - 7)) %>% 
  mutate(txt = (new_cases_smoothed_per_million - lag(new_cases_smoothed_per_million)/7)) %>% 
  filter(!is.na(txt)) %>% 
  arrange(-txt) %>% 
  select(location, txt)

mortes <- europe %>%
  group_by(location) %>% 
  filter(date == max(date)) %>% 
  select(location, new_deaths_smoothed_per_million) %>% 
  mutate(new_deaths_smoothed_per_million = replace_na(new_deaths_smoothed_per_million, 0))

vacin <- europe %>% 
  group_by(location) %>% 
  slice(which.max(people_fully_vaccinated_per_hundred)) %>% 
  select(location, people_fully_vaccinated_per_hundred)

rank_vacin <- vacin %>% 
  arrange(-people_fully_vaccinated_per_hundred) %>%
  ungroup() %>% 
  mutate(n = row_number())

index_medidas <- europe %>% 
  group_by(location) %>% 
  filter(!is.na(stringency_index)) %>% 
  filter(date == max(date)) %>% 
  select(location, stringency_index)

index_medidas <- countries %>% 
  select(location_pt) %>% 
  rename('location' = 'location_pt') %>% 
  left_join(index_medidas) 




countries <- list()

for (i in 1:length(top_changes$location)) {
  countries[i] <- list(
    list(
      pais = top_changes$location[i],
      txt_cres = top_changes$txt[i],
      mortes = mortes %>% filter(location == top_changes$location[i]) %>% pull(new_deaths_smoothed_per_million),
      vacin = vacin %>% filter(location == top_changes$location[i]) %>% pull(people_fully_vaccinated_per_hundred),
      rank_vacin = rank_vacin %>% filter(location == top_changes$location[i]) %>% pull(n),
      index_medidas = index_medidas %>% filter(location == top_changes$location[i]) %>% pull(stringency_index),
      data =  europe %>% filter(location == top_changes$location[i]) %>% select(-iso_code, -location)
    )
  )
}
  
  







#Export data

data <- list(
  date_update = date_long,
  max_Y = europe %>% filter(new_cases_per_million == max(europe$new_cases_per_million, na.rm = T)) %>%  pull(new_cases_per_million),
  data_paises = countries
)

data <- data %>% toJSON(pretty = FALSE, auto_unbox = TRUE, na = "null")

data %>% write('data.json')
