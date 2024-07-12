rm(list = ls())

require(tidyverse)
require(sjmisc)
require(readxl)
require(stringr)
require(stringi)
require(fuzzyjoin)

data <- readRDS('temp/ICD10.rds') %>%
  mutate(age_cate = age,
         age = case_when(age == '0' ~ 0,
                         age == '1-4' ~ 1,
                         age == '5-9' ~ 5,
                         age == '10-14' ~ 10,
                         age == '15-19' ~ 15,
                         age == '20-24' ~ 20,
                         age == '25-29' ~ 25,
                         age == '30-34' ~ 30,
                         age == '35-39' ~ 35,
                         age == '40-44' ~ 40,
                         age == '45-49' ~ 45,
                         age == '50-54' ~ 50,
                         age == '55-59' ~ 55,
                         age == '60-64' ~ 60,
                         age == '65-69' ~ 65,
                         age == '70-74' ~ 70,
                         age == '75-79' ~ 75,
                         age == '80-84' ~ 80,
                         age == '85+' ~ 85),
         sex = ifelse(sex == 1, 'Men', 'Women'),
         sex2 = ifelse(sex =='Men', 'm', 'f')) %>%
  arrange(country, sex, sex2, year, age)

all_causes = data %>%
  filter(cause == 'AAA') %>%
  select(country, sex, sex2, year, age, deaths.allcauses = deaths)


OECD_Eurostat_avoidable_104 <- read_excel("data/OECD-Eurostat avoidable.xlsx") %>%
  mutate(preventable = ifelse(!is.na(preventable), as.numeric(preventable), 0),
         treatable = ifelse(!is.na(treatable), as.numeric(treatable), 0),
         avoidable = preventable + treatable) %>%
  rename(code = code_104) %>% select(-code_103) 

OECD_Eurostat_avoidable_103 <- read_excel("data/OECD-Eurostat avoidable.xlsx") %>%
  filter(!is.na(code_103)) %>%
  mutate(preventable = ifelse(!is.na(preventable), as.numeric(preventable), 0),
         treatable = ifelse(!is.na(treatable), as.numeric(treatable), 0),
         avoidable = preventable + treatable) %>%
  rename(code = code_103) %>% select(-code_104)


avoidable_104 <- data %>% filter(list == '104') %>% 
  filter(cause != 'AAA') %>%
  fuzzy_inner_join(., OECD_Eurostat_avoidable_104, by = c("cause" = "code"), match_fun = str_starts)

avoidable_103 <- data %>% filter(list == '103') %>% 
  filter(cause != 'AAA') %>%
  fuzzy_inner_join(., OECD_Eurostat_avoidable_103, by = c("cause" = "code"), match_fun = str_starts)


avoidable <- rbind(avoidable_104, avoidable_103) %>%
  arrange(country, year, sex, cause, age) %>%
  mutate(preventable = ifelse(preventable != 0 & age >= 75, 0, preventable),
         treatable = ifelse(treatable != 0 & age >= 75, 0, treatable),
         avoidable = ifelse(avoidable != 0 & age >= 75, 0, avoidable)) %>%
  ungroup()


avoidable_by_cause = avoidable %>% 
  mutate(deaths.preventable = deaths * preventable,
         deaths.treatable = deaths * treatable,
         deaths.avoidable = deaths * avoidable) %>%
  arrange(country, sex, year, age)

saveRDS(avoidable_by_cause, 'temp/avoidable_by_cause.rds')


avoidable_grouped = avoidable_by_cause %>% 
  group_by(country, year, sex, sex2, age, cause_group) %>%
  summarise(pop = first(pop), 
            deaths.preventable = sum(deaths.preventable),
            deaths.treatable = sum(deaths.treatable),
            deaths.avoidable = sum(deaths.avoidable)) 

avoidable_grouped_totals = avoidable_grouped %>%
  group_by(country, year, sex, sex2, age) %>%
  summarise(pop = first(pop), 
            cause_group = 'Total',
            deaths.preventable = sum(deaths.preventable),
            deaths.treatable = sum(deaths.treatable),
            deaths.avoidable = sum(deaths.avoidable))

avoidable_grouped = rbind(avoidable_grouped, avoidable_grouped_totals) %>%
  pivot_wider(names_from = cause_group, values_from = c(deaths.preventable, deaths.treatable, deaths.avoidable), values_fill = 0, names_sep = '.') %>%
  left_join(., all_causes, by = c('country', 'year', 'sex', 'sex2', 'age'))

saveRDS(avoidable_grouped, 'temp/avoidable_grouped.rds')
