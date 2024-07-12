rm(list = ls())

require(tidyverse)

data = readRDS('temp/avoidable_grouped.rds') %>%
  pivot_longer(`deaths.preventable.Adverse effects of medical and surgical care`:deaths.avoidable.Total, 
               names_to = c('type', 'group'), names_pattern = 'deaths.(.*)\\.(.*)', values_to = 'deaths') %>%
  mutate(mx.allcauses = deaths.allcauses / pop,
         mx.avoided = deaths/pop) %>%
  select(-deaths.allcauses, -deaths)

NMS <- c('Croatia', 'Czech Republic', 'Estonia', 'Hungary', 'Latvia', 'Lithuania', 'Poland', 'Slovakia', 'Slovenia', 'Bulgaria', 'Romania', 'Malta', 'Cyprus')
OMS <- c('Austria', 'Belgium', 'Denmark', 'Finland', 'France', 'Germany', 'Italy', 'Luxembourg', 'Netherlands', 'Spain', 'Sweden', 'United Kingdom')


EU.15.average = data %>%
  filter(country %in% OMS) %>%
  group_by(year, sex, sex2, age, type, group) %>%
  summarise(mx.average = weighted.mean(mx.avoided, pop))

EU.15.best = data %>%
  filter(country %in% OMS) %>%
  group_by(year, sex, sex2, age, type, group) %>%
  summarise(mx.lowest = min(mx.avoided, pop))

### First, dataset for deleted lifetables

scenario1 = data %>%
  filter(group == 'Total') %>%
  mutate(scenario = paste('deleted', type, sep = '.')) %>%
  select(-group, -pop, -type)


## For deleted lt: we're setting mx to represent the diff between current mx and the average/lowest observed avoidable mx in EU15
## For OMS, we set the improvement to 0 in these scenarios

scenario2 = data %>%
  filter(group == 'Total') %>%
  left_join(., EU.15.average) %>%
  mutate(mx.avoided = ifelse(country %in% NMS & mx.avoided > mx.average, mx.avoided-mx.average, 0),
         scenario = paste('average', type, sep = '.')) %>%
  select(-mx.average, -group, -pop, -type)
  
  
scenario3 = data %>%
  filter(group == 'Total') %>%
  left_join(., EU.15.best) %>%
  mutate(mx.avoided = ifelse(country %in% NMS & mx.avoided > mx.lowest, mx.avoided-mx.lowest, 0),
         scenario = paste('lowest', type, sep = '.')) %>%
  select(-mx.lowest, -group, -pop, -type)


result = rbind(scenario1, scenario2, scenario3) %>%
  mutate(NMS = ifelse(country %in% NMS, 'NMS', 'OMS'))%>%
  arrange(scenario, NMS, country, year, sex, age)

saveRDS(result, 'temp/scenarios_deleted_lt.rds')


## Second, dataset for decomposition analysis

## We need a baseline for comparison

baseline = data %>%
  rename(mx.avoidable = mx.avoided) %>%
  pivot_wider(names_from = group, values_from = mx.avoidable, names_prefix = 'baseline.mx.') %>%
  mutate(baseline.mx.notAvoidable = mx.allcauses-baseline.mx.Total) %>%
  select(-pop, -mx.allcauses, -baseline.mx.Total)

## The first scenario is deleting all avoidable, preventable, or treatable illnesses 
## This means that we set all the mx in the relevant category to 0

## NA's to be filled in with nMxd from the cause-deleted lifetables in next steps

scenario1 = data %>%
  rename(mx.avoidable = mx.avoided) %>%
  mutate(mx.avoidable = 0) %>%
  pivot_wider(names_from = group, values_from = mx.avoidable, names_prefix = 'mx.') %>%
  mutate(mx.notAvoidable = NA,
         scenario = paste('deleted', type, sep = '.')) %>%
  select(-pop, -mx.allcauses, -mx.Total)


## For decomposition: we're setting mx to represent the actually observed average/lowest mx in EU15
## For OMS, we use the status quo values, since we're not including them in these scenarios

scenario2 = data %>%
  rename(mx.avoidable = mx.avoided) %>%
  left_join(., EU.15.average) %>%
  mutate(mx.avoidable = ifelse(country %in% NMS, mx.average, mx.avoidable)) %>%
  select(-mx.average) %>%
  pivot_wider(names_from = group, values_from = mx.avoidable, names_prefix = 'mx.') %>%
  mutate(mx.notAvoidable = NA,
         scenario = paste('average', type, sep = '.')) %>%
  select(-pop, -mx.allcauses, -mx.Total)


scenario3 = data %>%
  rename(mx.avoidable = mx.avoided) %>%
  left_join(., EU.15.best) %>%
  mutate(mx.avoidable = ifelse(country %in% NMS, mx.lowest, mx.avoidable)) %>%
  select(-mx.lowest) %>%
  pivot_wider(names_from = group, values_from = mx.avoidable, names_prefix = 'mx.') %>%
  mutate(mx.notAvoidable = NA,
         scenario = paste('lowest', type, sep = '.')) %>%
  select(-pop, -mx.allcauses, -mx.Total)


result = rbind(scenario1, scenario2, scenario3) %>%
  left_join(., baseline) %>%
  mutate(NMS = ifelse(country %in% NMS, 'NMS', 'OMS'))%>%
  arrange(country, year, sex, scenario, age) %>%
  select(-type)

saveRDS(result, 'temp/scenarios_decomp.rds')
