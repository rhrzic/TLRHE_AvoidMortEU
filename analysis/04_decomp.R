rm(list = ls())

require(tidyverse)
require(DemoDecomp)

lt_d = readRDS('temp/result_lt_d.rds')
source('analysis/functions.R')

#### HORIUCHI AGE DECOMPOSITION ###

result_decomp_age = lt_d %>%
  ungroup() %>%
  group_by(year, country, NMS, sex, scenario) %>%
  group_modify(~horiuchi_age(age = .x$age, mx.baseline = .x$nMx, mx.altered = .x$nMxd), 
               country = country,
               NMS = NMS,
               year = year,
               sex = sex,
               scenario = scenario) %>%
  left_join(lt_d, ., by = c('country', 'NMS', 'year','sex','scenario', 'age'))


## Testing for deviation between gains calculated in the deleted lifetable and with the decomposition. They should tend to 0

result_e0_dag = readRDS('temp/result_e0_edag.rds')

test = result_decomp_age %>%
  group_by(year, country, sex, scenario) %>%
  summarise(e0.gain.decomp = sum(contribution.e0),
            edag.gain.decomp = sum(contribution.edag)) %>%
  left_join(., result_e0_dag) %>%
  mutate(decomp.problem.e0 = round(e0.gain - e0.gain.decomp, 2),
         decomp.problem.edag = round(edag.gain - edag.gain.decomp, 2))
  

ggplot(test, aes(x = year, y = decomp.problem.e0, colour=country)) + geom_point() + facet_grid(sex~scenario) + ylab('Difference from deleted lt estimate')
ggplot(test, aes(x = year, y = decomp.problem.edag, colour=country)) + geom_point() + facet_grid(sex~scenario) + ylab('Difference from deleted lt estimate')

saveRDS(result_decomp_age, 'temp/result_decomp_age.rds')


#### HORIUCHI AGE-CAUSE DECOMPOSITION ###

nMxd = lt_d %>%
  select(country, year, sex, age, scenario, nMxd)

data = readRDS('temp/scenarios_decomp.rds') %>%
  left_join(., nMxd) %>%
  group_by(year, country, NMS, sex, scenario,age) %>%
  mutate(persistent.mortality = sum(c_across(`mx.Adverse effects of medical and surgical care`:`mx.Pregnancy, childbirth and perinatal period`)),
    mx.notAvoidable = nMxd-persistent.mortality) %>%
  ungroup() %>%
  select(-persistent.mortality, -nMxd)

result_decomp_age_cause = data %>%
  ungroup() %>%
  group_by(year, country, NMS, sex, scenario) %>%
  group_modify(~horiuchi_age_cause(age = .x$age, 
                             mx.baseline = select(.x, starts_with('baseline.')), 
                             mx.altered = select(.x, starts_with('mx.'))), 
               country = country,
               NMS = NMS,
               year = year,
               sex = sex,
               scenario = scenario)

saveRDS(result_decomp_age_cause, 'temp/result_decomp_age_cause.rds')


test = result_decomp_age_cause %>%
  group_by(year, country, sex, scenario) %>%
  summarise(e0.gain.decomp = sum(contribution.e0),
            edag.gain.decomp = sum(contribution.edag)) %>%
  left_join(., result_e0_dag) %>%
  mutate(decomp.problem.e0 = round(e0.gain - e0.gain.decomp, 2),
         decomp.problem.edag = round(edag.gain - edag.gain.decomp, 2))

ggplot(test, aes(x = year, y = decomp.problem.e0, colour=country)) + geom_point() + facet_grid(sex~scenario) + ylab('Difference from deleted lt estimate')

