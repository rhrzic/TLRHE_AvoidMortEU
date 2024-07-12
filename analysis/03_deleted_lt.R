rm(list = ls())

require(tidyverse)
require(DemoTools)

data = readRDS('temp/scenarios_deleted_lt.rds')
source('analysis/functions.R')


lt = data %>%
  ungroup() %>%
  group_by(year, country, NMS, sex, scenario) %>%
  group_modify(~ltab(Mx = .x$mx.allcauses), 
               country = country,
               NMS = NMS,
               year = year,
               sex = sex,
               scenario = scenario) %>%
  left_join(data, ., by = c('country', 'NMS', 'year','sex','scenario', 'age')) %>%
  arrange(year, country, sex, scenario)

saveRDS(lt, 'temp/result_lt.rds')


lt_d = lt %>%
  ungroup() %>%
  group_by(year, country, NMS, sex, scenario) %>%
  group_modify(~lt_deleted(nMx = .x$mx.allcauses, nMxi = .x$mx.avoided, nqx = .x$nqx, age = .x$age, ageint = .x$ageint, nax = .x$nax),
               country = country,
               NMS = NMS,
               year = year,
               sex = sex,
               scenario = scenario) %>%
  left_join(lt, ., by = c('country', 'NMS', 'year', 'sex','scenario', 'age','ageint')) %>%
  arrange(year, country, sex, scenario)

saveRDS(lt_d, 'temp/result_lt_d.rds')


result = lt_d %>%
  group_by(year, country, NMS, sex, scenario) %>%
  summarise(e0 = first(ex), e0.d = first(exd), e0.gain = round(e0.d-e0,2),
            edag = ed(Mx = nMx),
            edag.d = ed(Mx = nMxd),
            edag.gain = round(edag.d-edag,2))

saveRDS(result, 'temp/result_e0_edag.rds')

result = readRDS('temp/result_e0_edag.rds')

## Sanity check

result %>%
  filter(scenario %in% c('deleted.avoidable'), sex == 'Women') %>%
  ggplot(aes(x = country, y = e0.gain, fill = NMS))+
  geom_col()+
  facet_wrap(.~year)

# Comparison with HMD

require(HMDHFDplus)

countries = unique(result$country)
cntries = filter(getHMDcountries(), Country %in% countries) %>% select(CNTRY)

HMD.e0 = NULL

for (i in cntries$CNTRY) {
  print(i)
  temp = readHMDweb(i,"E0per",username = 'rok.hrzic@gmail.com', password = 'MPq.K3Pf')
  temp$CNTRY = i
  HMD.e0 = rbind(HMD.e0, temp)
}


HMD.e0.diff = HMD.e0 %>% filter(Year %in% 2004:2019) %>%
  select(-Total) %>%
  left_join(., getHMDcountries()) %>%
  pivot_longer(Male:Female, names_to = 'sex', values_to = 'e0.HMD') %>%
  mutate(sex = ifelse(sex == 'Male', 'Men', 'Women')) %>%
  left_join(., result, by = c('Year' = 'year', 'Country' = 'country', 'sex')) %>%
  group_by(Country, Year, sex) %>%
  summarise(e0 = first(e0), e0.HMD = first(e0.HMD), e0.diff = e0 - e0.HMD, e0.rel.diff = 100*e0.diff/e0.HMD)


supp_p1 = ggplot(HMD.e0.diff, aes(x = Year, y = e0.rel.diff, group = Year)) + 
  geom_boxplot() + 
  facet_grid(.~sex) + 
  ylab('Difference from HMD estimate (%)') +
  theme_bw()
  

ggsave('plots/supp_p1.png', supp_p1, width = 200, height = 150, units = 'mm')
