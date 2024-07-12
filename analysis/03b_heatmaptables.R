rm(list=ls())

library(tidyverse)
library(data.table)
library(gt)
library(scales)

dat1<-readRDS("temp/result_e0_edag.rds") %>%
  filter(year %in% 2005:2019 & country != 'Greece') %>%
  filter(!scenario %in% c('lowest.avoidable', 'lowest.preventable', 'lowest.treatable')) %>%
  group_by(country, sex, scenario) %>%
  summarise(e0 = mean(e0),
            edag = mean(edag),
            e0.gain = mean(e0.gain),
            edag.gain = mean(edag.gain))

dat2 = readRDS("temp/result_e0_edag.rds") %>%
  filter(year %in% 2005:2019 & country != 'Greece') %>%
  filter(!scenario %in% c('lowest.avoidable', 'lowest.preventable', 'lowest.treatable')) %>%
  group_by(NMS, sex, scenario) %>%
  summarise(e0 = mean(e0),
            edag = mean(edag),
            e0.gain = mean(e0.gain),
            edag.gain = mean(edag.gain)) %>%
  mutate(country = NMS,
         country = ifelse(country == 'NMS', 'New member states', 'Established member states')) %>%
  ungroup() %>%
  select(-NMS)

dat_summary = rbind(dat1, dat2) 


## LE, men

NMS <- c('Croatia', 'Czech Republic', 'Estonia', 'Hungary', 'Latvia', 'Lithuania', 'Poland', 'Slovakia', 'Slovenia', 'Bulgaria', 'Romania', 'Malta', 'Cyprus', 'New member states')

supp_table_1 = dat_summary %>%
  ungroup() %>%
  filter(sex == 'Men') %>%
  select(-c(sex, edag, edag.gain)) %>%
  mutate(e0 = round(e0, 2),
         e0.gain = round(e0.gain, 2)) %>%
  pivot_wider(names_from = scenario, values_from = e0.gain) %>%
  flextable() %>% 
  set_header_labels(., values = list(country='Country', 
                                     e0 = 'Status Quo', 
                                     average.avoidable = 'EMS avoidable mortality in NMS',
                                     average.preventable = 'EMS preventable mortality in NMS',
                                     average.treatable = 'EMS treatable mortality in NMS',
                                     deleted.avoidable = 'All avoidable deaths removed',
                                     deleted.preventable = 'Only preventable deaths removed',
                                     deleted.treatable = 'Only treatable deaths removed')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 1: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("Life expectancy gains in men under different scenarios, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  footnote(i = 1, j = 3:5,
           value = as_paragraph("NMS = New member states, EMS = Established member states"),
           ref_symbols = "a",
           part = "header", inline = TRUE) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_1, path = 'tables/supp_table1.docx')


## LE, women

supp_table_2 = dat_summary %>%
  ungroup() %>%
  filter(sex == 'Women') %>%
  select(-c(sex, edag, edag.gain)) %>%
  mutate(e0 = round(e0, 2),
         e0.gain = round(e0.gain, 2)) %>%
  pivot_wider(names_from = scenario, values_from = e0.gain) %>%
  flextable() %>% 
  set_header_labels(., values = list(country='Country', 
                                     e0 = 'Status Quo', 
                                     average.avoidable = 'EMS avoidable mortality in NMS',
                                     average.preventable = 'EMS preventable mortality in NMS',
                                     average.treatable = 'EMS treatable mortality in NMS',
                                     deleted.avoidable = 'All avoidable deaths removed',
                                     deleted.preventable = 'Only preventable deaths removed',
                                     deleted.treatable = 'Only treatable deaths removed')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 1: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("Life expectancy gains in women under different scenarios, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  footnote(i = 1, j = 3:5,
           value = as_paragraph("NMS = New member states, EMS = Established member states"),
           ref_symbols = "a",
           part = "header", inline = TRUE) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all')  %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_2, path = 'tables/supp_table2.docx')

## LD, men

supp_table_3 = dat_summary %>%
  ungroup() %>%
  filter(sex == 'Men') %>%
  select(-c(sex, e0, e0.gain)) %>%
  mutate(edag = round(edag, 2),
         edag.gain = round(edag.gain, 2)) %>%
  pivot_wider(names_from = scenario, values_from = edag.gain) %>%
  flextable() %>% 
  set_header_labels(., values = list(country='Country', 
                                     edag = 'Status Quo', 
                                     average.avoidable = 'EMS avoidable mortality in NMS',
                                     average.preventable = 'EMS preventable mortality in NMS',
                                     average.treatable = 'EMS treatable mortality in NMS',
                                     deleted.avoidable = 'All avoidable deaths removed',
                                     deleted.preventable = 'Only preventable deaths removed',
                                     deleted.treatable = 'Only treatable deaths removed')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 1: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("Lifespan disparity reductions in men under different scenarios, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  footnote(i = 1, j = 3:5,
           value = as_paragraph("NMS = New member states, EMS = Established member states"),
           ref_symbols = "a",
           part = "header", inline = TRUE) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all')  %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_3, path = 'tables/supp_table3.docx')


## LD, men

supp_table_4 = dat_summary %>%
  ungroup() %>%
  filter(sex == 'Women') %>%
  select(-c(sex, e0, e0.gain)) %>%
  mutate(edag = round(edag, 2),
         edag.gain = round(edag.gain, 2)) %>%
  pivot_wider(names_from = scenario, values_from = edag.gain) %>%
  flextable() %>% 
  set_header_labels(., values = list(country='Country', 
                                     edag = 'Status Quo', 
                                     average.avoidable = 'EMS avoidable mortality in NMS',
                                     average.preventable = 'EMS preventable mortality in NMS',
                                     average.treatable = 'EMS treatable mortality in NMS',
                                     deleted.avoidable = 'All avoidable deaths removed',
                                     deleted.preventable = 'Only preventable deaths removed',
                                     deleted.treatable = 'Only treatable deaths removed')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 1: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("Lifespan disparity reductions in women under different scenarios, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  footnote(i = 1, j = 3:5,
           value = as_paragraph("NMS = New member states, EMS = Established member states"),
           ref_symbols = "a",
           part = "header", inline = TRUE) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_4, path = 'tables/supp_table4.docx')
