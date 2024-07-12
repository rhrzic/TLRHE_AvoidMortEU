rm(list = ls())

require(tidyverse)
require(directlabels)
require(scales)
require(viridis)
require(cowplot)


result = readRDS('temp/result_e0_edag.rds') %>%
  filter(year %in% 2005:2019 & country != "Greece")


## Overview plot

plot = result %>% filter(scenario %in% c('deleted.avoidable', 'average.avoidable')) %>%
  mutate(scenario = case_when(scenario == 'deleted.avoidable' ~ 'All avoidable deaths removed',
                              scenario == 'average.avoidable' ~ 'Average avoidable mortality rates\napplied to new member states'),
         scenario = factor(scenario, levels = c('Average avoidable mortality rates\napplied to new member states',
                                                'All avoidable deaths removed')),
         NMS = ifelse(NMS == 'NMS', 'New member states', 'Established member states'),
         NMS = factor(NMS, levels = c('Established member states', 'New member states'))) %>%
  group_by(NMS, sex, year, scenario) %>%
  summarise(e0 = mean(e0),
            e0.d = mean(e0.d),
            e0.gain = mean(e0.gain),
            edag = mean(edag),
            edag.d = mean(edag.d),
            edag.gain = mean(edag.gain)) %>%
  mutate(arrow = case_when(NMS == 'New member states' & year %in% c(2006, 2011, 2016) ~ year,
                           NMS == 'Established member states' & year %in% c(2007, 2012, 2017) & scenario == 'All avoidable deaths removed' ~ year))

a = ggplot(plot, aes(x = year, color = NMS, group = interaction(NMS, scenario, sex)))+
  geom_line(aes(y = e0, linetype = 'Baseline'))+
  geom_line(aes(y = e0.d, linetype = 'Scenario'))+
  geom_segment(aes(x = arrow, xend = arrow, y = e0, yend = e0.d), lineend = 'round', arrow = arrow(length = unit(2, "mm")), key_glyph = "rect")+
  facet_grid(sex~scenario)+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_blank(),
        text = element_text(size = 14))+
  xlab('Year')+
  ylab('Life expectancy at birth')+
  scale_x_continuous(breaks= pretty_breaks())


b = ggplot(plot, aes(x = year, color = NMS, group = interaction(NMS, scenario, sex)))+
  geom_line(aes(y = edag, linetype = 'Baseline'))+
  geom_line(aes(y = edag.d, linetype = 'Scenario'))+
  geom_segment(aes(x = arrow, xend = arrow, y = edag, yend = edag.d), lineend = 'round', arrow = arrow(length = unit(2, "mm")), key_glyph = "rect")+
  facet_grid(sex~scenario)+
  theme_bw()+
  theme(legend.position = 'none',
        legend.title = element_blank(),
        text = element_text(size = 14))+
  xlab('Year')+
  ylab('Lifespan disparity')+
  scale_x_continuous(breaks= pretty_breaks())


p1 <- plot_grid(a,b, ncol = 1, nrow = 2, align = "hv", axis = "tblr", labels = 'auto')

ggsave('plots/p1.pdf', p1, units = 'mm', width = 190, height = 250)


## Overview table

require(flextable)
require(officer)

table_baseline = result %>%
  filter(scenario == 'deleted.avoidable') %>%
  mutate(scenario = 'Status quo',
         NMS = ifelse(NMS == 'NMS', 'NMS', 'EMS')) %>%
  group_by(NMS, sex, scenario) %>%
  summarise(e0 = mean(e0),
            edag = mean(edag)) %>%
  pivot_wider(names_from = NMS, values_from = c(e0, edag))

table = result %>%
  filter(!scenario %in% c('lowest.avoidable', 'lowest.preventable', 'lowest.treatable')) %>%
  mutate(scenario = case_when(scenario == 'deleted.avoidable' ~ 'All avoidable deaths removed',
                              scenario == 'deleted.preventable' ~ 'Only preventable deaths removed',
                              scenario == 'deleted.treatable' ~ 'Only treatable deaths removed',
                              scenario == 'average.avoidable' ~ 'Average mortality rates for all avoidable causes applied to NMS',
                              scenario == 'average.preventable' ~ 'Average mortality rates for only preventable causes applied to NMS',
                              scenario == 'average.treatable' ~ 'Average mortality rates for only treatable causes applied to NMS'),
         NMS = ifelse(NMS == 'NMS', 'NMS', 'EMS')) %>%
  group_by(NMS, sex, scenario) %>%
  summarise(e0 = mean(e0.d),
            edag = mean(edag.d)) %>%
  pivot_wider(names_from = NMS, values_from = c(e0, edag)) %>%
  rbind(table_baseline) %>%
  mutate(scenario = factor(scenario, 
                    levels = c('Status quo',
                               'Average mortality rates for all avoidable causes applied to NMS',
                               'Average mortality rates for only preventable causes applied to NMS',
                               'Average mortality rates for only treatable causes applied to NMS',
                               'All avoidable deaths removed',
                               'Only preventable deaths removed',
                               'Only treatable deaths removed'))) %>%
  arrange(sex, scenario) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  mutate(e0_difference = e0_NMS-e0_EMS,
         edag_difference = edag_NMS-edag_EMS) %>%
  select(sex, scenario, 
         e0_NMS, e0_EMS, e0_difference,
         edag_NMS, edag_EMS, edag_difference)
         


table2 = flextable(table) %>%
  merge_v(., j = 'sex') %>%
  set_header_labels(., values = list(sex = 'Sex', scenario = 'Scenario', 
                    e0_NMS = 'NMS', e0_EMS = 'EMS', e0_difference = 'Difference',
                    edag_NMS = 'NMS', edag_EMS= 'EMS', edag_difference = 'Difference')) %>%
  add_header_row(., values = c('', 'Life expectancy at birth', 'Lifespan disparity'), colwidths = c(2,3,3)) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  autofit() %>%
  footnote(i = 2, j = 3:4,
           value = as_paragraph(
             c(
               "New member states",
               "Established member states"
             )
           ),
           ref_symbols = c("a", "b"),
           part = "header", inline = TRUE) %>%
  bold(bold = TRUE, part = "header") %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  set_caption(caption = as_paragraph(
    as_chunk("Table 1: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("Life expectancy at birth and lifespan disparity in new and established member states under different scenarios, 2005-2019, by sex", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE)
  

save_as_docx(table2, path = 'tables/table2.docx', pr_section = prop_section(page_size(orient = 'landscape')))
