rm(list = ls())

require(tidyverse)
require(gt)
require(cowplot)
require(flextable)
require(officer)


### Overview of contributions by NMS, scenario, and age

decomp = readRDS('temp/result_decomp_age_cause.rds') %>%
  filter(year %in% 2005:2019 & country != 'Greece') %>%
  mutate(age= as.integer(age),
         group = str_sub(group, start = 4L),
         group = ifelse(group == 'notAvoidable', 'Nonavoidable deaths', group),
         group = str_replace(group, 'Alcohol.', 'Alcohol-'),
         group = str_replace_all(group, 'Pregnancy.', 'Pregnancy,'),
         group = str_replace_all(group, '\\.', ' '),
         age_group = case_when(age == 0 ~ '0-1',
                               age == 1 ~ '1-4',
                               between(age, 5, 84) ~paste0(age, '-', age+4),
                               age > 84 ~ '85+'),
         age_group = factor(age_group, levels = c('0-1','1-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39',
                                                  '40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79',
                                                  '80-84','85+')),
         NMS = ifelse(NMS == "NMS", 'New member states', 'Established member states'))
  

decomp_by_NMS = decomp %>%
  filter(scenario %in% c('deleted.avoidable', 'average.avoidable')) %>%
  group_by(NMS, sex, scenario, age_group, group) %>%
  summarise(contribution.e0 = mean(contribution.e0),
            contribution.edag= mean(contribution.edag))

## Figure 2

decomp_by_NMS_simplified = decomp_by_NMS %>%
  mutate(group2 = case_when(group == 'Cancer' ~ 'Cancer',
                            group == "Diseases of the circulatory system" ~ "Diseases of the circulatory system",
                            group == 'Injuries' ~ 'Injuries',
                            group == "Pregnancy, childbirth and perinatal period" ~ "Pregnancy, childbirth and perinatal period",
                            .default = 'Other causes')) %>%
  group_by(NMS, sex, scenario, age_group, group2) %>%
  summarise(contribution.e0 = sum(contribution.e0),
            contribution.edag = sum(contribution.edag))

a = ggplot(filter(decomp_by_NMS_simplified, scenario == 'deleted.avoidable' & NMS == "New member states"), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.e0))+
  facet_grid(sex  ~ NMS)+
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  xlab('')+
  ylab('Gain in life expectancy (years)')+
  ylim(c(-0.02,1.5))

b = ggplot(filter(decomp_by_NMS_simplified, scenario == 'deleted.avoidable' & NMS == 'Established member states'), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.e0))+
  facet_grid(sex  ~ NMS)+
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  xlab('')+
  ylab('')+
  ylim(c(-0.02,1.5))

c = ggplot(filter(decomp_by_NMS_simplified, scenario == 'average.avoidable' & NMS == "New member states"), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.e0))+
  facet_grid(sex ~ NMS) +
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  xlab('Age')+
  ylab('')+
  ylim(c(-0.02,1.5))

d = ggplot(filter(decomp_by_NMS_simplified, scenario == 'average.avoidable' & NMS == "New member states"), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.e0))+
  facet_grid(sex ~ NMS) +
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank())+
  xlab('Age')+
  ylab('')+
  ylim(c(-0.02,1.5))


legend = get_legend(d)

p2 <- plot_grid(a,b,c,legend, ncol = 2, nrow = 2, align = "hv", axis = "tblr", labels = c('a','','b','')) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave('plots/p2.pdf', p2, units = 'mm', width = 250, height = 190)
ggsave('plots/p2.png', p2, units = 'mm', width = 250, height = 190)


### Supplementary material

### edag figure

e = ggplot(filter(decomp_by_NMS_simplified, scenario == 'deleted.avoidable' & NMS == "New member states"), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.edag))+
  facet_grid(sex  ~ NMS)+
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  xlab('')+
  ylab('Reduction in lifespan disparity (years)')+
  ylim(c(-0.75,0.02))

f = ggplot(filter(decomp_by_NMS_simplified, scenario == 'deleted.avoidable' & NMS == 'Established member states'), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.edag))+
  facet_grid(sex  ~ NMS)+
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  xlab('')+
  ylab('')+
  ylim(c(-0.75,0.02))

g = ggplot(filter(decomp_by_NMS_simplified, scenario == 'average.avoidable' & NMS == "New member states"), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.edag))+
  facet_grid(sex ~ NMS) +
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  xlab('Age')+
  ylab('')+
  ylim(c(-0.75,0.02))

h = ggplot(filter(decomp_by_NMS_simplified, scenario == 'average.avoidable' & NMS == "New member states"), aes(x = age_group, fill = group2)) +
  geom_col(aes(y = contribution.edag))+
  facet_grid(sex ~ NMS) +
  theme_bw()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank())+
  xlab('Age')+
  ylab('')+
  ylim(c(-0.75,0.02))


legend = get_legend(h)

supp_p2 <- plot_grid(e,f,g,legend, ncol = 2, nrow = 2, align = "hv", axis = "tblr", labels = c('a','','b','')) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave('plots/supp_p2.png', supp_p2, units = 'mm', width = 250, height = 190)



proportions = decomp_by_NMS %>% filter(scenario == 'average.avoidable' & NMS == "New member states") %>% 
  group_by(sex) %>% mutate(total.e0 = sum(contribution.e0), proportion.e0 = contribution.e0/total.e0,
                           total.edag = sum(contribution.edag), proportion.edag = contribution.edag/total.edag) %>% 
  group_by(sex, group) %>% summarise(proportion.e0 = sum(100*proportion.e0),
                                     proprtion.edag = sum(100*proportion.edag))



### Supplementary overview by country


decomp_by_country = decomp %>%
  filter(scenario %in% c('deleted.avoidable', 'average.avoidable')) %>%
  group_by(country, sex, scenario, age_group, group) %>%
  summarise(contribution.e0 = mean(contribution.e0),
            contribution.edag = mean(contribution.edag))

decomp_by_NMS = decomp_by_NMS %>%
  ungroup() %>%
  mutate(country = NMS) %>%
  select(-NMS)


decomp_by_country_NMS = rbind(decomp_by_country, decomp_by_NMS)

NMS <- c('Croatia', 'Czech Republic', 'Estonia', 'Hungary', 'Latvia', 'Lithuania', 'Poland', 'Slovakia', 'Slovenia', 'Bulgaria', 'Romania', 'Malta', 'Cyprus', 'New member states')


## deleted avoidable, men

supp_table_5 = decomp_by_country_NMS %>%
  ungroup() %>%
  filter(scenario == 'deleted.avoidable', sex == 'Men', !age_group %in% c('75-79', '80-84', '85+')) %>%
  mutate(contribution.e0 = round(contribution.e0, 2)) %>%
  select(-c(sex, scenario, contribution.edag)) %>%
  pivot_wider(names_from = age_group, values_from = contribution.e0) %>%
  flextable() %>% 
  merge_v(., j = 'country') %>%
  set_header_labels(., values = list(country='Country', group = 'Cause Group')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 5: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("The average contribution of cause groups to the estimated gains in male life expectancy by EU member state if all avoidable deaths were averted, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE)  %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_5, path = 'tables/supp_table5.docx', pr_section = prop_section(page_size(orient = 'landscape')))


## deleted avoidable, women

supp_table_6 = decomp_by_country_NMS %>%
  ungroup() %>%
  mutate(contribution.e0 = round(contribution.e0, 2)) %>%
  filter(scenario == 'deleted.avoidable', sex == 'Women', !age_group %in% c('75-79', '80-84', '85+')) %>%
  select(-c(sex, scenario, contribution.edag)) %>%
  pivot_wider(names_from = age_group, values_from = contribution.e0) %>%
  flextable() %>% 
  merge_v(., j = 'country') %>%
  set_header_labels(., values = list(country='Country', group = 'Cause Group')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 6: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("The average contribution of cause groups to the estimated gains in female life expectancy by EU member state if all avoidable deaths were averted, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_6, path = 'tables/supp_table6.docx', pr_section = prop_section(page_size(orient = 'landscape')))


## average avoidable, NMS, men

supp_table_7 = decomp_by_country_NMS %>%
  ungroup() %>%
  mutate(contribution.e0 = round(contribution.e0, 2)) %>%
  filter(scenario == 'average.avoidable', sex == 'Men', country %in% NMS, !age_group %in% c('75-79', '80-84', '85+')) %>%
  select(-c(sex, scenario, contribution.edag)) %>%
  pivot_wider(names_from = age_group, values_from = contribution.e0) %>%
  flextable() %>% 
  merge_v(., j = 'country') %>%
  set_header_labels(., values = list(country='Country', group = 'Cause Group')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 7: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("The average contribution of cause groups to the estimated gains in male life expectancy by new member state if they were assigned average avoidable mortality rates observed across the established member states, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_7, path = 'tables/supp_table7.docx', pr_section = prop_section(page_size(orient = 'landscape')))

## average avoidable, NMS, women


supp_table_8 = decomp_by_country_NMS %>%
  ungroup() %>%
  mutate(contribution.e0 = round(contribution.e0, 2)) %>%
  filter(scenario == 'average.avoidable', sex == 'Women', country %in% NMS, !age_group %in% c('75-79', '80-84', '85+')) %>%
  select(-c(sex, scenario, contribution.edag)) %>%
  pivot_wider(names_from = age_group, values_from = contribution.e0) %>%
  flextable() %>% 
  merge_v(., j = 'country') %>%
  set_header_labels(., values = list(country='Country', group = 'Cause Group')) %>%
  valign(valign ='top') %>%
  align(align = 'left', part = 'all') %>%
  set_table_properties(layout = "autofit") %>%
  bold(bold = TRUE, part = "header") %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  set_caption(caption = as_paragraph(
    as_chunk("Supplementary Table 8: ", props = fp_text(bold = TRUE, italic = TRUE, font.family = 'Times New Roman')),
    as_chunk("The average contribution of cause groups to the estimated gains in female life expectancy by new member state if they were assigned average avoidable mortality rates observed across the established member states, 2005-2019", 
             props = fp_text(bold = TRUE, font.family = 'Times New Roman'))),
    align_with_table = TRUE) %>%
  bg(i = ~ country %in% NMS, bg = "#F2F2F2", part = "body")

save_as_docx(supp_table_8, path = 'tables/supp_table8.docx', pr_section = prop_section(page_size(orient = 'landscape')))

