# Plotting SSB and TB phytoplankton biovolumes
library(tidyverse)
library(scales)
library(patchwork)
#Load data
#taxanthangs = read_csv('SSBTBtaxalevel.csv')
#taxanthangs$sample_date=mdy(taxanthangs$sample_date)

#taxanthangs.shrike = select(taxanthangs, 4,8,23,47,48) %>% 
#  mutate(month = month(sample_date)) %>% 
#  mutate(year = year(sample_date))

#load in the adjusted one 
taxanthangs.adj = read_csv('data/SSBTBtaxalevel.adj.csv')
taxanthangs.adj$sample_date=mdy(taxanthangs.adj$sample_date)

taxanthangs.shrike.adj = select(taxanthangs.adj, 4,8,23,47,48) %>% 
  mutate(month = month(sample_date)) %>% 
  mutate(year = year(sample_date))



# The only tricky thing here is converting the x-axis to the same origin
p1 = ggplot(taxanthangs.shrike.adj, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, 
                                    x = as.Date(yday(sample_date), "1970-01-01"))) + 
  geom_bar(position="stack", stat="identity", width = 12) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~sample_id + year) +
  xlab('Sample Date') +
  ylab(bquote(Biovolume ~ (mu*m^3~ml^-1)))+
  theme_classic()

p2 = ggplot(taxanthangs.shrike.adj, aes(fill=division, y=relative_total_biovolume, 
                                    x = as.Date(yday(sample_date), "1970-01-01"))) + 
  geom_bar(position="stack", stat="identity", width = 12) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~sample_id + year) +
  xlab('Sample Date') +
  ylab('Relative total biovolume') +
  theme_classic()

p1/p2 + plot_layout(guides = "collect")+plot_annotation(tag_levels = 'A')+
  plot_annotation(caption = 'Figure 17: South Sparkling Bog 2019 and 2020 and Trout Bog 2020 total phytoplankton biovolume (A) 
                  and relative total biovolume (B). Phytoplankton proccessed at PhycoTech Inc.',
                  theme = theme(plot.caption = element_text(size = 8)))
  
ggsave('17.RelativeTotalBiovolumes_SSB_TB_finalgraph.png', width = 6, height = 6, dpi = 500)


######################
#for the powerpoint

taxanthangs.shrike.adj.2 = select(taxanthangs.adj, 4,8,23,47,48) %>% 
  mutate(month = month(sample_date)) %>% 
  mutate(year = year(sample_date))%>%
  filter(sample_id =='SSB')



# The only tricky thing here is converting the x-axis to the same origin
p1 = ggplot(taxanthangs.shrike.adj.2, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, 
                                        x = as.Date(yday(sample_date), "1970-01-01"))) + 
  geom_bar(position="stack", stat="identity", width = 12) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~sample_id + year) +
  xlab('Sample Date') +
  ylab(bquote(Biovolume ~ (mu*m^3~ml^-1)))+
  theme_classic(base_size = 18)
ggsave('ppt.Biovolumes_SSB_TB_finalgraph.short.png', width = 8, height = 5, dpi = 500)


p2 = ggplot(taxanthangs.shrike.adj.2, aes(fill=division, y=relative_total_biovolume, 
                                        x = as.Date(yday(sample_date), "1970-01-01"))) + 
  geom_bar(position="stack", stat="identity", width = 12) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~sample_id + year) +
  xlab('Sample Date') +
  ylab('Relative total biovolume') +
  theme_classic(base_size = 18)

ggsave('ppt.RelativeTotalBiovolumes_SSB_TB_finalgraph.png', width = 8, height = 5, dpi = 500)
