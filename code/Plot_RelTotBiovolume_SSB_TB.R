# Plotting SSB and TB phytoplankton biovolumes
library(tidyverse)
library(scales)
library(patchwork)
#Load data
taxanthangs = read_csv('SSBTBtaxalevel.csv')
taxanthangs$sample_date=mdy(taxanthangs$sample_date)

taxanthangs.shrike = select(taxanthangs, 4,8,23,47,48) %>% 
  mutate(month = month(sample_date)) %>% 
  mutate(year = year(sample_date))

# The only tricky thing here is converting the x-axis to the same origin
p1 = ggplot(taxanthangs.shrike, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, 
                               x = as.Date(yday(sample_date), "1970-01-01"))) + 
  geom_bar(position="stack", stat="identity", width = 12) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~sample_id + year) +
  xlab('Sample Date') +
  ylab('Total biovolume (add units)') +
  theme_classic()

p2 = ggplot(taxanthangs.shrike, aes(fill=division, y=relative_total_biovolume, 
                               x = as.Date(yday(sample_date), "1970-01-01"))) + 
  geom_bar(position="stack", stat="identity", width = 12) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~sample_id + year) +
  xlab('Sample Date') +
  ylab('Relative total biovolume') +
  theme_classic()

p1/p2 + plot_layout(guides = "collect")
ggsave('RelativeTotalBiovolumes_SSB_TB.png', width = 6, height = 6, dpi = 500)
