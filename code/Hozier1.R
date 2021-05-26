# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)
library(pastecs)
library('unikn')
library(tidyverse)
library(lubridate)
library(patchwork)

##################

BE <-bind_rows(No.Plan.Jan,No.Plan.Feb,No.Plan.March,No.Plan.April)
#chloros, cryptos, bacilliarophyta, cyanos
BE1<-ggplot(BE, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=sample_date)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y="Total Biovolume um3/ml")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())

BE1

ggsave("ppt.controlssb2019phytoz.png", plot = last_plot(), height = 10, width = 12, units = "in")


Be2<-ggplot(BE, aes(fill=division, y=relative_total_biovolume, x=sample_date)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y="Relative Biovolume um3/ml")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())
Be2


Be3=(BE1/Be2)+plot_layout(guides = 'collect')+
  plot_annotation(caption = 'Figure 15: Phytoplankton data from South Sparkling Bog during the control year, 2019.',
                  theme = theme(plot.caption = element_text(size = 8)))

ggsave("controlssb2019phytoz.png", plot = last_plot(), height = 10, width = 7, units = "in")


######

As<-bind_rows(Nobody.Jan,Nobody.Jan.2,Nobody.Feb,Nobody.March, Nobody.May)

as1<- ggplot(As, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=sample_date)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y="Total Biovolume um3/ml")+
  theme_classic()+
  theme(axis.title.x = element_blank())
#ggsave("SSB2020bymonth.png", plot = last_plot(), height = 8, width = 12, units = "in")

as2<- ggplot(As, aes(fill=division, y=relative_total_biovolume, x=sample_date)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y="Relative Biovolume um3/ml")+
  theme_classic()+
  theme(axis.title.x = element_blank())
#ggsave("SSB2020bymonth.png", plot = last_plot(), height = 8, width = 12, units = "in")

as3=(as1/as2)+plot_layout(guides = 'collect')+
  plot_annotation(caption = 'Figure 16: Phytoplankton data from South Sparkling Bog during the experimental year, 2020.',
                  theme = theme(plot.caption = element_text(size = 8)))
ggsave("expssb2020phytoz.png", plot = last_plot(), height = 10, width = 7, units = "in")





ISC1=as2+theme_bw(base_size = 18)+theme(axis.title.x = element_blank())
ISC1
  


ISC3=Be2+theme_bw(base_size = 18)
ISC3+ISC1+plot_layout(guides = 'collect')
ISEECOLOR
ggsave("pptgraphs.png", plot = last_plot(), height = 10, width = 7, units = "in")

######

Talk<-bind_rows(Sing.Jan,Sing.Feb,Sing.March)

t1<- ggplot(Talk, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=sample_date)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y="Total Biovolume um3/ml")+
  theme_classic()+
  theme(axis.title.x = element_blank())

t2<- ggplot(Talk, aes(fill=division, y=relative_total_biovolume, x=sample_date)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y="Relative Biovolume um3/ml")+
  theme_classic()+
  theme(axis.title.x = element_blank())
#ggsave("SSB2020bymonth.png", plot = last_plot(), height = 8, width = 12, units = "in")

t3=(t1/t2)+plot_layout(guides = 'collect')+
  plot_annotation(caption = 'Figure 17: Phytoplankton data from Trout Bog during 2020.',
                  theme = theme(plot.caption = element_text(size = 8)))

ggsave("TB2020phytoz.png", plot = last_plot(), height = 10, width = 7, units = "in")


tot.things=(BE1+Be2)/(as1+as2)/(t1+t2)+plot_layout(guides = 'collect')+plot_annotation(tag_levels = 'A')
ggsave("thisshouldbebetter.png", plot = last_plot(), height = 10, width = 7, units = "in")

#tot.things2=(Be3)/(as3)/(t3)+plot_layout(guides = 'collect')

#######

Cherry.wine <- bind_rows(BE,As,Talk)


ggplot(Cherry.wine, aes(fill=division, y=relative_total_biovolume, x=sample_date))+
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  facet_wrap(~sample_id + year)
ggsave("QuickSaveforHil.png", plot = last_plot(), height = 10, width = 7, units = "in")


ggplot(Cherry.wine, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=sample_date))+
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  facet_wrap(~sample_id + year)
ggsave("QuickSaveforHil2.png", plot = last_plot(), height = 10, width = 7, units = "in")
