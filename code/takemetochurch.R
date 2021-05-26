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
##############

#thisisit = read_csv('data/SSBTB_Indices.csv')

taxanthangs = read_csv('data/SSBTBtaxalevel_spelling.csv')
taxanthangs$sample_date=mdy(taxanthangs$sample_date)

##########
taxanthangs.reduction = select(taxanthangs, 4,8, 16,18,20,23,24,26,28,31,33,38,41:54)
taxanthangs.pb=select(taxanthangs.reduction, 1,2,6:12,19,20)
taxanthangs.shrike = select(taxanthangs, 4,8,23,47,48)

write.csv(taxanthangs.pb, 'data/taxanthangs.pb.2020.Dec.csv', row.names = F)

#stacked bar chart#

#####
taxanthangs.pb$month=month(taxanthangs.pb$sample_date)
taxanthangs.pb$year=year(taxanthangs.pb$sample_date)

taxanthangs.pb.2019= filter(taxanthangs.pb, year ==2019)
taxanthangs.pb.2020= filter(taxanthangs.pb, year ==2020)
taxanthangs.pb.2020.SSB=filter(taxanthangs.pb.2020, sample_id =='SSB')
taxanthangs.pb.2020.TB= filter(taxanthangs.pb.2020, sample_id =='TB')

write.csv(taxanthangs.pb.2019, 'data/taxanthangs.pb.2019.csv', row.names = F)
write.csv(taxanthangs.pb.2020.SSB, 'data/taxanthangs.pb.2020.SSB.csv', row.names = F)
write.csv(taxanthangs.pb.2020.TB, 'data/taxanthangs.pb.2020.TB.csv', row.names = F)



######
taxanthangs.shrike$month=month(taxanthangs.shrike$sample_date)
taxanthangs.shrike$year=year(taxanthangs.shrike$sample_date)

taxanthangs.shrike.2019= filter(taxanthangs.shrike, year ==2019)
taxanthangs.shrike.2020= filter(taxanthangs.shrike, year ==2020)
taxanthangs.shrike.2020.SSB=filter(taxanthangs.shrike.2020, sample_id =='SSB')
taxanthangs.shrike.2020.TB= filter(taxanthangs.shrike.2020, sample_id =='TB')
# Stacked
almost= ggplot(taxanthangs.shrike.2019, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()
 

sweet=ggplot(taxanthangs.shrike.2020.SSB, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=sample_date)) + 
    geom_bar(position="stack", stat="identity")+
  theme_classic()

music=ggplot(taxanthangs.shrike.2020.TB, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()

cherry= (almost+sweet+music)+plot_annotation(caption = 'Figure: SSB 2018-2019 on left 2019-2020 of SSB the middle and 2019-2020 TB on the right note the very very different y scales :)',
                                            theme = theme(plot.caption = element_text(size = 8)))
ggsave("SSB phytos 2 of hopefully many.png", plot = last_plot(), height = 8, width = 20, units = "in")

+plot_layout(guides = 'collect')+ plot_annotation(caption = 'Figure: SSB 2018-2019 on right 2019-2020 on left note the very very different y scales :)',
                                                                       theme = theme(plot.caption = element_text(size = 8)))
ggsave("SSB phytos 1 of hopefully many.png", plot = last_plot(), height = 8, width = 12, units = "in")

ggplot(taxanthangs.shrike, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(year, sample_id)) +
  theme_classic()
ggsave("SSB phytos 3 of hopefully many.png", plot = last_plot(), height = 8, width = 12, units = "in")

taxanthangs.shrike
write.csv(taxanthangs.shrike, 'data/taxanthangs.shrike.csv', row.names = F)



movement= read_csv('data/taxanthangs.movement.csv')
movement$sample_date=mdy(movement$sample_date)

ggplot(movement, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=year)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(year, sample_id)) +
  theme_classic()



movement
###########
unique(taxanthangs.shrike.2019$division) #8 divisions total
#[1] "Chlorophyta"     "Chrysophyta"     "Cryptophyta"     "Cyanophyta"      "Pyrrhophyta"     "Miscellaneous"  
#[7] "Haptophyta"      "Bacillariophyta"

No.Plan.Jan<- filter(taxanthangs.shrike.2019, month ==1)
unique(No.Plan.Jan$division) #6
#[1] "Chlorophyta"   "Chrysophyta"   "Cryptophyta"   "Cyanophyta"    "Pyrrhophyta"   "Miscellaneous"
write.csv(No.Plan.Jan, 'data/No.Plan.Jan.csv', row.names = F)


No.Plan.Feb<- filter(taxanthangs.shrike.2019, month ==2)
unique(No.Plan.Feb$division) #5
#[1] "Chrysophyta" "Haptophyta"  "Chlorophyta" "Cryptophyta" "Cyanophyta" 
write.csv(No.Plan.Feb, 'data/No.Plan.Feb.csv', row.names = F)


No.Plan.March<- filter(taxanthangs.shrike.2019, sample_date =="2019-03-09")
unique(No.Plan.March$division) #4
#[1] "Chrysophyta" "Chlorophyta" "Cryptophyta" "Cyanophyta" 
write.csv(No.Plan.March, 'data/No.Plan.March.csv', row.names = F)

No.Plan.April<- filter(taxanthangs.shrike.2019, month ==4)
unique(No.Plan.April$division) #6
#[1] "Chlorophyta"     "Bacillariophyta" "Chrysophyta"     "Cryptophyta"     "Cyanophyta"      "Miscellaneous"
write.csv(No.Plan.April, 'data/No.Plan.April.csv', row.names = F)

BE <-bind_rows(No.Plan.Jan,No.Plan.Feb,No.Plan.March,No.Plan.April)

write.csv(BE, 'data/BE.12052020.csv', row.names = F)


#chloros, cryptos, bacilliarophyta, cyanos
ggplot(BE, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()

ggplot(BE, aes(fill=division, y=relative_total_biovolume, x=month)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()

unique(taxanthangs.shrike.2019$division) #8 divisions total over SSB 2019, shows all names #54 things in year 2019 SSB

################
unique(taxanthangs.shrike.2020.SSB$division) #9
#[1] "Cyanophyta"      "Chrysophyta"     "Chlorophyta"     "Cryptophyta"     "Pyrrhophyta"     "Miscellaneous"  
#[7] "Euglenophyta"    "Bacillariophyta" "Haptophyta"

Nobody.Jan<- filter(taxanthangs.shrike.2020.SSB, sample_date =="2020-01-07")
unique(Nobody.Jan$division) #6
#[1] "Cyanophyta"    "Chrysophyta"   "Chlorophyta"   "Cryptophyta"   "Pyrrhophyta"   "Miscellaneous"

write.csv(Nobody.Jan, 'data/Nobody.Jan.shrike.csv', row.names = F)


Nobody.Jan.2<- filter(taxanthangs.shrike.2020.SSB, sample_date =="2020-01-31")
unique(Nobody.Jan.2$division) #6
#[1] "Chrysophyta"  "Chlorophyta"  "Cryptophyta"  "Cyanophyta"   "Euglenophyta" "Pyrrhophyta"
write.csv(Nobody.Jan.2, 'data/Nobody.Jan.2.shrike.csv', row.names = F)



Nobody.Feb<- filter(taxanthangs.shrike.2020.SSB, month == 2)
unique(Nobody.Feb$division) #5
#[1] "Chrysophyta"  "Chlorophyta"  "Cryptophyta"  "Cyanophyta"   "Euglenophyta"
write.csv(Nobody.Feb, 'data/Nobody.Feb.csv', row.names = F)

Nobody.March<- filter(taxanthangs.shrike.2020.SSB, month == 3)
unique(Nobody.March$division) #4
#[1] "Cyanophyta"   "Chrysophyta"  "Chlorophyta"  "Euglenophyta"
write.csv(Nobody.March, 'data/Nobody.March.csv', row.names = F)



Nobody.May<- filter(taxanthangs.shrike.2020.SSB, month == 5)
unique(Nobody.May$division) #9
#[1] "Cyanophyta"      "Chlorophyta"     "Pyrrhophyta"     "Bacillariophyta" "Chrysophyta"     "Haptophyta"     
#[7] "Cryptophyta"     "Euglenophyta"    "Miscellaneous"  
write.csv(Nobody.May, 'data/Nobody.May.csv', row.names = F)

As<-bind_rows(Nobody.Jan,Nobody.Jan.2,Nobody.Feb,Nobody.March, Nobody.May)


write.csv(As, 'data/As.12052020.csv', row.names = F)

###############
unique(taxanthangs.shrike.2020.TB$division) #10 including an NA

Sing.Jan<- filter(taxanthangs.shrike.2020.TB, month == 1)
unique(Sing.Jan$division) #7
#"Chlorophyta"  "Chrysophyta"  "Haptophyta"   "Cryptophyta"  "Cyanophyta"   "Euglenophyta" "Pyrrhophyta"
write.csv(Sing.Jan, 'data/Sing.Jan.csv', row.names = F)


Sing.Feb<- filter(taxanthangs.shrike.2020.TB, month == 2)
unique(Sing.Feb$division) #8 
#[1] "Chlorophyta"   NA   "Chrysophyta"   "Cryptophyta"   "Cyanophyta"    "Euglenophyta"  "Pyrrhophyta"   "Miscellaneous"
write.csv(Sing.Feb, 'data/Sing.Feb.csv', row.names = F)

Sing.March<- filter(taxanthangs.shrike.2020.TB, month == 3)
unique(Sing.March$division) #9
#[1] "Chlorophyta"     NA  "Bacillariophyta" "Chrysophyta"     "Cryptophyta"     "Cyanophyta"      "Euglenophyta"   
#[8] "Pyrrhophyta"     "Miscellaneous" 
write.csv(Sing.March, 'data/Sing.March.csv', row.names = F)

Talk<-bind_rows(Sing.Jan,Sing.Feb,Sing.March)

write.csv(Talk, 'data/talk.12052020.csv', row.names = F)


################SHANNON#######
#to the tune of Nina!

thisisit.reducereduce = select(thisisit,1,2,3,4,9)
thisisit.reducereduce$sample_date=mdy(thisisit.reducereduce$sample_date)
thisisit.reducereduce$month=month(thisisit.reducereduce$sample_date)
thisisit.reducereduce$year=year(thisisit.reducereduce$sample_date)


ggplot(thisisit.reducereduce, aes(fill=division, y=total_biovolume_cubic_um_per_ml_, x=month)) + 
  geom_line(position="stack", stat="identity")+
  facet_wrap(vars(year, sample_id)) +
  theme_classic()



rrrrr<-ggplot(thisisit.reducereduce) +
  geom_line(aes(x = sample_date, y = richness)) +
  scale_colour_viridis_c() +
  theme_bw() +
  facet_wrap(vars(year, sample_id))
  #labs(title = 'Sparkling Bog Light Diff') + ylab('lumins') + xlab('Date') +
  NULL
rrrrr

mddddd<-ggplot(thisisit.reducereduce) +
  geom_line(aes(x = sample_date, y = maximum_diversity)) +
  geom_point(aes(x = sample_date, y = maximum_diversity)) +
  scale_colour_viridis_c() +
  theme_bw() +
  facet_wrap(vars(year, sample_id))
#labs(title = 'Sparkling Bog Light Diff') + ylab('lumins') + xlab('Date') +
NULL
mddddd
ggsave("SSB diversity 1.png", plot = last_plot(), height = 8, width = 12, units = "in")


shsatb<-ggplot(thisisit.reducereduce) +
  geom_point(aes(x = sample_date, y = shannon_diversity_standard_algal_total_biovolume)) +
  geom_line(aes(x = sample_date, y = shannon_diversity_standard_algal_total_biovolume)) +
  scale_colour_viridis_c() +
  theme_bw() +
  facet_wrap(vars(year, sample_id))
#labs(title = 'Sparkling Bog Light Diff') + ylab('lumins') + xlab('Date') +
shsatb

ggsave("SSB Shannon 1 of hopefully many.png", plot = last_plot(), height = 8, width = 12, units = "in")




