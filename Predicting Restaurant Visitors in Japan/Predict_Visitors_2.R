require(data.table)
require(dplyr)
require(ggplot2)
require(knitr)
require(stringi)
require(lubridate)
library(reshape2)
require(scales)

setwd("/Users/decolvin/Box Sync/1 Northwestern MSPA/PREDICT 498_DL Capstone/Final Project/")

#load data
air_stores <- read.csv("Data/air_store_info.csv", stringsAsFactors = FALSE,
                       encoding="UTF-8")
## problems with some accents, change it
air_stores$air_area_name <- stri_trans_general(air_stores$air_area_name,"Latin-ASCII")


air_visits <- read.csv("Data/air_visit_data.csv",  stringsAsFactors = FALSE,
                       encoding="UTF-8")
air_reserve <- read.csv("Data/air_reserve.csv",  stringsAsFactors = FALSE,
                        encoding="UTF-8")
hpg_reserve <- read.csv("Data/hpg_reserve.csv",  stringsAsFactors = FALSE,
                        encoding="UTF-8")
str(hpg_reserve)
hpg_stores <- read.csv("Data/hpg_store_info.csv", stringsAsFactors = FALSE,
                       encoding="UTF-8")
hpg_stores$hpg_area_name <- stri_trans_general(hpg_stores$hpg_area_name,"Latin-ASCII")

store_mapping <- read.csv("Data/store_id_relation.csv", stringsAsFactors = FALSE,
                          encoding="UTF-8")
holidays <- read.csv("Data/date_info 2.csv", stringsAsFactors = FALSE,
                     encoding="UTF-8")
#End of load data

#Genre type of restaurant (air stores)
genre_values <- air_stores %>% group_by(air_genre_name) %>% tally() %>% 
  summarise(mean_n=round(mean(n,na.rm=T),2),
            median_n = round(median(n,na.rm=T),2))  %>% 
  rbind(c('line blue','line red'))

kable(air_stores %>% group_by(air_genre_name) %>% tally() %>% arrange(desc(n)),caption = 'Type of restaurant')

#Areas of air stores
air_stores %>% group_by(air_genre_name) %>% tally() %>% 
  ggplot(aes(x=reorder(air_genre_name,+n),y=n)) + geom_bar(stat='identity') + coord_flip() + 
  geom_hline(yintercept = as.numeric(genre_values$mean_n[1]),size=1,color='blue',linetype='dashed') +
  geom_hline(yintercept= as.numeric(genre_values$median_n[1]),size=1,color='red',linetype="dashed") +
  xlab("Type restaurant") + ylab("counts AIR") 

air_stores %>% group_by(air_area_name) %>% tally() %>% 
  arrange(desc(n)) %>% head(10)  %>% 
  ggplot(aes(x=reorder(air_area_name,+n),y=n)) + geom_bar(stat='identity') + coord_flip() + 
  xlab("Type restaurant") + ylab("counts") 

#Look names tokyo,fukuoka,osaka and compare genre of restaurant.
air_stores %>% filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                               "Fukuoka area","Osaka area"))) %>% 
  group_by(area2,air_genre_name) %>%  tally() %>% 
  mutate(freq = n / sum(n)) %>% 
  #    group_by(area2) %>% mutate(n_size = n()) %>% ungroup() %>%   
  # group_by(area2,air_genre_name) %>%  mutate(n2=n()/n_size) %>% 
  ggplot(aes(x=reorder(air_genre_name,+freq),y=freq,fill=area2)) + geom_bar(stat='identity') + coord_flip() + 
  scale_y_continuous(labels=percent) +  facet_wrap(~area2,nrow=3) + xlab("Type of restaurant") +
  ylab("Relative Freq.") + theme(strip.background = element_rect(fill="white",size = 12),
                                 legend.position="none") 
#Look visitors by month
air_stores %>% filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                               "Fukuoka area","Osaka area"))) %>% 
  group_by(area2,air_genre_name) %>%  tally() %>% 
  mutate(freq = n / sum(n)) %>%  arrange(desc(freq,air_genre_name))  %>% kable()

air_visits %>% mutate(visit_date=as.Date(visit_date),
                      air_month = month(visit_date),
                      air_wday = wday(visit_date)) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month) %>% 
  summarise(mean_vis = mean(visitors,na.rm=T) ,
            median_vis = median(visitors,na.rm=T)) %>% kable()

#Group data by month and auxiliar area and compare mean and median.
air_stores %>% filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                               "Fukuoka area","Osaka area"))) %>% 
  group_by(area2) %>% tally()


air_visits %>% mutate(visit_date=as.Date(visit_date),
                      air_month = month(visit_date),
                      air_wday = wday(visit_date)) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month) %>% 
  summarise(mean_vis = mean(visitors,na.rm=T) ,
            median_vis = median(visitors,na.rm=T)) %>% kable()

air_stores <- air_stores %>% # filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 =        ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                               "Tokyo area",
                               ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                                      "Fukuoka area",
                                      ifelse(grepl("Osaka",air_area_name,ignore.case=T),
                                             "Osaka area","Others area"))))       

air_stores %>% group_by(area2) %>% tally() %>% kable()

air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        air_wday = wday(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  # air_mday = mday(visit_date))
  
  group_by(air_month,area2) %>% 
  summarise(mean_vis = mean(visitors,na.rm=T) ,
            median_vis = median(visitors,na.rm=T)) %>% 
  melt(., c('area2','air_month')) %>% 
  ggplot(aes(value,fill=variable)) + 
  geom_density() + facet_wrap(~area2)

#Median by year/month/auxiliar areas
air_visits_median_ya2 <- air_visits %>%   mutate(visit_date=as.Date(visit_date),
                                                 air_month = month(visit_date),
                                                 air_wday = wday(visit_date),
                                                 air_year = year(visit_date)) %>%
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>%
  # air_mday = mday(visit_date))
  group_by(air_year,area2) %>% summarise(median_y2 = median(visitors,na.rm=T))

kable(air_visits_median_ya2)

air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month,air_year,area2) %>% 
  summarise(median_vis = median(visitors,na.rm=T)) %>%
  ggplot(aes(factor(air_month),median_vis)) + geom_bar(stat='identity',aes(fill=area2)) +
  facet_wrap(~area2+air_year,nrow=4) + theme(strip.background = element_rect(fill="white",size = 12),
                                             legend.position="none") +
  xlab("Months") + ylab("Median of visitors")  +
  # geom_hline(yintercept = 12, 
  #             slope = 0,data=dplyr::filter(., area2 == 'Fukuoka area')) + 
  # geom_text(data = dplyr::filter(., year == 2016 & sub_area == "Activities"), 
  # # geom_vline(xintercept=c(5,10,15))
  # geom_vline(data=meanData, aes(xintercept=meanCDR), colour="red", lty=3) +
  # geom_line() +
  # geom_text(data=meanData, 
  #           aes(label=round(meanCDR,1), x=40, y=ypos), colour="red",
  #           hjust=1) 
  geom_hline(data=air_visits_median_ya2, aes(yintercept=median_y2),
             colour="black",linetype = 'dashed',size=1)

#Mean by aux. area/month/year with mark holiday_flag
holidays$calendar_date <- as.Date(holidays$calendar_date)
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month,air_year,area2,holiday_flg) %>% 
  summarise(mean_vis = mean(visitors,na.rm=T)) %>%
  
  ggplot(aes(factor(air_month),mean_vis,fill=factor(holiday_flg))) +
  
  geom_bar(stat='identity',position='dodge',width=0.75) +
  
  
  facet_wrap(~area2+air_year,nrow=4) + theme(strip.background = element_rect(fill="white",size = 12),
                                             legend.position="none") +
  xlab("Months") + ylab("Mean of visitors") + 
  scale_fill_brewer(palette="Set2") 

##########END OF AIR STORES###########################

#HPG 
genre_values <- hpg_stores %>% group_by(hpg_genre_name) %>% tally() %>% 
  summarise(mean_n=round(mean(n,na.rm=T),2),
            median_n = round(median(n,na.rm=T),2))  %>% 
  rbind(c('line blue','line red'))

kable(hpg_stores %>% group_by(hpg_genre_name) %>% tally() %>% arrange(desc(n)),caption = 'Type of restaurant')

#Areas of air stores
hpg_stores %>% group_by(hpg_genre_name) %>% tally() %>% 
  ggplot(aes(x=reorder(hpg_genre_name,+n),y=n)) + geom_bar(stat='identity') + coord_flip() + 
  geom_hline(yintercept = as.numeric(genre_values$mean_n[1]),size=1,color='blue',linetype='dashed') +
  geom_hline(yintercept= as.numeric(genre_values$median_n[1]),size=1,color='red',linetype="dashed") +
  xlab("Type restaurant") + ylab("counts HPG") 

hpg_stores %>% group_by(hpg_area_name) %>% tally() %>% 
  arrange(desc(n)) %>% head(10)  %>% 
  ggplot(aes(x=reorder(hpg_area_name,+n),y=n)) + geom_bar(stat='identity') + coord_flip() + 
  xlab("Type restaurant") + ylab("counts") 

#Look names tokyo,fukuoka,osaka and compare genre of restaurant.
hpg_stores %>% filter(grepl("Tokyo|Hyogo|Osaka|Hiroshima",hpg_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",hpg_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Hyogo",hpg_area_name,ignore.case=T),
                               "Hyogo area",ifelse(grepl("Hiroshima",hpg_area_name,ignore.case=T),
                                                   "Hiroshima area","Osaka area")))) %>% 
  group_by(area2,hpg_genre_name) %>%  tally() %>% 
  mutate(freq = n / sum(n)) %>% 
  #    group_by(area2) %>% mutate(n_size = n()) %>% ungroup() %>%   
  # group_by(area2,air_genre_name) %>%  mutate(n2=n()/n_size) %>% 
  ggplot(aes(x=reorder(hpg_genre_name,+freq),y=freq,fill=area2)) + geom_bar(stat='identity') + coord_flip() + 
  scale_y_continuous(labels=percent) +  facet_wrap(~area2,nrow=3) + xlab("Type of restaurant") +
  ylab("Relative Freq.") + theme(strip.background = element_rect(fill="white",size = 12),
                                 legend.position="none") 


#Look visitors by month
hpg_stores %>% filter(grepl("Tokyo|Hyogo|Osaka|Hiroshima",hpg_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",hpg_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Hyogo",hpg_area_name,ignore.case=T),
                               "Hyogo area",ifelse(grepl("Hiroshima",hpg_area_name,ignore.case=T),
                                                   "Hiroshima area","Osaka area")))) %>% 
  group_by(area2,hpg_genre_name) %>%  tally() %>% 
  mutate(freq = n / sum(n)) %>%  arrange(desc(freq,hpg_genre_name))  %>% kable()

hpg_reserve %>% mutate(visit_datetime=as.Date(visit_datetime),
                       hpg_month = month(visit_datetime),
                       hpg_wday = wday(visit_datetime)) %>% 
  # air_mday = mday(visit_date))
  group_by(hpg_month) %>% 
  summarise(mean_vis = mean(reserve_visitors,na.rm=T) ,
            median_vis = median(reserve_visitors,na.rm=T)) %>% kable()

hpg_stores <- hpg_stores %>% # filter(grepl("Tokyo|Hyogo|Osaka|Hiroshima",hpg_area_name,ignore.case=T)) %>% 
  mutate(area2 =        ifelse(grepl("Tokyo",hpg_area_name,ignore.case=T),
                               "Tokyo area",
                               ifelse(grepl("Hyogo",hpg_area_name,ignore.case=T),
                                      "Hyogo area",ifelse(grepl("Hiroshima",hpg_area_name,ignore.case=T),
                                                          "Hiroshima area","Osaka area"))))       

hpg_stores %>% group_by(area2) %>% tally() %>% kable()

hpg_reserve %>%   mutate(visit_datetime=as.Date(visit_datetime),
                         hpg_month = month(visit_datetime),
                         hpg_wday = wday(visit_datetime)) %>% 
  left_join(hpg_stores, by = c("hpg_store_id" = "hpg_store_id")) %>% 
  # air_mday = mday(visit_date))
  
  group_by(hpg_month,area2) %>% 
  summarise(mean_vis = mean(reserve_visitors,na.rm=T) ,
            median_vis = median(reserve_visitors,na.rm=T)) %>% 
  melt(., c('area2','hpg_month')) %>% 
  ggplot(aes(value,fill=variable)) + 
  geom_density() + facet_wrap(~area2)

#Median by year/month/auxiliar areas
air_visits_median_ya2 <- air_visits %>%   mutate(visit_date=as.Date(visit_date),
                                                 air_month = month(visit_date),
                                                 air_wday = wday(visit_date),
                                                 air_year = year(visit_date)) %>%
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>%
  # air_mday = mday(visit_date))
  group_by(air_year,area2) %>% summarise(median_y2 = median(visitors,na.rm=T))

kable(air_visits_median_ya2)

air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month,air_year,area2) %>% 
  summarise(median_vis = median(visitors,na.rm=T)) %>%
  ggplot(aes(factor(air_month),median_vis)) + geom_bar(stat='identity',aes(fill=area2)) +
  facet_wrap(~area2+air_year,nrow=4) + theme(strip.background = element_rect(fill="white",size = 12),
                                             legend.position="none") +
  xlab("Months") + ylab("Median of visitors")  +
  # geom_hline(yintercept = 12, 
  #             slope = 0,data=dplyr::filter(., area2 == 'Fukuoka area')) + 
  # geom_text(data = dplyr::filter(., year == 2016 & sub_area == "Activities"), 
  # # geom_vline(xintercept=c(5,10,15))
  # geom_vline(data=meanData, aes(xintercept=meanCDR), colour="red", lty=3) +
  # geom_line() +
  # geom_text(data=meanData, 
  #           aes(label=round(meanCDR,1), x=40, y=ypos), colour="red",
  #           hjust=1) 
  geom_hline(data=air_visits_median_ya2, aes(yintercept=median_y2),
             colour="black",linetype = 'dashed',size=1)

#Mean by aux. area/month/year with mark holiday_flag
holidays$calendar_date <- as.Date(holidays$calendar_date)
hpg_reserve %>%   mutate(visit_datetime=as.Date(visit_datetime),
                         hpg_month = month(visit_datetime),
                         hpg_wday = wday(visit_datetime),
                         hpg_year = year(visit_datetime)) %>% 
  left_join(hpg_stores, by = c("hpg_store_id" = "hpg_store_id")) %>% 
  left_join(holidays,by=c("visit_datetime"="calendar_date")) %>% 
  # air_mday = mday(visit_date))
  group_by(hpg_month,hpg_year,area2,holiday_flg) %>% 
  summarise(mean_vis = mean(reserve_visitors,na.rm=T)) %>%
  
  ggplot(aes(factor(hpg_month),mean_vis,fill=factor(holiday_flg))) +
  
  geom_bar(stat='identity',position='dodge',width=0.75) +
  
  
  facet_wrap(~area2+hpg_year,nrow=4) + theme(strip.background = element_rect(fill="white",size = 12),
                                             legend.position="none") +
  xlab("Months") + ylab("Mean of visitors") + 
  scale_fill_brewer(palette="Set2") 
