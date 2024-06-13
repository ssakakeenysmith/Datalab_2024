#Load library
library(tidyverse)
#load clean data
dives <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/whales-dives.csv')
head(dives)
#load messy data
messy_dives <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/whales-dives-messy.csv')
head(messy_dives)
#making dates usable
messy_dives2 <- messy_dives %>%
  mutate(YEAR = str_pad(YEAR,width=3,side="left",pad="0")) %>%
  mutate(YEAR = str_pad(YEAR,width=4,side="left",pad="2")) %>%
  mutate(Day = str_pad(Day,width=2,side="left",pad="0")) %>%
  mutate(Month = str_pad(Month,width=2,side="left",pad="0")) %>% 
  mutate(sit=substr(sit,10,12))
#Combining the date and sighting id into the id column 
messy_dives2$id<-paste0(messy_dives2$YEAR,messy_dives2$Month,messy_dives2$Day,messy_dives2$sit)
#remove YEAR, Month, Day, and sit columns
messy_dives2<-subset(messy_dives2,select = -c(YEAR,Month,Day,sit))
#reorder coulmns 
messy_dives2<-messy_dives2 %>% 
  select(id,Species.ID,bhvr,PreyVolume,PreyDepth,Dive_Time,Surfacetime,Blow.Interval,Blow_number_count)
#rename columns using dput(names(dives)) to grab names in an easy order
names(messy_dives2) <- c("id", 
                        "species",
                        "behavior",
                        "prey.volume",
                        "prey.depth",
                        "dive.time",
                        "surface.time",
                        "blow.interval",
                        "blow.number")
#remove NAs from data
messy_dives2<-messy_dives2 %>% 
  na.omit(TRUE)
#find duplicates
messy_dives2<-messy_dives2 %>% 
  distinct()
#remove improper sintax
messy_dives2<-messy_dives2 %>% 
  mutate(species=case_when(species%in%c('fin','finderbender','FinW','FinWhale','fw') ~'FW',
                           !species %in% c('fin','finderbender','FinW','FinWhale','fw') ~ species)) %>% 
  mutate(species=case_when(species%in%c('humperdink','hw','Hw') ~'HW',
                           !species %in% c('humperdink','hw','Hw') ~ species))

#arrange in decending order
messy_dives2<-messy_dives2 %>%
  arrange(id)
