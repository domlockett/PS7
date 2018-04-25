library("dplyr")
setwd("C:/Users/Dominique/OneDrive/Documents/Spring 2018/Applied Statistical Programming/git/Week 14")
library(ggplot2)
attach(crime_stl)

##1

crime_stl <- read.csv("C:/Users/Dominique/OneDrive/Documents/Spring 2018/Applied Statistical Programming/git/Week 14/March2018.CSV")
View(crime_stl)
##2
class(crime_stl$Description)

crime_dt <- crime_stl %>% 
  mutate(DateOccur ,Date=substr(DateOccur,1,10)) %>%
  mutate(Description, Crime = sapply(as.character(crime_stl$Description), function(x){strsplit(x = x, split = "[-/]")[[1]][1]})) %>%
  group_by(Description) %>%
  summarise(count =n()) 
crime_dt$Crime

most_crime<-arrange(crime_dt, desc(count))
most_crime
#Leaving the scene of a crime!

##3

crime_dn <- crime_stl %>% 
  mutate(DateOccur ,Date=substr(DateOccur,1,10)) %>%
  group_by(Crime, Neighborhood) %>%
  summarise(count =n()) 
neigh_crime<-arrange(crime_dn, desc(count))
neigh_crime
 ## Neighborhood 35

crime_dn <- crime_stl %>% 
  mutate(DateOccur ,Date=substr(DateOccur,1,10)) %>%
  group_by(Crime, Neighborhood) %>%
  summarise(count =n()) 
neigh_crime<-arrange(crime_dn, desc(count))

##4

crime_rd <- data %>% 
  group_by(District) %>% 
  dplyr::filter(grepl('ROBBERY', Description)) %>% 
  summarise (count = n()) %>%
  mutate(freq = count / sum(count))
