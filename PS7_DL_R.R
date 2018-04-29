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
  mutate(crime_stl$DateOccur ,Date=substr(DateOccur,1,10)) %>%
  mutate(Description, Crime = sapply(as.character(crime_stl$Description), function(x){strsplit(x = x, split = "[-/]")[[1]][1]})) %>%
  group_by(Description) %>%
  summarise(count =n()) 

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


##4

crime_rd <- crime_stl %>% 
  group_by(District) %>% 
  dplyr::filter(grepl('ROBBERY', Description)) %>% 
  summarise (count = n()) %>%
  mutate(freq = count / sum(count))

crime_rd
##5

crime_stl$DateOccur<-as.Date(data$DateOccur,"%m/%d/%Y")
crime_date<-arrange(crime_stl, crime_stl$DateOccur)
date <- data %>%
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count)) + 
  geom_line() + 
  labs(
    y="# of crimes", 
    x="Date", 
    title="Daily Crimes")

#6
crime_stl$DateOccur<-as.Date(crime_stl$DateOccur,"%m/%d/%Y")
crime_date<-arrange(crime_stl, DateOccur)
date <- crime_stl %>%
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur,District) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count,group=(District))) + 
  geom_line(aes(color=factor(District)))+
  scale_color_manual(name="District",values=c('pink', 'purple','orange','blue','green','red','gray'))+
  labs(
    y="# of crimes", 
    x="Date", 
    title="Daily Crime by District")  
