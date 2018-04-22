library("dplyr")
setwd("C:/Users/Dominique/OneDrive/Documents/Spring 2018/Applied Statistical Programming/git/Week 14")


##1

crime_stl <- read.csv("C:/Users/Dominique/OneDrive/Documents/Spring 2018/Applied Statistical Programming/git/Week 14/March2018.CSV")

View(crime_stl)

##2

attach(crime_stl)
crime_stl<-mutate(crime_stl, Date=substr(DateOccur,1,10))
crime_stl<-select(crime_stl, Date, District, Description, Neighborhood)
crime_stl$Date<-as.Date(crime_stl$Date, "%m/%d/%Y")
crime_stl$Description<-as.character(crime_stl$Description)
class(crime_stl$Neighborhood)
func<-function(x){ strsplit(x = x, split = "[-/]")[[1]][1]}
crime_stl$Description<-lapply(crime_stl$Description, func)
group_by(Description, Date)
  summarise(count =n()) #summarize the count

class(crimeType_Day)