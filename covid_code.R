# Load the packages
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(DT)
library(scales)
           
# Read datasets/confirmed_cases_worldwide.csv into confirmed_cases_worldwide
cases_worldwide <- read_excel("Covid_19/covid_19_data.xlsx")

glimpse(cases_worldwide)

#change the datatype of ObservationDate columnn to date
cases_worldwide$`ObservationDate` <- as.Date(cases_worldwide$`ObservationDate`,"%m/%d/%y")
str(cases_worldwide)

cases_worldwide$day<-factor(day(cases_worldwide$`ObservationDate`))
cases_worldwide$month<-factor(month(cases_worldwide$`ObservationDate`,label = TRUE))
cases_worldwide$year<-factor(year(cases_worldwide$`ObservationDate`))
cases_worldwide$weekDay<-factor(wday(cases_worldwide$`ObservationDate`,label = TRUE))

str(cases_worldwide)

#cases as per day
day_cases <- cases_worldwide %>%
  group_by(day) %>%
  dplyr::summarise(Total = n())
datatable(day_cases)

ggplot(day_cases,aes(day,Total))+
  geom_bar(stat = "identity",fill="red",color="black")+
  ggtitle("Cases as per day")+
  theme_light()+
  scale_y_continuous(labels = comma)

#cases as per month
month_cases <- cases_worldwide%>%
  group_by(month) %>%
  dplyr::summarise(Total = n())

datatable(month_cases)
    #Bar
ggplot(month_cases,aes(month,Total))+
  geom_bar(stat = "identity", fill = "steelblue", color = "red")+
  ggtitle("Cases as per month")+
  theme_light()+
  scale_y_continuous(labels = comma)

   #Line
ggplot(month_cases,aes(month,Total))+
  geom_line(stat = "identity", color = "red",group = 1)+
  ggtitle("Cases as per month")+
  theme_light()+
  scale_y_continuous(labels = comma)


#cases grouped by  days and month
day_month <- cases_worldwide %>%
  group_by(day,month) %>%
  dplyr::summarise(Total = n())
datatable(day_month)

#Cases as per month grouped by days
ggplot(day_month, aes(month,Total,fill = day))+
  geom_bar(stat = "identity")+
  ggtitle("Cases as per month grouped by days")+
  theme_light()+
  scale_y_continuous(labels = comma)

#Cases as per days grouped by months
ggplot(day_month, aes(day,Total,fill = month))+
  geom_bar(stat = "identity")+
  ggtitle("Cases as per days grouped by months")+
  theme_light()+
  ylim(0,3000)


