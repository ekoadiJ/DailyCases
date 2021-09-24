

kasusharian <-read.csv("c:\\Users\\LENOVO\\Downloads\\Data R\\dailynational.csv", header = TRUE, stringsAsFactors = FALSE) 
View(kasusharian)
kasusharian$dates <-as.Date(kasusharian$dates)
str(kasusharian)
head(kasusharian,n=10)
library(dplyr)
kasusharian%>% select(dates,daily_cases,daily_recovered,daily_deaths)%>% head(n=10)

library(ggplot2)  
ggplot(data=kasusharian,mapping = aes(x=dates))+geom_line(aes(y=daily_cases,color="Baru"))+ geom_line(aes(y=daily_recovered, color="Sembuh"))+geom_line(aes(y=daily_deaths,color="Meninggal"))+theme_minimal()
ggplot(data=kasusharian,mapping = aes(x=dates))+geom_line(aes(y=daily_cases,color="Baru"), size=0.8)+geom_line(aes(y=daily_recovered,color ="Sembuh"), size=0.8)+ geom_line(aes(y=daily_deaths,color="Meninggal"), size=0.8)+theme_minimal()+ scale_color_manual(name="Cases",values = c("Baru"="skyblue","Sembuh"="green","Meninggal"="red"))
ggplot(data=kasusharian,mapping = aes(x=dates))+geom_line(aes(y=daily_cases,color="Baru"), size=0.8)+geom_line(aes(y=daily_recovered,color ="Sembuh"), size=0.8)+ geom_line(aes(y=daily_deaths,color="Meninggal"), size=0.8)+theme_minimal()+ scale_color_manual(name="Cases",values = c("Baru"="skyblue","Sembuh"="green","Meninggal"="red")) + labs(title = "Indonesia Daily cases Trend", x="Dates",y="Kasus Harian")

ggplot(data=kasusharian,mapping = aes(x=dates))+geom_line(aes(y=daily_cases,color="Baru"), size=0.8)+geom_line(aes(y=daily_recovered,color ="Sembuh"), size=0.8)+ geom_line(aes(y=daily_deaths,color="Meninggal"), size=0.8)+theme_minimal()+ scale_color_manual(name="Cases",values = c("Baru"="skyblue","Sembuh"="green","Meninggal"="red")) + labs(title = "Indonesia Daily cases Trend", x="Dates",y="Kasus Harian")+ scale_x_date(date_breaks ="2 weeks",date_labels = "%d/%m") 

# Trend Lines
ks <-ksmooth(x=kasusharian$dates,y=kasusharian$daily_cases, kernel ="normal",bandwidth = 10, x.points = kasusharian$dates) 
kasusharian$daily_treated <-ks$y

ggplot(data=kasusharian,mapping = aes(x=dates))+geom_line(aes(y=daily_cases,color="Baru"), size=0.8)+geom_line(aes(y=daily_recovered,color ="Sembuh"), size=0.8)+ geom_line(aes(y=daily_deaths,color="Meninggal"), size=0.8)+theme_minimal()+ scale_color_manual(name="Cases",values = c("Baru"="skyblue","Sembuh"="green","Meninggal"="red")) + labs(title = "Indonesia Daily cases Trend", x="Dates",y="Kasus Harian")+ scale_x_date(date_breaks ="2 weeks",date_labels = "%d/%m")+geom_line(aes(y=daily_treated,color="Trend"),size=0.8) 

ggplot(data=kasusharian,mapping = aes(x=dates))+geom_line(aes(y=daily_cases,color="Baru"), size=0.8)+geom_line(aes(y=daily_recovered,color ="Sembuh"), size=0.8)+ geom_line(aes(y=daily_deaths,color="Meninggal"), size=0.8)+theme_minimal()+ scale_color_manual(name="Cases",values = c("Baru"="skyblue","Sembuh"="green","Meninggal"="red")) + labs(title = "Indonesia Daily cases Trend", x="Dates",y="Kasus Harian")+ scale_x_date(date_breaks ="2 weeks",date_labels = "%d/%m")+geom_line(aes(y=daily_treated,color="Trend"),size=0.8)+ theme(legend.position = "top") 
