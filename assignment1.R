
rm(list=ls()) # clears workspace


#load packages
library(tidyverse)
library(reshape2)

#1.
data <- read.csv("catch_data.csv",h=T) #fish data
hyd <- read.csv("hydro.csv", h=T)#hydrology data
head(data)
head(hyd)
#2.
View(data)
data[1,3]
data$year[4]
summary(data) #summary of all my variables with class, mean and etc. 

#3
mean(data$catch)
sd(data$catch)
data$species <- factor(data$spp)

#4
f1 <- aggregate(catch~year+river,mean,data=data)

#create a new data frame
dat1 = data %>% 
  group_by(year,river,spp)%>% #group by year, river, and species
  summarise(N = n()) 

#getting my data frame to wide format
dat1_wide = dat1 %>%
  pivot_wider(names_from = spp, values_from = N,values_fill = 0) 

head(dat1_wide)


##Create my own table
#make the flex table
library(flextable)
tdt<- flextable(data = dat1_wide) 
tdt <- theme_booktabs(tdt) 
tdt<-  autofit(tdt) 
tdt

##MAKE REALLY PRETTY REPORT
#https://davidgohel.github.io/officer/articles/word.html
library(officer)
library(flextable)
library(magrittr)

#magrittr uses piping %>% to simplifly code
#the officer package has whole-hog embraced piping - remember to say the word "then"
#http://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html
#https://www.datacamp.com/community/tutorials/pipe-r-tutorial

my_doc <- read_docx() %>% #create a word document
  body_add_par(value = "Assignment 1", style = "heading 1") %>% 
  body_add_par("Table 1. Summary of fish catch in Amazon River Basin by species.", style = "Normal") %>% #add a caption to your table
  body_add_flextable(tdt)%>% #add your flex table (ft) to your report
  print(target = "AM_catch_report.docx") #output the file

