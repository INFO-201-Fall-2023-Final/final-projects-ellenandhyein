#Info 201 Team BH: Thao Nguyen, Hyein Hwang
#Final Project: Data Wrangling 
library(dplyr)
library(stringr)
library(tidyverse)
  eighten_df <- read.csv("2018.csv")
  twenty_df <- read.csv("WHR20_DataForFigure2.1 2.csv")
  suicide_df <- read.csv("Suicide Rate.csv")
  df <- merge(x= eighten_df, y=twenty_df, by.x= c("Country.or.region"), by.y=c("Country.name"))
  df <- merge(x= df, y=suicide_df, by.x= c("Country.or.region"), by.y= c("Country")) 
 
#Data Joining
eighten_df<-eighten_df %>% mutate(Year=2018,Country=Country.or.region,Happiness.score=Score)
twenty_df<-twenty_df %>% mutate(Year=2020,Country=Country.name,GDP.per.capita=Logged.GDP.per.capita,Happiness.score=Ladder.score)
df_1<-rbind(eighten_df[,c("Country","Year","Happiness.score","Healthy.life.expectancy","GDP.per.capita","Social.support","Freedom.to.make.life.choices","Generosity")],twenty_df[,c("Country","Year","Happiness.score","Healthy.life.expectancy","GDP.per.capita","Social.support","Freedom.to.make.life.choices","Generosity")])


#adding a Categorical Column 
#called Happiness.type to categorize happiness as country has the score over
#3.5 is selected into group (1) and the ones that are below will be selected 
#to group (0)
happiness_type_list <- c()
for (Country in df$"Score.20"){
  if (Country >= 3.5){
    happiness_type_listt <- c(happiness_type_list, 1)
  }
  else{
    happiness_type_list <- c(happiness_type_list, 0)
  }
}
df$Happiness.type <- happiness_type_list


#adding Numerical Column 
#called Happiness.Suicide.Diff which is the difference between
#the world happiness and the suicide rates in 2020 to get an idea of the relevance between
#happiness rates and suicides rates in each country.


