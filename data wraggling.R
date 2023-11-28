library(dplyr)
library(stringr)
library(tidyverse)

# Data joining
eighten_df <- read.csv("2018.csv")
twenty_df <- read.csv("WHR20_DataForFigure2.1 2.csv")
suicide_df <- read.csv("Suicide Rate.csv")

colnames(eighten_df)[colnames(eighten_df) == "Score"] <- "Score.18"
colnames(eighten_df)[colnames(eighten_df) == "GDP.per.capita"] <- "GDP.per.capita.18"
colnames(eighten_df)[colnames(eighten_df) == "Social.support"] <- "Social.support.18"
colnames(eighten_df)[colnames(eighten_df) == "Healthy.life.expectation.x"] <- "Healthy.life.expectation.18"
colnames(eighten_df)[colnames(eighten_df) == "Freedom.to.make.life.choices"] <- "Freedom.to.make.life.choices.18"
colnames(eighten_df)[colnames(eighten_df) == "Generosity"] <- "Generosity.18"
colnames(eighten_df)[colnames(eighten_df) == "Perceptions.of.corruption"] <- "Perceptions.of.corruption.18"

colnames(suicide_df)[colnames(suicide_df) == "GDP.per.capita"] <- "GDP.per.capita.suicide"

df <- merge(x= eighten_df, y=twenty_df, by.x= c("Country.or.region"), by.y=c("Country.name"))

df <- merge(x= df, y=suicide_df, by.x= c("Country.or.region"), by.y= c("Country")) 
