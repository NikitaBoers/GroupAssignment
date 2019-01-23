setwd("C:/Users/Reina Borst/Desktop/UCU sem 4/Data Analysis for LAS/Group Project")
library(tidyverse)
library(plyr)

train <- read.csv('train.csv')

#remove unnecessary columns
train <- train[, -4] 
train <- train[, -c(9:10)]
train <- train[, -c(14:151)]

#looks prettier with actual country names instead of abbreviations
train$Cntry <- revalue(train$Cntry, c("CAN"="Canada"))
train$Nat <- revalue(train$Nat, c("CAN"="Canada"))
train$Cntry <- revalue(train$Cntry, c("SWE"="Sweden"))
train$Nat <- revalue(train$Nat, c("SWE"="Sweden"))
train$Cntry <- revalue(train$Cntry, c("FIN"="Finland"))
train$Nat <- revalue(train$Nat, c("FIN"="Finland"))
train$Cntry <- revalue(train$Cntry, c("USA"="United States"))
train$Nat <- revalue(train$Nat, c("USA"="United States"))

#mention in report that we selected based on nationality rather than country they play for or both (because in original 
#dataset we used birthplace as country determiner), but discuss limitations or whatever
salary_canada <- subset(train, Nat == "Canada")
salary_sweden <- subset(train, Nat == "Sweden")
salary_finland <- subset(train, Nat == "Finland")
salary_usa <- subset(train, Nat == "United States")

#make separate csv files so these don't have to be computed every time I run this document
write.csv(salary_canada, "salary_canada.csv")
write.csv(salary_finland, "salary_finland.csv")
write.csv(salary_sweden, "salary_sweden.csv")
write.csv(salary_usa, "salary_usa.csv")