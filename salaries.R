setwd("C:/Users/Reina Borst/Desktop/UCU sem 4/Data Analysis for LAS/Group Project")
library(tidyverse)
library(plyr)

salary_canada <- read.csv('salary_canada.csv')
salary_finland <- read.csv('salary_finland.csv')
salary_sweden <- read.csv('salary_finland.csv')
salary_usa <- read.csv('salary_usa.csv')

#copy birth months plot Canada
canadians2 <- read.csv('new_canada_only2.csv')

canadians_adapted <- canadians2 %>% 
  mutate(
    dates = as.Date(birthDate),
    months = format(dates,'%m')
  )

canadians_adapted <- canadians_adapted %>%
  mutate( 
    month_text = case_when( 
      months == '01' ~ 'January', 
      months == '02' ~ 'February', 
      months == '03' ~ 'March', 
      months == '04' ~ 'April', 
      months == '05' ~ 'May', 
      months == '06' ~ 'June', 
      months == '07' ~ 'July', 
      months == '08' ~ 'August', 
      months == '09' ~ 'September', 
      months == '10' ~ 'October', 
      months == '11' ~ 'November', 
      months == '12' ~ 'December' 
    ) 
  )

canadians_adapted$month_text <- factor(canadians_adapted$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot() +
  geom_bar(data=canadians_adapted, mapping=aes(x=month_text, fill=month_text), colour='black') +
  scale_fill_brewer(palette="Set3") +
  scale_fill_discrete(name="Months") +
  labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1870-1999", x = "Months", y = "Number of players")

#compute mean salaries per month
can_salary_adapted <- salary_canada %>% 
  mutate(
    dates = as.Date(Born),
    months = format(dates,'%m'),
    month_text = case_when( 
      months == '01' ~ 'January', 
      months == '02' ~ 'February', 
      months == '03' ~ 'March', 
      months == '04' ~ 'April', 
      months == '05' ~ 'May', 
      months == '06' ~ 'June', 
      months == '07' ~ 'July', 
      months == '08' ~ 'August', 
      months == '09' ~ 'September', 
      months == '10' ~ 'October', 
      months == '11' ~ 'November', 
      months == '12' ~ 'December' 
    ) 
  )

salary_can_jan <- subset(can_salary_adapted, month_text == "January") 
salary_can_feb <- subset(can_salary_adapted, month_text == "February")
salary_can_mar <- subset(can_salary_adapted, month_text == "March")
salary_can_apr <- subset(can_salary_adapted, month_text == "April")
salary_can_may <- subset(can_salary_adapted, month_text == "May")
salary_can_jun <- subset(can_salary_adapted, month_text == "June")
salary_can_jul <- subset(can_salary_adapted, month_text == "July")
salary_can_aug <- subset(can_salary_adapted, month_text == "August")
salary_can_sep <- subset(can_salary_adapted, month_text == "September")
salary_can_oct <- subset(can_salary_adapted, month_text == "October")
salary_can_nov <- subset(can_salary_adapted, month_text == "November")
salary_can_dec <- subset(can_salary_adapted, month_text == "December")

mean_sal_jan = mean(salary_can_jan$Salary, na.rm = TRUE)
mean_sal_feb = mean(salary_can_feb$Salary, na.rm = TRUE)
mean_sal_mar = mean(salary_can_mar$Salary, na.rm = TRUE)
mean_sal_apr = mean(salary_can_apr$Salary, na.rm = TRUE)
mean_sal_may = mean(salary_can_may$Salary, na.rm = TRUE)
mean_sal_jun = mean(salary_can_jun$Salary, na.rm = TRUE)
mean_sal_jul = mean(salary_can_jul$Salary, na.rm = TRUE)
mean_sal_aug = mean(salary_can_aug$Salary, na.rm = TRUE)
mean_sal_sep = mean(salary_can_sep$Salary, na.rm = TRUE)
mean_sal_oct = mean(salary_can_oct$Salary, na.rm = TRUE)
mean_sal_nov = mean(salary_can_nov$Salary, na.rm = TRUE)
mean_sal_dec = mean(salary_can_dec$Salary, na.rm = TRUE)

mean_sal_year <- c(mean(salary_can_jan$Salary, na.rm = TRUE), mean_sal_feb = mean(salary_can_feb$Salary, na.rm = TRUE), mean(salary_can_mar$Salary, na.rm = TRUE), mean(salary_can_apr$Salary, na.rm = TRUE), mean(salary_can_may$Salary, na.rm = TRUE), mean(salary_can_jun$Salary, na.rm = TRUE), mean(salary_can_jul$Salary, na.rm = TRUE), mean(salary_can_aug$Salary, na.rm = TRUE), mean(salary_can_sep$Salary, na.rm = TRUE), mean(salary_can_oct$Salary, na.rm = TRUE), mean(salary_can_nov$Salary, na.rm = TRUE), mean(salary_can_dec$Salary, na.rm = TRUE))
months_for_plot <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
ordered_months <- factor(months_for_plot, levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'), ordered=TRUE)

#Attempt to plot salary against birth months
ggplot() +
  geom_point(mapping=aes(x=ordered_months, y=mean_sal_year), colour='black') +
  labs(title = "", x = "Month of birth", y = "Mean salary")

