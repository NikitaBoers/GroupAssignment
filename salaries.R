setwd("C:/Users/Reina Borst/Desktop/UCU sem 4/Data Analysis for LAS/Group Project")
library(tidyverse)
library(plyr)

reinapalette <- c('#FFB5E8', '#B28DFF', '#AFF8D8', '#FFABAB', '#6EB5FF', '#FFC8C1', '#BFFCC8', '#85E3FF', '#D5AAFF', '#FF9CEE', '#A79AFF', '#FFFFFF')

salary_canada <- read.csv('salary_canada.csv')
salary_finland <- read.csv('salary_finland.csv')
salary_sweden <- read.csv('salary_sweden.csv')
salary_usa <- read.csv('salary_usa.csv')

# ---------------->> CANADA <<----------------------

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

mean_sal_can_jan = mean(salary_can_jan$Salary, na.rm = TRUE)
mean_sal_can_feb = mean(salary_can_feb$Salary, na.rm = TRUE)
mean_sal_can_mar = mean(salary_can_mar$Salary, na.rm = TRUE)
mean_sal_can_apr = mean(salary_can_apr$Salary, na.rm = TRUE)
mean_sal_can_may = mean(salary_can_may$Salary, na.rm = TRUE)
mean_sal_can_jun = mean(salary_can_jun$Salary, na.rm = TRUE)
mean_sal_can_jul = mean(salary_can_jul$Salary, na.rm = TRUE)
mean_sal_can_aug = mean(salary_can_aug$Salary, na.rm = TRUE)
mean_sal_can_sep = mean(salary_can_sep$Salary, na.rm = TRUE)
mean_sal_can_oct = mean(salary_can_oct$Salary, na.rm = TRUE)
mean_sal_can_nov = mean(salary_can_nov$Salary, na.rm = TRUE)
mean_sal_can_dec = mean(salary_can_dec$Salary, na.rm = TRUE)

mean_sal_can_year <- c(mean(salary_can_jan$Salary, na.rm = TRUE), mean(salary_can_feb$Salary, na.rm = TRUE), mean(salary_can_mar$Salary, na.rm = TRUE), mean(salary_can_apr$Salary, na.rm = TRUE), mean(salary_can_may$Salary, na.rm = TRUE), mean(salary_can_jun$Salary, na.rm = TRUE), mean(salary_can_jul$Salary, na.rm = TRUE), mean(salary_can_aug$Salary, na.rm = TRUE), mean(salary_can_sep$Salary, na.rm = TRUE), mean(salary_can_oct$Salary, na.rm = TRUE), mean(salary_can_nov$Salary, na.rm = TRUE), mean(salary_can_dec$Salary, na.rm = TRUE))
months_for_plot <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
ordered_months <- factor(months_for_plot, levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'), ordered=TRUE)

#Attempt to plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_can_year), colour='black') +
  scale_fill_manual(values=reinapalette) +
  labs(title = "Distribution of salary according to birth month for Canadian ice hockey players", x = "Month of birth", y = "Mean salary")

# ---------------->> FINLAND <<----------------------

#compute mean salaries per month
fin_salary_adapted <- salary_finland %>% 
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

salary_fin_jan <- subset(fin_salary_adapted, month_text == "January") 
salary_fin_feb <- subset(fin_salary_adapted, month_text == "February")
salary_fin_mar <- subset(fin_salary_adapted, month_text == "March")
salary_fin_apr <- subset(fin_salary_adapted, month_text == "April")
salary_fin_may <- subset(fin_salary_adapted, month_text == "May")
salary_fin_jun <- subset(fin_salary_adapted, month_text == "June")
salary_fin_jul <- subset(fin_salary_adapted, month_text == "July")
salary_fin_aug <- subset(fin_salary_adapted, month_text == "August")
salary_fin_sep <- subset(fin_salary_adapted, month_text == "September")
salary_fin_oct <- subset(fin_salary_adapted, month_text == "October")
salary_fin_nov <- subset(fin_salary_adapted, month_text == "November")
salary_fin_dec <- subset(fin_salary_adapted, month_text == "December")

mean_sal_fin_jan = mean(salary_fin_jan$Salary, na.rm = TRUE)
mean_sal_fin_feb = mean(salary_fin_feb$Salary, na.rm = TRUE)
mean_sal_fin_mar = mean(salary_fin_mar$Salary, na.rm = TRUE)
mean_sal_fin_apr = mean(salary_fin_apr$Salary, na.rm = TRUE)
mean_sal_fin_may = mean(salary_fin_may$Salary, na.rm = TRUE)
mean_sal_fin_jun = mean(salary_fin_jun$Salary, na.rm = TRUE)
mean_sal_fin_jul = mean(salary_fin_jul$Salary, na.rm = TRUE)
mean_sal_fin_aug = mean(salary_fin_aug$Salary, na.rm = TRUE)
mean_sal_fin_sep = mean(salary_fin_sep$Salary, na.rm = TRUE)
mean_sal_fin_oct = mean(salary_fin_oct$Salary, na.rm = TRUE)
mean_sal_fin_nov = mean(salary_fin_nov$Salary, na.rm = TRUE)
mean_sal_fin_dec = mean(salary_fin_dec$Salary, na.rm = TRUE)

mean_sal_fin_year <- c(mean(salary_fin_jan$Salary, na.rm = TRUE), mean(salary_fin_feb$Salary, na.rm = TRUE), mean(salary_fin_mar$Salary, na.rm = TRUE), mean(salary_fin_apr$Salary, na.rm = TRUE), mean(salary_fin_may$Salary, na.rm = TRUE), mean(salary_fin_jun$Salary, na.rm = TRUE), mean(salary_fin_jul$Salary, na.rm = TRUE), mean(salary_fin_aug$Salary, na.rm = TRUE), mean(salary_fin_sep$Salary, na.rm = TRUE), mean(salary_fin_oct$Salary, na.rm = TRUE), mean(salary_fin_nov$Salary, na.rm = TRUE), mean(salary_fin_dec$Salary, na.rm = TRUE))

#Attempt to plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_fin_year), colour='black') +
  scale_fill_manual(values=reinapalette) +
  labs(title = "Distribution of salary according to birth month for Finnish ice hockey players", x = "Month of birth", y = "Mean salary")

#Missing values due to really small sample = limitation

# ---------------->> Sweden <<----------------------

#compute mean salaries per month
swe_salary_adapted <- salary_sweden %>% 
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

salary_swe_jan <- subset(swe_salary_adapted, month_text == "January") 
salary_swe_feb <- subset(swe_salary_adapted, month_text == "February")
salary_swe_mar <- subset(swe_salary_adapted, month_text == "March")
salary_swe_apr <- subset(swe_salary_adapted, month_text == "April")
salary_swe_may <- subset(swe_salary_adapted, month_text == "May")
salary_swe_jun <- subset(swe_salary_adapted, month_text == "June")
salary_swe_jul <- subset(swe_salary_adapted, month_text == "July")
salary_swe_aug <- subset(swe_salary_adapted, month_text == "August")
salary_swe_sep <- subset(swe_salary_adapted, month_text == "September")
salary_swe_oct <- subset(swe_salary_adapted, month_text == "October")
salary_swe_nov <- subset(swe_salary_adapted, month_text == "November")
salary_swe_dec <- subset(swe_salary_adapted, month_text == "December")

mean_sal_swe_jan = mean(salary_swe_jan$Salary, na.rm = TRUE)
mean_sal_swe_feb = mean(salary_swe_feb$Salary, na.rm = TRUE)
mean_sal_swe_mar = mean(salary_swe_mar$Salary, na.rm = TRUE)
mean_sal_swe_apr = mean(salary_swe_apr$Salary, na.rm = TRUE)
mean_sal_swe_may = mean(salary_swe_may$Salary, na.rm = TRUE)
mean_sal_swe_jun = mean(salary_swe_jun$Salary, na.rm = TRUE)
mean_sal_swe_jul = mean(salary_swe_jul$Salary, na.rm = TRUE)
mean_sal_swe_aug = mean(salary_swe_aug$Salary, na.rm = TRUE)
mean_sal_swe_sep = mean(salary_swe_sep$Salary, na.rm = TRUE)
mean_sal_swe_oct = mean(salary_swe_oct$Salary, na.rm = TRUE)
mean_sal_swe_nov = mean(salary_swe_nov$Salary, na.rm = TRUE)
mean_sal_swe_dec = mean(salary_swe_dec$Salary, na.rm = TRUE)

mean_sal_swe_year <- c(mean(salary_swe_jan$Salary, na.rm = TRUE), mean(salary_swe_feb$Salary, na.rm = TRUE), mean(salary_swe_mar$Salary, na.rm = TRUE), mean(salary_swe_apr$Salary, na.rm = TRUE), mean(salary_swe_may$Salary, na.rm = TRUE), mean(salary_swe_jun$Salary, na.rm = TRUE), mean(salary_swe_jul$Salary, na.rm = TRUE), mean(salary_swe_aug$Salary, na.rm = TRUE), mean(salary_swe_sep$Salary, na.rm = TRUE), mean(salary_swe_oct$Salary, na.rm = TRUE), mean(salary_swe_nov$Salary, na.rm = TRUE), mean(salary_swe_dec$Salary, na.rm = TRUE))

#Attempt to plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_swe_year), colour='black') +
  scale_fill_manual(values=reinapalette) +
  labs(title = "Distribution of salary according to birth month for Swedish ice hockey players", x = "Month of birth", y = "Mean salary")

# ---------------->> UNITED STATES <<----------------------

#compute mean salaries per month
usa_salary_adapted <- salary_usa %>% 
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

salary_usa_jan <- subset(usa_salary_adapted, month_text == "January") 
salary_usa_feb <- subset(usa_salary_adapted, month_text == "February")
salary_usa_mar <- subset(usa_salary_adapted, month_text == "March")
salary_usa_apr <- subset(usa_salary_adapted, month_text == "April")
salary_usa_may <- subset(usa_salary_adapted, month_text == "May")
salary_usa_jun <- subset(usa_salary_adapted, month_text == "June")
salary_usa_jul <- subset(usa_salary_adapted, month_text == "July")
salary_usa_aug <- subset(usa_salary_adapted, month_text == "August")
salary_usa_sep <- subset(usa_salary_adapted, month_text == "September")
salary_usa_oct <- subset(usa_salary_adapted, month_text == "October")
salary_usa_nov <- subset(usa_salary_adapted, month_text == "November")
salary_usa_dec <- subset(usa_salary_adapted, month_text == "December")

mean_sal_usa_jan = mean(salary_usa_jan$Salary, na.rm = TRUE)
mean_sal_usa_feb = mean(salary_usa_feb$Salary, na.rm = TRUE)
mean_sal_usa_mar = mean(salary_usa_mar$Salary, na.rm = TRUE)
mean_sal_usa_apr = mean(salary_usa_apr$Salary, na.rm = TRUE)
mean_sal_usa_may = mean(salary_usa_may$Salary, na.rm = TRUE)
mean_sal_usa_jun = mean(salary_usa_jun$Salary, na.rm = TRUE)
mean_sal_usa_jul = mean(salary_usa_jul$Salary, na.rm = TRUE)
mean_sal_usa_aug = mean(salary_usa_aug$Salary, na.rm = TRUE)
mean_sal_usa_sep = mean(salary_usa_sep$Salary, na.rm = TRUE)
mean_sal_usa_oct = mean(salary_usa_oct$Salary, na.rm = TRUE)
mean_sal_usa_nov = mean(salary_usa_nov$Salary, na.rm = TRUE)
mean_sal_usa_dec = mean(salary_usa_dec$Salary, na.rm = TRUE)

mean_sal_usa_year <- c(mean(salary_usa_jan$Salary, na.rm = TRUE), mean(salary_usa_feb$Salary, na.rm = TRUE), mean(salary_usa_mar$Salary, na.rm = TRUE), mean(salary_usa_apr$Salary, na.rm = TRUE), mean(salary_usa_may$Salary, na.rm = TRUE), mean(salary_usa_jun$Salary, na.rm = TRUE), mean(salary_usa_jul$Salary, na.rm = TRUE), mean(salary_usa_aug$Salary, na.rm = TRUE), mean(salary_usa_sep$Salary, na.rm = TRUE), mean(salary_usa_oct$Salary, na.rm = TRUE), mean(salary_usa_nov$Salary, na.rm = TRUE), mean(salary_usa_dec$Salary, na.rm = TRUE))

#Attempt to plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_usa_year), colour='black') +
  scale_fill_manual(values=reinapalette) +
  labs(title = "Distribution of salary according to birth month for North American ice hockey players", x = "Month of birth", y = "Mean salary")
