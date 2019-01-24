setwd("C:/Users/Reina Borst/Desktop/UCU sem 4/Data Analysis for LAS/Group Project")
library(tidyverse)
library(plyr)

salary_canada <- read.csv('salary_canada.csv')
salary_finland <- read.csv('salary_finland.csv')
salary_sweden <- read.csv('salary_sweden.csv')
salary_usa <- read.csv('salary_usa.csv')

reinapalette <- c('#FFB5E8', '#B28DFF', '#AFF8D8', '#FFABAB', '#6EB5FF', '#FFC8C1', '#BFFCC8', '#85E3FF', '#D5AAFF', '#FF9CEE', '#A79AFF', '#FFFFFF')
breaks <- seq(0, 4, 1)
labels <- paste(breaks, "million", sep=' ')

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

mean_sal_can_year <- c(mean(salary_can_jan$Salary, na.rm = TRUE), mean(salary_can_feb$Salary, na.rm = TRUE), mean(salary_can_mar$Salary, na.rm = TRUE), mean(salary_can_apr$Salary, na.rm = TRUE), mean(salary_can_may$Salary, na.rm = TRUE), mean(salary_can_jun$Salary, na.rm = TRUE), mean(salary_can_jul$Salary, na.rm = TRUE), mean(salary_can_aug$Salary, na.rm = TRUE), mean(salary_can_sep$Salary, na.rm = TRUE), mean(salary_can_oct$Salary, na.rm = TRUE), mean(salary_can_nov$Salary, na.rm = TRUE), mean(salary_can_dec$Salary, na.rm = TRUE))
months_for_plot <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
ordered_months <- factor(months_for_plot, levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'), ordered=TRUE)

#Plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_can_year, fill=ordered_months)) +
  scale_y_continuous(breaks = 1000000*breaks, labels = labels) +
  scale_fill_manual(values=reinapalette) +
  theme(legend.position = "none") + 
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

mean_sal_fin_year <- c(mean(salary_fin_jan$Salary, na.rm = TRUE), mean(salary_fin_feb$Salary, na.rm = TRUE), mean(salary_fin_mar$Salary, na.rm = TRUE), mean(salary_fin_apr$Salary, na.rm = TRUE), mean(salary_fin_may$Salary, na.rm = TRUE), mean(salary_fin_jun$Salary, na.rm = TRUE), mean(salary_fin_jul$Salary, na.rm = TRUE), mean(salary_fin_aug$Salary, na.rm = TRUE), mean(salary_fin_sep$Salary, na.rm = TRUE), mean(salary_fin_oct$Salary, na.rm = TRUE), mean(salary_fin_nov$Salary, na.rm = TRUE), mean(salary_fin_dec$Salary, na.rm = TRUE))

#Plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_fin_year, fill=ordered_months)) +
  scale_y_continuous(breaks = 1000000*breaks, labels = labels) +
  scale_fill_manual(values=reinapalette) +
  theme(legend.position = "none") + 
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

mean_sal_swe_year <- c(mean(salary_swe_jan$Salary, na.rm = TRUE), mean(salary_swe_feb$Salary, na.rm = TRUE), mean(salary_swe_mar$Salary, na.rm = TRUE), mean(salary_swe_apr$Salary, na.rm = TRUE), mean(salary_swe_may$Salary, na.rm = TRUE), mean(salary_swe_jun$Salary, na.rm = TRUE), mean(salary_swe_jul$Salary, na.rm = TRUE), mean(salary_swe_aug$Salary, na.rm = TRUE), mean(salary_swe_sep$Salary, na.rm = TRUE), mean(salary_swe_oct$Salary, na.rm = TRUE), mean(salary_swe_nov$Salary, na.rm = TRUE), mean(salary_swe_dec$Salary, na.rm = TRUE))

#Plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_swe_year, fill=ordered_months)) +
  scale_y_continuous(breaks = 1000000*breaks, labels = labels) +
  scale_fill_manual(values=reinapalette) +
  theme(legend.position = "none") + 
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

mean_sal_usa_year <- c(mean(salary_usa_jan$Salary, na.rm = TRUE), mean(salary_usa_feb$Salary, na.rm = TRUE), mean(salary_usa_mar$Salary, na.rm = TRUE), mean(salary_usa_apr$Salary, na.rm = TRUE), mean(salary_usa_may$Salary, na.rm = TRUE), mean(salary_usa_jun$Salary, na.rm = TRUE), mean(salary_usa_jul$Salary, na.rm = TRUE), mean(salary_usa_aug$Salary, na.rm = TRUE), mean(salary_usa_sep$Salary, na.rm = TRUE), mean(salary_usa_oct$Salary, na.rm = TRUE), mean(salary_usa_nov$Salary, na.rm = TRUE), mean(salary_usa_dec$Salary, na.rm = TRUE))

#Plot salary against birth months
ggplot() +
  geom_bar(stat="identity", mapping=aes(x=ordered_months, y=mean_sal_usa_year, fill=ordered_months)) +
  scale_y_continuous(breaks = 1000000*breaks, labels = labels) +
  scale_fill_manual(values=reinapalette) +
  theme(legend.position = "none") + 
  labs(title = "Distribution of salary according to birth month for North American ice hockey players", x = "Month of birth", y = "Mean salary")

# ---------------->> ALL COUNTRIES IN ONE PLOT <<----------------------

salary_adapted <- bind_rows(can_salary_adapted, fin_salary_adapted, swe_salary_adapted, usa_salary_adapted)

salary_means = salary_adapted %>%
  dplyr::group_by(Nat, month_text) %>%
  dplyr::summarize(mean_salary = mean(Salary, na.rm=T))

salary_means$month_text <- factor(salary_means$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot(
  data = salary_means,
  mapping = aes(x=month_text, y=mean_salary)
) +
  geom_point(mapping=aes(colour = Nat)) +
  geom_line(mapping=aes(group=Nat, colour = Nat)) +
  scale_y_continuous(breaks = 1000000*breaks, labels = labels) +
  labs(title = "Mean salaries of ice hockey players in different countries, based on birth month", x = "Months", y = "Mean salary", color='Country')