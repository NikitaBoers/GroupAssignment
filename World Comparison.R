setwd("C:/Users/Reina Borst/Desktop/UCU sem 4/Data Analysis for LAS/Group Project")
library(tidyverse)
library(plyr)

#Select only Canada in CLI
finland2 <- read.csv('finland_only2.csv')

#Make sure birth dates are recognised as dates
finland_adapted <- finland2 %>%
  mutate(
    dates = as.Date(birthDate),
    months = format(dates,'%m')
  )

finland_adapted$months <- revalue(finland_adapted$months, c("01"="1"))
finland_adapted$months <- revalue(finland_adapted$months, c("02"="2"))
finland_adapted$months <- revalue(finland_adapted$months, c("03"="3"))
finland_adapted$months <- revalue(finland_adapted$months, c("04"="4"))
finland_adapted$months <- revalue(finland_adapted$months, c("05"="5"))
finland_adapted$months <- revalue(finland_adapted$months, c("06"="6"))
finland_adapted$months <- revalue(finland_adapted$months, c("07"="7"))
finland_adapted$months <- revalue(finland_adapted$months, c("08"="8"))
finland_adapted$months <- revalue(finland_adapted$months, c("09"="9"))

finland_adapted2 <- finland_adapted %>%
  mutate(
    month_text = case_when(
      months == 1 ~ 'January',
      months == 2 ~ 'February',
      months == 3 ~ 'March',
      months == 4 ~ 'April',
      months == 5 ~ 'May',
      months == 6 ~ 'June',
      months == 7 ~ 'July',
      months == 8 ~ 'August',
      months == 9 ~ 'September',
      months == 10 ~ 'October',
      months == 11 ~ 'November',
      months == 12 ~ 'December'
    )
  )

finland_adapted2$abbmonths <- factor(finland_adapted2$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#Select only Canada in CLI
sweden2 <- read.csv('sweden_only2.csv')

#Make sure birth dates are recognised as dates
sweden_adapted <- sweden2 %>%
  mutate(
    dates = as.Date(birthDate),
    months = format(dates,'%m')
  )

sweden_adapted$months <- revalue(sweden_adapted$months, c("01"="1"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("02"="2"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("03"="3"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("04"="4"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("05"="5"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("06"="6"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("07"="7"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("08"="8"))
sweden_adapted$months <- revalue(sweden_adapted$months, c("09"="9"))

sweden_adapted2 <- sweden_adapted %>%
  mutate(
    month_text = case_when(
      months == 1 ~ 'January',
      months == 2 ~ 'February',
      months == 3 ~ 'March',
      months == 4 ~ 'April',
      months == 5 ~ 'May',
      months == 6 ~ 'June',
      months == 7 ~ 'July',
      months == 8 ~ 'August',
      months == 9 ~ 'September',
      months == 10 ~ 'October',
      months == 11 ~ 'November',
      months == 12 ~ 'December'
    )
  )

sweden_adapted2$abbmonths <- factor(sweden_adapted2$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot() +
  geom_bar(data=sweden_adapted2, mapping=aes(x=abbmonths, fill=abbmonths), colour='black') +
  scale_fill_brewer(palette="Set3") +
  scale_fill_discrete(name="Months") +
  theme(legend.position = "none") +
  labs(title = "Number of Swedish ice hockey players \nbased on their month of birth", x = "Months", y = "Number of players")

us2 <- read.csv('us_only2.csv')

#Make sure birth dates are recognised as dates
us_adapted <- us2 %>%
  mutate(
    dates = as.Date(birthDate),
    months = format(dates,'%m')
  )

us_adapted$months <- revalue(us_adapted$months, c("01"="1"))
us_adapted$months <- revalue(us_adapted$months, c("02"="2"))
us_adapted$months <- revalue(us_adapted$months, c("03"="3"))
us_adapted$months <- revalue(us_adapted$months, c("04"="4"))
us_adapted$months <- revalue(us_adapted$months, c("05"="5"))
us_adapted$months <- revalue(us_adapted$months, c("06"="6"))
us_adapted$months <- revalue(us_adapted$months, c("07"="7"))
us_adapted$months <- revalue(us_adapted$months, c("08"="8"))
us_adapted$months <- revalue(us_adapted$months, c("09"="9"))

us_adapted2 <- us_adapted %>%
  mutate(
    month_text = case_when(
      months == 1 ~ 'January',
      months == 2 ~ 'February',
      months == 3 ~ 'March',
      months == 4 ~ 'April',
      months == 5 ~ 'May',
      months == 6 ~ 'June',
      months == 7 ~ 'July',
      months == 8 ~ 'August',
      months == 9 ~ 'September',
      months == 10 ~ 'October',
      months == 11 ~ 'November',
      months == 12 ~ 'December'
    )
  )

us_adapted2$abbmonths <- factor(us_adapted2$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot() +
  geom_bar(data=us_adapted2, mapping=aes(x=abbmonths, fill=abbmonths), colour='black') +
  scale_fill_brewer(palette="Set3") +
  scale_fill_discrete(name="Months") +
  theme(legend.position = "none") +
  labs(title = "Number of USA ice hockey players \nbased on their month of birth", x = "Months", y = "Number of players")

setwd("C:/Users/Reina Borst/Desktop/UCU sem 4/Data Analysis for LAS/Group Project")
library(tidyverse)
library(plyr)

#Select only Canada in CLI
canadians2 <- read.csv('new_canada_only2.csv')

#Make sure birth dates are recognised as dates
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

ggplot()+
  geom_point(data = canadians_adapted, mapping=aes(x=month_text, y=..count..,color='Canada'), stat='count')+
  geom_line(data =canadians_adapted, mapping = aes(x=month_text, y=..count.., group=1, color='Canada'), stat='count')+
  geom_point(data = finland_adapted2, mapping=aes(x=month_text, ..count.., color='Finland'), stat='count')+
  geom_line(data =finland_adapted2, mapping = aes(x=month_text, ..count..,group=1, color='Finland'), stat='count')+
  geom_point(data = sweden_adapted2, mapping=aes(x=month_text, ..count..,color='Finland'), stat='count' )+
  geom_line(data =sweden_adapted2, mapping = aes(x=month_text, ..count..,group=1, color='Finalnd'), stat='count')+
  geom_point(data = us_adapted2, mapping=aes(x=month_text, ..count.., color='USA'), stat='count')+
  geom_line(data =us_adapted2, mapping = aes(x=month_text, ..count..,group=1, color='USA'), stat='count')+
  labs(title = "Comparison of ice hockey players from different countries \nbased on their month of birth in different groups based on birthyear", x = "Months", y = "Number of players", color='Birth year')


