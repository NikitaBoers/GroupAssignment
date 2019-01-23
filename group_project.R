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

ggplot() +
  geom_bar(data=canadians_adapted, mapping=aes(x=month_text, fill=month_text), colour='black') +
  scale_fill_brewer(palette="Set3") +
  scale_fill_discrete(name="Months") +
  labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1870-1999", x = "Months", y = "Number of players")




             #Same thing for subgroups based on birthyear
             #1870- 1899
             birthdate1870<-read.csv('1870.1899.csv')
             birthdate1870 <- birthdate1870 %>% mutate(months = format(as.Date(dates), '%m')) 
             
             birthdate1870 <- birthdate1870 %>% 
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
             birthdate1870$month_text <- factor(birthdate1870$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             ggplot() +
               geom_bar(data=birthdate1870, mapping=aes(x=month_text, fill=month_text), colour='black') +
               scale_fill_brewer(palette="Set3") +
               scale_fill_discrete(name="Months") +
               labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1870-1899", x = "Months", y = "Number of players")
             
             
             #1900-1924
             birthdate1900<-read.csv('1900.1924.csv')
             birthdate1900 <- birthdate1900 %>% mutate(months = format(as.Date(dates), '%m')) 
             
             birthdate1900 <- birthdate1900 %>% 
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
             birthdate1900$month_text <- factor(birthdate1900$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             ggplot() +
               geom_bar(data=birthdate1900, mapping=aes(x=month_text, fill=month_text), colour='black') +
               scale_fill_brewer(palette="Set3") +
               scale_fill_discrete(name="Months") +
               labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1900-1924", x = "Months", y = "Number of players")
             
             #1925-1949
             birthdate1925<-read.csv('1925.1949.csv')
             birthdate1925 <- birthdate1925 %>% mutate(months = format(as.Date(dates), '%m')) 
             
             birthdate1925 <- birthdate1925 %>% 
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
             birthdate1925$month_text <- factor(birthdate1925$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             ggplot() +
               geom_bar(data=birthdate1925, mapping=aes(x=month_text, fill=month_text), colour='black') +
               scale_fill_brewer(palette="Set3") +
               scale_fill_discrete(name="Months") +
               labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1925-1949", x = "Months", y = "Number of players")
             
             #1950-1974
             birthdate1950<-read.csv('1950.1974.csv')
             birthdate1950 <- birthdate1950 %>% mutate(months = format(as.Date(dates), '%m')) 
             
             birthdate1950 <- birthdate1950 %>% 
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
             birthdate1950$month_text <- factor(birthdate1950$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             ggplot() +
               geom_bar(data=birthdate1950, mapping=aes(x=month_text, fill=month_text), colour='black') +
               scale_fill_brewer(palette="Set3") +
               scale_fill_discrete(name="Months") +
               labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1950-1974", x = "Months", y = "Number of players")
             
             #1975-1999
             birthdate1975<-read.csv('1975.1999.csv')
             birthdate1975 <- birthdate1975 %>% mutate(months = format(as.Date(dates), '%m')) 
             
             birthdate1975 <- birthdate1975 %>% 
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
             birthdate1975$month_text <- factor(birthdate1975$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             ggplot() +
               geom_bar(data=birthdate1975, mapping=aes(x=month_text, fill=month_text), colour='black') +
               scale_fill_brewer(palette="Set3") +
               scale_fill_discrete(name="Months") +
               labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, born in 1975-1999", x = "Months", y = "Number of players")
             
ggplot()+
  geom_point(data = birthdate1870, mapping=aes(x=month_text, y=..count..,color='1870-1899'), stat='count')+
  geom_line(data =birthdate1870, mapping = aes(x=month_text, y=..count.., group=1, color='1870-1899'), stat='count')+
  geom_point(data = birthdate1900, mapping=aes(x=month_text, ..count.., color='1900-1924'), stat='count')+
  geom_line(data =birthdate1900, mapping = aes(x=month_text, ..count..,group=1, color='1900-1924'), stat='count')+
  geom_point(data = birthdate1925, mapping=aes(x=month_text, ..count..,color='1925-1949'), stat='count' )+
  geom_line(data =birthdate1925, mapping = aes(x=month_text, ..count..,group=1, color='1925-1949'), stat='count')+
  geom_point(data = birthdate1950, mapping=aes(x=month_text, ..count.., color='1950-1974'), stat='count')+
  geom_line(data =birthdate1950, mapping = aes(x=month_text, ..count..,group=1, color='1950-1974'), stat='count')+
  geom_point(data = birthdate1975, mapping=aes(x=month_text, ..count.., color='1975-1999'), stat='count')+
  geom_line(data =birthdate1975, mapping = aes(x=month_text, ..count..,group=1,color='1975-1999'), stat='count')+
  labs(title = "Number of Canadian ice hockey players \nbased on their month of birth in different groups based on birthyear", x = "Months", y = "Number of players", color='Birth year')
  
