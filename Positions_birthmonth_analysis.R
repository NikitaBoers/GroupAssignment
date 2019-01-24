library('tidyverse')
library('plyr')
#load in data
canadian_hockey_players<-read.csv('new_canada_only2.csv')
new_hockey_data<-read.csv('hockey_players_external_dataset.csv', stringsAsFactors = FALSE)

#Select canadians from new data
new_hockey_data<-new_hockey_data%>%filter(country=='CAN')

#Make names lower case in canadian_hockey_players
canadian_hockey_players$name<-tolower(canadian_hockey_players$name)

#Delete extra part of names
canadian_hockey_players<- canadian_hockey_players%>% mutate(name=str_remove(name, ' \\([1-9a-z\ ]+\\)'))

# flip names in new_hockey_data to first name last name
new_hockey_data<-new_hockey_data%>% mutate(name=gsub('(.+?) (.+)', '\\2 \\1', name))

# Remove columns
new_hockey_data$club<- NULL
new_hockey_data$year<-NULL
new_hockey_data$no<-NULL
new_hockey_data$weight<-NULL
new_hockey_data$age<-NULL
new_hockey_data$bmi<-NULL
new_hockey_data$height<-NULL
new_hockey_data$side<-NULL
new_hockey_data$birth<-NULL

# Merge two csv files into one file
merged_files<-merge(canadian_hockey_players,new_hockey_data, by = "name")

#Make unique
merged_files<-unique(merged_files)

#Make sure birth dates are recognised as dates and names of positions are spelled out
merged_files <- merged_files %>% 
  mutate(
    birthDate = as.Date(birthDate),
    months = format(birthDate,'%m')
  )
merged_files <- merged_files %>%
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

merged_files$month_text <- factor(merged_files$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

merged_files <- merged_files %>%
  mutate( 
    position = case_when( 
      position == 'D' ~ 'Defender', 
      position == 'F' ~ 'Forward', 
      position == 'G' ~ 'Goalkeeper'
     ) 
  )
#Make plot
nikitapalette <- c("#A1C3D1", "#B39BC8", "#F172A1")

ggplot() +
  geom_bar(data=merged_files, mapping=aes(x=month_text, fill=position), colour='black', position = 'stack') +
  scale_fill_discrete(name="Positions") +
  scale_fill_manual(values=nikitapalette) +
  labs(title = "Number of Canadian ice hockey players \nbased on their month of birth, grouped on position", x = "Months", y = "Number of players")
