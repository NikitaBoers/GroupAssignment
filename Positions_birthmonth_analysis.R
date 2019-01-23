library('tidyverse')
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
new_hockey_data<-new_hockey_data%>% mutate(name=factor(unlist(strsplit(name, " "))),levels=c())
canadians_adapted$month_text <- factor(canadians_adapted$month_text, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


# Merge two csv files into one file
merged_files<-merge(canadian_hockey_players,new_hockey_data, by = "name")