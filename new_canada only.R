#working with the canada only dataset

install.packages('tidyverse')
library('tidyverse')

#performing some checks on the data
canadaonly <- hockey_players2 %>% 
  filter(birthplace %in% 'canada')
