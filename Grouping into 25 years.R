library(tidyverse)

Hockey <- read.csv('new_canada_only2.csv', stringsAsFactors = F)
  canadians_adapted <- Hockey %>%
  mutate(
    dates = as.Date(birthDate)
  )

Y1870.1899 <- canadians_adapted %>%
  filter(dates < as.Date('1899-12-31'))
Y1900.1924 <- canadians_adapted %>%
  filter(dates > as.Date('1899-12-31'), dates < as.Date('1924-12-31'))
Y1925.1949 <- canadians_adapted %>%
  filter(dates > as.Date('1924-12-31'), dates < as.Date('1949-12-31'))
Y1950.1974 <- canadians_adapted %>%
  filter(dates > as.Date('1949-12-31'), dates < as.Date('1974-12-31'))
Y1975.1999 <- canadians_adapted %>%
  filter(dates > as.Date('1974-12-31'), dates < as.Date('1999-12-31'))
Y2000 <- canadians_adapted %>%
  filter(dates > as.Date('1899-12-31'))

write.csv(Y1870.1899, '1870.1899.csv')
write.csv(Y1900.1924, '1900.1924.csv')
write.csv(Y1925.1949, '1925.1949.csv')
write.csv(Y1950.1974, '1950.1974.csv')
write.csv(Y1975.1999, '1975.1999.csv')
write.csv(Y2000, '2000.csv')
