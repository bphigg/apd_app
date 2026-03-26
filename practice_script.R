library(tidyverse)
library(stringr)

arrests <- read_csv("apd/APD_Arrests.csv")
head(arrests)
str(arrests)

help(mutate)

arrests <- arrests %>%
  mutate(address=str_remove_all(address, "-BLK")) %>%
  mutate(city='Asheville') %>%
  mutate(state='NC')


pd_force1 <- read_csv("apd/APD_Use_Of_Force_2021-2024.csv")
str(pd_force1)
pd_force2 <- read_csv("apd/APD_Use_Of_Force_2018-2020_data.csv")
str(pd_force2)
attributes(pd_force1)$names
attributes(pd_force2)$names

length(unique(pd_force1$objectid_1)) == length(pd_force$objectid_1)
