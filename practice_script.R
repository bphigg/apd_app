library(tidyverse)
library(stringr)

arrests <- read_csv("apd/APD_Arrests.csv")
head(arrests)
str(arrests)

help(mutate)

arrests <- arrests %>%
  mutate(address=str_remove_all(address, "-BLK")) %>%
  mutate(address=paste(address, "Asheville, NC"))
