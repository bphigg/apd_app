library(tidyverse)
library(stringr)
library(lubridate)

## Read in Arrest Data
arrests <- read_csv("apd/APD_Arrests.csv")
head(arrests)
str(arrests)

help(mutate)
help("drop_na")

## Generalize Address Column, add City and State
arrests <- arrests %>%
  mutate(address=str_remove_all(address, "-BLK")) %>%
  mutate(city='Asheville') %>%
  mutate(state='NC')

## find date range
max(arrests$date_arrested)
min(arrests$date_arrested)

## read in Use of Force Files
pd_force1 <- read_csv("apd/APD_Use_Of_Force_2021-2024.csv")
str(pd_force1)
pd_force2 <- read_csv("apd/APD_Use_Of_Force_2018-2020_data.csv")
str(pd_force2)
attributes(pd_force1)$names
attributes(pd_force2)$names

## Figure out what ojectid_1 is
length(unique(pd_force1$objectid_1)) == length(pd_force1$objectid_1)
max(pd_force1$OBJECTID)

## missing values in subject_id
sum(is.na(pd_force2$subject_id))

## data cleansing on file1
pd_force1 <- pd_force1 %>%
  select(-OBJECTID, -objectid_1) %>%
  mutate(occurred_date = ymd(occurred_date)) %>%
  rename("incident_date" = occurred_date)

## data cleansing on file2
pd_force2 <- pd_force2 %>%
  select(-objectid) %>%
  relocate(date_occurred, .after = county_location) %>%
  mutate(date_occurred = mdy(date_occurred)) %>%
  rename("incident_date" = date_occurred)

## combine file1 and file2
pd_force <- rbind(pd_force1, pd_force2)

## explore data columns, drop na date column
length(unique(pd_force$ia_no)) == length(pd_force$ia_no)
sum(is.na(pd_force$incident_date))
pd_force <- drop_na(pd_force, incident_date)
max(pd_force$incident_date)
unique(pd_force$type_force_used)

## list of all descriptions in type_force_used column
physical_restraint <- c("Physical Force-Restraint", "Physical Force-Restr.", "Phsysical Force", "Phsysical Force-Restr.", "Physical Force-Struggle", "Physical Force-Push Physical Force-Restr.", "Physical Force- Struggle, Physical Force Restr., Physical Force-Push", "Physical Force", "Physical Force-Struggle, Physical Force-Restr., Physical Force-Push", "Physical Force-Struggle, Physical Force-Restr.", "Physical Force-Push, Physical Force-Restr.", "Physical Force-Restraing", "Physcial Force-Struggle", "Physical Force-Stuggle", "Phsysical Force-Struggle", "Show of Force", "Physical Force - Restrict", "Physical Force - Push", "Physical Force - Struggle", "Physical Force-Struggle, Physical Force-Restraint", "Physical Force-Restr., Physical Force Struggle", "Physical Force-Struggle, Physical Force Restr.", "Phsyical Force-Struggle", "Physical Force-Push")

## recategorize similar descriptions in type_force_used column

knee_leg_strike <- c("Physical Force-Knee Strike(s)", "Knee Strike", "Knee/Leg Strike(s)")

taser <- c("Physical Force, Taser Deployment", "Taser Deployment", "Physical Force-Struggle, Taser Deployed","Taser Deployment, Closed Hand Strike(s)")

other <- c("Other (Describe)", "Other", "Physical Force-Struggle, Other(Describe)")

firearm <- c("Use of Firearm", "Firearm")

deadly_force <- c("Deadly Force")

closed_hand_strike <- c("Closed Hand Strike(s)", "Closed Hand Strike", "Physical Force, Close Hand Strike(s)", "Physical Force-Struggle, Closed Hand Strike", "Closed Fist Strike")

open_hand_strike <- c("Open-Hand Strike(s)")

impact_weapon <- c("Impact Weapon-Baton")

mace_spray <- c("Pepperball", "OC Spray", "OC Deployment")

## check for any missing category descriptions
length(taser) + length(knee_leg_strike) + length(physical_restraint) + length(other) + length(firearm) + length(deadly_force) + length(closed_hand_strike) + length(open_hand_strike) + length(impact_weapon) + length(mace_spray)

## create new column desc_of_force to contain to standardize type_force_used descriptions
pd_force <- pd_force %>%
  mutate("desc_of_force" = if_else(type_force_used %in% physical_restraint, "physical restraint",
                            if_else(type_force_used %in% knee_leg_strike, "knee or leg strike",
                            if_else(type_force_used %in% taser, "taser",
                            if_else(type_force_used %in% other, "other",
                            if_else(type_force_used %in% firearm, "firearm",
                            if_else(type_force_used %in% deadly_force, "deadly force",
                            if_else(type_force_used %in% closed_hand_strike, "fist strike",
                            if_else(type_force_used %in% open_hand_strike, "slap",
                            if_else(type_force_used %in% impact_weapon, "impact weapon",
                            if_else(type_force_used %in% mace_spray, "mace", "ERROR")))))))))))

## confirm na entries in type_force_used == "Error" in desc_of_force; count affected entries; delete entries
pd_force_error <- filter(pd_force, desc_of_force == "ERROR")
sum(is.na(pd_force$type_force_used))
length(pd_force_error$ia_no)

## view totals of desc_of_force
pd_force |> count(desc_of_force)
help(count)

## EDA
table(pd_force$subject_race, pd_force$desc_of_force)
race_count <- pd_force |> count(subject_race)
str(race_count)
table(pd_force$county_location, pd_force$subject_race)
bw_pdforce <- pd_force %>%
  filter(subject_race == "Black" | subject_race == "White")

table(bw_pdforce$county_location, bw_pdforce$subject_race)

## 1) proportion of force incidents on Black
n <- length(pd_force$subject_race)
p <- length(filter(pd_force, subject_race == "Black")[[1]])/n
print(p)
n*p
n*(1-p)
 