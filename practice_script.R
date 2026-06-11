library(tidyverse)
library(stringr)
library(lubridate)
install.packages("distributions3")
library(distributions3)

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
p_samp <- length(filter(pd_force, subject_race == "Black")[[1]])/n
print(p_samp)
p_pop <- 0.082
n*p_pop
n*(1-p_pop)

## We a have population proportion of 8.2% Black residents in the city of Asheville. We have a sample proportion of 28.0% Black subjects in Asheville Police Dept Use of Force reports.
## Question: is it feasible that the sample proportion falls within a 95% CI of the normal distribution of the population proportion?
## H0 -> the proportion of use of force on Black residents is the same as the proportion of Black residents
z_stat <- (p_samp-p_pop)/sqrt(p_pop*(1-p_pop)/n)
Z <- Normal(0,1)
p_stat <- 2 * cdf(Z, -abs(z_stat))
print(p_stat)
## didn't choose an alpha value, but the p_stat value is really close to zero, which tells us that it is highly unlikely that our sample proportion falls within the distribution of our population proportion. The two proportions are unrelated. So, why is the proportion of Black subjects in the Use of Force report significantly greater than the proportion of Black residents?

zstat <- function(){
  (p_samp-p_pop)/sqrt(p_pop*(1-p_pop)/n)
}
zstat()

## ARRESTS DATA

## Read in Arrest Data
arrests <- read_csv("apd/APD_Arrests.csv")
head(arrests)
str(arrests)

help(mutate)
help("drop_na")

library(stringr)

## Generalize Address Column, add City and State
arrests <- arrests %>%
  mutate(address=str_remove_all(address, "-BLK")) %>%
  mutate(city='Asheville') %>%
  mutate(state='NC') %>%
  mutate(street_=word(address, 1)) %>%
  mutate(street = if_else(str_detect(street_, "[A-Z]"), address, 
                          word(address, 2, -1))) %>%
  mutate(street_no = if_else(str_detect(street_, "[A-Z]"), "9999", 
                             word(address, 1)))

## find date range
max(arrests$date_arrested)
min(arrests$date_arrested)

## proportion of arrests
unique(arrests$offense_type)
unique(arrests$street)
count(arrests, offense_type, subject_gender, sort = TRUE)
arrests |> count(subject_race)
count(arrests, address, sort = TRUE)
count(arrests, street, sort=TRUE)

hist(as.numeric(arrests$time_arrested), breaks=24)

unique(filter(arrests, street == "MELROSE AVE")$street_no)

count(filter(arrests, street == "TUNNEL RD" & street_no == "100"), offense_type, sort=TRUE)
count(filter(arrests, str_detect(street, "WOODVALE")), offense_type, sort=TRUE)

count(filter(arrests, subject_gender == "F"), offense_type, sort = TRUE)

# F/M split? does this same proportion hold by race?