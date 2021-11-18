library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)
View(acc)
person <- acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)
## 1. Create a data frame containing the persons who are fatally hurt in the accidents (see FARS manual and look up variable INJ_SEV)
sevInj <- data.frame(person$AGE,person$PER_NO,person$INJ_SEV)

##2. Create a data frame containing the most dangerous vehicle make in each state. The number of persons fatally hit in the vehicle make is used to assess the (non-)safety of a make. Make sure to handle the missing values appropriately. (look up variable MAKE)
make <- data.frame(person$MAKE, person$STATE)
make <- make %>% 
  rename(MAKE = person.MAKE, STATE = person.STATE)
View(make)
counts <- rename(count(make,STATE,MAKE), Freq = n)
counts <- counts %>%
  group_by(STATE) %>%
  filter(Freq == max(Freq))
View(counts)

##3. Create a map, and label each state with the most dangerous vehicle.
stateID <- read_xlsx("C:\\Data Science\\DS202\\FRPP_GLC_-_United_StatesNov42021.xlsx")
state1 <- data.frame(stateID$`State Code`, stateID$`State Name`)
View(state1)
counts <- merge(counts,state1, by.x = "STATE", by.y = "stateID..State.Code.")
counts <- distinct(counts,STATE,.keep_all = TRUE)
counts$region <- tolower(counts$stateID..State.Name.)
states <- map_data("state")
nomatch1 <- counts %>%
  anti_join(states, by = "region")
unique(nomatch1$state)
nomatch2 <- states %>%
  anti_join(counts, by = "region")
unique(nomatch2$state)
View(counts)
vehicleMap <- counts %>%
  left_join(states, by="region") %>%
  distinct(region,.keep_all = TRUE)
View(vehicleMap)
  ggplot(vehicleMap,aes(x = long, y =lat)) +
    geom_polygon(aes(group = group)) + 
    geom_text(aes(label = MAKE), color = 'white', data = vehicleMap)
  
##4. Join the accident and person table (work out which variable(s) to use)
joinData <- merge(person,acc, by = "ST_CASE")
View(joinData)
##5. Tally the number of accidents by day of the week (DAY_WEEK), hour of the day (HOUR) and gender (SEX).
table(joinData$DAY.x)
table(joinData$HOUR.x)
table(joinData$SEX.x)

##6. Now plot a choropleth map of the number of deaths on a county level. 
county <- map_data('state')
ggplot(states, aes(x=long, y=lat)) + geom_polygon(aes(group=group))
