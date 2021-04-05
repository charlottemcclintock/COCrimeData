
# C. McClintock 
# Racial Disparity, CLEAR Act

# .........................................................................

# load libraries
library(tidyverse)

# read in the data
clear <- read_csv("HB19-1297Data.csv")

# .........................................................................

# clean names
names(clear) <- str_to_lower(gsub(" ", "", names(clear)))

# subset to q4 of 2020
clear <- subset(clear, qtryear=="2020" & qtr=="4")
# bookings and releases are quarterly 

# select variables of interest
clear <- select(clear, county, bookings, releases, measure, 
                total, black:unknownethnicity)

# total counts statewide
state_total <- clear %>% group_by(measure) %>% summarize(num=sum(total, na.rm=T))

clear$whitenothispanic <- clear$white-clear$hispanic
clear$whitehispanic <- clear$hispanic

clear <- select(clear, -c(white, hispanic, `non-hispanic`, unknownethnicity))

# race/ethnicity to long
clear <- gather(clear, black:whitehispanic, key="raceethnicity", value="num")


# total statewide by race/ethnicity
state_race <- clear %>% group_by(measure, raceethnicity) %>% summarize(num=sum(num, na.rm=T))

# separate out race and ethnicity 
# state_race$facet <- ifelse(state_race$raceethnicity %in% 
#                              c("hispanic", "non-hispanic"), 
#                            "Ethnicity","Race")

top3counties <- clear %>% 
  subset(county %in% c("El Paso", "Denver", "Jefferson"))  %>% 
  group_by(measure, raceethnicity) %>% 
  summarize(num=sum(num, na.rm=T))

# .........................................................................

state_race %>% subset(measure=="Number of inmates") %>% 
  ggplot(aes(raceethnicity, num)) + 
    geom_bar(stat="identity") + 
  coord_flip()


state_race %>% 
  subset(measure %in% 
           c("Number of inmates", "Sentenced", "Unsentenced - Hold", 
             "Unsentenced - No Hold", "Unsentenced - No Hold Misdemeanors", 
             "Unsentenced - No Hold Felonies")) %>% 
  ggplot(aes(x=measure, y=num, fill=raceethnicity)) + 
  geom_bar(position = "fill", stat="identity") 









