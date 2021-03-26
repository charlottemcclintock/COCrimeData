
# C. McClintock 
# create gifs of county jail changes

# .................................................................................

library(gganimate)
library(tidyverse)
library(lubridate)

# .................................................................................

# read in detailed jail population data
depop <- read_csv("data/depop-specific.csv", n_max = 47)

# add county to county variable for later mating
depop$County <- paste0(depop$County, " County")

# select variables of interest
depop <- select(depop, 1, 3, `Mar26`:`Dec 31`)

# gather to long form
depop <- gather(depop, 2:43, key="date", value="pop")

# create statewide aggregate for later
statewide <- depop %>% group_by(date) %>% summarize(totalpop=sum(pop, na.rm=T))

# read in population data, arrange, and subset
pop <- read_csv("data/pop.csv", skip=2)[,1:3]
names(pop) <- str_to_lower(gsub(" ", "", names(pop)))
pop <- arrange(pop, -estimatedpopulation)
pop <- pop[2:12,]
largest <- pop$jurisdictionbygeography

# subset to 11 largest counties
depop <- subset(depop, County %in% largest)

# create day variable by pulling numbers out of date string
m <- gregexpr('[0-9]+', depop$date)
depop$num <- regmatches(depop$date,m)

# create month variable by stripping numbers from date string
depop$month <- gsub('[[:digit:]]+', '', depop$date)

# recode month names to numeric
depop$month <- fct_recode(depop$month, 
                          "1"="ADP in January", 
                          "3"="Mar", 
                          "4"="Apr", 
                          "4"="Apr ", 
                          "5"="May ", 
                          "6"="June ", 
                          "7"="July ", 
                          "8"="August ", 
                          "9"="Sept ", 
                          "9"="September ", 
                          "10"="October ", 
                          "11"="November ", 
                          "12"="December ", 
                          "12"="Dec ")

# month back to numeric
depop$month <- as.numeric(as.character(depop$month))

# add year variable
depop$year <- 2020

# day variable to numeric and recode january
depop$num <- as.numeric(depop$num)
depop$num <- ifelse(is.na(depop$num), "1", depop$num)

# paste year, month, day together
depop$datenum <- paste0(depop$year,"-", depop$month, "-", depop$num)

# add new clean variable to statewide data
statewide <- left_join(statewide, select(depop, date, datenum), by="date")

# drop unnecessary variables
depop <- select(depop, County, pop, datenum)

# use lubridate to clean dates
depop$datenum <- ymd(depop$datenum)
statewide$datenum <- ymd(statewide$datenum)

# recode county as factor with levels in order of population
depop$County <- factor(depop$County, levels=largest)

# .................................................................................

# static line graph by county
ggplot(depop, aes(x=datenum, y=pop, group=County, color=County)) +
  geom_line() + geom_point(size=0.5)

ggplot(statewide, aes(x=datenum, y=totalpop, color="red")) +
  geom_line() + geom_point(size=0.5)

# dynamic gif visualization for top three most populous counties
top3 <- subset(depop, County %in% c("Denver County", "El Paso County", "Arapahoe County"))

top3 %>% ggplot(aes(x=datenum, y=pop, group=County, color=County)) +
  geom_line() +
  geom_point() +
  ggtitle("Jail Population During Covid") +
  theme_minimal() +
  transition_reveal(datenum)


# dynamic visualization of 11 most populous counties
anim <- depop %>% ggplot(aes(x=datenum, y=pop, group=County, color=County)) +
  geom_line() +
  geom_point() +
  ggtitle("Jail Population During Covid") +
  theme_minimal() +
  transition_reveal(datenum)

anim_save("bigcounties.gif", anim,  duration=20, height=800, width=2200)

# .................................................................................

# read in crime data from Colorado Crime Stats
crime <- read_csv("data/crime-bydate.csv", skip=3)[,1:2]
names(crime) <- c("date", "crimes")

# standardize dates
crime$date <- mdy(crime$date)

# subset to 2020
crime <- subset(crime, year(date)=="2020")

# investigate why this has 370 rows
arb <- crime %>% group_by(date) %>% count() %>% arrange(-n) # some dates appear twice


# remove dates that appear twice
crime <- left_join(crime, arb, by="date")
crime <- subset(crime, n==1)

ggplot(crime, aes(x=date,y=crimes, color="red")) +
  geom_line() + geom_point(size=0.5)

# decompose trends
crime$sevenday <- zoo::rollmean(crime$crimes, k = 7, fill = NA)
crime$thirtyday <- zoo::rollmean(crime$crimes, k = 30, fill = NA)
crime$detrend <- crime$sevenday-crime$thirtyday

# seven day and thirty day rolling averages 
ggplot(crime, aes(x=date)) +
  geom_line(aes(y=sevenday), color="red") + geom_point(aes(y=sevenday), size=0.5, color="red")+
  geom_line(aes(y=thirtyday), color="black") + geom_point(aes(y=thirtyday), size=0.5, color="black")

# seasonal trend
ggplot(crime, aes(x=date, y=detrend)) +
  geom_line() + geom_point(size=0.5)

# gif form
ggplot(crime, aes(x=date)) +
  geom_line(aes(y=sevenday), color="red") + geom_point(aes(y=sevenday), size=0.5, color="red")+
  geom_line(aes(y=thirtyday), color="black") + geom_point(aes(y=thirtyday), size=0.5, color="black")+
  ggtitle("Crime 7 Day Rolling Average for 2020") +
  theme_minimal() +
  transition_reveal(date)

# .................................................................................

# create proportional versions
crime$prop <- crime$thirtyday/crime$thirtyday[15]
statewide$prop <- statewide$totalpop/statewide$totalpop[1]

# visualization
ggplot()  +
  geom_line(aes(x=crime$date,y=crime$prop), color="red") + 
  geom_point(aes(x=crime$date,y=crime$prop), size=0.5, color="red")+
  geom_line(aes(x=statewide$datenum,y=statewide$prop), color="black") + 
  geom_point(aes(x=statewide$datenum,y=statewide$prop), size=0.5, color="black") + 
  geom_hline(aes(yintercept=1))

# .................................................................................

# separate by type of crime

