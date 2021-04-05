
# C. McClintock 
# Racial Disparity, Arrests

# .........................................................................

# load libraries
library(tidyverse)
library(scales)

# read in the data
arrests <- read_csv("race-arrestsummons-offense.csv", skip=5) %>% select(-X7)

# .........................................................................

# clean names
names(arrests) <- str_to_lower(gsub(" ", "", names(arrests)))

# add spaces around slashes
arrests$arresttype <- gsub("/", " / ", arrests$arresttype)

# drop missing and unknown since they're all 0
arrests <- subset(arrests, !arresteerace %in% c("Unknown", "Missing"))
arrests <- subset(arrests, !arresttype=="Missing")

# overall counts by race/ethnicity
arb <- arrests %>% group_by(arresteerace, arresteeethnicity) %>% summarize(sum=sum(numberofarrestees, na.rm = T))
# not white latino is very small, dropping for simplicity

# recode so white is separated into white/latino and white/not latino
arrests$racespec <- ifelse(arrests$arresteerace=="White" & 
                             arrests$arresteeethnicity=="Not Hispanic or Latino", 
                           "White Alone", arrests$arresteerace)
arrests$racespec <- ifelse(arrests$arresteerace=="White" & 
                             arrests$arresteeethnicity=="Hispanic or Latino", 
                           "White & Hispanic or Latino", arrests$racespec )
# resummarize based on 
arrests <- arrests %>% group_by(arrestoffense, arresttype, racespec) %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm=T))

rm(list="arb")
save.image("arrests.RData")


# .........................................................................

arrests %>% subset(arrestoffense %in% c("Crimes Against Person", 
                                        "Crimes Against Property", 
                                        "Crimes Against Society")) %>% 
  ggplot(aes(x=arresttype, y=numberofarrestees, fill=racespec)) + 
  geom_bar(stat="identity", position="fill") + facet_wrap(~arrestoffense) + 
  scale_x_discrete(labels=wrap_format(10)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))
# .........................................................................

p <- arrests %>% subset(arrestoffense=="Crimes Against Person") %>%  
  ggplot(aes(x=as.character(arresttype), y=numberofarrestees, fill=racespec)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_x_discrete(labels=wrap_format(10)) + theme_minimal() +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))
ggplotly(p)


arrests %>% group_by(arresteeethnicity) %>% summarize(sum=sum(numberofarrestees, na.rm = T))
