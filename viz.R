
# C. McClintock 
# March 2021 
# inquiry into changes in crime

# .............................................................................

# load libraries
library(tidyverse)
library(extrafont)
library(Cairo)
library(RColorBrewer)
library(scales)

# load fonts and set theme
loadfonts()
theme <- theme(text = element_text(family = "Source Sans Pro"),
               legend.position = "bottom", panel.grid.minor.x=element_blank())

# .............................................................................

# read in data
co20 <- read_csv("data/co-2020.csv", skip=3)
co19 <- read_csv("data/co-2019.csv", skip=3)
type20 <- read_csv("data/co-2020-bytype.csv", skip=3)[,1:3]
type19 <- read_csv("data/co-2019-bytype.csv", skip=3)[,1:3]

# clean names
names(co20) <- str_to_lower(gsub(" ", "", names(co20)))
names(co19) <- str_to_lower(gsub(" ", "", names(co19)))
names(type19) <- str_to_lower(gsub(" ", "", names(type19)))
names(type20) <- str_to_lower(gsub(" ", "", names(type20)))
names(co20) <- gsub("[^[:alnum:] ]", "", names(co20))
names(co19) <- gsub("[^[:alnum:] ]", "", names(co19))

# add year column to data
co20$year <- 2020
co19$year <- 2019
type20$year <- 2020
type19$year <- 2019

# .............................................................................

# NUMBER OF CRIMES, 2019 VS. 2020

# select counts and join both years
count <- full_join(select(co19, jurisdictionbygeography, numberofcrimes, year), 
                   select(co20, jurisdictionbygeography, numberofcrimes, year), 
                   by=c("jurisdictionbygeography", "numberofcrimes", "year"))

# drop the colorado total
count <- subset(count, !jurisdictionbygeography=="Colorado")

# visualize
ggplot(count, aes(reorder(jurisdictionbygeography, numberofcrimes), 
                  numberofcrimes, fill=as.factor(year))) + 
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 1), size=2.5, hjust=-.25) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000)) + theme_minimal() + 
  labs(y="Number of Crimes", x="County", fill="Year", 
       title="Number of Crimes in Fourteen Largest CO Counties, 2019 vs. 2020", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") + 
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(3,7)])


# .............................................................................

# CRIME RATE, 2019 VS. 2020

# select crime rate and join
rate <- full_join(select(co19, jurisdictionbygeography, crimerateper100000, year), 
                   select(co20, jurisdictionbygeography, crimerateper100000, year), 
                   by=c("jurisdictionbygeography", "crimerateper100000", "year"))

# drop colorado statewide estimate
rate <- subset(rate, !jurisdictionbygeography=="Colorado")

# visualize
ggplot(rate, aes(reorder(jurisdictionbygeography, crimerateper100000), 
                 crimerateper100000, fill=as.factor(year))) + 
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  geom_text(aes(label=prettyNum(round(crimerateper100000), big.mark=",")), 
            position = position_dodge(width = 1), size=2.5, hjust=-.25) +
  theme_minimal() + scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  labs(y="Crime Rate (per 100,000)", x="County", fill="Year", 
       title="Crime Rate per 100,000 in Fourteen Largest CO Counties, 2019 vs. 2020", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") + 
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(3,7)])

# .............................................................................

# comparison - total crime 

# join years together
type <- full_join(type19, type20, by=names(type19))

# subset unneeded offense types
type_co <- subset(type, jurisdictionbygeography=="Colorado" & 
                    !offensetype %in% c("Missing", "Non-Reportable NIBRS Offense"))

# visualize overall change in crime, with fill by type
ggplot(type_co, aes(factor(year), y=numberofcrimes, fill=offensetype)) + 
  geom_bar(stat="identity", width=0.8) + theme_minimal() + theme +
  annotate("text", x=c(1,2), y=c(350000,365000), label=c("340,228", "353,367"), family = "Source Sans Pro")+ 
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(4,5,6)])+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Colorado Number of Crimes, 2019 vs. 2020", 
       subtitle="Crimes against society decreased from 2019 to 2020, crimes against persons stayed the same, \nincrease in property crime accounts for the entirety of the increase in crime from 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021")+
  scale_y_continuous(labels = scales::comma) + 
  annotate("text", x=c(1,1,1), y=c(310000, 175000, 22000), label=c("64,104","233,807","42,317"), family = "Source Sans Pro")+ 
  annotate("text", x=c(2,2,2), y=c(320000, 180000, 15000), label=c("64,117", "257,982", "31,268"), family = "Source Sans Pro")
# crimes against society decreased, crimes against persons stayed the same, only 
# property crime increased 

# .............................................................................

# year to factor variable
type_co$year <- factor(type_co$year)

# visualize statewide change by type
g <- ggplot(type_co, aes(offensetype, y=numberofcrimes, fill=year)) + 
  geom_bar(stat="identity", position = "dodge") + theme_minimal()+
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(4,6)])+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Colorado Crimes by Offense Type, 2019 vs. 2020", 
       subtitle="Crimes against society decreased from 2019 to 2020, crimes against persons stayed the same, \nincrease in property crime accounts for the entirety of the increase in crime from 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 1), size=3.5, vjust=-.45)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 300000))
g + 
  annotate("text", x=c(1,2,3), y=c(100000, 285000, 80000), label=c("0.0% change","10.3% increase", "26.1% decrease"), 
           size=4, family="Source Sans Pro", lineheight = 1, fontface="italic") +
  geom_segment(aes(x=1,xend=1, y=70000,yend=90000), size=0.25)+
  geom_segment(aes(x=2,xend=2, y=262000,yend=274000), size=0.25)+
  geom_segment(aes(x=3,xend=3, y=50000,yend=70000), size=0.25) 

# .............................................................................


# subset unneeded offense types
type <- subset(type, !jurisdictionbygeography=="Colorado" & 
                    !offensetype %in% c("Missing", "Non-Reportable NIBRS Offense"))

# large faceted plot of change in crime for each county by type
ggplot(type, aes(x=offensetype, y=numberofcrimes, fill=factor(year))) +
  geom_bar(stat="identity", position="dodge")  + 
  facet_wrap(~jurisdictionbygeography, scales = "free_y", ncol=2) + theme_minimal() +
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(4,6)]) +
  scale_x_discrete(labels=wrap_format(15))+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 1), size=2.5, vjust=1.4)+
  labs(y="Number of Crimes", fill="Year", x="Offense Type", 
       title="Colorado Crimes by County and Offense Type, 2019 vs. 2020", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") 


# .............................................................................

# change shape of offense type data for visualization
type_wide <- spread(type, key="year", value="numberofcrimes")
type_wide$perc <- (type_wide$`2020`/type_wide$`2019`)-1
type_wide$diff <- (type_wide$`2020`-type_wide$`2019`)

# create subset for violent crime
person_wide <- subset(type_wide, offensetype=="Crimes Against Person")

# specific change in violent crime by offense type
ggplot(person_wide, aes(reorder(jurisdictionbygeography, perc), perc, fill=perc)) + 
  geom_bar(stat="identity") + theme_minimal() + theme +
  coord_flip() + 
  labs(title="Percent Change in Crimes Against Persons, 2019 to 2020", x="County", y="% Change", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021
       Note: Adams County reported exactly the same number of violent crimes (14,018) in 2019 and 2020, 
       which suggest a possible error in source data.") +
  scale_fill_gradient2(low = "#2166ac", mid = "#e4eff5", high="#b2182b") +
  scale_y_continuous(labels=percent, limits=c(-0.23, 0.2)) + guides(fill=F)+
  geom_text(aes(label=paste0(prettyNum(round(100*perc, 1)), "%")), 
            position = position_dodge(width = 1), size=2.9, hjust=1.2) +
  annotate("text", x=c(1:14), y=rep(0.2, 14), 
           label=paste0("n = ", arrange(person_wide, perc)$diff), 
           family="Source Sans Pro", size=2.8)

# .............................................................................

# big facet plot of change in crime by county by offense type
ggplot(type_wide, aes(offensetype, perc, fill=perc)) + 
  geom_bar(stat="identity", position="dodge") + theme_minimal() + theme +
  facet_wrap(~jurisdictionbygeography, ncol=2)+
  scale_fill_gradient2(low = "#2166ac", mid = "#e4eff5", high="#b2182b") +
  scale_y_continuous(labels=percent, limits=c(-0.65, 0.3)) + guides(fill=F) +
  labs(title="Percent Change in Number of Crimes by Offense Type, 2019 to 2020", 
       x="", y="% Change",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021
       Note: Adams County reported exactly the same number of violent crimes (14,018) in 2019 and 2020, 
       which suggest a possible error in source data.")+
  scale_x_discrete(labels=wrap_format(15), position="top") +
  geom_text(aes(label=paste0(round(100*perc, 1), "%", " (", diff, ")")), 
            position = position_dodge(width = 1), size=2.5, vjust=1.2)

# .............................................................................

# read in data with specific offenses and offense type
type_spec <- read_csv("data/type-specific.csv", skip=2)[,1:3]
names(type_spec) <- str_to_lower(gsub(" ", "", names(type_spec)))

# subset to each type of crime
person <- type_spec[1:32,]
property <- type_spec[33:84,]
society <- type_spec[85:110,]

# label
person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

# merge back together
type_spec <- rbind(person, property, society)

# reorder, factor in order, incident date to columns
type_spec <- select(type_spec, offensetypegen, offensetype, everything())
type_spec$offensetype <- factor(type_spec$offensetype, levels=unique(type_spec$offensetype))
type_spec <- spread(type_spec, key="incidentdate", value="numberofcrimes")

# calculate percent change
type_spec$percentchange <- round((type_spec$`2020`/type_spec$`2019`)-1, 2)

# calculate total difference
type_spec$difference <- (type_spec$`2020`-type_spec$`2019`)

# write detailed csv
write.csv(type_spec, "offensetype-detail.csv")

# calculate overall sums
type_spec %>% 
  subset(!offensetypegen==offensetype) %>% 
  group_by(offensetypegen) %>% summarize(`2020`=sum(`2020`, na.rm=T))

# change in violent crimes
type_spec %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Colorado Statewide Change in Violent Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values given for justifiable homicide offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits = c(-2250,2500)) +
  annotate("text", y=1500, x=7, label="Increase in aggravated assaults \nalmost entirely offset by \ndecrease in simple assault", 
           size=3.5, family="Source Sans Pro", lineheight = 1)+
  geom_segment(aes(x=1,xend=5.5, y=100,yend=1000), size=0.25) +
  geom_segment(aes(x=13,xend=8.5, y=1500,yend=1500), size=0.25) 
  
# change in property crimes
type_spec %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Property"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Property Crime Offense Type",
       title="Colorado Statewide Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits=c(-3500, 9000), breaks = c(-2000, 0, 2000, 4000, 6000, 8000)) +
  annotate("text", y=6000, x=17, label="increase in property crime driven by increase \nin motor vehicle theft, vandalism, theft from \na motor vehicle, and identity theft.", 
           size=3.5, family="Source Sans Pro", lineheight = 1)+
  geom_segment(aes(x=23,xend=18.5, y=7000,yend=7000), size=0.25)+
  geom_segment(aes(x=2.5,xend=2.5, y=300,yend=2500), size=0.25)+
  annotate("text", y=5300, x=2.5, label="declines in shoplifting, theft from building.", 
           size=3.5, family="Source Sans Pro", lineheight = 1)

# change in "crimes against society"
type_spec %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Society"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Crime Offense Type",
       title="Colorado Statewide Change in Crimes Against Society by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values for sports tampering offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits = c(-8000,6000)) +
  geom_segment(aes(x=2.8,xend=5, y=-4200,yend=-4200), size=0.25)+
  annotate("text", y=-4200, x=5.6, label="huge declines in drug crime \ndriving crimes against society decrease", 
           size=3.5, family="Source Sans Pro", lineheight = 1)


# .............................................................................

# read in jail depop and crime data
change <- read_csv("data/co-jaildepop-crime.csv") %>% select(-X1)

# long to wide for scatterplots
change <- spread(change, key=metric, value=perc)
names(change)[4:5] <- c("perc_crime","perc_jailpop")

# change in jail population and change in crime
ggplot(change, aes(x=perc_jailpop, y=perc_crime)) + 
  geom_point() + xlim(-0.5, 0.5) + ylim(-0.5, 0.5) + 
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) 

# read in crime type by county
ctype <- read_csv("data/county-type.csv", skip=3)[,1:4]

# clean, long to wide, select
names(ctype) <- str_to_lower(gsub(" ", "", names(ctype)))
ctype <- spread(ctype, key="incidentdate", value="numberofcrimes")
ctype <- ctype[,1:4]

# calculate percent change and difference
ctype$perc_crime <- (ctype$`2020`/ctype$`2019`)-1
ctype$diff <- (ctype$`2020` - ctype$`2019`)

# select variables of interest
diff <- select(ctype, jurisdictionbygeography, offensetype, diff)

# long to wide
ctype <- pivot_wider(ctype[,c(1,2,5)], id_cols="jurisdictionbygeography", names_from="offensetype", values_from=c("perc_crime"))

# join to jail data
jail <- left_join(jailpop, ctype, by=c("county"="jurisdictionbygeography"))
names(jail) <- str_to_lower(gsub(" ", "", names(jail)))

# violent crime x jail pop
ggplot(jail, aes(x=perc_jailpop, y=crimesagainstperson)) + 
  geom_point() + xlim(-0.5, 0.5) + ylim(-0.5, 0.5) + 
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) 

# property crime x jail pop
ggplot(jail, aes(x=perc_jailpop, y=crimesagainstproperty)) + 
  geom_point() + xlim(-0.5, 0.5) + ylim(-0.5, 0.5) + 
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) 

# social crimex jail pop
ggplot(jail, aes(x=perc_jailpop, y=crimesagainstsociety)) + 
  geom_point() + xlim(-0.5, 0.5) + ylim(-0.5, 0.5) + 
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) 


# .............................................................................
