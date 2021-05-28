
# C. McClintock 
# March 2021 
# ACLU inquiry 

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

lakewood <- read_csv("../data/lakewood.csv", skip=3)[,1:4]
names(lakewood) <- str_to_lower(gsub(" ", "", names(lakewood)))

lakewood <- subset(lakewood, jurisdictionbygeography=="Lakewood Police Department"&incidentdate %in% c("2020", "2019"))

lakewood <- lakewood[-c(1:2),]

person <- lakewood[1:32,]
property <- lakewood[33:84,]
society <- lakewood[85:110,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

lakewood <- rbind(person, property, society)

lakewood <- select(lakewood, offensetypegen, offensetype, everything())
lakewood$offensetype <- factor(lakewood$offensetype, levels=unique(lakewood$offensetype))

gen <- subset(lakewood, offensetypegen==offensetype)

lakewood <- spread(lakewood, key="incidentdate", value="numberofcrimes")

lakewood$percentchange <- round((lakewood$`2020`/lakewood$`2019`)-1, 2)

lakewood$difference <- (lakewood$`2020`-lakewood$`2019`)


# ...............................................................

ggplot(gen, aes(x=offensetype, y=numberofcrimes, fill=factor(incidentdate))) +
  geom_bar(stat="identity", position="dodge", color="black")+ theme_minimal()+
  theme + scale_fill_manual(values=c("#003f5c", "#444e86","#955196"))+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Lakewood Police Department Crimes by Offense Type, 2019 vs. 2020", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 129 March2021") +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 0.9), size=3.5, vjust=-.45)+
  scale_y_continuous(labels = scales::comma)

# ...............................................................

lakewood %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Lakewood Police Department Change in Violent Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 29 March2021 \nNo values given for justifiable homicide offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma)+ 
  theme()


lakewood %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Property"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Property Crime Offense Type",
       title="Lakewood Police Department Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 29 March2021") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma)


lakewood %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Society"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Crime Offense Type",
       title="Lakewood Police Department Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 29 March2021 \nNo values for sports tampering offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma) 

# .............................................................................

jeffco <- read_csv("../data/lakewood.csv", skip=3)[,1:4]
names(jeffco) <- str_to_lower(gsub(" ", "", names(jeffco)))

jeffco <- subset(jeffco, jurisdictionbygeography=="Jefferson County"&incidentdate %in% c("2020", "2019"))

jeffco <- jeffco[-c(1:2),]

person <- jeffco[1:32,]
property <- jeffco[33:84,]
society <- jeffco[85:110,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

jeffco <- rbind(person, property, society)

jeffco <- select(jeffco, offensetypegen, offensetype, everything())
jeffco$offensetype <- factor(jeffco$offensetype, levels=unique(jeffco$offensetype))

gen <- subset(jeffco, offensetypegen==offensetype)

jeffco <- spread(jeffco, key="incidentdate", value="numberofcrimes")

jeffco$percentchange <- round((jeffco$`2020`/jeffco$`2019`)-1, 2)

jeffco$difference <- (jeffco$`2020`-jeffco$`2019`)


# ...............................................................

ggplot(gen, aes(x=offensetype, y=numberofcrimes, fill=factor(incidentdate))) +
  geom_bar(stat="identity", position="dodge", color="black")+ theme_minimal()+
  theme + scale_fill_manual(values=c("#003f5c", "#444e86","#955196"))+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Jefferson County Crimes by Offense Type, 2019 vs. 2020", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 129 March2021") +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 0.9), size=3.5, vjust=-.45)+
  scale_y_continuous(labels = scales::comma)

# ...............................................................

jeffco %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Jefferson County Change in Violent Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 29 March2021 \nNo values given for justifiable homicide offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma)+ 
  theme()


jeffco %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Property"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Property Crime Offense Type",
       title="Jefferson County Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 29 March2021") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma)


jeffco %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Society"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Crime Offense Type",
       title="Jefferson County Change in Social Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 29 March2021 \nNo values for sports tampering offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma) 
