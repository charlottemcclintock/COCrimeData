
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

littleton <- read_csv("data/littletonpolicedept.csv", skip=3)[,1:4]
names(littleton) <- str_to_lower(gsub(" ", "", names(littleton)))

lpd <- subset(littleton, jurisdictionbygeography=="Littleton Police Department"&incidentdate %in% c("2020", "2019"))


person <- lpd[3:34,]
property <- lpd[35:86,]
society <- lpd[87:112,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

littleton <- rbind(person, property, society)

littleton <- select(littleton, offensetypegen, offensetype, everything())
littleton$offensetype <- factor(littleton$offensetype, levels=unique(littleton$offensetype))
littleton <- spread(littleton, key="incidentdate", value="numberofcrimes")

littleton$percentchange <- round((littleton$`2020`/littleton$`2019`)-1, 2)

littleton$difference <- (littleton$`2020`-littleton$`2019`)

write.csv(littleton, "littletonpd-detail.csv")


# ...............................................................


littleton <- read_csv("data/littletonpolicedept.csv", skip=3)[,1:4]
names(littleton) <- str_to_lower(gsub(" ", "", names(littleton)))

littleton <- subset(littleton, !jurisdictionbygeography=="Littleton Police Department"&incidentdate %in% c("2020", "2019"))


person <- littleton[7:102,]
property <- littleton[103:258,]
society <- littleton[259:336,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

littleton <- rbind(person, property, society)

littleton <- select(littleton, offensetypegen, offensetype, everything())
littleton$offensetype <- factor(littleton$offensetype, levels=unique(littleton$offensetype))
littleton <- spread(littleton, key="incidentdate", value="numberofcrimes")

littleton$percentchange <- round((littleton$`2020`/littleton$`2019`)-1, 2)

littleton$difference <- (littleton$`2020`-littleton$`2019`)

write.csv(littleton, "littleton-offensetype-detail.csv")



all3 <- littleton %>% 
  group_by(offensetypegen, offensetype) %>% 
  summarize(`2020`=sum(`2020`, na.rm = T), 
            `2019`=sum(`2019`, na.rm = T))
  
all3$percentchange <- round((all3$`2020`/all3$`2019`)-1, 2)

all3$difference <- (all3$`2020`-all3$`2019`)

write.csv(all3, "douglasjeffersonaraphoe-detail.csv")


all3 %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Change in Violent Crime by Offense Type, 2019 to 2020",
       subtitle="for Jefferson County, Arapahoe County, Douglas County",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values given for justifiable homicide offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits=c(-500, 400)) +
  annotate("text", y=150, x=7, label="Increase in aggravated assaults \noffset by much greater decrease \nin simple assault, declines \nin rape and fondling", 
           size=3.5, family="Source Sans Pro", lineheight = 1)+
  geom_segment(aes(x=1,xend=5, y=1,yend=150), size=0.25) +
  geom_segment(aes(x=9,xend=14.25, y=150,yend=150), size=0.25) 


all3 %>% 
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


littleton %>% 
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


