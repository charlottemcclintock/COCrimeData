
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

arvada <- read_csv("data/arvadapd.csv", skip=2)[,1:4]
names(arvada) <- str_to_lower(gsub(" ", "", names(arvada)))

arvada <- subset(arvada, jurisdictionbygeography=="Arvada Police Department"&incidentdate %in% c("2020", "2019"))


person <- arvada[1:32,]
property <- arvada[33:84,]
society <- arvada[85:110,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

arvada <- rbind(person, property, society)

arvada <- select(arvada, offensetypegen, offensetype, everything())
arvada$offensetype <- factor(arvada$offensetype, levels=unique(arvada$offensetype))

gen <- subset(arvada, offensetypegen==offensetype)

arvada <- spread(arvada, key="incidentdate", value="numberofcrimes")

arvada$percentchange <- round((arvada$`2020`/arvada$`2019`)-1, 2)

arvada$difference <- (arvada$`2020`-arvada$`2019`)


# ...............................................................

g <- ggplot(gen, aes(x=incidentdate, y=numberofcrimes, fill=offensetype)) +
  geom_bar(stat="identity", position="dodge", color="black")+ theme_minimal()+
  theme + scale_fill_manual(values=c("#003f5c", "#444e86","#955196"))+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Arvada Police Department Crimes by Offense Type, 2019 vs. 2020", 
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 16 March 2021") +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 0.9), size=3.5, vjust=-.45)+
  scale_y_continuous(labels = scales::comma)
g + 
  annotate("text", x=1.65, y=3000, label="violent crimes \ndecreased from 2019 \nto 2020 in Arvada", family="Source Sans Pro", fontface="italic", lineheight=1) +
  geom_segment(aes(x=1.65,xend=1.65, y=2400,yend=1100), size=0.25)+ 
  annotate("text", x=2.4, y=3000, label="social crimes \n(incl. drug crimes) \n decreased from 2019 \nto 2020 in Arvada", family="Source Sans Pro", fontface="italic", lineheight=1) +
  geom_segment(aes(x=2.4,xend=2.3, y=2400,yend=1100), size=0.25)+ 
  annotate("text", x=1.5, y=5000, label="slight increase in crime \nfrom 2019 to 2020 is \nentirely in property crime", family="Source Sans Pro", fontface="italic", lineheight=1) +
  geom_segment(aes(x=1.25,xend=1, y=5000,yend=5000), size=0.25)+
  geom_segment(aes(x=1.75,xend=2, y=5000,yend=5500), size=0.25)

# ...............................................................

arvada %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Arvada Police Department Change in Violent Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values given for justifiable homicide offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits=c(-30, 25))+ 
  theme()


arvada %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Property"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Property Crime Offense Type",
       title="Arvada Police Department Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma)


arvada %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Society"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Crime Offense Type",
       title="Arvada Police Department Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values for sports tampering offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits=c(-250, 200)) 

# ...............................................................


arvada <- read_csv("data/arvadaadamsjeff.csv", skip=3)[,1:4]
names(arvada) <- str_to_lower(gsub(" ", "", names(arvada)))

arvada <- subset(arvada, !jurisdictionbygeography=="Arvada Police Department"&incidentdate %in% c("2020", "2019"))

person <- arvada[1:64,]
property <- arvada[65:168,]
society <- arvada[169:228,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

arvada <- rbind(person, property, society)

arvada <- select(arvada, offensetypegen, offensetype, everything())
arvada$offensetype <- factor(arvada$offensetype, levels=unique(arvada$offensetype))
arvada <- spread(arvada, key="incidentdate", value="numberofcrimes")

arvada$percentchange <- round((arvada$`2020`/arvada$`2019`)-1, 2)

arvada$difference <- (arvada$`2020`-arvada$`2019`)




all3 <- arvada %>% 
  group_by(offensetypegen, offensetype) %>% 
  summarize(`2020`=sum(`2020`, na.rm = T), 
            `2019`=sum(`2019`, na.rm = T))

all3$percentchange <- round((all3$`2020`/all3$`2019`)-1, 2)

all3$difference <- (all3$`2020`-all3$`2019`)



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
  scale_y_continuous(labels = scales::comma)

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
  scale_y_continuous(labels = scales::comma)


all3 %>% 
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


