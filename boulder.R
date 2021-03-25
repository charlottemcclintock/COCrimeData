
boulder <- read_csv("data/boulderpolicedept-specific.csv", skip = 3)[,1:4]
cu <- read_csv("data/cupolicedept-specific.csv", skip = 3)[,1:4]

names(boulder) <- str_to_lower(gsub(" ", "", names(boulder)))
names(cu) <- str_to_lower(gsub(" ", "", names(cu)))

boulder <- subset(boulder, incidentdate %in% c("2020", "2019"))

person <- boulder[1:32,]
property <- boulder[33:84,]
society <- boulder[85:110,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

boulder <- rbind(person, property, society)

person <- cu[1:32,]
property <- cu[33:84,]
society <- cu[85:110,]

person$offensetypegen <- "Crimes Against Person"
property$offensetypegen <- "Crimes Against Property"
society$offensetypegen <- "Crimes Against Society"

cu <- rbind(person, property, society)

boulder <- select(boulder, offensetypegen, offensetype, everything())
boulder$offensetype <- factor(boulder$offensetype, levels=unique(boulder$offensetype))
boulder <- spread(boulder, key="incidentdate", value="numberofcrimes")

boulder$jurisdictionbygeography <- "Boulder Police Department"

cu <- select(cu, offensetypegen, offensetype, everything())
cu$offensetype <- factor(cu$offensetype, levels=unique(cu$offensetype))
cu <- spread(cu, key="incidentdate", value="numberofcrimes")

tot <- full_join(boulder, cu, by=names(cu))

boulder$percentchange <- round((boulder$`2020`/boulder$`2019`)-1, 2)
boulder$difference <- (boulder$`2020`-boulder$`2019`)

boulder %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Boulder Police Department Change in Violent Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values given for justifiable homicide or human trafficking offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma) 

boulder %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Property"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Property Crime Offense Type",
       title="Boulder Police Department Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma) 

boulder %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Society"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Crime Offense Type",
       title="Boulder Police Department Change in Crimes Against Society by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values for sports tampering offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits=c(-425, 200)) 

tot <- tot %>% 
  group_by(offensetypegen, offensetype) %>% 
  summarize(`2019`=sum(`2019`, na.rm=T), 
            `2020`=sum(`2020`, na.rm=T))

tot <- subset(tot, !(`2019`=="0"&`2020`=="0"))


tot$difference <- (tot$`2020`-tot$`2019`)

tot %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Person"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Violent Crime Offense Type",
       title="Boulder Police Department Change in Violent Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values given for justifiable homicide or human trafficking offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma) 

tot %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Property"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Property Crime Offense Type",
       title="Boulder Police Department Change in Property Crime by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma) 

tot %>% 
  subset(!offensetypegen==offensetype & offensetypegen=="Crimes Against Society"&!is.na(difference)) %>% 
  mutate(dir=ifelse(difference>0, "increase", "decrease")) %>% 
  ggplot(aes(x=reorder(offensetype, difference), y=difference,
             fill=dir)) +
  geom_bar(stat="identity")  + coord_flip()+
  scale_x_discrete(labels=wrap_format(25)) +
  theme_minimal() + theme + guides(fill=F) +
  labs(y="Change from 2019 to 2020", x="Crime Offense Type",
       title="Boulder Police Department Change in Crimes Against Society by Offense Type, 2019 to 2020",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nNo values for sports tampering offense type.") +
  geom_text(aes(label=prettyNum(difference, big.mark = ",")), 
            family="Source Sans Pro", size=3, hjust=1.2)+
  scale_y_continuous(labels = scales::comma, limits=c(-425, 200)) 

tot$perc <- (tot$`2020`/tot$`2019`)-1

tot <- subset(tot, !offensetypegen==offensetype)
boulder <- subset(boulder, !offensetypegen==offensetype)

boulder %>% group_by(offensetypegen) %>% summarize(difference=sum(difference, na.rm=T), 
                                                   sum=sum(`2019`, na.rm = T))
tot %>% group_by(offensetypegen) %>% summarize(difference=sum(difference, na.rm=T), 
                                               sum=sum(`2019`, na.rm = T))



# .............................................................................



# .............................................................................
