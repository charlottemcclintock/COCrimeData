
# crime type by race by arrest type
arrests %>% subset(arrestoffense %in% c("Crimes Against Person", 
                                        "Crimes Against Property", 
                                        "Crimes Against Society")) %>% 
  ggplot(aes(x=arresttype, y=numberofarrestees, fill=arresteerace)) + 
  geom_bar(stat="identity", position="fill") + facet_wrap(~arrestoffense) + 
  scale_x_discrete(labels=wrap_format(10)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))

# crime type by race by arrest type
arrests %>% subset(arrestoffense %in% c("Crimes Against Person", 
                                        "Crimes Against Property", 
                                        "Crimes Against Society")) %>% 
  subset(!arresteerace %in% c("Missing", "Unknown") & !arrestoffense=="Missing") %>% 
  ggplot(aes(x=arresteerace, y=numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill") + facet_wrap(~arrestoffense) + 
  scale_x_discrete(labels=wrap_format(10)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))

# ........................................................................

# low level crimes

arb <- arrests %>% group_by(arrestoffense) %>% 
  summarise(sum(numberofarrestees, na.rm=T))

lowlevel <- c("Destruction / Damage / Vandalism of Property", "Shoplifting", 
              "Simple Assault", 'Drug / Narcotic Violations', "All Other Larceny", 
              "Drug Equipment Violations", "Theft From Building", "Driving Under the Influence", 
              "Disorderly Conduct", "Credit Card / Automated Teller Machine Fraud","Liquor Law Violations", 
              "Theft of Motor Vehicle Parts / Accessories", "Curfew / Loitering / Vagrancy Violations")

# arrest type by race for low level offenses
arrests %>% 
  subset(!arrestoffense %in% lowlevel & !arresttype=="Missing" & 
           !arresteerace %in% c("Missing", "Unknown")) %>% 
  group_by(arresttype, arresteerace)  %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm = T)) %>% 
  ggplot(aes(arresteerace, numberofarrestees, fill=arresttype)) +
  geom_bar(stat="identity", position="fill") + coord_flip()

# arrests for low level offenses
arrests %>% 
  subset(!arrestoffense %in% lowlevel & !arresttype=="Missing") %>% 
  group_by(arresttype, arresteerace)  %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm = T)) %>% 
  ggplot(aes(arresttype, numberofarrestees, fill=arresteerace)) +
  geom_bar(stat="identity", position="fill")

# drug arrests
arrests %>% subset(arrestoffense=="Drug/Narcotic Violations") %>% 
  ggplot(aes(arresteerace, numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill")

# ........................................................................

# how often is someone taken into custody vs. summons for each offense type?

# arrests by specific offense type and arrest type 
arrests %>% subset(!arrestoffense %in% c("Crimes Against Person", 
                                         "Crimes Against Property", 
                                         "Crimes Against Society") & 
                     !numberofarrestees=="0") %>% 
  group_by(arrestoffense, arresttype) %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm = T)) %>% 
  ggplot(aes(reorder(arrestoffense, numberofarrestees), numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill") + coord_flip() + 
  scale_x_discrete(labels=wrap_format(25)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8)) 

# 

arrests %>% subset(arrestoffense=="Crimes Against Person" & !arresttype=="Missing") %>%  
  ggplot(aes(x=arresttype, y=numberofarrestees, fill=arresteerace)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_x_discrete(labels=wrap_format(10)) + theme_minimal() +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))

arrests %>% subset(arrestoffense=="Crimes Against Person" & !arresttype=="Missing" & 
                     !numberofarrestees=="0") %>%  
  ggplot(aes(x=arresteerace, y=numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_x_discrete(labels=wrap_format(10)) + theme_minimal() +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))


