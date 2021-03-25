
ggplot(subset(change, estimatedpopulation>100000), aes(x=reorder(county, -estimatedpopulation), y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) +
  theme_minimal() + theme +
  scale_x_discrete(labels=wrap_format(10))  +
  labs(title="No association between change in jail population and change in crime",
       subtitle="Colorado Change in Jail Population and Number of Crimes by County, 2019 to 2020 \nfor 11 largest counties collectively representing 85% of Colorado's population", 
       x="", y="% Change", fill="",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nAnnotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.8, 0.6), breaks=seq(-0.8, 0.6, 0.2)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro", 
            position = position_dodge(width = 0.7), size=4, vjust="outward") +
  geom_hline(aes(yintercept=0)) + theme(plot.title=element_text(face="bold")) + 
  annotate("text", x=6.9, y=0.5, label="Larimer County and Douglas County had similar decreases \nin jail population but crime decreased by 13% in Larimer County \nand increased by 11% in Douglas County.", 
           size=3, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=6.5,xend=6, y=0.38,yend=0.025), size=0.25)+
  geom_segment(aes(x=6.8,xend=6.8, y=0.38,yend=0.17), size=0.25)+ theme(plot.title=element_text(face="bold")) + 
  annotate("text", x=6.5, y=-0.65, label="Jefferson County had the greatest decrease \nin jail population and saw no change in overall crime.", 
           size=3, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=5.1, xend=4.3, y=-0.62,yend=-0.58), size=0.25) 


ggplot(subset(change, estimatedpopulation>100000), aes(x=reorder(county, -estimatedpopulation), y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) +
  theme_minimal() + theme +
  scale_x_discrete(labels=wrap_format(10))  +
  labs(title="No association between change in jail population and change in crime",
       subtitle="Colorado Change in Jail Population and Number of Crimes by County, 2019 to 2020 \nfor 11 largest counties collectively representing 85% of Colorado's population", 
       x="", y="% Change", fill="",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nAnnotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.75, 0.8), breaks=seq(-0.6, 0.6, 0.2)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro", 
            position = position_dodge(width = 0.7), size=4, vjust="outward") +
  geom_hline(aes(yintercept=0)) + theme(plot.title=element_text(face="bold")) + 
  annotate("text", x=6.9, y=0.5, label="Larimer County and Douglas County had similar decreases \nin jail population but crime decreased by 13% in Larimer County \nand increased by 11% in Douglas County.", 
           size=3, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=6.5,xend=6, y=0.38,yend=0.025), size=0.25)+
  geom_segment(aes(x=6.8,xend=6.8, y=0.38,yend=0.17), size=0.25)+ theme(plot.title=element_text(face="bold")) + 
  annotate("text", x=6.5, y=-0.65, label="Jefferson County had the greatest decrease \nin jail population and saw no change in overall crime.", 
           size=3, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=5.1, xend=4.3, y=-0.62,yend=-0.58), size=0.25) + 
  theme(axis.ticks=element_line(), panel.grid=element_blank(),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.minor.y = element_line(colour = "white"),
        panel.background = element_rect(fill = NA),
        panel.ontop = TRUE) 

  

  
  