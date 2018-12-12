library(ggplot2)
library(dplyr)

setwd("//home/nicola/DSSC/Data_Visualization/assignment/repo")

data = read.csv('laws.csv')


colors = rev(c('#FEE5D9', '#FCAE91', '#FB6A4A', '#DE2D26', '#A50F15'))

data$State <- tolower(data$State)

lev = c("Fully Illegal", "Medicinal", "Decriminalized", "Medicinal and Decriminalized", "Fully Legal")
data$Legalization <- factor(data$cat, levels=lev, ordered = TRUE)


us <- map_data("state")
us <- fortify(us, region="region")



gg <- ggplot() 

gg <- gg + geom_map(data=us, map=us, aes(x=long, y=lat, map_id=region, group=group), 
                    fill="#ffffff", color="#7f7f7f", size=0.25 ) 

gg <- gg + geom_map(data=data, map=us, aes(fill=Legalization, map_id=State), 
                    color="#7f7f7f", size=0.25)

gg <- gg + scale_fill_manual(values=colors) + xlab("") + ylab("") + theme(legend.position = "bottom")# + title("Legalization Level in 2018")


gg <- gg + ggtitle("Legalization Level in 2018")

gg
