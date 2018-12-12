library(ggplot2)
library(dplyr)
library(cowplot)

rm(list = ls())


setwd("/home/nicola/DSSC/Data_Visualization/assignment/repo")

data = read.csv('state_mean.csv')

data$State <- tolower(data$State)

data$Area <- NULL



head(data)

colnames(data)


data$Population <- as.numeric(data$Population)
data %>% select("State", "Population")


names(data)[ names(data) == "State"] <- "region"

data <- data[ data$region != "alaska", ]
data <- data[ data$region != "hawaii", ]

nrow(data)

names(data)[ names(data) == 'Violent.crime..rate.per.100.000' ] <- 'Violent_Crimes_Rate'
data$Violent_Crimes_Rate <- as.numeric(data$Violent_Crimes_Rate)

# data$Burglary..rate.per.100.000

names(data)[ names(data) == 'Robbery..rate.per.100.000' ] <- 'Robbery_Rate'
data$Robbery_Rate <- as.numeric(data$Robbery_Rate)

names(data)[ names(data) == 'Motor.vehicle.theft..rate.per.100.000' ] <- 'Vehicle_Theft_Rate'
data$Vehicle_Theft_Rate <- as.numeric(data$Vehicle_Theft_Rate)


names(data)[ names(data) == "High.Quality"] <- "High_Quality_Price"
names(data)[ names(data) == "Medium.Quality"] <- "Medium_Quality_Price"

as.numeric(data$HQ.Number)
plot(data$HQ.Number, data$MQ.Number)

data <- data %>% mutate( "HQ_St_Rate" = as.numeric(HQ.Number) / as.numeric(Population) * 100000)
data <- data %>% mutate( "MQ_St_Rate" = as.numeric(MQ.Number) / as.numeric(Population) * 100000)

data %>% select("State", "HQ_St_Rate")

summary(data)
colnames(data)


summary(data)


# '#00a000', '#006500', '#000c00'


summary(us)
us <- map_data("state")

total<-merge(us, data, y.by="region")
total$region<-factor(total$region)
total <- total[order(total$order),] # order the data [very important!]


centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)

centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]

centroids <- merge(data, centroids, by="region")

# NUMBER

cor(data$HQ_St_Rate, data$MQ_St_Rate)

gg1 <- ggplot(data, aes(map_id=region)) +
  geom_map(aes(fill = HQ_St_Rate), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat, fill=85) +
  scale_fill_gradient(low='white', high = 'green') 

gg1 <- gg1 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg1 <- gg1 + xlab("") + ylab("")  + ggtitle('Number of st for 100.000 inhabitants')

gg1 <- gg1 + with(centroids, annotate(geom="text", x=long, y=lat, label=abb, size=4, color="black", family="Times"))

gg1


gg2 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = MQ_St_Rate), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat, fill=85) +
  scale_fill_gradient(low='white', high = 'green') 

gg2 <- gg2 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg2 <- gg2 + xlab("") + ylab("") + ggtitle('Number of st for 100.000 inhabitants')

gg2

plot_grid(gg1, gg2)




# QUALITY
cor(data$High_Quality_Price, data$Medium_Quality_Price)

gg1 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = High_Quality_Price), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat, fill=400) +
  scale_fill_gradient(low='white', high = 'green') 

gg1 <- gg1 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg1 <- gg1 + xlab("") + ylab("")  + ggtitle('Medium Price for HQ weed')

gg1


gg2 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = HQ_St_Rate), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat) +
  scale_fill_gradient(low='white', high = 'green') 

gg2 <- gg2 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg2 <- gg2 + xlab("") + ylab("") + ggtitle('Medium Price for MQ weed')

gg2

plot_grid(gg1, gg2)





# NUMBER VS QUALITY



gg1 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = High_Quality_Price), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat) +
  scale_fill_gradient(low='white', high = 'green') 

gg1 <- gg1 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg1 <- gg1 + xlab("") + ylab("")  + ggtitle('Medium Price for HQ weed')

gg1


gg2 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = Medium_Quality_Price), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat, fill=400) +
  scale_fill_gradient(low='white', high = 'green') 

gg2 <- gg2 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg2 <- gg2 + xlab("") + ylab("") + ggtitle('Medium Price for MQ weed')

gg2

plot_grid(gg1, gg2)


cor(data$High_Quality_Price, as.numeric(data$HQ.Number))
cor(data$Medium_Quality_Price, as.numeric(data$MQ.Number))

cor(data$High_Quality_Price, data$HQ_St_Rate)





# quality vs absolute number

gg1 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = High_Quality_Price), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat) +
  scale_fill_gradient(low='white', high = 'green') 

gg1 <- gg1 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg1 <- gg1 + xlab("") + ylab("")  + ggtitle('Medium Price for HQ weed')

gg1




gg2 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = as.numeric(HQ.Number)), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat, fill=400) +
  scale_fill_gradient(low='white', high = 'green') 

gg2 <- gg2 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg2 <- gg2 + xlab("") + ylab("") + ggtitle('Medium Price for MQ weed')

gg2

plot_grid(gg1, gg2)



# Vehicle thefts

cor(data$High_Quality_Price, data$Vehicle_Theft_Rate)
cor(data$Medium_Quality_Price, data$Vehicle_Theft_Rate)

gg1 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = Vehicle_Theft_Rate), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat) +
  scale_fill_gradient(low='white', high = 'green') 


gg1 <- gg1 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg1 <- gg1 + xlab("") + ylab("")  + ggtitle('Vehicle Thefts for 100.000 inhabitants')

gg1




# violent
cor(data$Violent_Crimes_Rate, data$HQ_St_Rate)
cor(data$Violent_Crimes_Rate, data$High_Quality_Price)

gg1 <- ggplot(data, aes(map_id=State)) +
  geom_map(aes(fill = Violent_Crimes_Rate), map=us, color='black') + 
  expand_limits(x=us$long, y=us$lat) +
  scale_fill_gradient(low='white', high = 'green') 


gg1 <- gg1 + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                     axis.text.x = element_blank(), axis.text.y = element_blank())

gg1 <- gg1 + xlab("") + ylab("")  + ggtitle('Violent Crimes for 100.000 inhabitants')

gg1
