# Unit 7 
# Visualization

WHO <- read.csv("./data/WHO.csv")
str(WHO)

plot(WHO$GNI, WHO$FertilityRate)

## GGPLOT

library(ggplot2)
scatterplot <- ggplot(WHO, aes(x = GNI, y = FertilityRate))

### Scatter plot
scatterplot + geom_point()

### Line graph
scatterplot + geom_line()

### Add color
scatterplot + geom_point(color = "blue", size = 3, shape = 17) # adding parameters

scatterplot + geom_point(color = "darkred", size = 3, shape = 8) # adding parameters

### Add title
fertilityGNIplot <- scatterplot + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility Rate vs. Gross National Income")

### Check the lecture footnotes for info related to shapes and colors

### Saving plots
pdf("Myplot.pdf")
print(fertilityGNIplot)
dev.off()


### Color by region
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

### categorical variables will have different colors, continuous will have color gradient

### Exploring data
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

#### Log transformation to find linear trend
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

### Linear Regression model predicting % under15

model <- lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model)

### Adding regression line to plot
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm") #by default 95% confidence interval

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99) #CI 99%

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = F) #Removing CI

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", color = "orange") #change color of line

### Question
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette="Dark2")


# PREDICTIVE POLICING

mvt <- read.csv("./data/mvt.csv", stringsAsFactors = F)
str(mvt)
mvt$Date <- strptime(mvt$Date, format = "%m/%d/%y %H:%M")

mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour

WeekdayCounts <- as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

library(ggplot2)
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))

WeekdayCounts$Var1 <- factor(WeekdayCounts$Var1, ordered = TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2, alpha = 0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

## Heat Map
DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts)
DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))

### One line each for each day of the week
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group= Var1, color = Var1), size = 2)


### Creating heatmap

DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Theft", low = "white", high = "red") +theme(axis.title.y = element_blank())


## MAP PLOTS

library(ggmap)
library(maps)

chicago <- get_map(location = "chicago", zoom = 11)

ggmap(chicago) #Geographical map of chicago

ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

### Crime counts dataframe

LatLonCounts <- as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude, 2)))

str(LatLonCounts)

LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))

LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")

### using geom_time for heatmap

ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill = "red")


## Quick Question

LatLonCounts2 <- subset(LatLonCounts, LatLonCounts$Freq > 0)

str(LatLonCounts2)

ggmap(chicago) + geom_point(data = LatLonCounts2, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")


## Heatmap on USA

murders <- read.csv("./data/murders.csv")
str(murders)

### Map of USA

statesMap <- map_data("state")
str(statesMap)


### Plot the map

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

murders$region <- tolower(murders$State)

### joining the two data frames

murderMap <- merge(statesMap, murders, by = "region")

str(murderMap)

### Plotting the murders 

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")


### Population check
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")


### Plotting murder rate

murderMap$MurderRate <- (murderMap$Murders/murderMap$Population*100000)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10)) # Limiting the values to 10


## Quick Question

### Map of Gunowners

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")


# Recitation 7

library(ggplot2)
intl <- read.csv("./data/intl.csv")


### Bar plot
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label = PercentOfIntl)) #stat identity uses value of y variable as is

### transform the data frame

intl <- transform(intl, Region = reorder(Region, -PercentOfIntl))
intl$PercentOfIntl <- intl$PercentOfIntl*100

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + 
    geom_bar(stat = "identity", fill = "dark blue") + 
    geom_text(aes(label = PercentOfIntl), vjust = -0.4) + 
    ylab("Percent of International Students") +
    theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

## Map with MIT data
library(ggmap)
intlall <- read.csv("./data/intlall.csv", stringsAsFactors = F)
head(intlall)

intlall[is.na(intlall)] = 0

world_map <- map_data("world")
str(world_map)

world_map <- merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator") #data reordered

world_map <- world_map[order(world_map$group, world_map$order),]

unique(intlall$Citizenship)
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] <- "China"

### Merge again
world_map <- merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)

world_map <- world_map[order(world_map$group, world_map$order),]
world_map$Total.x

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total.x), color = "black") + coord_map("mercator") #data reordered


ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total.x), color = "black") + coord_map("ortho", orientation = c(-37,175,0)) 


## Household data

households <- read.csv("./data/households.csv")

str(households)

# Year group fraction

library(reshape2)

households[,1:3]

### Changing wide data to long data
melt(households, id = "Year")


ggplot(melt(households, id = "Year"), aes(x = Year, y = value, color = variable)) + geom_line(size = 2) + geom_point(size = 5) + ylab("Percentage of Households")


### Assignment 1 - Election Forecast

library(ggmap)
library(ggplot2)
