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

