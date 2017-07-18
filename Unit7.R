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


