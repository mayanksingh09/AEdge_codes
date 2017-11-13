Sys.setlocale("LC_ALL", "C")

summary(mtcars)
WHO <- read.csv("./data/WHO.csv")
WHO_Europe <- subset(WHO, Region == "Europe")
plot(WHO$GNI, WHO$FertilityRate)
outlier <- subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(outlier)
outlier[c("Country", "GNI", "FertilityRate")]
WHO$Country[which.min(WHO$Over60)]
WHO$Country[which.max(WHO$LiteracyRate)]


hist(WHO$CellularSubscribers) #histogram, useful for understanding distribution of variable

boxplot(WHO$LifeExpectancy ~ WHO$Region) #Box plot useful for understanding statistical range of a variable 

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region")

##Summary tables

table(WHO$Region)

tapply(WHO$Over60, WHO$Region, mean) #splits observations by region and computes means of Over60 variable

tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = T) #remove NAs

tapply(WHO$ChildMortality, WHO$Region, mean)


#USDA Data

USDA <- read.csv("./data/USDA.csv")

str(USDA) #structure of the DF
summary(USDA)

USDA$Description[which.max(USDA$Sodium)]

HighSodium <- subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description

match("CAVIAR", USDA$Description) #match the description CAVIAR in description

USDA$Sodium[match("CAVIAR", USDA$Description)]

summary(USDA$Sodium)
sd(USDA$Sodium, na.rm = T) #remove the NAs

#Plotting USDA

plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red") #scatter plot

hist(USDA$VitaminC, xlab = "Vitamin C(mg)", main = "Histogram of Vitamin C Levels", xlim = c(0,100), breaks = 2000) #breaks to divide the interval, xlim to limit the values

boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels")


USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = T))
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = T))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = T))
USDA$HighCarbohydrate <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = T))

str(USDA)

table(USDA$HighSodium, USDA$HighFat) #to compare two variables

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = T) #group arg1 by arg2 and apply arg3

tapply(USDA$VitaminC, USDA$HighCarbohydrate, max, na.rm = T)

tapply(USDA$VitaminC, USDA$HighCarbohydrate, summary, na.rm = T)

#Assignment 1

mvt <- read.csv("./data/mvtWeek1.csv")

str(mvt)

max(mvt$ID)

min(mvt$Beat)

sum(mvt$Arrest) #Ans 1.5

table(mvt$LocationDescription) #Ans 1.6

summary(mvt)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert) #Ans 2.2

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

which.min(table(mvt$Month)) #Ans 2.3

which.max(table(mvt$Weekday)) #Ans 2.4

table(mvt$Month)
table(mvt$Month, mvt$Arrest) #Ans 2.5 (Jan)

hist(mvt$Date, breaks=100)

boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Year, mvt$Arrest)

2152/(2152+18517) #Ans 3.3

1212/(1212 + 13068) #Ans 3.4

550/(550 + 13542)
table(mvt$Year)

sort(table(mvt$LocationDescription))

library(dplyr)

Top5 <- subset(mvt, LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL"))

Top5$LocationDescription <- factor(Top5$LocationDescription)

str(Top5)

table(Top5$LocationDescription, Top5$Arrest)

table(Top5$Weekday, Top5$LocationDescription)

#Assignment 2

IBM <- read.csv("./data/IBMStock.csv")
GE <- read.csv("./data/GEStock.csv")
ProcterGamble <- read.csv("./data/ProcterGambleStock.csv")
CocaCola <- read.csv("./data/CocaColaStock.csv")
Boeing <- read.csv("./data/BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(IBM) #Ans 1.1
summary(IBM$StockPrice) #Ans 1.4
summary(GE$StockPrice) #Ans 1.5
summary(CocaCola$StockPrice) #Ans 1.6
summary(Boeing)
sd(ProcterGamble$StockPrice) #Ans 1.8

plot(CocaCola$Date, CocaCola$StockPrice, type = "l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("1983-01-01")), lwd=2)
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
plot(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="black")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="black")
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-12-01")), lwd=2)

sort(tapply(IBM$StockPrice,months(IBM$Date), mean, na.rm = T)) #Ans 4.1

mean(IBM$StockPrice)

sort(tapply(Boeing$StockPrice,months(Boeing$Date), mean, na.rm = T))

sort(tapply(GE$StockPrice,months(GE$Date), mean, na.rm = T))
sort(tapply(CocaCola$StockPrice,months(CocaCola$Date), mean, na.rm = T))


#Assignment 3

CPS <- read.csv("./data/CPSData.csv")

summary(CPS)
str(CPS)

nrow(CPS) #Ans 1.1

sort(table(CPS$Industry)) #Ans 1.2

sort(table(CPS$State)) #Ans 1.3

table(CPS$Citizenship) #Ans 1.4
(116639 + 7073)/(116639 + 7073 + 7590) 

#table(CPS$Region, is.na(CPS$MetroAreaCode))[,2]

table(CPS$Race, CPS$Hispanic) #Ans 1.5

summary(CPS) #Ans 2.1

table(CPS$Age, is.na(CPS$Married)) #Ans 2.2

table(CPS$State, is.na(CPS$MetroAreaCode)) #Ans 2.3

table(CPS$Region, is.na(CPS$MetroAreaCode)) #Ans 2.4

10674/(20010+10674)
8084/(25093+8084)

sort(round(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean, na.rm = T),2)) #Ans 2.5

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean, na.rm = T)) #Ans 2.6

MetroAreaMap <- read.csv("./data/MetroAreaCodes.csv")
CountryMap <- read.csv("./data/CountryCodes.csv")

nrow(MetroAreaMap)
nrow(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE) #merge the datasets to map the values

summary(CPS)

sort(table(CPS$MetroArea)) #Ans 3.3

sort(tapply((CPS$Hispanic == 1), CPS$MetroArea, mean, na.rm = T)) #Ans 3.4

sort(tapply((CPS$Race == "Asian"), CPS$MetroArea, mean, na.rm = T)) #Ans 3.5

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = T)) #Ans 3.6


#map country of origin
CountryMap$Code
CPS$CountryOfBirthCode

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE) #merge the datasets to map the values

summary(CPS) #Ans 4.1

sort(table(CPS$Country)) #Ans 4.2

table((CPS$Country != "United States"), (CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")) #Ans 4.3
1668/(1668+3736)

sort(table(CPS$MetroArea, (CPS$Country == "India"))[,2]) #Ans 4.4

sort(table(CPS$MetroArea, (CPS$Country == "Brazil"))[,2])

sort(table(CPS$MetroArea, (CPS$Country == "Somalia"))[,2])
