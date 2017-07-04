mvt <- read.csv("mvtWeek1.csv")
str(mvt)
max(mvt$ID)
min(mvt$Beat)
#number of true value in the Arrest variable
sum(as.numeric(mvt$Arrest))
#the number of observations have a LocationDescription value of ALLEY
nrow(subset(mvt, mvt$LocationDescription == "ALLEY"))
#first entry of date
mvt$Date[1]
##Date convert
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
##adding variables of Month and Weekday
mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)
##replace the old variable
mvt$Date <- DateConvert
##in which month did the fewest motor vehicle thefts occur
table(mvt$Month)
##on which weekday did the most motor vehicle thefts occur
table(mvt$Weekday)
##which month has the largest number of motor vehicle thefts for which an arrest was made?
table(subset(mvt, mvt$Arrest == TRUE)$Month)
##histogram plot
hist(mvt$Date,breaks = 100)
##box plot
boxplot(mvt$Date~mvt$Arrest)
##what proportion of motor vehicle thefts in 2001 was an arrest made
nrow(subset(mvt, mvt$Year == 2001 & mvt$Arrest == TRUE))/ nrow(mvt[which(mvt$Year == 2001),])
##find the location where the theft accours most often
sort(table(mvt$LocationDescription))
##create TOP 5 subset
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)
##refresh the data
Top5$LocationDescription <- factor(Top5$LocationDescription)
##One of the locations has a much higher arrest rate than the other locations. Which is it?
table(Top5$Arrest,Top5$LocationDescription)
##On which day of the week do the most motor vehicle thefts at gas stations happen?
Tsub <- subset(Top5,Top5$LocationDescription == "GAS STATION")
table(Tsub$Weekday)
table(Top5$LocationDescription, Top5$Weekday)
