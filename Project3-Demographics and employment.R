CPS <- read.csv("CPSData.csv")

##Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.
table(CPS$Industry)

##Which state has the largest number of interviewees?
sort(table(CPS$State))

##What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)

##The CPS differentiates between race (with possible values American Indian, Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity. A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable. For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? (Select all that apply.)
table(CPS$Hispanic, CPS$Race)

##Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)

##How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
table(CPS$State,is.na(CPS$MetroAreaCode))

##Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
a <- tapply(is.na(CPS$MetroAreaCode),CPS$State,mean) 

##Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
a <- as.data.frame(a)
a[order(a,decreasing = TRUE),]

##read dictionary map
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

##Integrating Metropolitan Area Data
CPS <- merge(CPS, MetroAreaMap, by.x = "MetroAreaCode", by.y = "Code", all.x = TRUE)

##How many interviewees have a missing value for the new metropolitan area variable?
sum(is.na(CPS$MetroArea))

##Which of the following metropolitan areas has the largest number of interviewees?
b <- as.data.frame(table(CPS$MetroArea))
b[which.max(b$Freq),]

##Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 
sort(tapply(CPS$Hispanic,CPS$MetroArea, mean),decreasing = TRUE)

##determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian",CPS$MetroArea, mean))

##merge in the country of birth information from the CountryMap data frame, replacing the CPS data frame with the result
CPS <- merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)

##Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

##What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
newdata <- subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & CPS$Country != "United States" , na.rm = TRUE)
newdata1 <- subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm = TRUE)
nrow(newdata)/nrow(newdata1)
# table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

##Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
data2 <- subset(CPS, CPS$Country == "Brazil",na.rm = TRUE)
sort(table(data2$MetroArea),decreasing = TRUE)
#sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
#sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
#sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
