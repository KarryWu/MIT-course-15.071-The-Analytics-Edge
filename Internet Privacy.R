poll <- read.csv("AnonymityPoll.csv")

##look at the breakdown of the number of people with smartphones
table(poll$Smartphone)
summary(poll$Smartphone)

##Which of the following are states in the Midwest census region? (Select all that apply.)
table(poll$State,poll$Region)

##How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Internet.Use == 0, poll$Smartphone == 0)

##How many interviewees have a missing value for their Internet use?
sum(is.na(poll$Internet.Use))

##Use the subset function to obtain a data frame called "limited", which is limited to interviewees who reported Internet use or who reported smartphone use.
limited <- subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1)

##How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet)

##What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet? 
table(limited$Worry.About.Info)

##Build a histogram of the age of interviewees. What is the best represented age group in the population?
hist(limited$Age)

##What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable? 
max(table(limited$Age,limited$Info.On.Internet))

##What is the average Info.On.Internet value for smartphone users?
tapply(limited$Info.On.Internet,limited$Smartphone,mean)

##What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
tapply(limited$Tried.Masking.Identity == 1, limited$Smartphone == 1, mean, na.rm = TRUE)

