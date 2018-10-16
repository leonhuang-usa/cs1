##How many breweries are present in each state?

library('plyr')
library('ggplot2')
library('dplyr')
library('tidyr')
library('reshape2')

Brew <- read.csv(file="Breweries.csv", header=TRUE, sep=",")  
Beers <- read.csv(file="Beers.csv", header=TRUE, sep=",")  

data.frame(table(Brew$State))
BPS <- data.frame(table(Brew$State)) #Breweries Per State

##Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.

head(Beers)
head(Brew)
BBeers <- rename(Beers, Brew_ID=Brewery_id, Beer_name=Name) #change headers to prepare for merge
head(BBeers)
FullData<-merge(BBeers, Brew)
FullData
head(FullData, 6)
tail(FullData, 6)
#Report the number of NA's in each column.

NAs<-colSums(is.na(FullData)) #sums the NA values in each column
NAs

#Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

medians<-aggregate(FullData[, 4:5], list(FullData$State), median, na.rm=TRUE)
head(medians)
names(medians)[1] <- "State"

a<-ggplot(medians, aes(State, ABV))
b<-ggplot(medians, aes(State, IBU))
a +geom_bar(stat="identity", aes(fill = ABV)) + ggtitle("Median Values of ABV")
b +geom_bar(stat="identity", aes(fill = IBU)) + ggtitle("Median Values of IBU")


#Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
max(FullData$ABV, na.rm = TRUE) #0.128
max(FullData$IBU, na.rm = TRUE) #138

maxstate<-subset(FullData, ABV == max(FullData$ABV, na.rm = TRUE) | IBU == max(FullData$IBU, na.rm = TRUE))
maxstate
#Colorado has the maximum alcoholic beer, and Oregon has the most bitter beer.

#Summary statistics for the ABV variable.
summary(FullData$ABV)
#Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.

sp <- ggplot(FullData, aes(x=ABV, y=IBU)) + geom_point()
sp + labs(title="Relationship Between Bitterness and Alcohol Content",
          x ="Alcohol by Volume", y = "International Bitterness Unit")
