---
title: "Predictive_Modeling_Q2"
output: html_document
---
The following is an analysis of Austin Bergstrom International Airport. The data
has been grouped in a way that shows optimal times for flying in regards to 
month, day of week, time of day, and carrier in order to avoid arrival and/or
departure delays.

```{r, echo=TRUE}
library(mosaic)
library(tidyverse)
library(ggplot2)

data1 <-"/Users/trishaschutter/Desktop/MSBA/SUMMER/Predictive Modeling/Second Half/STA380-master/data/ABIA.csv"
ABIA = read.csv(data1)
#ABIA = read.csv('ABIA.csv')
head(ABIA)

# Delays vs. Month
plot(ABIA$Month, ABIA$ArrDelay, main = "Arrival Delays Occur Most in Dec. and March", xlab= "Month", ylab= "Arrival Delay")
```
This is showing the arrival delays per month. From this, it appears that most arrival delays occur in December and March.
```{r, echo=TRUE}

plot(ABIA$Month, ABIA$DepDelay, main = "Departure Delay Occur Most in March and August", xlab= "Month", ylab= "Departure Delay")
```
To compare arrival delays with depature delays, departure delays were aslo ran against months. Departure delays occur most in March and August. As March was present in both results, March seems to be full of delays of both kinds.
```{r, echo=TRUE}
# Delays vs. Day of Week
plot(ABIA$DayOfWeek, ABIA$ArrDelay, main = "Tuesday Has Lowest Arrival Delay", xlab= "Day of Week", ylab= "Arrival Delay")
plot(ABIA$DayOfWeek, ABIA$DepDelay, main = "Tuesday Has Lowest Departure Delay", xlab= "Day of Week", ylab= "Departure Delay")
```
These are a couple plots that represent what day is best to fly on in order to avoid delays. Tuesday was found as the best day to fly in order to avoid delays.

```{r, echo=TRUE}

# Departure Time vs. Delay
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepTime, y = DepDelay)) +
  ggtitle("Later Departure Flights Have Higher Delays")

# Arrival Time vs. Delay
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrTime, y = ArrDelay)) +
  ggtitle("      Arrival Delays Greatest At Night")
```
Now that month and day have been narrowed down, the next step was to narrow down a time to fly. From these plots, it appears that mornings are best to fly. However this does not mean morning as in midnight, but rather around 5AM. The increase around midnight is rollover from the high level of delays as it gets later in the night.

```{r, echo=TRUE}
################################################

# Departure Delay vs. Distance
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepDelay, y = Distance, color = Distance)) + 
  ggtitle("Departure Delay Greatest For Small and Large Distances")

# Arrival Delay vs. Distance
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrDelay, y = Distance, color = Distance)) + 
  ggtitle("Arrival Delay Greatest For Small and Large Distances")
```
Next, I wanted to see whether distance was a factor in delays. From these plots, we can see that delays are greatest both on the extremes. This makes sense as shorter flights don't have enough distance to make up for lost time and long distances tend to require more preparations.

```{r, echo=TRUE}
# Arrival Delay vs. Carrier
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrDelay, y = UniqueCarrier, color = UniqueCarrier)) + 
  ggtitle("Arrival Delay Greatest Among JetBlue and SouthWest")

# Departure Delay vs. Carrier
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepDelay, y = UniqueCarrier, color = UniqueCarrier)) + 
  ggtitle("Departure Delay Greatest Among JetBlue")
```
These plots are testing whether a specific carrier tends to have more delays than others. Both Jetblue and SouthWest tend to have the most arrival delays, while JetBlue also seems to have the greatest departure delays. To avoid delays, one might want to fly with someone other than JetBlue.

```{r, echo=TRUE}

###
# facets
###

#####################################################################
# Departure Delay vs. Day of Week vs. Month
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepDelay, y = DayOfWeek)) + 
  facet_wrap(~ Month, nrow = 2) +
  ggtitle("What Day to Fly, Per Month to Avoid Departure Delays")

# Arrival Delay vs. Day of Week vs. Month
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrDelay, y = DayOfWeek)) + 
  facet_wrap(~ Month, nrow = 2) +
  ggtitle("What Day to Fly, Per Month to Avoid Arrival Delays")
###################################################################33
```
In preparation for your next flight, you might want to know what a certain day looks like on the particular month you need to travel. These two plots will point you in the right direction, where you will first be able to look at the month that you are flying and then find the smallest line, indicating that is the day with the fewest delays during that month.
