rm(list = ls())

setwd("C:\\Users\\Kevin\\Dropbox")

library(ggplot2)

family <- read.csv("Canadian Family Parliament Data.csv")

table(family$Chamber)

table(family$Family)

unique(family$Family)

table(family$Province)

table(family$Province, family$Terms)

sum(family$Terms)

table(family$Party)

table(family$Party, family$Province)

hist(family$First.Elected, breaks = 15)
hist(family$Last.Elected, breaks = 15)

table(family$Relation)

table(family$Note)
# 3 repeats for Commons, 3 for Senate

family$Years.Served <- family$Last.Elected-family$First.Elected
summary(family$Years.Served)
summary(family$Terms)

### Divide between House and Senate

commons <- family[family$Chamber == "Commons",]
c <- family[family$Chamber == "Commons ",]
commons <- rbind(commons, c)

senate <- family[family$Chamber == "Senate",]

summary(commons$Years.Served)
summary(senate$Years.Served)

summary(commons$Terms)
plot(commons$Terms, commons$Years.Served)

hist(commons$First.Elected, breaks = 15)
hist(commons$Last.Elected, breaks = 15)

table(commons$Province, commons$Years.Served)

table(commons$Province)

# Years served by relation
rel <- with(commons, tapply(Years.Served, Relation, summary))
rel

summary(commons$Years.Served[commons$Relation == "Grandfather"])
summary(commons$Years.Served[commons$Relation == "Father"])
summary(commons$Years.Served[commons$Relation == "Son"])
summary(commons$Years.Served[commons$Relation == "Grandson"])

summary(commons$Years.Served[commons$Relation == "Brother"])
summary(commons$Years.Served[commons$Relation == "Husband"])
summary(commons$Years.Served[commons$Relation == "Wife"])

### Timelines

fy <- ggplot(commons, aes(x = First.Elected)) + geom_histogram(binwidth = 10, color = "black", fill = "gray")
fy <- fy + ggtitle("Year First Elected to House of Commons") + 
  theme_light(base_size = 16) + 
  xlab("Year") + 
  ylab("Number")
fy

ly <- ggplot(commons, aes(x = Last.Elected)) + geom_histogram(binwidth = 10, color = "black", fill = "gray")
ly <- ly + ggtitle("Year Last Elected to House of Commons") + 
  theme_light(base_size = 16) + 
  xlab("Year") + 
  ylab("Number") +
  expand_limits(y = 45)
ly

### From Provinces
