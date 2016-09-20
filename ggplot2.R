install.packages("ggplot2")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggrepel")

library(stringr)
library(ggplot2)
library(dplyr)
library(ggrepel)

#setwd("D:/Data")
#housing <- read.csv("landprice.csv", header = T)
#http://datatoolkits.lincolninst.edu/subcenters/land-values/data/landdata-states-2016q1.xls

# read from online source - xls file 
install.packages("gdata")
require(gdata)
housing <- read.xls('http://datatoolkits.lincolninst.edu/subcenters/land-values/data/landdata-states-2016q1.xls'
                    , skip = 1)

housing$homeValue <- str_sub(housing$Home.Value, 2, length(housing$Home.Value))
housing$homeValue <- as.numeric(gsub(",", "", housing$homeValue))

# base - histogram
hist(housing$homeValue)

# ggplot2 - histogram
ggplot(housing, aes(x = homeValue)) +
  geom_histogram()


#base - colored scatter plot
plot(homeValue ~ Date, data = subset(housing, STATE == "MA"))
points(homeValue ~ Date, col="red", data = subset(housing, STATE == "TX"))
legend(19750, 400000, c("MA", "TX"), title="State", col = c("black", "red"), pch=c(1,1))

#ggplot - colored scatter plot
ggplot(subset(housing, STATE %in% c("MA","TX")),
       aes(x = Date,
           y = homeValue,
           color = STATE)) +
  geom_point()

# aes() - position, color, fill, shape, linetype, size

# geom - points, lines, boxplot
hp2001Q1 <- subset(housing, Date == "2001Q1")
p <- ggplot(hp2001Q1,
       aes(y= homeValue, x = Home.Price.Index)) +
  geom_point()

HV_HPI <- lm(homeValue ~ Home.Price.Index, data = hp2001Q1)
hp2001Q1$pred_price <- predict(HV_HPI)

p1 <- ggplot(hp2001Q1, aes(x = Home.Price.Index, y = homeValue))

# add regression line
p + geom_line(aes(y = pred_price))

# add smooth line
p1 + geom_point() + geom_smooth()
p + geom_smooth()
# use geom(smooth(method = "lm")) 
p + geom_smooth(method = "lm")

# scatter plot but using text
p1 + geom_text(aes(label = STATE), size = 3) + geom_line(aes(y=pred_price)) + geom_smooth()

# use both dot and label - ggrepel
install.packages("ggrepel")
p1 + geom_point() +
  geom_text_repel(aes(label = STATE))

# two dimentions plot
p1 <- ggplot(filter(housing, STATE %in% c("TX","NY")), aes(x = Date, y = homeValue))
p1 + geom_point(aes(color = STATE))

# dplyr - mutate() ifelse()
housing_mut <- mutate(filter(housing,STATE %in% c("NY","CT","AL","CA")), 
                      region = ifelse(STATE %in% c("NY","CT","DE","ME","MD","MA","NH","MJ","PA","RI","VT"), "NEA", 
                                      "OTEHR"))
p1 <- ggplot(housing_mut, aes(x= Date, y= homeValue))
p1+geom_point(aes(color = region, shape = STATE))

# statistics
# histogram
p2 <- ggplot(housing, aes(x= log(homeValue)))
p2 + geom_histogram(binwidth = 0.05)
