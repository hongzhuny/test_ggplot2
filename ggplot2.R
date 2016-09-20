setwd("D:/Data")
housing <- read.csv("landprice.csv", header = T)
housing$homeValue <- str_sub(housing$Home.Value, 2, length(housing$Home.Value))
housing$homeValue <- as.numeric(gsub(",", "", housing$homeValue))

# base - histogram
hist(housing$homeValue)

# ggplot2 - histogram
library(ggplot2)
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

