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

# aes() - position, color, fill, shape, linetype, size

# geom - points, lines, boxplot

hp2001Q1 <- subset(housing, Date == "2001Q1")
ggplot(hp2001Q1,
       aes(y= homeValue, x = Home.Price.Index)) +
  geom_point()

HV_HPI <- lm(homeValue ~ Home.Price.Index, data = hp2001Q1)
hp2001Q1$pred_price <- predict(HV_HPI)

p1 <- ggplot(hp2001Q1, aes(x = Home.Price.Index, y = homeValue))

# add regression line
p1 + geom_point(aes(color = "red")) +
  geom_line(aes(y = pred_price))

# add smooth line
p1 + geom_point() + geom_smooth()

# scatter plot but using text
p1 + geom_text(aes(label = STATE), size = 3) + geom_line(aes(y=pred_price)) + geom_smooth()
