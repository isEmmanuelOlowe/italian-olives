# Runs all the componenets of the project
library("dslabs")
data(olive)

parameter <- "palmitoleic"
# parameter <- "palmitic"
north <- olive[olive$region == "Northern Italy", c("region", parameter)]
south <- olive[olive$region == "Southern Italy", c("region", parameter)]
sardinia <- olive[olive$region == "Sardinia", c("region", parameter)]

# Makes sure the data
# res <- aov(palmitoleic ~ region, data = olive)
summary(north)
summary(south)
summary(sardinia)

boxplot(palmitoleic ~ region, data = olive, main = "Olive Region Data",
   xlab = "Region", ylab = parameter, col = c("red", "green", "blue"))

boxplot(olive$palmitoleic, add = TRUE)