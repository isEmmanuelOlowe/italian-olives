# importing the dataset of Olive Data
library("dslabs")
data(olive)

parameter <- "palmitoleic"
# parameter <- "palmitic"
north <- olive[olive$region == "Northern Italy", c("region", parameter)]
south <- olive[olive$region == "Southern Italy", c("region", parameter)]
sardinia <- olive[olive$region == "Sardinia", c("region", parameter)]

# Makes sure the data
res <- aov(palmitoleic ~ region, data = olive)