# importing the dataset of Olive Data
library("dslabs")
data(olive)

# Creates a JPEG of the density of the palmitoleic in all the groups
jpeg("results/plot.jpg")

parameter <- "palmitoleic"

north <- olive[olive$region == "Northern Italy", c("region", parameter)]
south <- olive[olive$region == "Southern Italy", c("region", parameter)]
sardinia <- olive[olive$region == "Sardinia", c("region", parameter)]


p1 <- hist(as.numeric(unlist(north[2])), seq(from = 0, to = 3, by = 0.2))
p2 <- hist(as.numeric(unlist(south[2])), seq(from = 0, to = 3, by = 0.2))
p3 <- hist(as.numeric(unlist(sardinia[2])), seq(from = 0, to = 3, by = 0.2))

plot(p1,
    col = rgb(1, 0, 0, 1 / 4), xlim = c(0, 3),
    ylim = c(0, 90), main = "Palmitoleic Density Plot", xlab = "palmitoleic"
)

plot(p2, col = rgb(0, 0, 1, 1 / 4), add = T)

plot(p3, col = rgb(0, 1, 0, 1 / 4), add = T)

legend("topleft", c("Northern Italy", "Southern Italy", "Sardinia"),
    fill = c("red", "blue", "green")
)

# Closes the file
dev.off()

# Generates the box plot of the regions
jpeg("results/boxplot.jpg")

boxplot(palmitoleic ~ region, data = olive, main = "Olive Region Data",
   xlab = "Region", ylab = parameter, col = c("red", "green", "blue"))

dev.off()


# Generates QQNorm plots for all the regions
jpeg("results/north_qq.jpg")
qqnorm(north$palmitoleic, main = "Northern Italy Normal Q-Q Plot")
qqline(north$palmitoleic)
dev.off()

jpeg("results/south_qq.jpg")
qqnorm(south$palmitoleic, main = "Southern Italy Normal Q-Q Plot")
qqline(south$palmitoleic)
dev.off()

jpeg("results/sardinia_qq.jpg")
qqnorm(sardinia$palmitoleic, main = "Sardinia Normal Q-Q Plot")
qqline(sardinia$palmitoleic)
dev.off()
