# importing the dataset of Olive Data
library("dslabs")
data(olive)

# Creates a JPEG of the density of the palitoleic
jpeg("results/plot.jpg")

parameter <- "palmitoleic"

north <- olive[olive$region == "Northern Italy", c("region", parameter)]
south <- olive[olive$region == "Southern Italy", c("region", parameter)]
sardinia <- olive[olive$region == "Sardinia", c("region", parameter)]


p1 <- hist(as.numeric(unlist(north[2])), seq(from = 0, to = 3, by = 0.2))
p2 <- hist(as.numeric(unlist(south[2])), seq(from = 0, to = 3, by = 0.2))
p3 <- hist(as.numeric(unlist(sardinia[2])), seq(from = 0, to = 3, by = 0.2))

plot(p1,
    col = rgb(0, 0, 1, 1 / 4), xlim = c(0, 3),
    ylim = c(0, 90), main = "Plot of palmitoleic", xlab = "palmitoleic"
)

plot(p2, col = rgb(1, 0, 0, 1 / 4), add = T)

plot(p3, col = rgb(0, 1, 0, 1 / 4), add = T)

legend("topleft", c("Northern Italy", "Southern Italy", "Sardinia"),
    fill = c("red", "blue", "green")
)

# Closes the file
dev.off()