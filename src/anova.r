
anova_test <- function(data) {
    results <- aov(palmitoleic ~ region, data = data)
    summary(res)
    summary(res)[[1]][["Pr(>F)"]][1]
}