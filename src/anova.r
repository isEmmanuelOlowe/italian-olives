# Performs are 1 way anova test on the data
# INPUT:
#   data: the dataset the test is being performed on
#   p_value: the value to rejected the NULL hypothesis if less than
# OUTPUT:
#   TRUE if the NULL Hypothesis is rejected
anova_test <- function(data, p_value = 0.05) {
    # Performs the test
    results <- aov(palmitoleic ~ region, data = data)
    # Extracts the P Value
    test_p_value <- summary(results)[[1]][["Pr(>F)"]][1]

    #  Determines if the NULL hypothesis has been rejected
    if (test_p_value < p_value) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}