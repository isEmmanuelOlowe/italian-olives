# Performs Kruskal-Wallis Rank Sum Test
# INPUT:
#   data: the dataset the test is being performed on
#   p_value: the value to rejected the NULL hypothesis if less than
# OUTPUT:
#   TRUE if the NULL hypothesis is rejected
kwrs <- function(data, p_value = 0.05) {
    # Performs the tests
    res <- kruskal.test(palmitoleic ~ region, data = data)

    # Determines if NULL Hypothesis is rejected
    if (res$p.value < p_value) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}