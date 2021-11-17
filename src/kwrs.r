# Generates Kruskal-Wallis Rank Sum Test at certain significance level.
# INPUT:
#   p_value: the significance level
# OUTPUT:
#   function which computes Kruskal-Wallis Rank Sum Test
kwrs <- function(p_value) {
    # Performs Kruskal-Wallis Rank Sum Test
    # INPUT:
    #   data: the dataset the test is being performed on
    #   p_value: the value to rejected the NULL hypothesis if less than
    # OUTPUT:
    #   TRUE if the NULL hypothesis is rejected
    kwrs <- function(data) {
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
}