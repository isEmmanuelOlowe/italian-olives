# Runs all the componenets of the project
source("src/simulation.r")
source("src/anova.r")
source("src/kwrs.r")
library("dslabs")
data(olive)

# Performs the simulation study
res1 <- simulation(anova_test)
res2 <- simulation(kwrs)

# Prints the results of simulation study
print_results("ONE WAY ANOVA", res1)
print_results("KRUSKAL-WALLIS RANK RUM")

# Prints the results of the tests on the dataset
print(paste0("ANOVA TEST RESULT: ", anova_test(olive)))
print(paste0("KRUSKAL-WALLIS RANK SUM TEST RESULT: ", kwrs(olive)))