# Runs all the componenets of the project
source("src/simulation.r")
source("src/anova.r")
source("src/kwrs.r")
library("dslabs")
data(olive)

# Prints the results of the tests on the dataset
print("Running Statistical Tests on Dataset...")

# Runs the Simulation Study upon the dataset
print("Running Simulation Study...")
simulation_study <- function(should_round, p_value) {
    print(paste0("ROUNDING ENABLED: ", should_round))
    print(paste0("SIGNFICANCE LEVEL: ", p_value))
    # Performs the simulation study
    res1 <- simulation(anova_test(p_value), should_round = should_round)
    res2 <- simulation(kwrs(p_value), should_round = should_round)

    # Prints the results of simulation study
    print_results("ONE WAY ANOVA", res1)
    print_results("KRUSKAL-WALLIS RANK RUM", res2)
}

hypothesis_test <- function(p_value) {
    print(paste0("Hypothesis Test at ", p_value, " significance level"))
    print(paste0("ANOVA TEST RESULT: ", anova_test(p_value)(olive)))
    print(paste0("KRUSKAL-WALLIS RANK SUM TEST RESULT: ", kwrs(p_value)(olive)))
}

print("Running Hypotheis Tests...")
hypothesis_test(0.05)
hypothesis_test(0.01)
print("Running Simulation Study...")
simulation_study(TRUE, 0.05)
simulation_study(FALSE, 0.05)
simulation_study(TRUE, 0.01)
simulation_study(FALSE, 0.01)