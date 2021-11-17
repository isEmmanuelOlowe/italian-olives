# Runs the simiulation study on the proposed method
set.seed(3)

# Runs the simulation study upon the data
# INPUT:
#   method: The statistical test the simulation is being ran on
#   simulations: the number of different simulations that will be ran on test
#   mean: the range in which to generate mean values in
#   sd: the range in which to generate sd values in
#   samples: the number of samples each simulation should generate
# OUTPUT:
#   $power:the power of test
#   $size: the size of the test
#   $accuracy: the accuracy of the test
simulation <- function(method, simulations = 5000,
            mean=c(0, 3), sd=c(0.1, 0.26), samples = 600, should_round = TRUE) {

    # The probability of test correctly rejects the null hypothesis if false
    power <- c(0, 0)
    # The probability of incorrectly rejecting the null hypothesis if it is true
    size <- c(0, 0)
    # The overall accuracy of the statistical test
    accuracy <- 0
    # Smallest difference in mean correct differentiated
    difference <- Inf
    for (i in 1:simulations) {
        # Determines if the samples will come from the sample distribution
        type <- sample(c(1, 2, 3), 1)
        # Simulation that represents the null hypothesis
        if (type == 1) {
            # Generates random mean for the distirbution
            data_mean <- runif(1, min = mean[1], max = mean[2])
            # Genereates random sd for the distribution
            data_sd <- runif(1, min = sd[1], max = sd[2])
            # Generates sample palmitoleics from a single normal distribution
            palmitoleic <- rnorm(samples, mean = data_mean, sd = data_sd)
            palmitoleic <- conditional_round(palmitoleic, 2, should_round)

            # Assigns them all groups
            north <- rep("Northern Italy", samples * 0.35)
            south <- rep("Southern Italy", samples * 0.45)
            sardinia <- rep("Sardinia", samples * 0.20)

            # Creates the data frame
            region <- c(north, south, sardinia)
            data <- data.frame(region, palmitoleic)
            # Increments the number of True Null Hypotheses
            size[1] <- size[1] + 1

            if (method(data) == FALSE) {
                accuracy <- accuracy + 1
            }
            else {
                size[2] <- size[2] + 1
            }
        # 1 of the regions is from a different distribution
        } else if (type == 2) {
            # Generates 2 means for the distributions
            data_1_mean <- runif(1, min = mean[1], max = mean[2])
            data_2_mean <- runif(1, min = mean[1], max = mean[2])
            # Generates 2 standard deviations for the distributions
            data_1_sd <- runif(1, min = sd[1], max = sd[2])
            data_2_sd <- runif(1, min = sd[1], max = sd[2])
            # Creates the sample data from 2 seperate normal distributions
            data_1 <- rnorm(samples * 0.7, mean = data_1_mean, sd = data_1_sd)
            data_2 <- rnorm(samples * 0.3, mean = data_2_mean, sd = data_2_sd)
            palmitoleic <- c(data_1, data_2)
            palmitoleic <- conditional_round(palmitoleic, 2, should_round)

            # Generates the labels of the data
            # North is for the data_2
            north <- rep("Northern Italy", samples * 0.3)
            south <- rep("Southern Italy", samples * 0.45)
            sardinia <- rep("Sardinia", samples * 0.25)

            # Generates the dataframe
            region <- c(south, sardinia, north)
            data <- data.frame(region, palmitoleic)
            # Increments the number of false null hypotheses
            power[1] <- power[1] + 1
            if (method(data) == TRUE) {
                power[2] <- power[2] + 1
                accuracy <- accuracy + 1
                difference <- min(difference, abs(data_1_mean - data_2_mean))
            }
        }
        # All the regions have different distributions
        else {
            # Generates the 3 means for the distributions
            data_1_mean <- runif(1, min = mean[1], max = mean[2])
            data_2_mean <- runif(1, min = mean[1], max = mean[2])
            data_3_mean <- runif(1, min = mean[1], max = mean[2])
            # Generates 3 standard deviatiosn for the distributions
            data_1_sd <- runif(1, min = sd[1], max = sd[2])
            data_2_sd <- runif(1, min = sd[1], max = sd[2])
            data_3_sd <- runif(1, min = sd[1], max = sd[2])
            # Generates the samples from 3 normal distributions
            data_1 <- rnorm(samples * 0.4, mean = data_1_mean, sd = data_1_sd)
            data_2 <- rnorm(samples * 0.35, mean = data_2_mean, sd = data_2_sd)
            data_3 <- rnorm(samples * 0.25, mean = data_3_mean, sd = data_3_sd)

            # Generates labels for the data
            north <- rep("Northern Italy", samples * 0.35)
            south <- rep("Southern Italy", samples * 0.4)
            sardinia <- rep("Sardinia", samples * 0.25)

            region <- c(south, north, sardinia)
            palmitoleic <- c(data_1, data_2, data_3)
            palmitoleic <- conditional_round(palmitoleic, 2, should_round)

            # Generates data frame for the simulation
            data <- data.frame(region, palmitoleic)
            # Increments the number of false null hypotheses
            power[1] <- power[1] + 1

            if (method(data) == TRUE) {
                power[2] <- power[2] + 1
                accuracy <- accuracy + 1
                difference <- min(difference, abs(data_1_mean - data_2_mean),
                                        abs(data_1_mean - data_3_mean),
                                        abs(data_2_mean - data_3_mean))
            }
        }
    }

    return(list(difference  = difference,
                power = power[2] / power[1],
                size = size[2] / size[1],
                accuracy = accuracy / simulations))
}

# Prints the results of the simulation study
# INPUT:
#  test: the staticcal test being tested
#  results: output of simulation
print_results <- function(test, results) {
    print(paste0("RESULTS OF ", test, " SIMULATION STUDY:"))
    print(results)
}


# Determines if rounding should occur
# INPUT:
#   x: the object being rounded
#   digits: the number of digits that should be rounded to
# OUTPUT:
#   x rounded if rounded is set to true else just returns x
conditional_round <- function(x, digits = 2, should_round = TRUE) {
    if (should_round) {
        return(round(x, digits = digits))
    }
    else {
        return(x)
    }
}
