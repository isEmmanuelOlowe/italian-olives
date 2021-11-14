# Runs the simiulation study on the proposed method
set.seed(11)

simulation <- function(method, simulations = 10000,
                mean=c(0, 3), sd=c(0.1, 0.26), samples = 1000) {

    # The probability of test correct rejects the null hypothesis if it is false
    power <- 0
    # The probability of incorrectly rejecting the null hypothesis if it is true
    size <- 0
    # The overall accuracy of the statistical test
    accuracy <- 0
    # Smallest difference in mean correct differentiated
    difference <- Inf
    for (i in 1:simulations) {
        # Determines if the samples will come from the sample distribution
        type <- sample(c(1, 2, 3), 1)
        # Simulation that represents the null hypothesis
        if (type == 1) {
            data_mean <- runif(1, min = mean[1], max = mean[2])
            data_sd <- runif(1, min = mean[1], max = mean[2])
            data <- rnorm(samples, mean = data_mean, sd = data_sd)
            if (method(data == FALSE) {
                accuracy <- accuracy + 1
            }
            else {
                size <- size + 1
            }
        # 1 of the samples is from a different distribution
        } else if (type == 2) {
            data_1_mean <- runif(1, min = mean[1], max = mean[2])
            data_2_mean <- runif(1, min = mean[1], max = mean[2])
            data_1_sd <- runif(1, min = mean[1], max = mean[2])
            data_2_sd <- runif(1, min = mean[1], max = mean[2])
            data_1 <- rnorm(samples * 0.7, mean = data_1_mean, sd = data_1_sd)
            data_2 <- rnorm(samples * 0.3, mean = data_2_mean, sd = data_2_sd)
            data <- c(data_1, data_2)
            if (method(data) == TRUE) {
                power <- power + 1
                accuracy <- accuracy + 1
                difference <- min(difference, data_1_mean - data_2_mean)
            }
        }
        else {
            data_1_mean <- runif(1, min = mean[1], max = mean[2])
            data_2_mean <- runif(1, min = mean[1], max = mean[2])
            data_3_mean <- runif(1, min = mean[1], max = mean[2])
            data_1_sd <- runif(1, min = mean[1], max = mean[2])
            data_2_sd <- runif(1, min = mean[1], max = mean[2])
            data_3_sd <- runif(1, min = mean[1], max = mean[2])
            data_1 <- rnorm(samples * 0.7, mean = data_1_mean, sd = data_1_sd)
            data_2 <- rnorm(samples * 0.3, mean = data_2_mean, sd = data_2_sd)
            data_3 <- rnorm(samples * 0.3, mean = data_3_mean, sd = data_3_sd)
            data <- c(data_1, data_2, data_3)
            if (method(data) == TRUE) {
                power <- power + 1
                accuracy <- accuracy + 1
                difference <- min(difference, data_1_mean - data_2_mean,
                                        data_1_mean - data_3_mean,
                                        data_2_mean - data_3_mean)
            }
        }
    }

    return(list(difference  = difference,
                power = power / simulations,
                size = size / simulations,
                accuracy = accuracy / simulations))
}