set.seed(5783)
student <- c(1:15)
entrance_exam <- c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
GPA <- c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96)


data <- data.frame(student, entrance_exam, GPA)

correlation <- cor(data$entrance_exam, data$GPA)

print(correlation)

library(ggplot2)
ggplot(data, aes(x=entrance_exam, y=GPA)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="red") +
  labs(title="Scatter plot of GPA versus Entrance Exam",
       x="Entrance Exam Score",
       y="First Year GPA",
       caption=sprintf("Correlation: %.2f", correlation)) +
  theme_minimal()




set.seed(5783)
n_perms <- 10000
permuted_correlations <- numeric(n_perms)


for (i in 1:n_perms) {
  
  permuted_GPA <- sample(data$GPA)
  
  
  permuted_correlations[i] <- cor(data$entrance_exam, permuted_GPA)
}

observed_correlation <- cor(data$entrance_exam, data$GPA)

p_value <- mean(permuted_correlations > observed_correlation)

print(p_value)





library(dplyr)
set.seed(5783)

rainfall_data <- read.csv("rainfall.csv")

rainfall_in_range <- rainfall_data %>%
  filter(Rainfall >= 2, Rainfall <= 3) %>%
  nrow()

total_rainfall <- nrow(rainfall_data)

empirical_frequency <- rainfall_in_range / total_rainfall

print(empirical_frequency)




alpha <- 4.26
beta <- 6.07

prob <- pgamma(3, shape=alpha, rate=beta) - pgamma(2, shape=alpha, rate=beta)

print(prob)




n <- 10000


samples <- rgamma(n, shape=alpha, rate=beta)
prob_estimate <- sum(samples >= 2 & samples <= 3) / n

print(prob_estimate)



# Y_theta is (a+b)/2
theta <- (2*alpha/beta - (2+3)/2)

samples <- rgamma(n, shape=alpha, rate=beta-theta)

# weights for importance sampling
weights <- dgamma(samples, shape=alpha, rate=beta) / dgamma(samples, shape=alpha, rate=beta-theta)

prob_estimate_B <- sum(weights * (samples >= 2 & samples <= 3)) / n

print(prob_estimate_B)



set.seed(5783)

arrival_times <- read.csv('arrival_times.csv')

hist(arrival_times$arrival_time, breaks=16, col='lightblue', border='black', main='Histogram of Student Arrival Times', xlab='Time (hours)', ylab='Number of Students')




Time <-8
N_T <- length(arrival_times)

lambda_hat <- N_T / Time

print(lambda_hat)


## use this function to simulate realizations of the poisson process
make_sample_df <- function(run, tmax, lambda)
{
  ## set the starting time 
  x <- 0
  
  ## while the cumulation of time is within the time T, 
  ## we keep generating new count
  while(sum(x) < tmax) x <- c(x, rexp(1, lambda))
  
  ## output is a dataframe with cumulated time points,
  ## number of counts and how many samples we want to generate
  data.frame(t = cumsum(x), N = seq_along(x), run = rep(run, length(x)))
}

tmax <- 8
lambda <- 5


df <- make_sample_df(run=1, tmax=tmax, lambda=lambda)

Tr <- max(df$t)

N_T <- max(df$N)

lambda_hat <- N_T / Tr

print(lambda_hat)



plot(df$t, df$N, type='s', main='Simulated Poisson Process', xlab='Time', ylab='Number of Arrivals')

points(Tr, N_T, col='red')





lambda0 <- 5
reps <- 10000
lambda_hat_obs <- N_T / Tr

df_sim <- do.call(rbind, lapply(1:reps, function(run) make_sample_df(run, tmax, lambda0)))


lambda_hat_sim <- with(df_sim, ave(N, run, FUN = function(x) length(x) / Tr))

test_stat_obs <- (lambda_hat_obs - lambda0)^2
test_stat_sim <- (lambda_hat_sim - lambda0)^2
p_value <- mean(test_stat_sim >= test_stat_obs)


print(p_value)




