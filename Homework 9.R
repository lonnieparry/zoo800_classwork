####HOMEWORK 9####


##OBJECTIVE 1##

###A###
set.seed(123)
#define my constants
alpha <- 20      
beta  <- 8       
sigma <- 15
#get my data, making sure it is between -100 and 100
x <- runif(100, min = 0, max = 10)
#generate random errors
error <- rnorm(100, mean = 0, sd = sigma)
#generate y
y <- alpha + beta * x + error
range(y)
linearplot(x, y, main = "Simulated Linear Relationship",
     xlab = "x (Predictor)", ylab = "y (Response)",
     pch = 19, col = "blue")

###B###

library(ggplot2)
#we need new sigma values for this one
sigma_values <- c(1, 10, 25)
# We'll create one big data frame that includes sigma as a variable.
sigma_data <- do.call(rbind, lapply(sigma_values, function(sigma) {
x <- runif(100, 0, 10)                       
error2 <- rnorm(100, 0, sigma)               
y <- alpha + beta * x + error2               # linear model
data.frame(x = x, y = y, sigma = paste("σ =", sigma))
}))
#Moving on to create the plot
ggplot(sigma_data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ sigma, nrow = 1) +
  labs(
    title = "Effect of Increasing Observation Error (σ) on the Relationship Between y and x",
    x = "Predictor (x)",
    y = "Response (y)"
  ) +
  theme_minimal(base_size = 14)


#### OBJECTIVE 2####

###A###
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(12345)
probabilities <- c(0.55, 0.6, 0.65)
n_range <- 1:20
n_sims <- 5000             # number of simulated experiments per (n,p) (reduce/increase as desired)
alpha_level <- 0.05
