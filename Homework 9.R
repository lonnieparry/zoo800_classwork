####HOMEWORK 9####


##OBJECTIVE 1##

###A###
set.seed(123)

# define constants
alpha <- 20      
beta  <- 8       
sigma <- 15

# get predictor variable (x)
x <- runif(100, min = 0, max = 10)

# generate random error
error <- rnorm(100, mean = 0, sd = sigma)

# generate response variable (y)
y <- alpha + beta * x + error

# check range of y
range(y)

# scatterplot of the simulated relationship
plot(x, y,
     main = "Simulated Linear Relationship",
     xlab = "x (Predictor)",
     ylab = "y (Response)",
     pch = 19, col = "blue")
abline(lm(y ~ x), col = "red", lwd = 2)  # adds regression line

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

###A and B are kind of combined.
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(12345)
probabilities <- c(0.55, 0.6, 0.65)
n_range <- 1:20
n_sims <- 5000             # number of simulated experiments per (n,p) (reduce/increase as desired)
alpha_level <- 0.05

# Precompute one-sided exact p-value lookup tables for each n (p0 = 0.5)
one_sided_pvals_table <- function(n, p0 = 0.5) {
  ks <- 0:n
  tail_probs <- sapply(ks, function(k) {
    if (k == 0) {
      1.0
    } else {
      1 - pbinom(k - 1, size = n, prob = p0)
    }
  })
  # named vector indexed by k (0..n)
  names(tail_probs) <- as.character(0:n)
  return(tail_probs)
}

# Run simulations
results_list <- list()
for (ptrue in probabilities) {
  for (n in n_range) {
    # simulate counts of heads for n_sims experiments
    counts <- rbinom(n_sims, size = n, prob = ptrue)
    ptab <- one_sided_pvals_table(n, p0 = 0.5)
    pvals <- ptab[as.character(counts)]
    prop_sig <- mean(pvals < alpha_level)           
    out_of_100 <- prop_sig * 100                    
    results_list[[length(results_list) + 1]] <- data.frame(
      ptrue = ptrue,
      n = n,
      prop_sig = prop_sig,
      out_of_100 = out_of_100
    )
  }
}
df <- bind_rows(results_list)

# Plot with ggplot
df_plot <- df %>% mutate(ptrue_f = factor(ptrue, levels = probabilities, labels = paste0("p = ", probabilities)))

ggplot(df_plot, aes(x = n, y = out_of_100, color = ptrue_f)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = n_range) +
  labs(
    x = "Number of flips (n)",
    y = "Expected # of significant detections out of 100",
    color = "True p",
    title = "Power to detect Don Corleone's loaded coin (one-sided test: H0: p=0.5 vs Ha: p>0.5)",
    subtitle = paste0("Simulated (n_sims = ", n_sims, ") — alpha = ", alpha_level)
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

