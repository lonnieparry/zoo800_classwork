###HOMEWORK 6###


###OBJECTIVE 1###

library(tidyr)
library(ggplot2)
library(dplyr)

# Parameters
#intrinsic growth
r <- 0.2         
# carrying capacity
K <- 1000        
#int pop size
N0 <- 50        
#time
years <- 10      

#vector to store population sizes
N <- numeric(years + 1)
N[1] <- N0

#making a loop to calculate through time
for (t in 1:years) {
  N[t + 1] <- N[t] + r * N[t] * (1 - N[t] / K)
}

#finally the data frame for plotting
hellbender_data <- data.frame(
  Year = 0:years,
  Population = N
)

View (hellbender_data)

#moving on to plotting

ggplot(hellbender_data, aes(x = Year, y = Population)) +
  geom_line(color = "mediumblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Hellbender Population Growth (10-Year Simulation)",
    x = "Year",
    y = "Population Size"
  ) +
  theme_minimal(base_size = 14)

###OBJECTIVE 2###

set.seed(123)

# Parameters
r_mean <- 0.2
r_sd <- 0.03
K <- 1000
N0 <- 50
years <- 10
#numb simulations
n_sim <- 50  

# Create a matrix to store simulations
sim_matrix <- matrix(NA, nrow = years + 1, ncol = n_sim)
#this will set the initial population for all simulations
sim_matrix[1, ] <- N0  


# Loop over each simulation
for (sim in 1:n_sim) {
  # Draw a random r for this simulation
  r <- rnorm(1, mean = r_mean, sd = r_sd)
  
  # Loop through time
  for (t in 1:years) {
    sim_matrix[t + 1, sim] <- sim_matrix[t, sim] + r * sim_matrix[t, sim] * (1 - sim_matrix[t, sim] / K)
  }
}

# Convert to long-format data
sim_df <- as.data.frame(sim_matrix)
sim_df$Year <- 0:years
sim_df_long <- pivot_longer(sim_df, cols = -Year, names_to = "Simulation", values_to = "Population")

ggplot() +
  geom_line(data = sim_df_long, aes(x = Year, y = Population, group = Simulation),
            color = "skyblue", alpha = 0.4) +
  geom_line(data = hellbender_data, aes(x = Year, y = Population),
            color = "darkgreen", size = 1.5) +
  geom_point(data = hellbender_data, aes(x = Year, y = Population),
             color = "darkgreen", size = 2) +
  labs(
    title = "Hellbender Population Growth: Deterministic vs Stochastic Simulations",
    x = "Year",
    y = "Population Size"
  ) +
  theme_minimal(base_size = 14)
