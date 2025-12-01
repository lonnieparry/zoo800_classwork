#####HOMEWORK WEEK 13#####
   ###LONNIE PARRY####


###OBJECTIVE 1###
dragons<-read.csv("dragon_data.csv")
y<-dragons$acres_on_fire
x<-cbind(1, dragons$size)
b <- solve(t(x) %*% x) %*% (t(x) %*% y)
b

###OBJECTIVE 2###

##PART A##
x<- dragons$size
y<- dragons$acres_on_fire
n <- length(y)
sse <- function(b0, b1) {
  sum((y - (b0 + b1*x))^2)}

# Define search grid
b0_vals <- seq(-10, 10, by = 0.1)    
b1_vals <- seq(-10, 10, by = 0.1)

# Create grid 
grid <- expand.grid(b0 = b0_vals, b1 = b1_vals)

# Compute SSE 
grid$sse <- mapply(sse, grid$b0, grid$b1)

# Find minimum SSE
best <- grid[which.min(grid$sse), ]
best

##PART B##
sse_optim <- function(par) {
  b0 <- par[1]
  b1 <- par[2]
  sum((y - (b0 + b1*x))^2)}


ols_optim <- optim(
  par = c(mean(y), 0),     
  fn = sse_optim)

ols_optim$par    
ols_optim$convergence

##PART C##
ols_optim$convergence
starts <- list(c(0,0), c(10,-10), c(-5,5), c(-15,15), c(-20,20))

results <- lapply(starts, function(s) {
  optim(par = s, fn = sse_optim)$par
})

names(results) <- paste0("start_", seq_along(starts))
results

###OBJECTIVE 3###

##PART A##

negloglik <- function(b0, b1, sigma) {
  mu <- b0 + b1*x
  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))}

b0_vals <- seq(-10, 10, by = 0.1)
b1_vals <- seq(-10, 10, by = 0.1)
sigma_vals <- seq(0.1, 10, by = 0.1)

grid <- expand.grid(b0 = b0_vals, b1 = b1_vals, sigma = sigma_vals)

grid$nll <- mapply(negloglik, 
                   b0 = grid$b0, 
                   b1 = grid$b1, 
                   sigma = grid$sigma)

best <- grid[which.min(grid$nll), ]
best

##PART B##

nll_optim <- function(par) {
  b0 <- par[1]
  b1 <- par[2]
  sigma <- par[3]
  #sigma has to return as positive
  if (sigma <= 0) return(Inf)  
  
  mu <- b0 + b1*x
  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))}

ml_start <- c(0, 0, 1)   # (b0, b1, sigma)

ml_fit <- optim(par = ml_start,
                fn = nll_optim,
                method = "Nelder-Mead")

ml_fit$par      # ML estimates
ml_fit$convergence

##PART C##

starts <- list(
  c(0, 0, 1),
  c(10, -10, 5),
  c(-5, 5, 2),
  c(20, 20, 15),
  c(-20, -20, 3))

lapply(starts, function(s) {
  optim(par = s, fn = nll_optim, method = "Nelder-Mead")$par})


###OBJECTIVE 4###

#The slope and intercepts for the 3 different approaches we used are all very similar. The only real difference is with the standard deviation estimate from the maximum likelihood estimation, which is not calculated in the ordinary least squares approach. The estimates for the slope and intercept are nearly identical across all three methods, indicating that they all converge to similar solutions for this dataset. 
