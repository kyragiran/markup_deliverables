# Monte Carlo Simulation for Confidence Interval Validity
set.seed(123) 

# Parameters
n <- 30           
mu <- 100        
sigma <- 15       
num_simulations <- 1000 


simulate_ci <- function(n, mu, sigma) {
  sample_data <- rnorm(n, mean = mu, sd = sigma)
  sample_mean <- mean(sample_data)
  sample_se <- sd(sample_data) / sqrt(n)
  
  # 95% Confidence Interval
  ci_lower <- sample_mean - 1.96 * sample_se
  ci_upper <- sample_mean + 1.96 * sample_se
  
  return(c(ci_lower, ci_upper))
}

# Run simulations
results <- replicate(num_simulations, simulate_ci(n, mu, sigma))


contain_mu <- (mu >= results[1, ]) & (mu <= results[2, ])
coverage_rate <- mean(contain_mu) 


cat("Coverage rate:", coverage_rate, "\n")
