############################################################
# Example 1: Lunch Packet Weights
# Goal: CI for mean packet weight
############################################################

# Parameters (from example)
xbar <- 298.2   # sample mean
s    <- 3.9     # sample standard deviation
n    <- 36      # sample size
df   <- n - 1   # degrees of freedom

# Standard error
SE <- s / sqrt(n)

# Critical t* value for 95% CI
t_star <- qt(0.975, df = df)   # two-sided, alpha = 0.05

# Confidence interval
CI_lower <- xbar - t_star * SE
CI_upper <- xbar + t_star * SE

cat("Lunch Packet 95% CI: (", round(CI_lower, 2), ",", round(CI_upper, 2), ")\n")


############################################################
# Example 2: App Adoption Test
# Goal: CI for true adoption proportion
############################################################

# Parameters (from example)
p_hat <- 0.32   # observed adoption rate (32%)
n     <- 200    # sample size
P0    <- 0.30   # hypothesized adoption rate

# Standard error
SE <- sqrt(p_hat * (1 - p_hat) / n)

# Critical Z* value for 95% CI
z_star <- qnorm(0.975)   # 1.96

# Confidence interval
CI_lower <- p_hat - z_star * SE
CI_upper <- p_hat + z_star * SE

cat("App Adoption 95% CI: (", round(CI_lower, 3), ",", round(CI_upper, 3), ")\n")
