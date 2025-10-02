##############################################
# One-sample Z-test for a proportion
# Example: p = 0.32, P = 0.30, n = 200
##############################################

# Parameters
p_hat <- 0.32    # sample proportion
P0    <- 0.30    # hypothesized population proportion
n     <- 200     # sample size

# Compute Standard Error
SE <- sqrt(P0 * (1 - P0) / n)

# Observed Z statistic
Z_obs <- (p_hat - P0) / SE

# One-sided p-value (H1: p > P0)
p_value <- 1 - pnorm(Z_obs)

# Print results
cat("Observed Z:", Z_obs, "\n")
cat("p-value:", p_value, "\n")

##############################################
# Plotting the Z distribution with observed Z
##############################################

# Plot standard normal curve
curve(dnorm(x), from = -4, to = 4, lwd = 2,
      main = "Z Distribution for One-Sample Proportion Test",
      ylab = "Density", xlab = "Z value")

# Shade the right-tail region (p-value area)
x_vals <- seq(Z_obs, 4, length.out = 200)
y_vals <- dnorm(x_vals)
polygon(c(Z_obs, x_vals, 4), c(0, y_vals, 0),
        col = "lightblue")

# Add vertical line for observed Z
abline(v = Z_obs, col = "red", lwd = 2, lty = 2)

# Annotate observed Z and p-value
text(Z_obs, 0.1, labels = paste0("Z_obs = ", round(Z_obs, 2)),
     pos = 4, col = "red")
text(2, 0.15, labels = paste0("p-value ≈ ", signif(p_value, 3)),
     col = "blue")




############################################################
# Pooled two-sample t-test example
# Undergrads vs Faculty (synthetic data)
############################################################

# Parameters
t_obs <- 5.75       # observed t-statistic (from pooled test)
df    <- 14         # degrees of freedom (n1 + n2 - 2)

# Two-sided p-value
p_value <- 2 * (1 - pt(abs(t_obs), df = df))
cat("Observed t:", t_obs, "\n")
cat("Degrees of freedom:", df, "\n")
cat("Two-sided p-value:", p_value, "\n")

############################################################
# Plot t-distribution with observed t and shaded p-value
############################################################

# Plot the t-density curve
curve(dt(x, df = df), from = -6, to = 6, lwd = 2,
      main = "t Distribution (df = 14)",
      ylab = "Density", xlab = "t value")

# Shade right tail
x_right <- seq(t_obs, 6, length.out = 200)
y_right <- dt(x_right, df = df)
polygon(c(t_obs, x_right, 6), c(0, y_right, 0), col = "lightblue")

# Shade left tail (symmetry for two-tailed test)
x_left <- seq(-6, -t_obs, length.out = 200)
y_left <- dt(x_left, df = df)
polygon(c(-6, x_left, -t_obs), c(0, y_left, 0), col = "lightblue")

# Add vertical lines for ±t_obs
abline(v = c(-t_obs, t_obs), col = "red", lwd = 2, lty = 2)

# Annotate observed t and p-value
text(t_obs, 0.05, labels = paste0("t_obs = ", round(t_obs, 2)),
     pos = 4, col = "red")
text(0, 0.1, labels = paste0("p ≈ ", signif(p_value, 3)),
     col = "blue")



############################################################
# Paired t-test: Faculty sign-ups before vs after campaign
############################################################

# Data
before <- c(18, 22, 15, 27, 12, 25, 19, 21, 16, 24)
after  <- c(26, 30, 19, 33, 17, 31, 23, 29, 18, 32)

# Differences
diffs <- after - before

# Paired t-test
t_out   <- t.test(after, before, paired = TRUE, alternative = "greater")
t_obs   <- as.numeric(t_out$statistic)
df      <- as.numeric(t_out$parameter)
p_value <- t_out$p.value

cat("Observed t:", round(t_obs, 2), "\n")
cat("Degrees of freedom:", df, "\n")
cat("One-sided p-value:", signif(p_value, 3), "\n")

############################################################
# Plot t-distribution with observed t and shaded p-value
############################################################

# Extend x-axis so t_obs is visible
curve(dt(x, df = df), from = -4, to = 12, lwd = 2,
      main = paste0("t Distribution (df = ", df, ")"),
      ylab = "Density", xlab = "t value",
      ylim = c(0, 0.45))

# Shade right-tail region
x_vals <- seq(t_obs, 12, length.out = 200)
y_vals <- dt(x_vals, df = df)
polygon(c(t_obs, x_vals, 12), c(0, y_vals, 0),
        col = "lightblue")

# Add vertical line for observed t
abline(v = t_obs, col = "red", lwd = 2, lty = 2)

# Annotate t_obs on the red line near x-axis
text(t_obs, 0.02, labels = paste0("t_obs = ", round(t_obs, 2)),
     pos = 4, col = "red", srt = 90)  # rotate so it "sticks" to line

# Annotate p-value along y-axis (left margin)
text(-3.5, 0.25, labels = paste0("p ≈ ", signif(p_value, 3)),
     col = "blue", adj = 0)
