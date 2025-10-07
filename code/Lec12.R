## ------------------------------------------------------------
## Difference in means CI (Welch)
## ------------------------------------------------------------



## ==== SUMMARY STATS  ====
xbar1 <- 22    # Airline A mean
xbar2 <- 14    # Airline B mean
s1    <- 5     # Airline A SD
s2    <- 6     # Airline B SD
n1    <- 30
n2    <- 28

## -------- Helper: finite-number check ----------
.all_finite <- function(...) {
  vals <- c(...)
  all(is.finite(vals))
}

## -------- Compute from raw ----------
use_raw <- exists("A") && exists("B") && is.numeric(A) && is.numeric(B) &&
  length(A) >= 2 && length(B) >= 2

if (use_raw) {
  A <- as.numeric(A); B <- as.numeric(B)
  xbar1 <- mean(A);  s1 <- sd(A);  n1 <- length(A)
  xbar2 <- mean(B);  s2 <- sd(B);  n2 <- length(B)
}

## -------- Validate inputs ----------
if (!.all_finite(xbar1, xbar2, s1, s2, n1, n2))
  stop("Non-finite input among xbar1, xbar2, s1, s2, n1, n2.")

if (n1 < 2 || n2 < 2) stop("Need n1 >= 2 and n2 >= 2.")
if (s1 < 0 || s2 < 0) stop("Standard deviations must be >= 0.")

## -------- Welch CI computation ----------
d_hat <- xbar1 - xbar2

SE <- sqrt((s1^2)/n1 + (s2^2)/n2)

## Guard against zero/near-zero SE (e.g., s1=s2=0)
if (!is.finite(SE) || SE <= 0) {
  warning("SE was non-finite or <= 0; using a tiny positive fallback to allow plotting.")
  SE <- .Machine$double.eps * 10
}

num <- ((s1^2)/n1 + (s2^2)/n2)^2
den <- (((s1^2)/n1)^2)/(n1 - 1) + (((s2^2)/n2)^2)/(n2 - 1)
df  <- num / den

if (!is.finite(df) || df <= 0) {
  warning("Computed df non-finite or <= 0; falling back to df = n1 + n2 - 2.")
  df <- n1 + n2 - 2
}

alpha  <- 0.05
t_star <- qt(1 - alpha/2, df = df)
ci_low <- d_hat - t_star * SE
ci_upp <- d_hat + t_star * SE

cat("Difference (A - B):", round(d_hat, 3), "\n")
cat("SE (Welch):", round(SE, 4), "  df:", round(df, 2), "\n")
cat("95% CI:", sprintf("(%.3f, %.3f)", ci_low, ci_upp), "\n")

## -------- Plot: Normal curve centered at d_hat + CI markers ----------
## Use SE as sd for a visual approximation of sampling distribution
x_from <- d_hat - 4 * SE
x_to   <- d_hat + 4 * SE

if (!.all_finite(x_from, x_to)) {
  stop("Plot range non-finite; check inputs (means, SDs, ns).")
}
if (x_from == x_to) {
  x_from <- x_from - 1
  x_to   <- x_to + 1
}

curve(dnorm(x, mean = d_hat, sd = SE),
      from = x_from, to = x_to, n = 501, lwd = 2,
      xlab = "Difference in means (A - B, minutes)",
      ylab = "Density",
      main = "Estimated Sampling Distribution with Computed 95% CI")

abline(v = c(ci_low, ci_upp), col = "steelblue", lwd = 2, lty = 2)  # CI
abline(v = d_hat, col = "red", lwd = 2)                               # estimate
abline(v = 0, col = "gray60", lty = 3)                                # zero ref

legend("topright",
       legend = c(paste0("Estimate = ", round(d_hat, 2)),
                  paste0("95% CI: (", round(ci_low, 2), ", ", round(ci_upp, 2), ")")),
       col = c("red", "steelblue"), lty = c(1, 2), lwd = c(2, 2), bty = "n")



##
# RESAMPLING WITH REPLACEMENT
##

# --- Setup ---
set.seed(42)   # for reproducibility
socks <- c(rep("Red", 3), rep("Yellow", 2), "Blue")
n <- length(socks)

# --- Bootstrap sampling function ---
bootstrap_colors <- function(num_resamples, sample_size = n) {
  replicate(num_resamples, {
    sample(socks, size = sample_size, replace = TRUE)
  })
}

# --- Perform bootstrapping ---
boot_1000 <- bootstrap_colors(1000)
boot_10000 <- bootstrap_colors(10000)

# --- Summarize color proportions ---
get_color_props <- function(boot_data) {
  apply(boot_data, 2, function(x) table(factor(x, levels = unique(socks))) / n)
}

props_1000 <- t(get_color_props(boot_1000))
props_10000 <- t(get_color_props(boot_10000))

# --- Plotting ---
par(mfrow = c(1, 2))
hist(props_1000[, "Red"], col = "tomato", main = "Proportion of Red (1000 resamples)",
     xlab = "Proportion", breaks = 20)
hist(props_10000[, "Red"], col = "tomato", main = "Proportion of Red (10000 resamples)",
     xlab = "Proportion", breaks = 20)

# --- Optional: Show mean & 95% CI ---
ci_10000 <- quantile(props_10000[, "Red"], c(0.025, 0.975))
mean_red <- mean(props_10000[, "Red"])

cat("Mean proportion of Red socks:", round(mean_red, 3), "\n")
cat("95% CI for proportion of Red socks:", paste(round(ci_10000, 3), collapse = " – "), "\n")




# ------------------------------------------------------------
# Bootstrap a proportion from a finite basket (with replacement)
# Basket: 3 Red, 2 Yellow, 1 Blue  -> P(Red) = 3/6 = 0.5 per draw
# We take n = 6 draws (with replacement), B = 1000 bootstrap resamples.
# Plot a smoothed density of the bootstrap proportions and overlay
# the Normal approximation for the sample proportion.
# ------------------------------------------------------------

set.seed(42)

# Basket & sample size
socks <- c(rep("Red", 3), rep("Yellow", 2), "Blue")
n <- length(socks)  # 6

# --- Bootstrap: proportion of Reds in one resample of size n ---
boot_props <- function(B, color = "Red") {
  replicate(B, mean(sample(socks, size = n, replace = TRUE) == color))
}

# 1,000 bootstrap resamples
props_1k <- boot_props(1000, color = "Red")

# --- Smoothing the discrete bootstrap distribution for visualization ---
# The support is {0, 1/6, 2/6, ..., 1}. To avoid a spiky density, we:
#  (1) add tiny Gaussian jitter, and (2) use a slightly larger bandwidth.
jitter_sd <- 0.02                        # try 0.015–0.03 for more/less smoothing
props_1k_s <- pmin(pmax(props_1k + rnorm(length(props_1k), 0, jitter_sd), 0), 1)

bw_val <- 0.12                           # larger bw -> smoother, lower peak
d_boot <- density(props_1k_s, bw = bw_val, from = 0, to = 1)

# --- Normal reference curve for the sample proportion ---
# Mean p = 3/6 = 0.5;  Var(proportion) = p(1-p)/n  (here, 0.25/6)
p  <- 3/6
sd <- sqrt(p * (1 - p) / n)
xg <- seq(0, 1, length.out = 400)
norm_pdf <- dnorm(xg, mean = p, sd = sd)

# --- Plot: smoothed bootstrap density + Normal overlay on same axes ---
plot(d_boot,
     col = "orange", lwd = 3,
     main = "Bootstrap Proportion of Red (B=1,000) with Normal Reference",
     xlab = "Proportion Red", ylab = "Density",
     xlim = c(0, 1),
     ylim = range(0, d_boot$y, norm_pdf))

lines(xg, norm_pdf, lwd = 2, lty = 2)   # Normal approximation (dashed)

abline(v = mean(props_1k), col = "gray40", lty = 3)  # bootstrap mean marker

legend("topright",
       legend = c("Bootstrap (smoothed)", "Normal N(p, p(1-p)/n)"),
       col = c("orange", "black"), lwd = c(3, 2), lty = c(1, 2), bty = "n")


cat("Bootstrap mean (B=1000):", round(mean(props_1k), 3), "\n")
cat("Normal mean:", round(p, 3), "  Normal SD:", round(sd, 3), "\n")
