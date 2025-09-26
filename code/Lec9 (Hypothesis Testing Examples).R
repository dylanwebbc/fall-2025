############################################################
# Hypothesis Testing Demos in R
# - One-sample Z (right/left tail) + shaded p-value
# - Quick p-value helpers from a Z statistic
# - One-sample t (right tail) + shaded p-value
# - Visual: compare t vs Normal (Z)
############################################################

##------------------------------##
## Helper: shade a right tail   ##
##------------------------------##
shade_right_tail <- function(x0, dens_fun, from = -4, to = 4, n = 400, col = "lightblue") {
  xs <- seq(x0, to, length.out = n)
  ys <- dens_fun(xs)
  polygon(c(x0, xs, to), c(0, ys, 0), col = col, border = NA)
}

##------------------------------##
## Helper: shade a left tail    ##
##------------------------------##
shade_left_tail <- function(x0, dens_fun, from = -4, to = 4, n = 400, col = "lightblue") {
  xs <- seq(from, x0, length.out = n)
  ys <- dens_fun(xs)
  polygon(c(from, xs, x0), c(0, ys, 0), col = col, border = NA)
}



############################################################
# 1) LUNCH PACKET EXAMPLE (Z-test, left tail)
#    H0: mu = 300  vs  H1: mu < 300
############################################################
mu0_L  <- 300    # null mean
xbar_L <- 298.2  # sample mean
s_L    <- 3.9    # treating as sigma here (large n)
n_L    <- 36

SE_L   <- s_L / sqrt(n_L)
Z_L    <- (xbar_L - mu0_L) / SE_L
p_L    <- pnorm(Z_L)       # left-tailed

curve(dnorm(x), from = -4, to = 4, lwd = 2,
      main = "Z distribution: Lunch Packets (H1: mu < 300)",
      xlab = "Z", ylab = "Density")
shade_left_tail(Z_L, dnorm)
abline(v = Z_L, col = "red", lwd = 2, lty = 2)
text(Z_L, 0.1, paste0("Z_obs = ", round(Z_L, 2)), pos = 2, col = "red")
text(-2.5, 0.15, paste0("p-value ≈ ", signif(p_L, 3)), col = "blue")

############################################################
# 3) QUICK p-VALUES FROM A GIVEN Z
#    One line each (left, right, two-sided)
############################################################
Z_given <- -2.77  # plug your Z here
p_left  <- pnorm(Z_given)
p_right <- 1 - pnorm(Z_given)
p_two   <- 2 * (1 - pnorm(abs(Z_given)))

# Print for reference
p_left; p_right; p_two

############################################################
# 4) DEALCOHOLIZED WINE (t-test, right tail)
#    H0: mu <= 450  vs  H1: mu > 450
#    Example where n is small and sigma is unknown -> use t
############################################################
mu0_t  <- 450
xbar_t <- 456.5
s_t    <- 8
n_t    <- 8
df_t   <- n_t - 1

SE_t   <- s_t / sqrt(n_t)
t_obs  <- (xbar_t - mu0_t) / SE_t
p_t    <- 1 - pt(t_obs, df = df_t)   # right-tailed

curve(dt(x, df = df_t), from = -4, to = 4, lwd = 2,
      main = paste0("t distribution (df=", df_t, "): Wine Polyphenols (H1: mu > 450)"),
      xlab = "t", ylab = "Density")
shade_right_tail(t_obs, function(z) dt(z, df = df_t))
abline(v = t_obs, col = "red", lwd = 2, lty = 2)
text(t_obs, 0.1, paste0("t_obs = ", round(t_obs, 2)), pos = 4, col = "red")
text(2, 0.2, paste0("p-value ≈ ", signif(p_t, 3)), col = "blue")

############################################################
# 5) VISUAL: t vs Normal (Z) comparison
#    Shows heavier tails for small df; with df=99 t ~ Normal
############################################################
# (Requires ggplot2)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Install ggplot2 for this section: install.packages('ggplot2')")
} else {
  library(ggplot2)
  
  x_vals <- seq(-4, 4, length.out = 400)
  dens_df <- data.frame(
    x    = rep(x_vals, 3),
    y    = c(dt(x_vals, df = 7),
             dnorm(x_vals),
             dt(x_vals, df = 99)),
    dist = rep(c("t, df=7", "Normal (Z)", "t, df=99)"), each = length(x_vals))
  )
  
  ggplot(dens_df, aes(x = x, y = y, color = dist)) +
    geom_line(linewidth = 1.2) +
    labs(title = "Comparing t-distributions with Normal (Z)",
         x = "Test statistic value", y = "Density") +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c("red", "green", "blue"))
}
