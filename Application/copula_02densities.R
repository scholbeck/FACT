################################################################################
######################## Set Up ################################################
################################################################################
source("Application/Laplace_dist.R")
# devtools::install_github("henrifnk/FuzzyDBScan")
source("Application/copula_01data.R")

library(ggplot2)
# library(ggExtra)
library(patchwork)
library(copula)
# library(GGally)

library(EnvStats)
set.seed(2)

################################################################################
############################# Plot Data  #######################################
################################################################################

plot_features = ggplot(data.frame(shape = 2, scale = 4)) +
  stat_function(fun = dpareto, args = list(location = 8, shape = 10), geom = "area", fill = "green", alpha = 0.35) +
  stat_function(fun = dlap, args = list(mu = 15, sigma=0.75), geom = "area", fill = "blue", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 20), geom = "area", fill = "red", alpha = 0.35) + xlim(2, 22)

plot_contour = function(data, featx, featy, dense_fun_l, bins = NULL, grid_size = 100L, sl = FALSE, ...) {
  quant_x = quantile(data[, featy], c(0.01, 0.99))
  # cut grid at 99% quantile to get rid of extreme outliers
  grid = expand.grid(seq(from = 7, to = quantile(data[, featx], 0.99), length.out = grid_size),
                     seq(from = 7, to = quantile(data[, featy], 0.99), length.out = grid_size))
  values = sapply(dense_fun_l, function(x) apply(grid[,1:2], 1, x))
  # normalize values
  values = scale(values)
  grid_dta = cbind.data.frame(grid, apply(values, 1, max, na.rm = TRUE), apply(values, 1, sum, na.rm = TRUE))
  colnames(grid_dta) = c(featx, featy, "max_density", "sum_density")
  
  ggplot(grid_dta, aes(x = !!sym(featx), y = !!sym(featy), z = sum_density)) +
    geom_contour_filled(breaks = bins, show.legend = sl) +
    xlim(8, quantile(data[, featx], 0.99)) + ylim(8, quantile(data[, featy], 0.99)) +
    geom_point(data = data[c(1:200, 1001:1200, 2001:2200),], aes(shape = cluster, z = NULL), ...) +
    theme(panel.background = element_blank(), legend.key = element_rect(fill = "#440154FF")) +
    ggtitle(paste0("joint density of", featx, " and ", featy))
}

# Feature x_1 x x_2
dense_fun_l = list(
  "norm" = function(x) dMvdc(x, mvnorm_copI),
  "clay" = function(x) dMvdc(x, mvclay_copI),
  "frank" = function(x) dMvdc(x, mvfrank_copI)
)
bins = c(-10, seq(1,12, length.out = 48), 100)
dense_1x2 = plot_contour(copula_dta, "feat1", "feat2", dense_fun_l, bins = bins, colour= "white", size = 1, alpha = 0.4)

# Feature x_1 x x_3
# independent
dense_fun_l = list(
  "norm" = function(x) dnorm(x[1], mean = 20)* dnorm(x[2], mean = 20),
  "par" = function(x) dpareto(x[1], location = 8, shape = 10) * dpareto(x[2], location =8, shape = 10),
  "frank" = function(x) dlap(x[1], mu = 15, sigma=0.75)* dlap(x[2], mu = 15, sigma=0.75)
)
bins = c(-5, seq(1,10, length.out = 48), 35)
dense_1x3 = plot_contour(copula_dta, "feat1", "feat3", dense_fun_l, bins, sl = F, colour= "white", size = 1, alpha = 0.4)

# Feature x_1 x x_4
# independent
dense_1x4 = plot_contour(copula_dta, "feat1", "feat4", dense_fun_l, bins, colour= "white", size = 1, alpha = 0.4)

# Feature x_2 x x_3
# independent
dense_2x3 = plot_contour(copula_dta, "feat2", "feat3", dense_fun_l, bins, colour= "white", size = 1, alpha = 0.4)

# Feature x_2 x x_4
# independent
dense_2x4 = plot_contour(copula_dta, "feat2", "feat4", dense_fun_l, bins, colour= "white", size = 1, alpha = 0.4)

# Feature x_3 x x_4
dense_fun_l = list(
  "norm" = function(x) dMvdc(x, mvnorm_copI),
  "clay" = function(x) dMvdc(x, mvclay_copI),
  "frank" = function(x) dMvdc(x, mvfrank_copI)
)
bins = c(-10, seq(1,12, length.out = 48), 100)
dense_3x4 = plot_contour(copula_dta, "feat3", "feat4", dense_fun_l, bins, sl = F, colour= "white", size = 1, alpha = 0.4)

(plot_features + dense_1x2 + dense_1x3 + dense_1x4)/
  (plot_spacer() + plot_features + dense_2x3 + dense_2x4)/
  (plot_spacer() + plot_spacer() + plot_features + dense_3x4)/
  (plot_spacer() + plot_spacer() + plot_spacer() + plot_features)

