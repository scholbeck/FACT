################################################################################
######################## Set Up ################################################
################################################################################
source("Application/Laplace_dist.R")
library(copula)
library(EnvStats)
set.seed(2)

################################################################################
############################# Data  ############################################
################################################################################

norm_cop = normalCopula(0.5, dim = 2L)
mvnorm_copI = mvdc(norm_cop, margins = c("norm", "norm"),
                   paramMargins = list(list(mean = 20, sd = 1), list(mean = 20, sd = 1)))
mvnorm_copII = mvdc(norm_cop, margins = c("norm", "norm"),
                    paramMargins = list(list(mean = 20, sd = 1), list(mean = 20, sd = 1)))
c_norm = cbind.data.frame(Cluster = "clust_norm",  rMvdc(1e3, mvnorm_copI), rMvdc(1e3, mvnorm_copII))

clay_cop = claytonCopula(0.5, dim = 2L)
var_pareto = function(location, shape) (shape * location^2) / ((shape-1)^2 * (shape-2))
var_pareto(8, 10)
mvclay_copI = mvdc(clay_cop, margins = c("pareto", "pareto"),
                   paramMargins = list(list(location = 8, shape = 10), list(location = 8, shape = 10)))
mvclay_copII = mvdc(clay_cop, margins = c("pareto", "pareto"),
                    paramMargins = list(list(location = 8, shape = 10), list(location = 8, shape = 10)))
c_clay = cbind.data.frame(Cluster = "clust_clay", rMvdc(1e3, mvclay_copI), rMvdc(1e3, mvclay_copII))

frank_cop = frankCopula(4, dim = 2L)
dlnorm_mean15 = function(x){
  15^2 * exp(x-1) -1
}
mvfrank_copI = mvdc(frank_cop, margins = c("lap", "lap"),
                    paramMargins = list(list(mu = 15, sigma=0.9), list(mu = 15, sigma=0.9)))
mvfrank_copII = mvdc(frank_cop, margins = c("lap", "lap"),
                     paramMargins = list(list(mu = 15, sigma=0.8), list(mu = 15, sigma=0.8)))
c_frank = cbind.data.frame(Cluster = "clust_frank", rMvdc(1e3, mvfrank_copI), rMvdc(1e3, mvfrank_copII))

copula_dta = rbind.data.frame(c_norm, c_clay, c_frank)
colnames(copula_dta) = c("cluster", "feat1", "feat2", "feat3", "feat4")
saveRDS(copula_dta, "Application/copula_dta.RDS")
