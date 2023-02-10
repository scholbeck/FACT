#### Prerequisits ##############################################################
library(ggplot2)
library(patchwork)
library(mlr3cluster)
library(mlr3viz)
library(mlr3)
library(data.table)
library(halflinger)
dir = "ExplaClust/R/"
sapply(paste(dir, list.files(dir), sep = ""), source)

#### Generate Data #############################################################
set.seed(123)
imbalanced_dta = rbind(
data.table(x1 = rnorm(50, mean = 50, sd = 15), x2 = rnorm(50, mean = 50, sd = 15), class = "Class 2"),
data.table(x1 = rnorm(20, mean = 50, sd = 25), x2 = rnorm(20, mean = -10, sd = 1), class = "Class 3"),
data.table(x1 = rnorm(20, mean = 110, sd = 4), x2 = rnorm(20, mean = 50, sd = 15), class = "Class 1")
)


#### Cluster Data ##############################################################
library(mlr3cluster)
mlr_learners
cmeans = lrn("clust.cmeans", centers = 3L)
imb_tsk = TaskClust$new(id = "imb_dta", backend = imbalanced_dta[, c("x1", "x2")])
cmeans$train(imb_tsk)
cluster = paste("Cluster", cmeans$assignments)


#### Plot Class vs Assignments #################################################
p1 =  ggplot(data = imbalanced_dta, aes(x = x1, y = x2, colour = class)) +
  geom_point() + 
  ggtitle("True Classes")
p2 = ggplot(data = imbalanced_dta, aes(x = x1, y = x2, colour = cluster)) +
  geom_point() + 
  ggtitle("Cluster Assignments")

ggsave("Simulations/cluster_imp.png", p1 + p2, width = 8, height = 3)

#### Calculate Feature Importance ##############################################

pred = ClustPredictor$new(cmeans, imb_tsk$data(), y = cmeans$assignments)
g2pc = calculate_g2pc(cmeans, imb_tsk, n.repetitions = 89)
classwise_f1 = ClusterFeatureImp$new(pred, n.repetitions = 89, metric = "f1")
macro_f1 = ClusterFeatureImp$new(pred, n.repetitions = 89, metric = "f1", avg = "macro")
micro_f1 = ClusterFeatureImp$new(pred, n.repetitions = 89, metric = "f1", avg = "micro")

#### Table CFIS ################################################################

fimp = cbind(
  data.table(feature = g2pc$feature),
  G2PC = g2pc$g2pc,
  macro_F1 = macro_f1$results$median,
  micro_F1 = micro_f1$results$median
)
xtable::xtable(fimp)
xtable::xtable(classwise_f1$results)
xtable::xtable(macro_f1$results)

#### Plot Feature Importance ###################################################
mcfi = macro_f1$plot()
ccfi = classwise_f1$plot()
ggsave("Simulations/compare_cfi.png", mcfi + ccfi, width = 8, height = 3)
