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
  data.table(x1 = rnorm(60, mean = 50, sd = 10), x2 = rnorm(60, mean = 5, sd = 3), class = "Class 3"),
  data.table(x1 = rnorm(20, mean = -5, sd = 3), x2 = rnorm(20, mean = 60, sd = 15), class = "Class 1"),
  data.table(x1 = rnorm(20, mean = 100, sd = 3), x2 = rnorm(20, mean = 60, sd = 15), class = "Class 2"),
  data.table(x1 = rnorm(20, mean = 50, sd = 3), x2 = rnorm(20, mean = 60, sd = 10), class = "Class 4"))
#### Cluster Data ##############################################################
library(mlr3cluster)
cmeans$param_set
cmeans = lrn("clust.cmeans", centers = 4L)
imb_tsk = TaskClust$new(id = "imb_dta", backend = imbalanced_dta[40:120, c("x1", "x2")])
cmeans$train(imb_tsk)
partition = cmeans$predict_newdata(imbalanced_dta[, c("x1", "x2")])$partition
cluster = paste("Cluster", partition)
#### Plot Class vs Assignments #################################################
p1 =  ggplot(data = imbalanced_dta, aes(x = x1, y = x2, colour = class)) +
  geom_point() + 
  ggtitle("True Classes")
p2 = ggplot(data = imbalanced_dta, aes(x = x1, y = x2, colour = cluster)) +
  geom_point() + 
  ggtitle("Cluster Assignments")
ggsave("Simulations/imbcl.png", p1 + p2, width = 8, height = 3)

#### Calculate Feature Importance ##############################################
set.seed(123)
pred = ClustPredictor$new(cmeans, imbalanced_dta[, c("x1", "x2")], y = partition)
macro_f1 = ClusterFeatureImp$new(pred, n.repetitions = 139, metric = "f1", avg = "macro")
acc = ClusterFeatureImp$new(pred, n.repetitions = 139, metric = "accuracy", avg = "micro")

#### Table CFIS ################################################################
macro_f1$results
acc$results
xtable::xtable(macro_f1$results)
xtable::xtable(acc$results)

#### Plot Feature Importance ###################################################
mcfi = macro_f1$plot()
accfi = acc$plot()
ggsave("Simulations/compare_f1bar.png", mcfi + accfi, width = 8, height = 3)
