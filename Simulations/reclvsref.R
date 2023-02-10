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
  data.table(x1 = rnorm(10, mean = -15, sd = 4), x2 = rnorm(10, mean = 15, sd = 4), class = "Class 2"),
  data.table(x1 = rnorm(10, mean = 15, sd = 4), x2 = rnorm(10, mean = 15, sd = 4), class = "Class 3"),
  data.table(x1 = rnorm(50, mean = 0, sd = 6), x2 = rnorm(50, mean = 0, sd = 2), class = "Class 1")
)

#### Cluster OG Data ###########################################################

cmeans = lrn("clust.cmeans", centers = 3L)
imb_tsk = TaskClust$new(id = "imb_dta", backend = imbalanced_dta[, c("x1", "x2")])
cmeans$train(imb_tsk)
cluster = paste("Cluster", cmeans$assignments)

#### Permute Data ##############################################################

cmeans_perm = lrn("clust.cmeans", centers = 3L)
mg  <- iml:::MarginalGenerator$new(imbalanced_dta, imbalanced_dta, features = "x1", cartesian = TRUE)
dta_permuted = mg$next.batch(nrow(imbalanced_dta)^2, y = TRUE)[, 1:2]

#### Cluster OG Data ###########################################################
perm_tsk = TaskClust$new(id = "perm_dta", backend = dta_permuted[, c("x1", "x2")])
cmeans_perm$train(perm_tsk)
cluster_perm = paste("Cluster", cmeans_perm$assignments)

#### Plot OG Data ##############################################################

p2 = ggplot(data = dta_permuted, aes(x = x1, y = x2, colour = cluster_perm)) +
  geom_point(alpha=0.1) + 
  geom_point(data = imbalanced_dta, aes(x = x1, y = x2, colour = cluster), size = 2) +
  labs(colour = "Cluster")


#### Predict
cluster_perm_og = paste("Cluster", cmeans$predict(perm_tsk)$partition)

p0 = ggplot(data = dta_permuted, aes(x = x1, y = x2, colour = cluster_perm_og)) +
  geom_point(alpha=0.1) + 
  geom_point(data = imbalanced_dta, aes(x = x1, y = x2, colour = cluster), size = 2)+
  theme(legend.position = "none")


ggsave("Simulations/reclvsref.png", p0+p2, width = 8, height = 3)
