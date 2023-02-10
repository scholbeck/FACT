dir = "ExplaClust/R/"
options(width = 120)
sapply(paste(dir, list.files(dir), sep = ""), source)
# devtools::install_github("henrifnk/FuzzyDBScan")
library(FuzzyDBScan)
library(factoextra)
library(patchwork)
library(ggplot2)
library(gridExtra)
set.seed(2)
multishapes <- multishapes[, 1:2]
plot(multishapes)ClustPredictor$new(res, as.data.frame(multishapes), y = res$results,
                                    predict.function = predict_prob, type = "prob")
eps = c(0, 0.2)
pts = c(3, 15)
# FuzzyDBScan$undebug("predict")
res <- FuzzyDBScan$new(multishapes, eps, pts)
res$plot("x", "y")

multishapes_non = factoextra::multishapes[factoextra::multishapes$shape != 5, 1:2]
d = dist(multishapes_non, method = "euclidean")
# ClustPredictor$undebug("initialize")
agnes = cluster::agnes(d, method = "single")
clusters = cutree(agnes, k = 5)
ggplot(multishapes_non, aes(x = x, y = y, colour =as.factor(clusters))) +
  geom_point()

#################### Predictor for hard labels #################################
predict_part = function(model, newdata) {
  model$predict(new_data = newdata, cmatrix = FALSE)$cluster
}
pred_part = ClustPredictor$new(res, as.data.frame(multishapes), y = res$clusters,
                               predict.function = predict_part, type = "partition")


macro_f1 = ClusterFeatureImp$new(pred_part, n.repetitions = 50, metric = "f1", avg = "macro")
classwise_f1 = ClusterFeatureImp$new(pred_part, n.repetitions = 50, metric = "f1")
classwise_f1$plot()
#################### Predictor for probabilities ###############################
predict_prob = function(model, newdata) {
  model$predict(new_data = newdata)
}
pred_prob = ClustPredictor$new(res, as.data.frame(multishapes), y = res$results,
                               predict.function = predict_prob, type = "prob")
# ClusterFeatureEffect$undebug("initialize")
fe_x = ClusterFeatureEffect$new(predictor = pred_prob, feature = "x", method = "pdp+ice", grid.size = NULL)
fe_y = ClusterFeatureEffect$new(predictor = pred_prob, feature = "y", method = "pdp+ice", grid.size = NULL)


fe_x$plot(2)
fe_x$plot_pdps(0.5) + res$plot("x", "y")

fe_y$plot(3)
fe_y$plot_pdps(0.5) + res$plot("y", "x")


fe_2d = ClusterFeatureEffect$new(predictor = pred_prob, feature = c("x", "y"), method = "pdp", grid.size = 5)
fe_2d$plot() + + ggplot(multishapes, aes(x=x, y=y, colour=as.factor(res$cluster)))+ geom_point()

# ################################################################################
# #################### Noise Experiment ##########################################
# ################################################################################
# data = cbind(multishapes, clust = res$clusters)
# 
# cl = sapply(sort(unique(res$clusters))[-1], function(x){
#   as.numeric(x == res$clusters) * 0.75
# })
# cl = as.data.frame(cl)
# names(cl) = paste0("c_dummy", sort(unique(res$clusters))[-1])
# noise = data.frame(matrix(runif(4*1100), nrow = 1100)) 
# names(noise) = paste0("noisy_f", 1:ncol(noise))
# multishape_noise = cbind(multishapes, cl, noise)
# 
# resII <- FuzzyDBScan$new(multishape_noise, eps = c(0, 0.8), pts = c(6, 20))
# resII$plot("x", "y") + res$plot("x", "y")
# all(res$clusters == resII$clusters)
# plotxy = resII$plot("x", "y")
# predict_part = function(model, newdata) {
#   model$predict(new_data = newdata, cmatrix = FALSE)$cluster
# }
# predII_part = ClustPredictor$new(resII, multishape_noise, y = resII$clusters,
#                                  predict.function = predict_part, type = "partition")
# 
# 
# 
# 
# macroII_f1 = ClusterFeatureImp$new(predII_part, n.repetitions = 25, metric = "f1", avg = "macro")
# macroII_f1$plot(log = TRUE)
# classwiseII_f1 = ClusterFeatureImp$new(predII_part, n.repetitions = 50, metric = "f1")
# # Interpretation:
# classwiseII_f1$plot(log = TRUE, "Cluster-1") + plotxy
# 
# classwiseII_f1$plot(log = TRUE, "Cluster1") + plotxy
# 
# classwiseII_f1$plot(log = TRUE, "Cluster2") + plotxy
# 
# classwiseII_f1$plot(log = TRUE, "Cluster3") + plotxy
# 
# classwiseII_f1$plot(log = TRUE, "Cluster4") + plotxy
# 
# classwiseII_f1$plot(log = TRUE, "Cluster5") + plotxy
# 
# 
# ################ Feature Effects ###############################################
# 
# predII_prob = ClustPredictor$new(resII, multishape_noise, y = resII$results,
#                                predict.function = predict_prob, type = "prob")
# 
# # ClusterFeatureEffect$debug("plot_pdps")
# feII_x = ClusterFeatureEffect$new(predictor = predII_prob, feature = "x", method = "pdp+ice")
# feII_y = ClusterFeatureEffect$new(predictor = predII_prob, feature = "y", method = "pdp+ice")
# 
# 
# # Die sind scheisse hierfür...
# feII_x$plot_pdps(0.5) + resII$plot("x", "y")
# feII_y$plot_pdps(0.5) + resII$plot("y", "x")
# 
# 
# 
# # ClusterFeatureImp$debug("initialize")
# feII_2d = ClusterFeatureEffect$new(predictor = predII_prob, feature = c("x", "y"), method = "pdp", grid.size = 50, noise.out = -1)
# cl_idx = grep("Cluster", colnames(feII_2d$results), value = TRUE)
# feII_2d$results = cbind(
#   Cluster_Noise = (apply(feII_2d$results[, ..cl_idx], 1, sum) == 0) * .Machine$double.eps,
#   feII_2d$results
# )
# 
# feII_2d$plot() + resII$plot("x", "y")

################################################################################
#################### Noise Only Experiment #####################################
################################################################################
data = cbind(multishapes, clust = res$clusters)
# We create cols with different noise
# Hypothesis: The algorithm will later on include those features to ab bigger extend with more nocht:= higher std deviation...
noise = data.frame(matrix(rnorm(5*1100, sd = seq(0.06, 0.1, by = 0.01)), nrow = 1100, byrow = TRUE)) 
# Proof of concept:
apply(noise, 2, sd)
names(noise) = paste0("noisy_f", 1:ncol(noise))
multishape_noise = cbind(multishapes, noise)

resIII <- FuzzyDBScan$new(multishape_noise, eps = c(0, 0.34), pts = c(4, 20))
resIII$plot("x", "y") + res$plot("x", "y")

sum(res$clusters == resIII$clusters) / length(res$clusters)
plotxy = resIII$plot("x", "y")

library(mlr3cluster)
cmeans = lrn("clust.cmeans", centers = 6L)
tsk_mnoise = TaskClust$new(id = "mnoise", backend = multishape_noise)
cmeans$train(tsk_mnoise)
ggplot(multishape_noise, aes(x, y, colour = as.factor(cmeans$assignments)))+
  geom_point()

# To be honest, IML is not an instrument for model selection in clustering. 
# It tells you how your model works or not but not more...
pred_cpart = ClustPredictor$new(cmeans, multishape_noise, y = cmeans$assignments, type = "partition")
macro_cf1 = ClusterFeatureImp$new(pred_cpart, n.repetitions = 25, metric = "f1", avg = "macro")

cmeans$predict_type = "prob"
pred_cprob = ClustPredictor$new(cmeans, multishape_noise, y = cmeans$model$membership,type = "prob")
fe_cx = ClusterFeatureEffect$new(predictor = pred_cprob, feature = "x", method = "pdp+ice", grid.size = 50L)
fe_cx$plot_pdps(0.5)

fe_cxy = ClusterFeatureEffect$new(predictor = pred_cprob, feature = c("x","y"), method = "pdp", grid.size = 50L)
fe_cxy$plot() +
  geom_point(data = multishape_noise, aes(x = x, y = y, fill = NULL, alpha = NULL))
  
### PCA
res.pca <- prcomp(multishape_noise,  scale = T)
fviz_pca_ind(res.pca, label="none", habillage = resIII$clusters)



predIII_part = ClustPredictor$new(resIII, multishape_noise, y = resIII$clusters,
                                 predict.function = predict_part, type = "partition")

# Feature Importance

macroIII_f1 = ClusterFeatureImp$new(predIII_part, n.repetitions = 25, metric = "f1", avg = "macro")


macroIII_f1$plot(log = TRUE) + 
  xlim(c(-1.5,0)) +
  ggtitle("DBSCAN Gloabl Imp") +
macro_cf1$plot(log = TRUE) +
  xlim(c(-1.5,0)) +
  ggtitle("Kmeans Gloabl Imp")


predIII_prob = ClustPredictor$new(resIII, multishape_noise, y = resIII$results,
                                 predict.function = predict_prob, type = "prob")

feIII_x = ClusterFeatureEffect$new(predictor = predIII_prob, feature = "x", method = "pdp+ice", grid.size = 50L)
feIII_x$plot_pdps(0.5) + resIII$plot("x", "y")
feIII_y = ClusterFeatureEffect$new(predictor = predIII_prob, feature = "y", method = "pdp+ice", grid.size = 50L)
feIII_y$plot_pdps(0.5) + resIII$plot("y", "x")
feIII_noise = ClusterFeatureEffect$new(predictor = predIII_prob, feature = "noisy_f5", method = "pdp+ice", grid.size = 50L)
feIII_noise$plot_pdps(0.5) + resIII$plot("y", "x")

feIII_2d = ClusterFeatureEffect$new(predictor = predIII_prob, feature = c("x", "y"), method = "pdp", grid.size = 50L, noise.out = -1)
cl_idx = grep("Cluster", colnames(feIII_2d$results), value = TRUE)
feIII_2d$results = cbind(
  Cluster_Noise = (apply(feIII_2d$results[, ..cl_idx], 1, sum) == 0) * .Machine$double.eps,
  feIII_2d$results
)

feIII_2d$plot() + resIII$plot("x", "y")

feIII_2d_large = ClusterFeatureEffect$new(predictor = predIII_prob, feature = c("x", "y"), method = "pdp", grid.size = 80L, noise.out = -1)
cl_idx = grep("Cluster", colnames(feIII_2d_large$results), value = TRUE)
feIII_2d_large$results = cbind(
  Cluster_Noise = (apply(feIII_2d_large$results[, ..cl_idx], 1, sum) == 0) * .Machine$double.eps,
  feIII_2d_large$results
)
# Non linear dependencies in 2D PDP plot
feIII_2d_large$plot() + resIII$plot("x", "y")
