################################################################################
######################## Set Up ################################################
################################################################################

dir = "ExplaClust/R/"
options(width = 120)
sapply(paste(dir, list.files(dir), sep = ""), source)
# devtools::install_github("henrifnk/FuzzyDBScan")
library(FuzzyDBScan)
copula_dta = readRDS("Application/copula_dta.RDS")
################################################################################
######################## Clustering ############################################
################################################################################
#start from zero and  integrate max size of clusters to achieve maximal fuzziness
# a point will only be clustered with 100% if all the other points of the clusters are around
# we sacrifice points that are further than a euclidean distance of 2 to the next core point 
eps = c(0, 0.8)
pts = c(4, 200)
res <- FuzzyDBScan$new(copula_dta[,-1], eps, pts)
table(res$clusters, copula_dta$cluster)

res$plot("feat1", "feat2")
res$plot("feat1", "feat3")
res$plot("feat1", "feat4")

################################################################################
######################## Calculate Feature Effects #############################
################################################################################

predict_prob = function(model, newdata) {
  model$predict(new_data = newdata)
}
pred_prob = ClustPredictor$new(res, copula_dta[,-1], y = res$results,
                               predict.function = predict_prob, type = "prob")

fe_1 = ClusterFeatureEffect$new(predictor = pred_prob, feature = "feat1",
                                method = "pdp+ice", grid.size = 100)
fe_2 = ClusterFeatureEffect$new(predictor = pred_prob, feature = "feat2",
                                method = "pdp+ice", grid.size = 100)
fe_3 = ClusterFeatureEffect$new(predictor = pred_prob, feature = "feat3",
                                method = "pdp+ice", grid.size = 100)
fe_4 = ClusterFeatureEffect$new(predictor = pred_prob, feature = "feat4",
                                method = "pdp+ice", grid.size = 100)

fe_1x2 = ClusterFeatureEffect$new(predictor = pred_prob, feature = c("feat1", "feat2"),
                                  method = "pdp", grid.size = 100L)
fe_1x3 = ClusterFeatureEffect$new(predictor = pred_prob, feature = c("feat1", "feat3"),
                                  method = "pdp", grid.size = 100L)
fe_1x4 = ClusterFeatureEffect$new(predictor = pred_prob, feature = c("feat1", "feat4"),
                                  method = "pdp", grid.size = 100L)

effects = list(one = fe_1, two = fe_2, three = fe_3, four = fe_4,
               onetwo = fe_1x2, onethree = fe_1x3, onefour = fe_1x4)


saveRDS(effects, "Application/effect.RDS")


# results_1 = fe_1$results
# fe_1$results = fe_1$results[,2:=NULL]
# fe_1$plot_pdps(mass = 0.2) + plot_features
# 
# 
# cl_idx = grep("Cluster", colnames(fe_1x2$results), value = TRUE)[-1]
# results = fe_1x2$results
# fe_1x2$results = cbind(
#   Cluster_Noise = (apply(fe_1x2$results[, ..cl_idx], 1, sum) == 0) * .Machine$double.eps,
#   fe_1x2$results[,3:=NULL]
# )
# fe_1x2$plot() + dense_1x2
# 
# 
# results[, dense:= scale(Cluster1) + scale(Cluster2)+ scale(Cluster3)] 
# results[dense == -Inf, dense := tail(unique(results$dense), 1) - 2]
# bins = c(seq(-2, 12, length.out = 49), 18)
# plot_effect1x2 = ggplot(results, aes(x = feat1, y = feat2, z = dense)) +
#   geom_contour_filled(show.legend = FALSE, breaks = bins) + xlim(c(8,22)) + ylim(c(8,22))+
#   geom_point(data = copula_dta[c(1:200, 601:800, 1201:1400),], aes(shape = cluster, z = NULL),
#              colour= "white", size = 1, alpha = 0.4) +
#   theme(panel.background = element_blank(), legend.key = element_rect(fill = "#440154FF"))
# 
# dense_1x2 + plot_effect1x2 + plot_annotation(title = 'true density vs density clustering DBSCAN (right)')


