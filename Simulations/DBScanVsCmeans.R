################################################################################
######################## Set Up ################################################
################################################################################
dir = "ExplaClust/R/"
options(width = 120)
sapply(paste(dir, list.files(dir), sep = ""), source)
# devtools::install_github("henrifnk/FuzzyDBScan")
library(FuzzyDBScan)
library(factoextra)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(mlr3cluster)
set.seed(2)
multishapes <- factoextra::multishapes[, 1:2]

################################################################################
######################## Clustering ############################################
################################################################################

eps = c(0, 0.2)
pts = c(3, 15)
res <- FuzzyDBScan$new(multishapes, eps, pts)
dbscan_plt = res$plot("x", "y") + ggtitle("Fuzzy DBScan clustering") + labs(color= "Cluster", alpha = "Pseudo-probs")

cmeans = lrn("clust.cmeans", centers = 5L)
tsk_mnoise = TaskClust$new(id = "mnoise", backend = multishapes)
cmeans$train(tsk_mnoise)
membership = apply(cmeans$model$membership, 1, max)
cmeans_plt = ggplot(multishapes, aes(x, y, colour = as.factor(cmeans$assignments), alpha = membership))+
  geom_point() + ggtitle("Cmeans clustering") + labs(color= "Cluster", alpha = "Pseudo-probs")

cmeans_plt + dbscan_plt


################################################################################
######################## Feature Importance ####################################
################################################################################

pred_cpart = ClustPredictor$new(cmeans, multishapes, y = cmeans$assignments, type = "partition")
macro_cf1 = ClusterFeatureImp$new(pred_cpart, n.repetitions = 50, metric = "f1", avg = "macro")
fi_cmeans = macro_cf1$plot() + ggtitle(expression(Feature ~ Importance ~ f[2]))


predict_dpart = function(model, newdata) model$predict(new_data = newdata, cmatrix = FALSE)$cluster
pred_dpart = ClustPredictor$new(res, as.data.frame(multishapes), y = res$clusters,
                               predict.function = predict_dpart, type = "partition")
macro_f1 = ClusterFeatureImp$new(pred_dpart, n.repetitions = 50, metric = "f1", avg = "macro")
fi_dbscan = macro_f1$plot() + ggtitle(expression(Feature ~ Importance ~ f[1]))

ggsave(filename = "pics/uc_fimp.png", fi_dbscan + fi_cmeans, height = 4,width = 8)
ggsave(filename = "pics/uc_fimp_ms.png", 
       fi_dbscan + scale_y_discrete(labels=c("y" = "market growth", "x" = "market share")) +
         fi_cmeans + scale_y_discrete(labels=c("y" = "market growth", "x" = "market share")),
       height = 4,width = 8)

ggsave(filename = "pics/uc_fimp_tc.png", 
       fi_dbscan + scale_y_discrete(labels=c("y" = "Word Embedding 1", "x" = "Word Embedding 2")) +
         fi_cmeans + scale_y_discrete(labels=c("y" = "Word Embedding 1", "x" = "Word Embedding 2")),
       height = 4,width = 8)


################################################################################
######################## Feature Effect Cmeans##################################
################################################################################

cmeans$predict_type = "prob"
pred_cprob = ClustPredictor$new(cmeans, multishapes, y = cmeans$model$membership,type = "prob")
fe_cx = ClusterFeatureEffect$new(predictor = pred_cprob, feature = "x", method = "pdp+ice", grid.size = 50L)

fe_cx$plot_pdps(0.5) + cmeans_plt

fe_cy = ClusterFeatureEffect$new(predictor = pred_cprob, feature = "y", method = "pdp+ice", grid.size = 50L)
fe_cy$plot_pdps(0.5) + cmeans_plt + coord_flip()

fe_cx$plot_pdps(0.8) + fe_cy$plot_pdps(0.8)


fe_cxy = ClusterFeatureEffect$new(predictor = pred_cprob, feature = c("x","y"), method = "pdp", grid.size = 50L)
fe_cxy$plot() +
  geom_point(data = multishapes, aes(x = x, y = y, fill = NULL, alpha = NULL))

################################################################################
######################## Feature Effect DBScan #################################
################################################################################

predict_prob = function(model, newdata) model$predict(new_data = newdata)
pred_prob = ClustPredictor$new(res, as.data.frame(multishapes), y = res$results,
                               predict.function = predict_prob, type = "prob")

fe_x = ClusterFeatureEffect$new(predictor = pred_prob, feature = "x", method = "pdp+ice", grid.size = 50L)
fe_y = ClusterFeatureEffect$new(predictor = pred_prob, feature = "y", method = "pdp+ice", grid.size = 50L)


fe_x$plot(2)
fe_x$plot_pdps(0.5) + res$plot("x", "y")

fe_y$plot(3)
fe_y$plot_pdps(0.5) + res$plot("y", "x")

fe_2d = ClusterFeatureEffect$new(predictor = pred_prob, feature = c("x", "y"), method = "pdp", grid.size = 50L)
cl_idx = grep("Cluster", colnames(fe_2d$results), value = TRUE)[2:6]
results = fe_2d$results
fe_2d$results
fe_2d$results = cbind(
  Cluster_Noise = (apply(fe_2d$results[, ..cl_idx], 1, sum) == 0) * .Machine$double.eps,
  fe_2d$results[,3:=NULL]
)

ggsave(filename = "pics/uc_fe.png", 
  (fe_x$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL) +
    fe_y$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL))/
  (fe_cx$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL) +
     fe_cy$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL)),
  height = 10, width = 10
)

ggsave(filename = "pics/uc_fe_cs.png", 
       (fe_x$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL) + xlab("market share") +
          fe_y$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL) + xlab("market growth"))/
         (fe_cx$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL) + xlab("market share") +
            fe_cy$plot_pdps(0.5) + labs(title = NULL, subtitle = NULL)+ xlab("market growth")),
       height = 10, width = 10
)

ggsave(filename = "pics/uc_fe2d_tc.png", 
  fe_cxy$plot() + xlab("Word Embedding 2") + ylab("Word Embedding 1") +
    fe_2d$plot() + xlab("Word Embedding 2") + ylab("Word Embedding 1") ,
  height = 4, width = 8
)

