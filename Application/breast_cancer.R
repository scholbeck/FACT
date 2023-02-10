#### Prerequisits ##############################################################
library(ggplot2)
library(factoextra)
library(patchwork)
library(gridExtra)
library(dplyr)
library(scales)
library(mlr3cluster)
library(iml)
library(data.table)
library(corrplot)
dir = "ExplaClust/R/"
sapply(paste(dir, list.files(dir), sep = ""), source)

#### Data ######################################################################

names <- c('id_number', 'diagnosis', 'radius_mean', 
           'texture_mean', 'perimeter_mean', 'area_mean', 
           'smoothness_mean', 'compactness_mean', 
           'concavity_mean','concave_points_mean', 
           'symmetry_mean', 'fractal_dimension_mean',
           'radius_se', 'texture_se', 'perimeter_se', 
           'area_se', 'smoothness_se', 'compactness_se', 
           'concavity_se', 'concave_points_se', 
           'symmetry_se', 'fractal_dimension_se', 
           'radius_worst', 'texture_worst', 
           'perimeter_worst', 'area_worst', 
           'smoothness_worst', 'compactness_worst', 
           'concavity_worst', 'concave_points_worst', 
           'symmetry_worst', 'fractal_dimension_worst')
breast_cancer <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data', sep = ',', col.names = names)

breast_cancer$id_number <- NULL

diagnosis = breast_cancer$diagnosis
breast_cancer$diagnosis = NULL
breast_cancer = scale(breast_cancer) %>% as.data.frame()

# Delete the least important features 
# se https://rpubs.com/raviolli77/352956
# breast_cancer$smoothness_se = breast_cancer$fractal_dimension_mean = NULL

cmeans = lrn("clust.cmeans", centers = 2L, method = "ufcl",
             rate.par = 0.1, iter.max = 1000)
tsk_cancer = TaskClust$new(id = "breast_cancer", backend = breast_cancer)
cmeans$train(tsk_cancer)
cluster_cancer = paste("Cluster", cmeans$assignments)
tab = table(cluster_cancer, diagnosis)
# accuracy clustering
accuracy = 1-(sum(diag(tab))/ length(diagnosis))
f1 = (2*tab[1,2]) / (2*tab[1,2] + tab[1,1] + tab[2,2])
tab = tab * 0.1
mcc = (tab[1,2] * tab[2,1] - tab[2,2] * tab[1,1]) / sqrt((tab[1,2] + tab[1,1]) * (tab[1,2] + tab[2,2]) * (tab[2,1] + tab[2,2]) * (tab[2,1] + tab[1,1]))

c(accuracy, f1, mcc)

#### plot correlations #########################################################
#corrplot(cor(breast_cancer))

# correlations = apply(cor(breast_cancer), 1, function(x) {
#   a = which(abs(x) > .85)
#   names(a)})
# 
# feature_groups = list(grp1 = correlations$radius_mean, grp2 = correlations$compactness_mean,
#                       grp3 = correlations$perimeter_se)

#### PCA #######################################################################

bc.pca <- prcomp(breast_cancer)
# Visualize
# Use habillage to specify groups for coloring
cl = fviz_pca_ind(bc.pca,
             label = "none", # hide individual labels
             habillage = cluster_cancer, # color by groups
             addEllipses = TRUE # Concentration ellipses
)
og = fviz_pca_ind(bc.pca,
                  label = "none", # hide individual labels
                  habillage = diagnosis, # color by groups
                  addEllipses = TRUE # Concentration ellipses
)

ggsave("Application/PCA.png", cl,  width = 5, height = 3)
#### Feature Importance ########################################################

predictor = ClustPredictor$new(cmeans, data = tsk_cancer$data(), y = cmeans$assignments)

microf1 = ClusterFeatureImp$new(predictor, n.repetitions = 30, metric = "f1", avg = "micro")
macrof1 = ClusterFeatureImp$new(predictor, n.repetitions = 30, metric = "f1", avg = "macro")

ggsave("Application/PFI_whole.png", microf1$plot(log = TRUE) + macrof1$plot(log = TRUE),
       height = 5, width = 8)


feature_measure = list(se = grep("_se", names, value = TRUE),
                       mean = grep("_mean", names, value = TRUE),
                       worst = grep("_worst", names, value = TRUE))
macrof1_means = ClusterFeatureImp$new(predictor, n.repetitions = 30, features = feature_measure, metric = "f1", avg = "macro")
macrof1_means$plot()


f1cluster = ClusterFeatureImp$new(predictor, n.repetitions = 30, metric = "f1", features = feature_measure)


feature_desc = list(area = grep("area", names, value = TRUE),
                    compactness = grep("compactness", names, value = TRUE),
                    concave_pts = grep("concave", names, value = TRUE),
                    concavity = grep("concavity", names, value = TRUE),
                    fractal = grep("fractal", names, value = TRUE),
                    perimeter = grep("perimeter", names, value = TRUE),
                    radius = grep("radius", names, value = TRUE),
                    smoothness = grep("smoothness", names, value = TRUE),
                    symmetry = grep("symmetry", names, value = TRUE),
                    texture = grep("texture", names, value = TRUE)
)

macrof1_meas = ClusterFeatureImp$new(predictor, n.repetitions = 30, features = feature_desc, metric = "f1")
ggsave("Application/groupedFI.png", f1cluster$plot(log = TRUE) + macrof1_meas$plot(log = TRUE),  width = 8, height = 3)

#### Feature Selection #########################################################

imp_micro = microf1$results[order(microf1$results$median),][1:3, "rn"]
imp_macro = macrof1$results[order(macrof1$results$median),][1:3, "rn"]
imp_feat = unlist(union(imp_macro, imp_micro))
breast_cancer_subset = breast_cancer[, imp_feat]
cmeans_sub = lrn("clust.cmeans", centers = 2L, method = "ufcl",
             rate.par = 0.2, iter.max = 1000)
tsk_cancer_sub = TaskClust$new(id = "breast_cancer_sub", backend = breast_cancer_subset)
cmeans_sub$train(tsk_cancer_sub)
cluster_cancer_sub = paste("Cluster", cmeans_sub$assignments)
# goodness of fit
tab = table(cluster_cancer_sub, diagnosis)
accuracy = 1-(sum(diag(tab))/ length(diagnosis))
f1 = (2*tab[1,2]) / (2*tab[1,2] + tab[1,1] + tab[2,2])
tab = tab * 0.1
mcc = (tab[1,2] * tab[2,1] - tab[2,2] * tab[1,1]) / sqrt((tab[1,2] + tab[1,1]) * (tab[1,2] + tab[2,2]) * (tab[2,1] + tab[2,2]) * (tab[2,1] + tab[1,1]))

### Rev Feature Selection  #####################################################
uimp_micro = microf1$results[order(microf1$results$median, decreasing = TRUE),][1:3, "rn"]
uimp_macro = macrof1$results[order(macrof1$results$median, decreasing = TRUE),][1:3, "rn"]
uimp_feat = unlist(union(uimp_macro, uimp_micro))
breast_cancer_usubset = breast_cancer[, uimp_feat]
cmeans_usub = lrn("clust.cmeans", centers = 2L, method = "ufcl",
                 rate.par = 0.2, iter.max = 1000)
tsk_cancer_usub = TaskClust$new(id = "breast_cancer_sub", backend = breast_cancer_usubset)
cmeans_usub$train(tsk_cancer_usub)
cluster_cancer_usub = paste("Cluster", cmeans_usub$assignments)
# goodness of fit
tab = table(cluster_cancer_usub, diagnosis)
accuracy = 1-(sum(diag(tab))/ length(diagnosis))
f1 = (2*tab[1,2]) / (2*tab[1,2] + tab[1,1] + tab[2,2])
tab = tab * 0.1
mcc = (tab[1,2] * tab[2,1] - tab[2,2] * tab[1,1]) / sqrt((tab[1,2] + tab[1,1]) * (tab[1,2] + tab[2,2]) * (tab[2,1] + tab[2,2]) * (tab[2,1] + tab[1,1]))


#### Feature Selection #########################################################

predictor_sub = ClustPredictor$new(cmeans_sub, data = tsk_cancer_sub$data(), y = cmeans_sub$assignments)

microf1_sub = ClusterFeatureImp$new(predictor_sub, n.repetitions = 30, metric = "f1", avg = "micro")
macrof1_sub = ClusterFeatureImp$new(predictor_sub, n.repetitions = 30, metric = "f1", avg = "macro")

microf1_sub$plot(log = TRUE) + macrof1_sub$plot(log = TRUE)

#### Feature Effect ############################################################

predictor_sub$model$predict_type = "prob"
predictor_sub$type = "prob"
fe_concave = ClusterFeatureEffect$new(predictor_sub, feature = "concavity_worst", grid.size = NULL)
concave_pdps = fe_concave$plot_pdps(mass = 0.7)
fe_concave$plot()
fe_compact = ClusterFeatureEffect$new(predictor_sub, feature = "compactness_worst", grid.size = NULL)
compact_pdps = fe_compact$plot_pdps(mass = 0.7)
fe_concavepts = ClusterFeatureEffect$new(predictor_sub, feature = "concave_points_worst", grid.size = NULL)
concavepts_pdps = fe_concavepts$plot_pdps(mass = 0.7)
lab = labs(title = NULL, subtitle = NULL) 
t = theme(legend.position="none", text = element_text(size = 18))
ggsave("Application/pdWDBC.png",
       concave_pdps + lab +t + compact_pdps + lab + t + concavepts_pdps + lab + t,
       width = 8, height = 3)



fe_2D = ClusterFeatureEffect$new(predictor_sub, feature = c("compactness_worst", "compactness_mean"), method = "pdp", grid.size = 50)
ggsave("Application/pd2WDBC.png", fe_2D$plot() + t,  width = 3, height = 3)
