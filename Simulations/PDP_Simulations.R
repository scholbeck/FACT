#### Prerequisits ##############################################################
library(ggplot2)
library(patchwork)
library(mlr3cluster)
library(mlr3viz)
library(mlr3)
library(data.table)
#library(halflinger)
library(gridExtra)
library(mvtnorm)
library(plotly)
library(latex2exp)
dir = "ExplaClust/R/"
sapply(paste(dir, list.files(dir), sep = ""), source)
theme_set(theme_bw())

matrix2latex <- function(matr) {
  
  printmrow <- function(x) {
    
    cat(cat(x,sep=" & "),"\\\\ \n")
  }
  
  cat("\\begin{bmatrix}","\n")
  body <- apply(matr,1,printmrow)
  cat("\\end{bmatrix}")
}

#### Generate Data #############################################################
set.seed(123456)
# Class 2
small_sig = matrix(rWishart(1,3,diag(c(0.15,0.15,0.15))), 3)
small_sig_eigen = eigen(small_sig)$values
small_sig_eigen
small = rmvnorm(50, c(0, 3, -3), small_sig)
# Class 1
mid_sig = matrix(rWishart(1,3,diag(c(0.3,0.3,0.3))), 3)
mid_sig_eigen = eigen(mid_sig)$values
mid_sig_eigen
mid = rmvnorm(50, c(3, -3, 0), mid_sig)
# Class 3
high_sig = matrix(rWishart(1,3,diag(c(0.6,0.6,0.6))), 3)
high_sig_eigen = eigen(high_sig)$values
high_sig_eigen
high = rmvnorm(50, c(-3, 0, 3), high_sig)

pd_dta = rbind(
  data.table(small, class = "Class 3"),
  data.table(mid, class = "Class 2"),
  data.table(high, class = "Class 1")
)

colnames(pd_dta) = c("x1", "x2", "x3", "class")

#### Cluster Data ##############################################################
cmeans = lrn("clust.cmeans", centers = 3L)
cmeans$predict_type = "prob"
dt_tsk = TaskClust$new(id = "dta", backend = pd_dta[, c("x1", "x2", "x3")])
cmeans$train(dt_tsk)
cluster = paste("Cluster", cmeans$assignments)


#### Plot Class vs Assignments #################################################
# plot_ly(x = pd_dta$x1, y = pd_dta$x2, z = pd_dta$x3, color = pd_dta$class,
#         type = 'scatter3d', mode = 'markers', marker = list(size = 2))
library(gg3D)
p1 = ggplot(data = pd_dta, aes(x = x1, y = x2, z = x3, colour = class)) +
  theme_void() +
  axes_3D() +
  stat_3D() +
  labs_3D(labs = c("x1", "x2", "x3"),
          hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 90)) +
  ggtitle("Original Classes")
p2 = ggplot(data = pd_dta, aes(x = x1, y = x2, z = x3, colour = cluster)) +
  theme_void() +
  axes_3D() +
  stat_3D() +
  labs_3D(labs = c("x1", "x2", "x3"),
          hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 90)) +
  ggtitle("Cluster Assignments")

p1+p2

ggsave("Simulations/PDP/pdp_sim.png", p1 + p2, width = 8, height = 3)


#### Calculate Feature Importance prob##########################################
cmeans$predict_type = "prob"
pred = ClustPredictor$new(cmeans, dt_tsk$data(), y = cmeans$assignments, type = "prob")
fe_x1 = ClusterFeatureEffect$new(predictor = pred, feature = "x1", method = "pdp+ice", grid.size = NULL)
plot_fec_x1 = fe_x1$plot(c=3) +
  theme(text = element_text(size = 18))
pdps_x1 = fe_x1$plot_pdps(0.6) +
  labs(fill="Cluster", colour = "Cluster", title = NULL, subtitle = NULL)+
  theme(legend.position="none", text = element_text(size = 18), panel.border = element_blank())

ggsave("Simulations/pdcice.png", plot_fec_x1, width = 8, height = 3)
ggsave("Simulations/pdcs.png", pdps_x1, width = 8, height = 3)
#ggsave("pics/icp_ex.png", grid.arrange(a[1]), width = 6, height = 3)

fe_x2 = ClusterFeatureEffect$new(predictor = pred, feature = "x2", method = "pdp+ice", grid.size = NULL)
plot_fec_x2 = fe_x2$plot(c=2)
pdps_x2 = fe_x2$plot_pdps(0.6) +
  labs(fill="Cluster", colour = "Cluster", title = NULL, subtitle = NULL)+
  theme(legend.position="none") 
ggsave("pics/rw_pdice.png", plot_fec_x2, width = 8, height = 3)

fe_x3 = ClusterFeatureEffect$new(predictor = pred, feature = "x3", method = "pdp+ice", grid.size = NULL)
plot_fec_x3 = fe_x3$plot()
pdps_x3 = fe_x3$plot_pdps(0.6)  +
  labs(fill="Class", colour = "Class", title = NULL, subtitle = NULL)+
  theme(legend.position="none") 
  
#ggsave("pics/icpc_ex.png", grid.arrange(b[1]), width = 8, height = 3)

ggsave("Simulations/PDP/check_pdp+ice.png", grid.arrange(plot_fec_x1, plot_fec_x2, plot_fec_x3, ncol = 3) , width = 8, height = 8)

ggsave("Simulations/PDP/check_pdps.png", pdps_x1 + pdps_x2 + pdps_x3 , width = 8, height = 3)


#### Calculate Feature Importance part##########################################

cmeans$predict_type = "partition"
pred$type = "partition"
fe_x1 = ClusterFeatureEffect$new(predictor = pred, feature = "x1", method = "pdp", grid.size = NULL)
plotx1 = fe_x1$plot() +
  #scale_fill_manual(breaks = c("1", "2", "3"), values=c("#F8766D", "#00BA38", "#00BCD8")) +
  labs(fill="cluster", x = "x1", y = "certainty") +
  theme(legend.position="none", text = element_text(size = 18))
ggsave("Simulations/pdch.png", plotx1, width = 8, height = 3)

fe_x2 = ClusterFeatureEffect$new(predictor = pred, feature = "x2", method = "pdp", grid.size = NULL)
plotx2 = fe_x2$plot() +
  #scale_fill_manual(breaks = c("1", "2", "3"), values=c("#F8766D", "#00BA38", "#00BCD8")) +
  labs(fill="cluster", x = "x2", , y = expression(rho)) +
  theme(legend.position = "none")

fe_x3 = ClusterFeatureEffect$new(predictor = pred, feature = "x3", method = "pdp", grid.size = NULL)
plotx3 = fe_x3$plot() +
  #scale_fill_manual(breaks = c("1", "2", "3"), values=c("#F8766D", "#00BA38", "#00BCD8")) +
  labs(fill="cluster", x = "x3", y = expression(rho)) +
  theme(legend.position = "none")

ggsave("Simulations/PDP/check_partpdps.png", plotx1 + plotx2 + plotx3 , width = 8, height = 3)

#### Calculate Feature cImportance #############################################
sapply(paste(dir, list.files(dir), sep = ""), source)
cmeans$predict_type = "prob"
pred$type = "prob"

fe_x1 = ClusterFeatureEffect$new(predictor = pred, feature = "x1", method = "cpdp+ice", grid.size = NULL)
X1 = fe_x1$plot(3) +
  theme(legend.position="none", text = element_text(size = 18)) +
  ylab("Cluster 3")
ggsave("Simulations/cpdcice.png", X1, width = 8, height = 3)

X1 = fe_x1$plot(3) +
  theme(legend.position="none")

fe_x2 = ClusterFeatureEffect$new(predictor = pred, feature = "x2", method = "cpdp+ice", grid.size = NULL)
X2 = fe_x2$plot(3) +
  theme(legend.position="none")
#ggsave("pics/icpc_ex.png", grid.arrange(b[1]), width = 8, height = 3)

fe_x3 = ClusterFeatureEffect$new(predictor = pred, feature = "x3", method = "cpdp+ice", grid.size = NULL)
X3 = fe_x3$plot(3)
ggsave("Simulations/PDP/check_cpdp+ice.png", X1 + X2 + X3 , width = 8, height = 8)

#### Calculate 2D PDP ##########################################################

fe_2d = ClusterFeatureEffect$new(predictor = pred, feature = c("x2", "x3"), method = "pdp", grid.size = 50)
plot2d = fe_2d$plot()
ggsave("Simulations/PDP/2D.png", plot2d , width = 5, height = 4)



