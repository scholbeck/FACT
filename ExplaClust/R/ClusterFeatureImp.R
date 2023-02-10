# TODO: documentation, plotmethod, thinning of attributes
ClusterFeatureImp <- R6Class("ClusterFeatureImp",
                             public = list(
                               initialize = function(predictor, features = NULL, metric = "f1",
                                                     avg = NULL, n.repetitions = 5) {
                                 assert_choice(avg, c("micro", "macro"), null.ok = TRUE)
                                 assert_choice(metric, c("recall", "precision", "f1",
                                                         "jaccard", "folkes_mallows", "accuracy"))
                                 assert_number(n.repetitions)
                                 self$avg <- avg
                                 self$metric <- metric 
                                 self$predictor <- predictor
                                 self$sampler <- predictor$data
                                 self$data.sample <- predictor$data$get.xy()
                                 self$n.repetitions <- n.repetitions
                                 
                                 
                                 # process features argument
                                 if (is.null(features)) {
                                   features <- predictor$data$feature.names
                                 }
                                 if (!is.list(features)) {
                                   features <- as.list(features)
                                   names(features) <- unlist(features)
                                 }
                                 assert_subset(unique(unlist(features)), predictor$data$feature.names, empty.ok = FALSE)
                                 self$features <- features
                                 
                                 # suppressing package startup messages
                                 self$results = suppressPackageStartupMessages(private$run(self$predictor$batch.size))
                               },
                               print = function() {
                                 cat("Interpretation method: ", class(self)[1], "\n")
                                 cat("\n\nAnalysed predictor: \n")
                                 self$predictor$print()
                                 cat("\n\nAnalysed data:\n")
                                 print(self$sampler)
                                 cat("\nMetric:", self$metric, "\n")
                                 if(is.null(self$avg)) cat("Classwise scores") else cat("Scores sumarized by ", self$avg)
                                 cat("\n\nHead of results:\n")
                                 if (!is.null(self$results)) {
                                   print(head(self$results))
                                 }
                               },
                               plot = function(log = FALSE, single_cl = NULL) {
                                 assert_logical(log, any.missing = FALSE, len = 1L)
                                 assert_choice(single_cl, choices = colnames(self$results)[-1], null.ok = TRUE)
                                 if(!is.null(self$avg)) assert_true(is.null(single_cl))
                                 plot_res = if(log == TRUE) {
                                   cbind(self$results[,1], log(self$results[,-1]))
                                 } else{self$results}
                                 if(!is.null(single_cl)){
                                   p = ggplot(data = plot_res, aes(x = !!sym(single_cl), y = reorder(features, !!sym(single_cl)))) +
                                     geom_bar(stat = "identity") + ylab("features")
                                   return(p)
                                 }
                                 if(!is.null(self$avg)){
                                   colnames(plot_res) = gsub("[[:punct:]]", "", colnames(plot_res))
                                   p = ggplot(data = plot_res, aes(x = median, y = reorder(rn, median))) +
                                     geom_bar(stat = "identity") +
                                     geom_errorbar(aes(xmin = quant5, xmax = quant95), width= 0.2,
                                                   position=position_dodge(0.9)) +
                                     labs(y = "feature", title = "CFI with certainty quantiles",
                                          caption = paste("Metric used:", self$metric, "\n", "Scores sumarized by ", self$avg))
                                 } else {
                                   cnames = grep("Cluster", colnames(plot_res), value = TRUE)
                                   lon_res = reshape(plot_res, idvar = "features", varying = list(cnames),
                                                     timevar = "Cluster", v.names = "median", direction = "long",
                                                     times = cnames)
                                   p = ggplot(lon_res, aes(x = median, y = reorder(features, median), fill = Cluster)) +
                                     geom_bar(stat = "identity") +
                                     labs(y = "feature", title = "clusterwise aggregated CFI",
                                          caption = paste("Metric used:", self$metric))
                                 }
                                 p
                               },
                               avg = NULL,
                               metric = NULL,
                               predictor = NULL,
                               data.sample = NULL,
                               sampler = NULL,
                               features = NULL,
                               n.repetitions = NULL,
                               results = NULL
                             ),
                             
                             private = list(
                               run = function(n) {
                                 result <- NULL
                                 
                                 estimate_confusion_fimp <- function(group, features, data.sample, y, metric,
                                                                     n.repetitions, y.names, predictor, 
                                                                     halfspaces) {
                                   cnames <- setdiff(colnames(data.sample), y.names)
                                   num_rep <- data.table::data.table()
                                   tables = list()
                                   n = nrow(data.sample)
                                   class_scores = data.table::data.table()
                                   cat("feature", group, "\n")
                                   pb = txtProgressBar(min = 0, max = self$n.repetitions, initial = 0, style=3)
                                   for (repi in 1:n.repetitions) {
                                     setTxtProgressBar(pb, repi * which(features == group))
                                     mg <- iml:::MarginalGenerator$new(data.sample, data.sample, 
                                                                       features = features, n.sample.dist = 1,
                                                                       y = y, cartesian = FALSE, id.dist = TRUE
                                     )
                                     while (!mg$finished) {
                                       data.design <- mg$next.batch(n, y = TRUE)
                                       if(!is.null(halfspaces)) {
                                         mass = predict(halfspaces, data.design[, cnames, with = FALSE])
                                         data.design = cbind(data.design, mass)
                                         rows = round(nrow(data.design) * 0.7, digits = 0)
                                         data.design = data.design[order(-mass), .SD[1:rows]]
                                       }
                                       num_rep <- rbind(num_rep, rep(repi, times = nrow(data.design)))
                                       y.vec <- data.design[, y.names, with = FALSE]
                                       qResults <- predictor$predict(data.table(data.design[, cnames, with = FALSE]))
                                     }
                                     class_scores = rbind(class_scores,
                                                          t(evaluate_class(y.vec, qResults, metric = metric))
                                     )
                                   }
                                   class_scores$features = group
                                   close(pb)
                                   class_scores
                                 }
                                 
                                 featurewise_results = mapply(estimate_confusion_fimp, group = names(self$features), features = self$features,
                                                              MoreArgs = list(data.sample = self$data.sample, y = self$sampler$y, metric = self$metric,
                                                                              n.repetitions = self$n.repetitions, y.names = self$sampler$y.names, 
                                                                              predictor = self$predictor, halfspaces = NULL),
                                                              SIMPLIFY = FALSE
                                 )
                                 if(!is.null(self$avg)){
                                   n_classes = table(self$data.sample$.y)
                                   if(self$avg == "micro"){
                                     results = lapply(featurewise_results, function(x) {
                                       as.matrix(x[, .SD, .SDcols = names(n_classes)]) %*% as.matrix(n_classes) / sum(n_classes)
                                     })}
                                   if(self$avg == "macro"){
                                     results = lapply(featurewise_results, function(x) {
                                       apply(x[, .SD, .SDcols = names(n_classes)], 1, mean)})
                                   }
                                   results = t(sapply(results, function(x){
                                     c("quant" = quantile(x, probs = 0.05), 
                                       "median" = median(x),
                                       "quant" = quantile(x, probs = 0.95)
                                     )}))
                                   return(data.table(results, keep.rownames = TRUE))
                                 }
                                 result <- rbindlist(unname(featurewise_results), use.names = TRUE)
                                 clusters = grep(pattern = "\\d", x = colnames(result))
                                 setnames(result, old = clusters, new = paste0("Cluster",  colnames(result)[clusters])) 
                                 clusters = colnames(result)[grep(pattern = "\\d", x = colnames(result))]
                                 result[, lapply(.SD, median), .SDcols = clusters, by = list(features)]
                               }
                             )
)
