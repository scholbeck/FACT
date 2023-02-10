library(R6)
library(checkmate)
library(data.table)
#library(halflinger)
library(ggplot2)
# Calculate feature importance for clustering wth of mlr3 and iml packages
calculate_g2pc <- function(lrn, tsk, n.repetitions = 5, hs = FALSE) {
  halfspaces = NULL
  # create permutations
  predictor = ClustPredictor$new(lrn, data = tsk$data(), y = lrn$assignments)
  # from class Interpretation method
  sampler = predictor$data
  getData = predictor$data$get.xy
  # before running
  data.sample = getData()
  result <- NULL
  n = predictor$batch.size
  features = as.list(sampler$feature.names)
  names(features) <- unlist(features)
  # run
  if(hs) halfspaces = train_depth(sampler$get.x()) 
  featurewise_results = mapply(estimate_feature_imp, group = names(features), features = features,
                               MoreArgs = list(data.sample = data.sample, y = sampler$y,
                                               n.repetitions = n.repetitions, y.names = sampler$y.names, 
                                               n = n, predictor = predictor, halfspaces = halfspaces),
                               SIMPLIFY = FALSE
  )
  
  #shape results
  result <- rbindlist(unname(featurewise_results), use.names = TRUE)
  result <- result[, list(
    "g2pc" = median(permutation_error),
    "g2pc.05" = quantile(permutation_error, probs = 0.05),
    "g2pc.95" = quantile(permutation_error, probs = 0.95)
  ), by = list(feature)]
  result <- result[order(result$g2pc, decreasing = TRUE), ]
  # Removes the n column
  result <- result[, list(
    feature, g2pc.05, g2pc, g2pc.95
  )]
  
  result
}

estimate_feature_imp <- function(group, features, data.sample, y, n.repetitions,
                                 y.names, n, predictor, halfspaces) {
  
  cnames <- setdiff(colnames(data.sample), y.names)
  qResults <- data.table::data.table()
  y.vec <- data.table::data.table()
  num_rep <- data.table::data.table()
  for (repi in 1:n.repetitions) {
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
      y.vec <- rbind(y.vec, data.design[, y.names, with = FALSE])
      qResults <- rbind(
        qResults,
        predictor$predict(data.frame(data.design[, cnames, with = FALSE]))
      )
    }
  }
  # AGGREGATE measurements
  results <- data.table::data.table(
    feature = group, actual = y.vec[[1]], predicted = qResults[[1]],
    num_rep = num_rep[[1]]
  )
  results <- results[, list("permutation_error" = sum(actual != predicted) / nrow(data.sample)),
                     by = list(feature, num_rep)
  ]
  results
}
