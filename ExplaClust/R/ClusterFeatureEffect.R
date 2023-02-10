ClusterFeatureEffect <- R6Class("ClusterFeatureEffect",
                                public = list(
                                  initialize = function(predictor, feature, method = "pdp+ice", grid.size = 20L,
                                                        noise.out = NULL) {
                                    assert_choice(method, c("pdp", "ice", "pdp+ice","cice", "cpdp+ice"))
                                    assert_numeric(grid.size, min.len = 1, null.ok = TRUE)
                                    self$predictor = predictor
                                    if(method %in% c("cpdp+ice", "cice")) assert_true(self$type == "prob")
                                    self$method = method
                                    self$feature <- private$sanitize.feature(
                                      feature, predictor$data$feature.names, method
                                    )
                                    self$mg = private$create_mg(predictor, feature, grid.size)
                                    self$noise.out = noise.out
                                    self$results = as.data.table(
                                      switch(self$type, prob = private$run_prob(predictor$data$n.rows),
                                             partition = private$run_part(predictor$data$n.rows))
                                    )
                                  },
                                  plot = function(c = NULL) {
                                    if(self$method %in% c("cpdp+ice", "cice")){
                                      classes = grep("Cluster", colnames(self$results), value = TRUE)
                                      if(is.null(c)) {
                                        class_plots = lapply(classes, private$plot_cpdp)
                                        return(do.call("grid.arrange", c(class_plots, ncol = 1)))
                                      }
                                      return(private$plot_cpdp(classes[c]))
                                    }
                                    theme_set(theme_minimal())
                                    if(length(self$feature) == 1L){
                                      if(self$type == "partition") {
                                        return(private$plot_part_1D())
                                      }
                                      classes = grep("Cluster", colnames(self$results), value = TRUE)
                                      if(is.null(c)) {
                                        class_plots = lapply(classes, private$plot_prob_1D)
                                        return(do.call("grid.arrange", c(class_plots, ncol = 1)))
                                      }
                                      return(private$plot_prob_1D(classes[c]))
                                    } else if(length(self$feature) == 2L) {
                                      return(private$plot_2D())
                                    }
                                  },
                                  plot_pdps = function(mass = NULL) {
                                    # mass: How many mass of ice lines in percent should be included included in the intervall
                                    assert_choice(self$method, c("pdp", "pdp+ice"))
                                    assert_character(self$feature, len = 1L)
                                    if(!is.null(mass) && self$method == "pdp+ice"){
                                      setkey(self$results, .type)
                                      high = self$results[.("ice"), lapply(.SD, quantile, probs = 1 - ((1 - mass)/2)),
                                                          .SDcols = grep("^Cluster", colnames(self$results)), by  = c(self$feature)]
                                      low = self$results[.("ice"), lapply(.SD, quantile, probs = (1 - mass)/2),
                                                         .SDcols = grep("^Cluster", colnames(self$results)), by  = c(self$feature)]
                                      aggr = list(melt(high, id.vars = c(self$feature), value.name = "upper"),
                                                  melt(low, id.vars = c(self$feature), value.name = "lower"),
                                                  melt(self$results[.("pdp"), ][, c(".type", ".id"):= NULL],
                                                       id.vars = c(self$feature), value.name = "pdp")
                                      )
                                      aggr = Reduce(function(...) merge(..., all = TRUE), aggr)
                                      ribbon = geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable),
                                                           linetype = 0, alpha = .2)
                                      labs =labs(title = "PD Class Likelihood Plots",
                                                 subtitle = paste(mass, "% ICE mass included in certainty boundaries")
                                      )
                                      
                                    } else{
                                      aggr = melt(self$results[.("pdp"), ][, c(".type", ".id"):= NULL],
                                                  id.vars = c(self$feature), value.name = "pdp")
                                      ribbon = NULL
                                      labs = labs(title = "PD Class Likelihood Plots")
                                    }
                                    
                                    ggplot(aggr, mapping = aes(!!!list(x = sym(self$feature)),
                                                               y = pdp, color = variable)) +
                                      geom_line(linewidth = 2) +
                                      scale_y_continuous(expression(f^{(k)})) +
                                      geom_rug(data = self$predictor$data$get.x(),
                                               mapping = aes(!!!list(x = sym(self$feature)), y = NULL, color = NULL)) +
                                      ribbon + labs
                                  },
                                  predictor = NULL,
                                  feature = NULL,
                                  method = NULL,
                                  mg = NULL,
                                  results = NULL,
                                  noise.out = NULL
                                ),
                                active = list(
                                  type = function() self$predictor$type
                                ),
                                private = list(
                                  sanitize.feature = function(feature, feature.names, method) {
                                    assert_character(feature, unique = TRUE)
                                    stopifnot(all(feature %in% feature.names))
                                    feature_idx <- which(feature[1] == feature.names)
                                    if (length(feature) == 2) {
                                      feature_idx <- c(feature, which(feature[2] == feature.names))
                                      assert_false(feature_idx[1] == feature_idx[2])
                                      if (method %in% c("ice", "pdp+ice")) {
                                        stop("ICE is not implemented for two features.")
                                      }
                                    }
                                    feature
                                  },
                                  create_mg = function(predictor, feature, grid.size){
                                    data.sample = predictor$data$get.xy()
                                    dta_filtered = data.sample[, feature, with = FALSE]
                                    if(is.null(grid.size)){
                                      grid.size = apply(dta_filtered, 2, function(x) length(unique(x)))
                                    }
                                    grid.dt <- iml:::get.grid(dta_filtered, grid.size, anchor.value = NULL)
                                    iml:::MarginalGenerator$new(grid.dt, data.sample, feature, 
                                                                id.dist = TRUE, cartesian = TRUE)
                                  },
                                  get_mode = function(x) {
                                    ux <- unique(x)
                                    ux[which.max(tabulate(match(x, ux)))]
                                  },
                                  get_certainty = function(x) {
                                    tab = table(x)
                                    max(tab) / sum(tab)
                                  },
                                  run_prob = function(n){
                                    y = vector()
                                    results.ice <- data.table()
                                    pb = txtProgressBar(min = 0, max = self$mg$n_total, initial = 0, style=3)
                                    while (!self$mg$finished) {
                                      setTxtProgressBar(pb, self$mg$.__enclos_env__$private$counter)
                                      results.ice.inter <- self$mg$next.batch(n)
                                      predictions <- self$predictor$predict(data.table(results.ice.inter[, self$predictor$data$feature.names, with = FALSE]))
                                      colnames(predictions) = paste0("Cluster", self$predictor$cnames)
                                      y = c(y, paste("Cluster", results.ice.inter$.y))
                                      results.ice.inter <- results.ice.inter[, c(self$feature, ".id.dist"), with = FALSE]
                                      results.ice.inter = cbind(results.ice.inter, predictions)
                                      results.ice <- rbind(results.ice, results.ice.inter)
                                    }
                                    results = data.table()
                                    cluster_names = colnames(predictions)
                                    if(!is.null(self$noise.out)) {
                                      noise_c = paste0("Cluster", self$noise.out)
                                      results.ice = results.ice[, !noise_c, with = F]
                                      cluster_names = colnames(predictions)[colnames(predictions) != noise_c]
                                    }
                                    
                                    if(self$method %in% c("cpdp+ice", "cice")) results.ice = cbind(results.ice, cluster = y)
                                    if (self$method %in% c("pdp", "pdp+ice", "cpdp+ice")) {
                                      results.aggregated = if(self$method != "cpdp+ice") {
                                        results.ice[, lapply(.SD, mean),
                                                    .SDcols = cluster_names, 
                                                    by  = c(self$feature)]
                                      } else {
                                        results.aggregated <- results.ice[, lapply(.SD, mean),
                                                                          .SDcols = cluster_names, 
                                                                          by  = c(self$feature, "cluster")]
                                      }
                                      results.aggregated$.type <- "pdp"
                                      results.aggregated$.id.dist = NA
                                      results <- rbind(results, results.aggregated)
                                    }
                                    
                                    if (self$method %in% c("ice", "pdp+ice", "cpdp+ice", "cice")) {
                                      results.ice$.type <- "ice"
                                      cols = c(self$feature, cluster_names, ".type", ".id.dist")
                                      if(self$method %in% c("cpdp+ice", "cice")) cols = c(cols, "cluster")
                                      results <- rbind(results, results.ice[, ..cols], fill = TRUE)
                                      results$.id <- results$.id.dist
                                      results$.id.dist <- NULL
                                      # sort by id
                                      setkeyv(results, ".id")
                                    }
                                    close(pb)
                                    data.table(results)
                                  },
                                  run_part = function(n){
                                    results.ice <- data.table()
                                    pb = txtProgressBar(min = 0, max = self$mg$n_total, initial = 0, style=3)
                                    while (!self$mg$finished) {
                                      setTxtProgressBar(pb, self$mg$.__enclos_env__$private$counter)
                                      results.ice.inter <- self$mg$next.batch(n)
                                      predictions <- self$predictor$predict(data.table(results.ice.inter[, self$predictor$data$feature.names, with = FALSE])) 
                                      results.ice.inter <- results.ice.inter[, c(self$feature, ".id.dist"), with = FALSE]
                                      results.ice.inter$.value <- paste("cluster", unlist(predictions))
                                      results.ice.inter$.class <- 1
                                      results.ice <- rbind(results.ice, results.ice.inter)
                                    }
                                    if(!is.null(self$noise.out)) {
                                      noise_c = paste("cluster", self$noise.out)
                                      setkey(results.ice, .value)
                                      results.ice = results.ice[! noise_c]
                                    }
                                    results <- data.table()
                                    if (self$method %in% c("pdp", "pdp+ice")) {
                                      results.aggregated <- results.ice[, list(.value = private$get_mode(.value),
                                                                               .cert = private$get_certainty(.value)),
                                                                        by = c(self$feature)]
                                      results.aggregated[, setdiff(colnames(results.ice), colnames(results.aggregated)):= NA]
                                      results.aggregated[, colnames(results.ice), with = FALSE]
                                      results.aggregated$.type <- "pdp"
                                      results <- rbind(results, results.aggregated)
                                    }
                                    
                                    if (self$method %in% c("ice", "pdp+ice")) {
                                      results.ice$.type <- "ice"
                                      results <- rbind(results, results.ice, fill = TRUE)
                                      results$.id <- results$.id.dist
                                      results$.id.dist <- NULL
                                      # sort by id
                                      setkeyv(results, ".id")
                                    }
                                    close(pb)
                                    data.table(results)
                                  },
                                  plot_cpdp = function(target){
                                    if(self$method == "cpdp+ice") pdp <- self$results[self$results$.type != "ice", ]
                                    ice = self$results[self$results$.type == "ice", ]
                                    ggplot(ice, mapping = aes(!!!list(x = sym(self$feature), y = sym(target)), colour = cluster)) +
                                      geom_rug(data = self$predictor$data$get.x(),
                                               mapping = aes(!!!list(x = sym(self$feature)), y = NULL, colour = NULL)) +
                                      geom_line(alpha = 0.2, mapping = aes(group = .id)) +
                                      if(self$method == "cpdp+ice") geom_line(data = pdp, linewidth = 2) else NULL
                                  },
                                  plot_part_1D = function(){
                                    stopifnot(exprObject = self$method %in% c("pdp", "pdp+ice"))
                                    aggr <- self$results[self$results$.type != "ice", ]
                                    ggplot(aggr, aes(!!!list(x= sym(self$feature)), y = .cert)) +
                                      geom_ribbon(aes(ymin = rep(0, times = nrow(self$results)),
                                                      ymax = .cert, fill = as.factor(.value)), linetype = 0) +
                                      geom_rug(data = self$predictor$data$get.x(), mapping = aes(!!!list(x = sym(self$feature)), y = NULL)) +
                                      scale_fill_discrete(name = "Marginal Cluster")
                                    },
                                  plot_prob_1D = function(target){
                                    curve = ggplot(self$results[self$results$.type == "ice", ], 
                                                   mapping = aes(!!!list(x = sym(self$feature), y = sym(target)))) +
                                      geom_rug(data = self$predictor$data$get.x(),
                                               mapping = aes(!!!list(x = sym(self$feature)), y = NULL))
                                    if(self$method %in% c("ice", "pdp+ice")) {
                                      curve = curve + geom_line(alpha = 0.2, mapping = aes(group = .id))
                                    }
                                    if(self$method %in% c("pdp", "pdp+ice")) {
                                      pdp <- self$results[self$results$.type != "ice", ]
                                      curve = curve + geom_line(data = pdp, linewidth = 2, color = "gold")
                                    }
                                    curve
                                  },
                                  plot_2D = function(){
                                    if(self$type == "prob") {
                                      cols = grep("Cluster", names(self$results), value = TRUE)
                                      self$results$.value = apply(self$results[, ..cols], 1, which.max)
                                      self$results$.cert = apply(self$results[, ..cols], 1, max)
                                    }
                                    self$results$.value = as.factor(self$results$.value)
                                    grid_p = ggplot(self$results, aes(!!!list(x = sym(self$feature[1]), y = sym(self$feature[2])),
                                                                             alpha = .cert, fill = .value)) +
                                      geom_tile() + geom_rug(data = self$predictor$data$get.x(), mapping = aes(alpha= NULL, fill = NULL))
                                    grid_p
                                  }
                                )
)
