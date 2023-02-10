#' @title Clustering Predictor Object
#' 
#' @description
#' A `ClustPredictor` object holds any unsupervized machine learning model (`mlr`, `caret`,
#' `randomForest`, ...) and the data to be used for analyzing the model. The
#' interpretation methods in the `iml_clust` package need the machine learning model
#' to be wrapped in a `ClustPredictor` object.
#'
#' @importFrom prediction find_data
#' 
#' @details
#' A Cluster Predictor object is a container for the unsupervized prediction model and the data.
#' This ensures that the machine learning model can be analyzed in a robust way.
#'
#' The Model inherits from `iml::Predictor` Object and adjusts this Object to container unsupervized Methods.
#'
#' @export
ClustPredictor <- R6Class("ClustPredictor",
                          inherit = iml::Predictor,
                          public = list(
                            initialize = function(model = NULL, data = NULL, predict.function = NULL,
                                                  y = NULL, class = NULL, batch.size = 1000, type = NULL) {
                              assert_number(batch.size, lower = 1)
                              if (is.null(model) & is.null(predict.function)) {
                                stop("Provide a model, a predict.fun or both!")
                              }
                              if (is.null(data)) {
                                tryCatch(
                                  {
                                    data <- prediction::find_data(model)
                                  },
                                  error = function(e) stop("Can't extract data from model, please provide via data=")
                                )
                              }
                              if (is.null(y)) {
                                y <- find_y(model)
                                # Extra fix for caret, because it renames the target in the data
                                if (inherits(model, "train")) {
                                  colnames(data)[colnames(data) == ".outcome"] <- y
                                }
                                # y not always needed, so ignore when not in data
                                if (is.character(y) && !(y %in% names(data))) {
                                  y <- NULL
                                }
                              }
                              
                              # data needs to be a data.table to work with class "Data" (#115)
                              if (inherits(data, "data.table")) {
                                data.table::setDF(data)
                              }
                              
                              self$data <- iml:::Data$new(data, y = y)
                              self$class <- class
                              self$model <- model
                              self$type <- type
                              self$task <- inferTaskFromModel(model)
                              self$type <- if(is.null(type)){
                                ifelse(ncol(data.table(y)) == 1L, "partition", "prob")
                              } else type
                              if(self$type == "prob") self$cnames <- colnames(y)
                              if(is.null(predict.function)){
                                self$prediction.function <- create_predict_fun(model, self$task,
                                                                               predict.function)
                              } else{
                                self$prediction.function <- function(newdata){
                                  pred = do.call(predict.function, list(model, newdata = newdata))
                                  data.table(pred)
                                }
                              }
                              self$batch.size <- batch.size
                            },
                            type = NULL,
                            cnames = NULL
                          )
)
