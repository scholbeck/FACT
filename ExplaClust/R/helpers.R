find_y <- function(mod) {
  if (is.null(mod)) {
    return(NULL)
  }
  UseMethod("find_y")
}

find_y.LearnerClust = function(mod) {
  mod$assignments
}

inferTaskFromModel <- function(model) {
  UseMethod("inferTaskFromModel")
}

inferTaskFromModel.Learner <- function(model) {
  if (!requireNamespace("mlr3")) {
    stop("Please install the mlr package.")
  }
  tsk <- model$task_type
  if (tsk == "classif") {
    if (model$predict_type != "prob") {
      warning("Output seems to be class instead of probabilities. 
               Automatically transformed to 0 and 1 probabilities.
               You might want to set predict.type = 'prob' for Learner!")
    }
    return("classification")
  } else if (tsk == "regr") {
    return("regression")
  } else if (tsk == "clust") {
    return("clustering")
  } else {
    stop(sprintf("mlr task type <%s> not supported", tsk))
  }
}

inferTaskFromModel.default <- function(model) {
  "unknown"
}

create_predict_fun <- function(model, task, predict.fun = NULL, type = NULL) {
  UseMethod("create_predict_fun")
}

create_predict_fun.Learner <- function(model, task, predict.fun = NULL, type = NULL) {
  if (!requireNamespace("mlr3")) {
    "Please install the mlr3 package."
  }
  if (task == "classification") {
    function(newdata) {
      if (model$predict_type == "response") {
        pred <- predict(model, newdata = newdata)
        factor_to_dataframe(pred)
      } else {
        data.table(predict(model, newdata = newdata, predict_type = "prob"), check.names = FALSE)
      }
    }
  } else if (task == "regression") {
    function(newdata) {
      data.table(predict(model, newdata = newdata))
    }
  } else if (task == "clustering") {
    function(newdata) {
      if(model$predict_type == "prob") {
        return(data.table(model$predict_newdata(newdata)$prob))
      }
      data.table(predict(model, newdata = newdata))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}
