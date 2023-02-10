#input actual & predicted vectors or actual vs predicted confusion matrix 
evaluate_class = function(actual, predicted, metric = "f1"){
  cm = calculate_confusion(actual, predicted)
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  rec = diag / rowsums #recall
  prec = diag / colsums # precision
  expAccuracy = sum(p*q) #random/expected accuracy
  score = switch(metric,
                 recall = rec,
                 precision = prec,
                 f1 = {a = 2 * prec * rec / (prec + rec)
                   a[is.nan(a)] = 0
                   a},
                 jaccard = diag / rowsums + colsums - diag,
                 folkes_mallows = sqrt((diag / colsums) * (diag / rowsums)),
                 accuracy = {
                   a = rep(sum(diag), times = length(diag)) / n
                   names(a) = 1:length(diag)
                   a}
  )
  c(score, n = n)
}

calculate_confusion = function(actual, predicted) {
  naVals = union(which(is.na(actual)), which(is.na(predicted)))
  if(length(naVals) > 0) {
   actual = actual[-naVals]
   predicted = predicted[-naVals]
  }
  predi = factor(unlist(predicted), levels = sort(unique(unlist(actual))))
  as.matrix(table(Actual = unlist(actual), Predicted = predi))
}

