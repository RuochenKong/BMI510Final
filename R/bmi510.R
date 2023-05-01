# bmi510.R

#' Sampling from dataset x
#'
#' @param x An atomic vector or a dataframe-like object.
#' @param n A number
#' @param replace A boolean
#' @return Same type as input x
#' @examples
#' rando(c(1,2,3), 2)
rando = function(x,n=1,replace=T){
  if (is.atomic(x) && is.vector(x)){
    return (sample(x, n,replace = replace))
  } else if (is.data.frame(x)) {
    sampleidx = sample(nrow(x), n,replace = replace)
    return (x[sampleidx,])
  } else {
    stop('Invalid input type of x')
  }
}

#' Check whether the element is the minimum
#'
#' @param x An atomic vector.
#' @param na.rm A boolean, whether na is allowed in input
#' @return A logical vector
#' @examples
#' is_min(c(0,1,1,0,3))
is_min = function(x,na.rm=T){
  if (is.atomic(x) && is.vector(x)){
    minx = min(x, na.rm = na.rm)
    return (ifelse(x == minx, T, F))
  } else {
    stop('Invalid input type of x')
  }
}

#' Check whether the element is the maximum
#'
#' @param x An atomic vector.
#' @param na.rm A boolean, whether na is allowed in input
#' @return A logical vector
#' @examples
#' is_max(c(0,1,1,0,3))
is_max = function(x,na.rm=T){
  if (is.atomic(x) && is.vector(x)){
    maxx = max(x, na.rm = na.rm)
    return (ifelse(x == maxx, T, F))
  } else {
    stop('Invalid input type of x')
  }
}

#' Repeat the input data by rows or by columns
#'
#' @param x A matrix or a dataframe.
#' @param M A integer, number of repeating by rows
#' @param N A integer, number of repeating by columns
#' @return Same type as input x
#'
rep_mat = function(x, M=1, N=1){
  if(is.matrix(x) || is.data.frame(x)){
    return (x[rep(c(1:nrow(x)), M), rep(c(1:ncol(x)), N)])
  } else {
    stop('Invalid input type of x')
  }
}

#' Find the classes of each variable in the input
#'
#' @param x A tibble.
#' @return Same type as input x
#'
classes = function(x){
  if (is_tibble(x)){
    return (sapply(x, class))
  } else {
    stop('Invalid input type of x')
  }

}

#' Scale the numeric columns of the input
#'
#' @param x A tibble.
#' @param center A boolean, whether centering
#' @param scale A boolean, whether scaling
#' @return Same type as input x
#'
df_scale = function(x, center = T, scale = T){
  numeric_cols = which(classes(x) == 'numeric')
  for (col in numeric_cols){
    x[col] = scale(x[col],center = center, scale = scale)
  }
  return (x)
}

#' Calculate the log-likelihood of x under the normal distribution
#'
#' @param x A numeric vector or matrix.
#' @param mean A number, mean of x.
#' @param sd A number, standard deviation of x.
#' @return A number
#'
log_likelihood_norm = function(x, mean, sd) sum(dnorm(x, mean = mean, sd = sd, log = TRUE))

#' Calculate the log-likelihood of x under the uniform distribution
#'
#' @param x A numeric vector or matrix.
#' @param min A number, min of x.
#' @param max A number, max of x.
#' @return A number
#'
log_likelihood_unif = function(x, min, max) sum(dunif(x, min = min, max = max, log = TRUE))

#' Calculate the log-likelihood of x under the chi-square distribution
#'
#' @param x A numeric vector or matrix.
#' @param df A number, degrees of freedom.
#' @return A number
#'
log_likelihood_chisq = function(x, df) sum(dchisq(x, df, log = TRUE))

#' Calculate the log-likelihood of x under the f distribution
#'
#' @param x A numeric vector or matrix.
#' @param df1 A number, degrees of freedom.
#' @param df2 A number, degrees of freedom.
#' @return A number
#'
log_likelihood_f = function(x, df1, df2) sum(df(x, df1 = df1, df2 = df2, log = TRUE))

#' Calculate the log-likelihood of x under the t distribution
#'
#' @param x A numeric vector or matrix.
#' @param df A number, degrees of freedom.
#' @return A number
#'
log_likelihood_t = function(x, df) sum(dt(x, df, log = TRUE))

#' Calculate the sensitivity of the prediction
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
sensitivity = function(pred,truth){
  pred = as.integer(pred)
  truth = as.integer(truth)
  idxP = which(truth == 1)
  idxTP = intersect(idxP, which(truth == pred))
  return (length(idxTP)/length(idxP))
}

#' Calculate the specificity of the prediction
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
specificity = function(pred,truth){
  pred = as.integer(pred)
  truth = as.integer(truth)
  idxN = which(truth == 0)
  idxTN = intersect(idxN, which(truth == pred))
  return (length(idxTN)/length(idxN))
}

#' Calculate the precision of the prediction
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
precision = function(pred,truth){
  pred = as.integer(pred)
  truth = as.integer(truth)
  idxPredP = which(pred == 1)
  idxTP = intersect(idxPredP, which(truth == pred))
  return (length(idxTP)/length(idxPredP))
}

#' Calculate the recall of the prediction
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
recall = function(pred,truth) sensitivity(pred,truth)

#' Calculate the accuracy of the prediction
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
accuracy = function(pred,truth) {
  pred = as.integer(pred)
  truth = as.integer(truth)
  correct = which(truth == pred)
  return(length(correct)/length(pred))
}

#' Calculate the f1 score of the prediction
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
f1 = function(pred,truth){
  prec = precision(pred,truth)
  rec = recall(pred,truth)
  return (2 * prec * rec / (prec + rec))
}

#' Calculate the minimum n needed for a two-sample t-test
#'
#' @param d A number, Cohen’s d.
#' @param power A number, using 0.,8 as the default
#' @return A number
#'
minimum_n_per_group = function(d,power = 0.8) ceiling(power.t.test(power = power, delta = d)$n)

#' Calculate the R-squared statistics
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
r2 = function(pred,truth) {
  ss_res = sum((pred-truth)^2)
  ss_tot = sum((pred - mean(pred))^2)
  return (1 - ss_res/ss_tot)
}

#' Calculate the adjusted R-squared statistics
#'
#' @param pred An atomic vector, prediction results
#' @param truth An atomic vector, ground truth label
#' @return A number
#'
adj_R2 = function(pred,truth,n_p) {
  r2_val = r2(pred,truth)
  n = length(pred)
  return (1-(1-r2_val)*(n-1)/(n-n_p-1))
}
