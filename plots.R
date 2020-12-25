library(boot)
library(ggplot2)
library(ROCR)
library(rio)
library(class)

dat <- import("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
colnames(dat) <- make.names(names(dat))

formulas <- list(Ensemble = "Survived ~ Sex + Age + Pclass",
                 Original = "Survived ~ Sex + Age",
                 Updated = "Survived  ~ Parents.Children.Aboard + Fare")

statistic <- function(data, formula) {
  fit <- glm(as.formula(formula), data = data)
  pred <- prediction(predict(fit, type = "response"), data[, names(fit$model)[1]])
  perf <- performance(pred, "tpr", "fpr")
  perf
}

boot.samples <- lapply(1:1000, function(i) dat[sample(1:nrow(dat), nrow(dat), replace = TRUE), ])
samples <- c(list(dat), boot.samples)
models <- setNames(nm = names(formulas))
model.list <- lapply(formulas, function(formula) lapply(samples, statistic, formula = formula))
data.list <- lapply(models, function(model.name) {
  performance.list <- model.list[[model.name]]
  lapply(performance.list, function(x) 
    data.frame(fpr = unlist(x@x.values), tpr = unlist(x@y.values), Model = model.name))
})
create_smooth_data <- function(datasets) {
  model.name <- unique(datasets[[1]]$Model)
  get_predictions <- function(train.data, test.data, method = "knn") {
    methods <- list(knn = function() {
      as.numeric(as.character(knn(train = matrix(train.data$fpr), 
                                  test = matrix(test.data), 
                                  cl = as.factor(train.data$tpr),
                                  k = 5)))
      },
      loess = function() {
        loess.fit <- with(train.data, loess(tpr ~ fpr, degree = 1))
        loess.pred <- predict(loess.fit, newdata = test.data)
        loess.pred
      })
    if (!any(method == names(methods)))
        stop ("No method with that name exists")
    fit <- methods[[method]]()
  }
  fpr.range <- seq(0, 1, 0.001)
  preds <- lapply(datasets, get_predictions, test.data = fpr.range)
  ci.data <- do.call(cbind, preds[-1])
  row_lb_ub <- function(data, ci.level = 0.95) {
    alpha <- (1 - ci.level)/2
    t(apply(data, 1, quantile, probs = c(alpha, ci.level + alpha)))
  }
  lb.ub <- row_lb_ub(ci.data)
  smooth.data <- data.frame(tpr = preds[[1]], lb = lb.ub[, 1], ub = lb.ub[, 2], fpr = fpr.range, Model = model.name)
  return (smooth.data)
}

smooth.data <- do.call(rbind, lapply(data.list, create_smooth_data))
noisy.data <- do.call(rbind, unlist(data.list, recursive = FALSE))

roc.plot <- ggplot() +
  # geom_point(aes(y= tpr, x = fpr, color = model, shape = model), data = noisy.data, alpha = 0.05) +
  geom_line(aes(y = tpr, x = fpr, color = Model), data = smooth.data) +
  geom_ribbon(aes(ymin = lb, ymax = ub, x = fpr, fill = Model), alpha = 0.5, data = smooth.data) +
  labs(x = "False positive rate", y = "True positive rate")

roc.plot
