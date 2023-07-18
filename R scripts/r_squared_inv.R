cont_performance <- function(x) {
  train_data <- generate_cbcl()
  test_data <- generate_cbcl()
  model <- model_lm_cbcl_test(train_data)
  test_pred <- predict(model, newdata = test_data)
  train_pred <- predict(model)

  outcome <- names(stats::model.frame(model))[1]
  observed_outcome <- test_data[, outcome]
  predicted_lp <- predict(model, newdata = test_data)
  metric_rsqared(predicted_lp, observed_outcome)$coef - metric_rsqared_old(predicted_lp, observed_outcome)$coef
}

sims <- sapply(1:100, cont_performance)
mean(sims)
min(sims)
max(sims)

train_data <- generate_cbcl()
test_data <- generate_cbcl()
model <- model_lm_cbcl_test(train_data)
test_pred <- predict(model, newdata = test_data)
train_pred <- predict(model)

outcome <- names(stats::model.frame(model))[1]
observed_outcome <- test_data[, outcome]
predicted_lp <- predict(model, newdata = test_data)
data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)

rsq_oosse(data)
rsq2(data)
rsq(data)

rsq_oosse(data) - rsq2(data)
