set.seed(1234)

match_length <- function(vec) {
  c(vec, rep(NA, 1000-length(vec)))
}
sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
data <- generate_continuous(10,100,sigmas)
test_data <- generate_continuous(10,100,sigma_e = sigmas$e, sigma_u = sigmas$u, train_data = data)
test_data_na_y <-test_data
test_data_na_y$y <- NA
test_data_new_studies <- generate_continuous(10,500,sigma_e = sigmas$e, sigma_u = sigmas$u)
test_data_new_studies$studyid <- as.integer(test_data_new_studies$studyid)+10


write.csv(data, file = "Data/cont_train.csv", row.names = FALSE)
write.csv(test_data, file = "Data/cont_test.csv", row.names = FALSE)
write.csv(test_data_new_studies, file = "Data/cont_test_new_study.csv", row.names = FALSE)


foreign::write.dta(test_data_new_studies,file = "Data/cont_test_new_study.dta" )

model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                    data = data)

model2 <- lme4::lmer("x1 ~ x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                    data = data)


pred_train <- predict(model, random.only = TRUE)
pred_train[1:10]



data2 <- data[2:nrow(data), ]
model2 <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                    data = data2)

pred_train_new <- predict(model2, random.only = TRUE, newdata = data)
pred_train_new[1:10]

(pred_train_new-pred_train)[1:10]

small_data <- data[1:10,]
pred_train_small <- predict(model2, random.only = TRUE, newdata = small_data)
pred_train_small <- c(pred_train_small, rep(NA, 1000-length(pred_train_small)))

pred_test <- predict(model, random.only = TRUE, newdata = test_data)
pred_test[1:10]
pred_test_na <- predict(model, random.only = TRUE, newdata = test_data_na_y)
pred_test[1:10]


pred_test_1 <- predict(model, random.only = TRUE, newdata = test_data[1,])
pred_test_1 <- c(pred_test_1, rep(NA, 1000-length(pred_test_1)))

pred_new_test_1 <- predict(model, random.only = TRUE, newdata = test_data_new_studies[1,], allow.new.levels = TRUE)
pred_new_test_100 <- predict(model, random.only = TRUE, newdata = test_data_new_studies[1:100,], allow.new.levels = TRUE)
pred_new_test_1000<- predict(model, random.only = TRUE, newdata = test_data_new_studies[1:1000,], allow.new.levels = TRUE)


preds <- tibble::tibble(pred_train,
               pred_train_new,
               pred_train_small,
               pred_test,
               pred_test_na,
               pred_test_1,
               match_length(pred_new_test_1),
               match_length(pred_new_test_100),
               pred_new_test_1000)
preds

sum(abs(preds$pred_test - preds$pred_train))
sum(abs(preds$pred_test_na - preds$pred_train))

cbcl_data$cbcl[1:10]

data$y_bin <- rbernoulli(1000, boot::inv.logit(data$y))
mean(data$y_bin)
m1 <- lme4::glmer(y_bin~x1+x2+x3+x4+x5+x6+x7+x8+(1|studyid), family = "binomial", data=data)

m1


