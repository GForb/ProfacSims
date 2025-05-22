
set.seed(123123)

data <- generate_cbcl()

plot(data$time_centered, data$cbcl, col = factor(data$studyid))
cor(data$cbcl, data$cbcl2)
plot(data$cbcl, data$cbcl2, col = factor(data$studyid))

write.csv(data, file = "Data/cbcl.csv", row.names = FALSE)


test_data <- generate_cbcl()
write.csv(test_data, file = "Data/cbcl_test.csv", row.names = FALSE)
