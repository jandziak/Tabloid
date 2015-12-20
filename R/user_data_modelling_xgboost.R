library(readr)
library(xgboost)

# Set a random seed for reproducibility
set.seed(1)
str(test)
names(test)
summary(test)
cat("reading the train and test data\n")
train <- read_csv("./data_prepared/data_final_label.csv")
test  <- read_csv("./data_prepared/data_final.csv")
str(train)
feature.names <- names(train)[4:(ncol(train))]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$is_bulw,
               eta         = 0.025,
               depth       = 10,
               nrounds     = 200,
               objective   = "binary:logistic")

cat("making predictions\n")
submission <- data.frame(Id=test$huid)
submission$Response <- as.integer(round(predict(clf, data.matrix(test[,feature.names]))))

cat("saving the submission file\n")
write_csv(submission, "./output/tabloid_submission.csv")
