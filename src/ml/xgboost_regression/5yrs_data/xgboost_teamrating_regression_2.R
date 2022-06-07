install.packages("xgboost")
library(data.table)
library(xgboost)
library(ggplot2)
library(lattice)
library(caret)
data = fread("C:\\Users\\Sumedh-Alienware\\Downloads\\nfl\\xgboost_dataset_2.csv")

set.seed(42)
rows <- sample(nrow(data))

#train-test split method
indexes = 120
train = data[1:indexes,]
test = data[indexes:64,]
x_train = train[,-54]
y_train = train[,54]
x_test = test[,-54]
y_test = test[,54]

xgb_train = xgb.DMatrix(data = as.matrix(x_train), label = as.matrix(y_train))
xgb_test = xgb.DMatrix(data = as.matrix(x_test), label = as.matrix(y_test))

xgbc = xgboost(data = xgb_train, max.depth = 2, nrounds = 50)
print(xgbc)

pred_y = predict(xgbc, xgb_test)

rsquare=(y_test - pred_y)^2
mse = lapply(rsquare, mean, na.rm = TRUE)
rmse = lapply(mse, sqrt)

saveRDS(xgbc, "./xgboost_regression.rds")

#cross-validation method
cv_data = xgb.DMatrix(data = as.matrix(data[,-54]), label = as.matrix(data[,54]))
xgb_crossvalidated = xgb.cv(data = cv_data, nrounds = 3, nthread = 2, nfold = 5, metrics = list("rmse"),
       max_depth = 3, eta = 1, objective = "reg:squarederror")
print(xgb_crossvalidated)
saveRDS(xgb_crossvalidated, "./xgboost_regression_crossvalidated.rds")