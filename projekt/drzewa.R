library(tree)

dane <- read.csv("Bangalore.csv")
dane <- dane[1:1951, ]

# drzewa klasyfikacyjne
sales_high_tree <- tree(SwimmingPool ~ . - SwimmingPool, data = dane)
summary(sales_high_tree)

plot(sales_high_tree)
text(sales_high_tree, pretty = 1)
sales_high_tree




# drzewa regresyjne
medv_tree <- tree(SwimmingPool ~ .-SwimmingPool, data = dane)
summary(medv_tree)
medv_tree
plot(medv_tree)
text(medv_tree)

set.seed(1)
n <- nrow(dane)
train <- sample(n, n / 2)
test <- -train
medv_tree <- tree(SwimmingPool ~ ., data = dane, subset = train)
medv_pred <- predict(medv_tree, newdata = dane[test,])
mean((medv_pred - dane$SwimmingPool[test])^2)

medv_cv <- cv.tree(medv_tree)
plot(medv_cv$size, medv_cv$dev, type = "b")

medv_pruned <- prune.tree(medv_tree, best = 4)
plot(medv_pruned)
text(medv_pruned)


library(randomForest)
#Bagging
medv_bag <- randomForest(SwimmingPool ~ ., data = dane, mtry = 13, importance = TRUE)
medv_bag

plot(medv_bag, type = "l", main="Wykres błędu OOB względem liczby drzew")

importance(medv_bag)

varImpPlot(medv_bag)

set.seed(2)
medv_bag <- randomForest(SwimmingPool ~ ., data = dane, subset = train, mtry = 13,
                         importance = TRUE)
medv_pred_bag <- predict(medv_bag, newdata = dane[test,])
mean((medv_pred_bag - dane$SwimmingPool[test])^2)

set.seed(2)
medv_bag_s <- randomForest(SwimmingPool ~ ., data = dane, subset = train, mtry = 13,
                           importance = TRUE, ntree = 25)
medv_pred_bag <- predict(medv_bag_s, newdata = dane[test,])
mean((medv_pred_bag - dane$SwimmingPool[test])^2)





#Lasy losowe
set.seed(2)
medv_rf <- randomForest(SwimmingPool ~ ., data = dane, subset = train,
                        importance = TRUE)
medv_pred_rf <- predict(medv_rf, newdata = dane[test,])
mean((medv_pred_rf - dane$SwimmingPool[test])^2)


set.seed(2)
medv_rf <- randomForest(SwimmingPool ~ ., data = dane, subset = train,mtry = 6,
                        importance = TRUE)
medv_pred_rf <- predict(medv_rf, newdata = dane[test,])
mean((medv_pred_rf - dane$SwimmingPool[test])^2)



library(gbm)
library(xgboost)
#Boosting
medv_boost <- gbm(SwimmingPool ~ .-Location, data = dane, distribution = "gaussian",
                  n.trees = 5000, interaction.depth = 4)
medv_boost
summary(medv_boost)

set.seed(2)
medv_boost <- gbm(SwimmingPool ~ .-Location, data = dane[train,], distribution = "gaussian",
                  interaction.depth = 4, n.trees = 5000)
medv_pred_boost <- predict(medv_boost, newdata = dane[test,], n.trees = 5000)
mean((medv_pred_boost - dane$SwimmingPool[test])^2)



set.seed(2)
medv_boost <- gbm(SwimmingPool ~ .-Location, data = dane[train,], distribution = "gaussian",
                  interaction.depth = 4, n.trees = 5000, shrinkage = 0.01)
medv_pred_boost <- predict(medv_boost, newdata = dane[test,], n.trees = 5000)
mean((medv_pred_boost - dane$SwimmingPool[test])^2)



set.seed(2)
medv_boost <- gbm(SwimmingPool ~ .-Location, data = dane[train,], distribution = "gaussian",
                   n.trees = 5000)
medv_pred_boost <- predict(medv_boost, newdata = dane[test,], n.trees = 5000)
mean((medv_pred_boost - dane$SwimmingPool[test])^2)