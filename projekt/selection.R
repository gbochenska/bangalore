library(leaps)
library(car)

dane <- read.csv("Bangalore.csv")
dane <- dane[1:1951, ]
dane <- na.omit(dane)
dane <- subset(dane, select = -Location)

#nie uwzględniam location bo za dużo
Hitters_bs <- regsubsets(Price ~ ., data = dane)
summary(Hitters_bs)

Hitters_bs <- regsubsets(Price ~ ., data = dane, nvmax = 19)
Hitters_bs_sum <- summary(Hitters_bs)
Hitters_bs_sum

Hitters_bs_sum$cp

bic_min <- which.min(Hitters_bs_sum$bic)
bic_min
Hitters_bs_sum$bic[bic_min]

plot(Hitters_bs_sum$bic, xlab = "Liczba zmiennych", ylab = "BIC", col = "green",
     type = "b", pch = 20)
points(bic_min, Hitters_bs_sum$bic[bic_min], col = "red", pch = 9)

plot(Hitters_bs, scale = "bic")

coef(Hitters_bs, id = 6)





Hitters_fwd <- regsubsets(Price ~ ., data = dane, nvmax = 19, 
                          method = "forward")
Hitters_fwd_sum <- summary(Hitters_fwd)
Hitters_fwd_sum
Hitters_back <- regsubsets(Price ~ ., data = dane, nvmax = 19, 
                           method = "backward")
Hitters_back_sum <- summary(Hitters_back)
Hitters_back_sum

n <- nrow(dane)
train <- sample(c(TRUE, FALSE), n, replace = TRUE)
test <- !train
Hitters_bs_v <- regsubsets(Price ~ ., data = dane[train,], nvmax = 19)


predict.regsubsets <- function(object, newdata, id, ...) {
  model_formula <- as.formula(object$call[[2]])
  mat <- model.matrix(model_formula, newdata)
  coefs <- coef(object, id = id)
  mat[, names(coefs)] %*% coefs
}


prediction_error <- function(i, model, subset) {
  pred <- predict(model, dane[subset,], id = i)
  mean((dane$Price[subset] - pred)^2)
}
val_errors <- sapply(1:19, prediction_error, model = Hitters_bs_v, subset = test)
val_errors




cv_errors <- colMeans(val_err)
cv_errors