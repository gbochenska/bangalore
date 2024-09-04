library(glmnet)


#Usuwanie NA
dane <- read.csv("Bangalore.csv")
dane <- dane[1:1951, ]
dane <- na.omit(dane)
dane$Area <- scale(dane$Area)
dane$Price <- scale(dane$Price)

X <- model.matrix(Price ~ .-Location, data = dane)[, -1]
y <- dane$Price

#Regresja grzbietowa
lambda_grid <- 10^seq(10, -2, length.out = 100)
fit_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_grid)
plot(fit_ridge)

dim(coef(fit_ridge))

fit_ridge$lambda[50]
coef_ridge <- coef(fit_ridge)[, 50]
coef_ridge
sqrt(sum(coef_ridge[-1]^2))


fit_ridge$lambda[70]
coef(fit_ridge)[, 70]
sqrt(sum(coef(fit_ridge)[-1, 70]^2))

predict(fit_ridge, s = 50, type = "coefficients")

#Estymujemy testowy MSE
set.seed(1)
n <- nrow(X)
train <- sample(n, n / 2)
test <- -train
fit_ridge <- glmnet(X[train,], y[train], alpha = 0, lambda = lambda_grid,
                    thresh = 1e-12)

pred_ridge <- predict(fit_ridge, s = 4, newx = X[test,])
mean((pred_ridge - y[test])^2)

pred_null <- mean(y[train])
mean((pred_null - y[test])^2)

pred_ridge_big <- predict(fit_ridge, s = 1e10, newx = X[test,])
mean((pred_ridge_big - y[test])^2)

pred_ridge_0 <- predict(fit_ridge, x = X[train,], y = y[train], s = 0, 
                        newx = X[test,], exact = TRUE)
mean((pred_ridge_0 - y[test])^2)

lm(y ~ X, subset = train)
predict(fit_ridge, x = X[train,], y = y[train], s = 0, exact = TRUE, 
        type = "coefficients")[1:20,]

set.seed(1)
cv_out <- cv.glmnet(X[train,], y[train], alpha = 0)
plot(cv_out)
cv_out$lambda.min

pred_ridge_opt <- predict(fit_ridge, s = cv_out$lambda.min, newx = X[test,])
mean((pred_ridge_opt - y[test])^2)

fit_ridge_full <- glmnet(X, y, alpha = 0)
predict(fit_ridge_full, s = cv_out$lambda.min, type = "coefficients")







fit_lasso <- glmnet(X[train,], y[train], alpha = 1)
plot(fit_lasso, xvar = "lambda")

cv_out <- cv.glmnet(X[train,], y[train], alpha = 1)
plot(cv_out)
cv_out$lambda.min
pred_lasso <- predict(fit_lasso, s = cv_out$lambda.min, newx = X[test,])
mean((pred_lasso - y[test])^2)

fit_lasso_full <- glmnet(X, y, alpha = 1)
predict(fit_lasso_full, s = cv_out$lambda.min, type = "coefficients")[1:130,]