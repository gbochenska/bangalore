dane <- read.csv("Bangalore.csv")
dane <- dane[1:1951, ]
dane <- na.omit(dane)


#Tworzymy zbiór uczący z połowy dostępnych 
#obserwacji — reszta będzie stanowić zbiór walidacyjny. 
#Dla zapewnienia powtarzalności obliczeń stosujemy funkcję set.seed.
set.seed(1)
n <- nrow(dane)
train <- sample(n, n / 2)

#opasowujemy model liniowy na zbiorze uczącym, następnie obliczamy MSE
#dla zbioru walidacyjnego.
Auto_lm <- lm(Price ~ Area, data = dane, subset = train)
validation_set <- dane[-train,]
mse <- mean((validation_set$Price - predict(Auto_lm, validation_set))^2)
mse

# Powtarzamy to samo dla regresji wielomianowej wyższych stopni
for (i in 2:5) {
  Auto_lm_poly <- lm(Price ~ poly(Area, degree = i), data = dane, 
                     subset = train)
  print(mean((validation_set$Price - predict(Auto_lm_poly, validation_set))^2))
}

#Powtarzamy obliczenia dla innego zbioru walidacyjnego.
set.seed(2)
train <- sample(n, n / 2)
validation_set <- dane[-train,]
degree_max <- 5
mse <- rep(0, times = degree_max)
for (i in 1:degree_max) {
  Auto_lm <- lm(Price ~ poly(Area, degree = i), data = dane, subset = train)
  mse[i] <- mean((validation_set$Price - predict(Auto_lm, validation_set))^2)
}
mse


plot(mse, xlab = "Stopień wielomianu", ylab = "MSE", type = "b", pch = 20, 
     col = "blue")

library(boot)
compute_loocv_mse <- function(degree) {
  Auto_glm <- glm(Price ~ poly(Area, degree), data = dane)
  cv.glm(dane, Auto_glm)$delta[1]
}
mse <- sapply(1:degree_max, compute_loocv_mse)
mse

plot(mse, xlab = "Stopień wielomianu", ylab = "LOOCV MSE", type = "b", pch = 20, 
     col = "blue")


#k-krotna walidacja krzyżowa
compute_kcv_mse <- function(degree, k) {
  Auto_glm <- glm(Price ~ poly(Area, degree), data = dane)
  cv.glm(dane, Auto_glm, K = k)$delta[1]
}
mse <- sapply(1:degree_max, compute_kcv_mse, k = 10)
mse
matplot(mse10, pch = 20, type = "l",
        xlab = "Stopień wielomianu", ylab = "Walidacyjny MSE")



#Bootstrap
lm_coefs <- function(data, index = 1:nrow(data)) {
  coef(lm(Price ~ Area, data = dane, subset = index))
}

n <- nrow(dane)
lm_coefs(dane, sample(n, n, replace = TRUE))

lm_coefs(dane)

#Obliczenie błędów standardowych metodą bootstrap z 1000 replikacji wygląda następująco.
boot(dane, lm_coefs, R = 1000)