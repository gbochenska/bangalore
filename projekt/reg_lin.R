# Wczytanie danych
dane <- read.csv("Bangalore.csv") # Zakładając, że dane znajdują się w pliku CSV


names(dane)
dim(dane)
?dane
head(dane)

fit_simple <- lm(Price ~ Area, data = dane)
summary(fit_simple)
# Albo to jest szersze zastosowanie
attach(dane)
# Dopasowanie modelu liniowego
fit_simple <- lm(Price ~ Area)
# Odłączenie danych
detach(dane)

fit_simple
class(fit_simple)
is.list(fit_simple)
names(fit_simple)

fit_simple$coefficients
coef(fit_simple)

?summary.lm
summary(fit_simple)

summaryList <- summary(fit_simple)
summaryList$sigma
summaryList$r.squared
summaryList$fstatistic

confint(fit_simple)

predict(fit_simple, newdata = data.frame(Area = c(5, 10, 15)), interval = "confidence")

predict(fit_simple, newdata = data.frame(Area = c(5, 10, 15)), interval = "prediction")





par(mfrow = c(2, 2))
plot(dane$Area, dane$Price, 
     xlab = "Powierzchnia", ylab = "Cena mieszkania",
     main = "Prosta regresja liniowa: Cena mieszkania od powierzchni")
abline(fit_simple, , col = "red")
plot(predict(fit_simple), residuals(fit_simple),
     xlab = "Przewidywane wartości", ylab = "Reszty",
     main = "Wykres reszt vs. wartości przewidywanych")
plot(predict(fit_simple), rstudent(fit_simple),
     xlab = "Przewidywane wartości", ylab = "Studentyzowane reszty",
     main = "Wykres studentyzowanych reszt vs. wartości przewidywanych")
plot(hatvalues(fit_simple), 
     xlab = "Indeks obserwacji", ylab = "Wartość hat",
     main = "Identyfikacja obserwacji wpływowych")
which.max(hatvalues(fit_simple))





# Dopasowanie modelu regresji wielokrotnej
fit_la <- lm(Price ~ Area + Bedrooms, data = dane)
summary(fit_la)

fit_all <- lm(Price ~ ., data = dane)
summary(fit_all)

fit_no_age <- lm(Price ~ . - Bedrooms, data = dane)
summary(fit_no_age)


fit_no_age2 <- update(fit_all, ~ . - Bedrooms)
summary(fit_no_age2)

library(ellipse)
plot(ellipse(fit_la, which = -1), type = "l")
la_coefs <- coef(fit_la)
points(la_coefs[2], la_coefs[3])

summary(lm(Price ~ Area * Bedrooms, data = dane))



fit_l2 <- lm(Price ~ Area + I(Area^2), data = dane)
summary(fit_l2)
anova(fit_simple, fit_l2)
fit_l5 <- lm(Price ~ poly(Area, 5), data = dane)
summary(fit_l5)
summary(lm(Price ~ log(Area), data = dane))




dane_bez_None <- dane %>%
  filter(!apply(., 1, function(row) any(grepl("9", row))))
dane_bez_None$MaintenanceStaff <- as.factor(dane_bez_None$MaintenanceStaff)

sales_all_ia_fit <- lm(Price ~ . + Area:Gymnasium, data = dane_bez_None)
summary(sales_all_ia_fit)
contrasts(dane_bez_None$MaintenanceStaff)