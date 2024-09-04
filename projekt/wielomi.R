
summary(fit_poly)
dane <- read.csv("Bangalore.csv")
dane <- dane[1:1951, ]

fit_poly <- lm(Price ~ poly(Area, 4), data = dane)
summary(fit_poly)

fit_poly_raw <- lm(Price ~ poly(Area, 4, raw = TRUE), data = dane)
summary(fit_poly_raw)


age_lims <- range(dane$Area)
age_grid <- seq(age_lims[1], age_lims[2])
pred_poly <- predict(fit_poly, list(Area = age_grid), se.fit = TRUE)
se_bands <- cbind(pred_poly$fit + 2 * pred_poly$se.fit, 
                  pred_poly$fit - 2 * pred_poly$se.fit)
plot(dane$Area, dane$Price, col = "darkgrey", cex = 0.5, xlim = age_lims)
lines(age_grid, pred_poly$fit, col = "red", lwd = 2)
matlines(age_grid, se_bands, col = "red", lty = "dashed")


data$HighPrice <- ifelse(data$Price > 100000)

# Przeprowadzenie regresji logistycznej z wielomianem do 4 potęgi dla zmiennej 'Area'
fit_log_poly <- glm(HighPrice ~ poly(Area, 4), data = data, family = binomial)

# Stworzenie siatki dla zmiennej 'Area'
area_grid <- seq(min(data$Area), max(data$Area), length.out = 100)

# Predykcja na siatce obszaru
pred_log_poly <- predict(fit_log_poly, list(Area = area_grid), se.fit = TRUE)
pred_probs <- plogis(pred_log_poly$fit)

# Obliczenie przedziałów ufności
se_bands_logit <- cbind(pred_log_poly$fit + 2 * pred_log_poly$se.fit,
                        pred_log_poly$fit - 2 * pred_log_poly$se.fit)
se_bands <- plogis(se_bands_logit)

# Wykreślenie wyników
plot(data$Area, data$HighPrice, xlim = range(data$Area), ylim = c(0, 1), 
     col = "darkgrey", cex = 0.5, ylab = "P(Price > 10 000 000 | Area)", xlab = "Area")
lines(area_grid, pred_probs, col = "red", lwd = 2)
matlines(area_grid, se_bands, lty = "dashed", col = "red")

# Dodanie szczegółów do wykresu
title("Regresja logistyczna dla cen nieruchomości w Bangalore")