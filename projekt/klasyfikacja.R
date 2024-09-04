library(MASS)
library(class)

dane <- read.csv("Bangalore.csv")
dane <- dane[1:1951, ]
head(dane)

numerical_columns <- sapply(dane, is.numeric)
correlation_matrix <- cor(dane[, numerical_columns])
print(correlation_matrix)

plot(dane$Area, main="Area of Properties", xlab="Index", ylab="Area")



# Regresja logistyczna
dir_logistic <- glm(SwimmingPool ~ 
                      MaintenanceStaff + Gymnasium +  
                      LandscapedGardens + JoggingTrack + 
                      RainWaterHarvesting + IndoorGames + ShoppingMall + 
                      Intercom + SportsFacility + ATM + ClubHouse + 
                      School + X24X7Security + PowerBackup + CarParking + 
                      StaffQuarter + Cafeteria + MultipurposeRoom + 
                      Hospital + WashingMachine + Gasconnection + AC + 
                      Childrensplayarea + LiftAvailable + BED + 
                      VaastuCompliant + GolfCourse, 
                    family = binomial, data = dane)
summary(dir_logistic)

dir_logistic$probs <- predict(dir_logistic, type = "response")
head(dir_logistic$probs)

dane$SwimmingPool <- as.factor(dane$SwimmingPool)
contrasts(dane$SwimmingPool)

dir_logistic$predicted <- ifelse(dir_logistic$probs > 0.5, "Up", "Down")

dir_logistic$cm <- table(dir_logistic$predicted, dane$SwimmingPool)
dir_logistic$cm

(dir_logistic$cm[1, 2] + dir_logistic$cm[2, 1]) / sum(dir_logistic$cm)
mean(dir_logistic$predicted != dane$SwimmingPool)


set.seed(123)
train_indices <- sample(1:nrow(dane), size = 0.7 * nrow(dane))
train_data <- dane[train_indices, ]
test_data <- dane[-train_indices, ]
Direction_test <- dane$SwimmingPool[-train_indices]

# Fit the logistic regression model
dir_log_t <- list()
dir_log_t$fit <- glm(SwimmingPool ~ Price + Area +
                       Bedrooms + Resale +
                       MaintenanceStaff + Gymnasium +  
                       LandscapedGardens + JoggingTrack + 
                       RainWaterHarvesting + IndoorGames + ShoppingMall + 
                       Intercom + SportsFacility + ATM + ClubHouse + 
                       School + X24X7Security + PowerBackup + CarParking + 
                       StaffQuarter + Cafeteria + MultipurposeRoom + 
                       Hospital + WashingMachine + Gasconnection + AC + 
                       Childrensplayarea + LiftAvailable + BED + 
                       VaastuCompliant + GolfCourse, 
                     family = binomial, data = train_data)

# Print the summary of the model
summary(dir_log_t$fit)
dir_log_t$probs <- predict(dir_log_t$fit, newdata = test_data, type = "response")
dir_log_t$predicted <- ifelse(dir_log_t$probs > 0.5, 1, 0)
table(dir_log_t$predicted, Direction_test)


# Fit the logistic regression model
dir_log_best2 <- list()
dir_log_best2$fit <- glm(SwimmingPool ~ Price + Area + 
                       Resale + Gymnasium +  
                       LandscapedGardens + JoggingTrack + 
                       RainWaterHarvesting + IndoorGames + 
                       Intercom + SportsFacility + ClubHouse + 
                       X24X7Security + PowerBackup + 
                       StaffQuarter + MultipurposeRoom + 
                       Gasconnection +
                       Childrensplayarea, 
                     family = binomial, data = train_data)

# Print the summary of the model
summary(dir_log_best2$fit)
dir_log_best2$probs <- predict(dir_log_best2$fit, newdata = test_data, type = "response")
dir_log_best2$predicted <- ifelse(dir_log_best2$probs > 0.5, 1, 0)
table(dir_log_best2$predicted, Direction_test)


mean(Direction_test != "Up")



#LDA
dir_lda <- list()
dir_lda$fit <- lda(SwimmingPool ~ Price + Area + Resale + Gymnasium +  
                     LandscapedGardens + JoggingTrack + 
                     RainWaterHarvesting + IndoorGames + Intercom + 
                     SportsFacility + ClubHouse + X24X7Security + 
                     PowerBackup + StaffQuarter + MultipurposeRoom + 
                     Gasconnection + Childrensplayarea, 
                   data = dane)
dir_lda$fit


dir_lda$predicted <- predict(dir_lda$fit, test_data)
table(dir_lda$predicted$class, Direction_test)


max(dir_lda$predicted$posterior[, 2])
max(dir_lda$predicted$posterior[, 1])




#QDA
dir_qda <- list()
dir_qda$fit <- qda(SwimmingPool ~ Price + Area + Resale + Gymnasium +  
                     LandscapedGardens + JoggingTrack + 
                     RainWaterHarvesting + IndoorGames + Intercom + 
                     SportsFacility + ClubHouse + X24X7Security + 
                     PowerBackup + StaffQuarter + MultipurposeRoom + 
                     Gasconnection + Childrensplayarea, 
                   data = dane)
dir_qda$fit


dir_qda$predicted <- predict(dir_qda$fit, test_data)
table(dir_qda$predicted$class, Direction_test)


dane1 = na.omit(dane)
Direction_test <- dane1$SwimmingPool[-train_indices]
