###################################### Packages ####################################################
library(doParallel)
library(kableExtra)
library(readxl)
library(caret)
library(randomForest)
library(rsample)

setwd("D:/Memoire/application")

################################ Marks in Portugal ################################################
data_classification <- read_excel("data_classification.xlsx")

data_classification$average_mark_factor[data_classification$average_mark_factor=="mark [0,05]"] <-
  "mark [0,10]"
data_classification$average_mark_factor[data_classification$average_mark_factor=="mark ]05,10]"] <-
  "mark [0,10]"
data_classification$average_mark_factor[data_classification$average_mark_factor=="mark ]10,15]"] <-
  "mark [10,20]"
data_classification$average_mark_factor[data_classification$average_mark_factor=="mark ]15,20]"] <-
  "mark [10,20]"

for(i in 1:32){
  i <- as.integer(i)
  data_classification[[i]] <- as.factor(data_classification[[i]])
}

set.seed(3)
data_classification_split <- data_classification %>% initial_split(prop = 0.8)
test <- data_classification_split %>% testing()
train <- data_classification_split %>% training()

## CV random forest
cl <- detectCores() %>% -1 %>% makeCluster
registerDoParallel(cl)

rfGrid <- expand.grid(mtry = c(2, 4, 8, 12, 16))
ctrlCv <- trainControl(method = "repeatedcv", repeats = 100, number = 5)
rf.caret <- train(
  average_mark_factor ~ ., data = train, method = "rf", na.action = na.omit,
  trControl = ctrlCv, tuneGrid = rfGrid
)

rf_best_classification <- rf.caret

stopImplicitCluster()

save(rf_best_classification, file="best_model_random_forest_classification.RData")


################################ German cars #####################################################
data_regression <- read_excel("data_regression.xlsx")
data_regression_bis <- data_regression[,-c(8)]
data_regression_bis$price <- as.integer(data_regression_bis$price)

for(i in c(2:6, 8)){
  i <- as.integer(i)
  data_regression_bis[[i]] <- as.factor(data_regression_bis[[i]])
}

set.seed(3)

data_regression_split <- data_regression_bis[,-c(2,3)] %>% initial_split(prop = 0.75)

test_regression <- data_regression_split %>% testing()
train_regression <- data_regression_split %>% training()

## CV random forest
cl <- detectCores() %>% -1 %>% makeCluster
registerDoParallel(cl)

rfGrid_regression <- expand.grid(mtry = c(2, 4, 6, 7))
ctrlCv_regression <- trainControl(method = "repeatedcv", repeats = 10, number = 5)
rf.caret_regression <- train(
  price ~ ., data = train_regression, method = "rf", na.action = na.omit,
  trControl = ctrlCv_regression, tuneGrid = rfGrid_regression
)

rf_optimal_regression <- rf.caret_regression

stopImplicitCluster()

save(rf_optimal_regression, file="best_model_random_forest_regression.RData")
