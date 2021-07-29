################## Importation of the databases and random forests ##################################
require(readxl)
require(rsample)
require(randomForest)
require(caret)
require(dplyr)

## databases
tryCatch(
  expr={
    print("D:/Memoire/rendu_final_code/ -> application charged") ; setwd("D:/Memoire/rendu_final_code")},
  error=function(e){print("C:/Perso/Memoire/ -> application charged") ; setwd("C:/Perso/Memoire/application")}
)

data_regression <- read_excel("data_regression.xlsx")
data_classification <- read_excel("data_classification.xlsx")


############################## Marks in Portugal dataset ###########################################
data_classification$average_mark_factor[
  data_classification$average_mark_factor=="mark [0,05]"
  ] <- "mark [0,10]"
data_classification$average_mark_factor[
  data_classification$average_mark_factor=="mark ]05,10]"
  ] <- "mark [0,10]"
data_classification$average_mark_factor[
  data_classification$average_mark_factor=="mark ]10,15]"
  ] <- "mark [10,20]"
data_classification$average_mark_factor[
  data_classification$average_mark_factor=="mark ]15,20]"
  ] <- "mark [10,20]"

for(i in 1:32){
  i <- as.integer(i)
  data_classification[[i]] <- as.factor(data_classification[[i]])
}
data_classification <- droplevels(data_classification)

set.seed(3)
## split the dataframe in 2 with test and train data
data_classification_split <- data_classification %>% initial_split(prop = 0.8)
test <- data_classification_split %>% testing()
train <- data_classification_split %>% training()


## random forest
rf_best_classification_bis <- 
  randomForest(
    average_mark_factor ~ ., data=train, method = "class", ntree = 500,
    parms = list(split = "gini"), mtry = 16, na.action = na.omit
    )



############################## German cars dataset ################################################
## I removed the feature hp because it was highly correlated with the feature price, 
## which could lead to problems for certain methods
data_regression_bis <- data_regression[,-c(8)]
data_regression_bis$price <- as.integer(data_regression_bis$price)

for(i in c(2:6, 8)){
  i <- as.integer(i)
  data_regression_bis[[i]] <- as.factor(data_regression_bis[[i]])
}

set.seed(3)
## split the dataframe in 2 with test and train data
data_regression_split <- data_regression_bis[,-c(2,3)] %>% initial_split(prop = 0.75)
test_regression <- data_regression_split %>% testing()
train_regression <- data_regression_split %>% training()


do_rf = FALSE
if(do_rf){
  require(doParallel)
  cl <- detectCores() %>% -1 %>% makeCluster
  registerDoParallel(cl)
  
  rf_optimal_regression_bis <- 
    randomForest(
      price ~ ., data=train_regression, method = "class", ntree = 500,
      parms = list(split = "gini"), mtry = 7, na.action = na.omit
      )
  
  stopImplicitCluster()
  
  save(rf_optimal_regression_bis, file="rf_model_regression_PDP.RData")
}else{
  load("D:/Memoire/rendu_final_code/rf_model_regression_PDP.RData")
}
