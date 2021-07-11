################ Import ###########################
source("fonctions.R")
data_mat <- read.csv("student-mat.csv", sep= ";")
data_por <- read.csv("student-por.csv", sep= ";")

############## creation moyenne ###################
data_mat$average_mark = (data_mat$G1 + data_mat$G2 + data_mat$G3)/3
data_por$average_mark = (data_por$G1 + data_por$G2 + data_por$G3)/3

############## creation matiere ###################
data <- rbind(data_mat, data_por)
data$subject <- "math"
data$subject[c(396:1044)] <- "portuguese"

############# nettoyage variable age ##############
data_mat <- clean(data_mat)
data_por <- clean(data_por)
data <- clean(data)

############ variable traveltime #################
data$traveltime[data$traveltime==1] <- "work < 15"
data$traveltime[data$traveltime==2] <- "work 15-30"
data$traveltime[data$traveltime==3] <- "work 30-1h"
data$traveltime[data$traveltime==4] <- "work > 1h"

############ variable studytime ##################
data$studytime[data$studytime==1] <- "study < 2h"
data$studytime[data$studytime==2] <- "study 2-5h"
data$studytime[data$studytime==3] <- "study 5-10h"
data$studytime[data$studytime==4] <- "study > 10h"

############ variable Medu #######################
data$Medu[data$Medu == "0"] <- "level 0"
data$Medu[data$Medu == "1"] <- "level 1"
data$Medu[data$Medu == "2"] <- "level 2"
data$Medu[data$Medu == "3"] <- "level 3"
data$Medu[data$Medu == "4"] <- "level 4"

########### variable Fedu ########################
data$Fedu[data$Fedu == "0"] <- "level 0"
data$Fedu[data$Fedu == "1"] <- "level 1"
data$Fedu[data$Fedu == "2"] <- "level 2"
data$Fedu[data$Fedu == "3"] <- "level 3"
data$Fedu[data$Fedu == "4"] <- "level 4"

########### variable famrel #####################
data$famrel[data$famrel == "1"] <- "Very bad"
data$famrel[data$famrel == "2"] <- "Bad"
data$famrel[data$famrel == "3"] <- "Average"
data$famrel[data$famrel == "4"] <- "Good"
data$famrel[data$famrel == "5"] <- "Very good"


########### variable classe #####################
for (i in 1:nrow(data)){
  if(data$age[i] - data$failures[i] == 15){data$class[i] <- "10th"}
  else if(data$age[i] - data$failures[i] == 16){data$class[i] <- "11th"}
  else if(data$age[i] - data$failures[i] %in% c(17, 18)){data$class[i] <- "12th"}
}

########### variable absence ####################
data$absences <- as.factor(cut(
  data$absences, c(0, 5, 10, 75), 
  labels = c("abs [0,5]", "abs ]5,10]", "abs ]10,75]"),
  include.lowest = TRUE)
  )

######### variable failures #####################
data$failures[data$failures==2] <- "2 or 3"
data$failures[data$failures==3] <- "2 or 3"

######## variable moyenne ###############
data$average_mark_factor <- cut(
  data$average_mark, c(0, 5, 10, 15, 20),
  labels=c("mark [0,05]", "mark ]05,10]", "mark ]10,15]", "mark ]15,20]")
  )

######### creation df que l'on va utiliser #####################
data_cleaned_classification <- data[, -c(3, 31:34)]
for (i in 1:31){data_cleaned_classification[, i] <- as.factor(data_cleaned_classification[, i])}
