library(rpart)
library(rpart.plot)


training_set <- read.table("entrenamiento.txt", sep=",", header=TRUE)
validation_set <- read.table("test.txt", sep=",", header=TRUE)

names(training_set)<-c("handicappedinfants",
                       "waterprojectcostsharing",
                       "adoptionofthebudgetresolution",
                       "physicianfeefreeze",
                       "elsalvadoraid",
                       "religiousgroupsinschools",
                       "antisatellitetestban",
                       "aidtonicaraguancontra",
                       "mxmissile",
                       "immigration",
                       "synfuelscorporationcutback",
                       "educationspending",
                       "superfundrighttosue",
                       "crime",
                       "dutyfreeexports",
                       "exportadministrationactsouthafrica", "votation")
names(validation_set)<-c("handicappedinfants",
               "waterprojectcostsharing",
               "adoptionofthebudgetresolution",
               "physicianfeefreeze",
               "elsalvadoraid",
               "religiousgroupsinschools",
               "antisatellitetestban",
               "aidtonicaraguancontra",
               "mxmissile",
               "immigration",
               "synfuelscorporationcutback",
               "educationspending",
               "superfundrighttosue",
               "crime",
               "dutyfreeexports",
               "exportadministrationactsouthafrica", "votation")


training_set$votation <- substr(training_set$votation, 1, nchar(training_set$votation)-1)
validation_set$votation <- substr(validation_set$votation, 1, nchar(validation_set$votation)-1)


#FORMULA
formula <- training_set$votation ~ handicappedinfants + waterprojectcostsharing + adoptionofthebudgetresolution +physicianfeefreeze+elsalvadoraid +religiousgroupsinschools +antisatellitetestban +aidtonicaraguancontra +mxmissile +immigration +synfuelscorporationcutback +educationspending +superfundrighttosue +crime +dutyfreeexports +exportadministrationactsouthafrica
#TREE
tree_rpart = rpart(formula, data = training_set, method = "class")
rpart.plot(tree_rpart)

predictionRPart = predict(tree, type="class", newdata = validation_set)
matrizConfusionRpart = table(predictionRPart, validation_set$votation)
matrizConfusionRpart
accuracyRpart = sum(diag(matrizConfusionRpart))/sum(matrizConfusionRpart)
accuracyRpart

#PRUNE
opt <- which.min(tree_rpart$cptable[,"xerror"])
cpmin <- tree_rpart$cptable[opt, "CP"]
treerpart_prune <- prune(tree_rpart, cp = cpmin)

predictionRPartPoda = predict(treerpart_prune, type="class", newdata = validation_set)
matrizConfusionRpartPoda = table(predictionRPartPoda, validation_set$votation)
matrizConfusionRpartPoda
accuracyRpartPoda = sum(diag(matrizConfusionRpartPoda))/sum(matrizConfusionRpartPoda)
accuracyRpartPoda

rpart.plot(tree_rpart)
rpart.plot(treerpart_prune)

