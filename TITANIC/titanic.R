library(rpart)
library(rpart.plot)

#
data <- read.csv("train.csv", stringsAsFactors = TRUE, header = TRUE)
ind = sample(1:2, nrow(data), replace=TRUE, prob=(c(0.8,0.2)))
training_set = data[ind == 1,]
validation_set = data[ind == 2,]

#FORMULA
formula <- training_set$Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked


#FIND MINIM MINBUCKET Y MINSPLIT
limit_param1 = 30
limit_param2 = 30
max_accuracy = 0
for (n in 1:limit_param1) {
  for(m in 1:limit_param2){
    tree = rpart(formula, data=training_set, method="class", minbucket=n, minsplit=m)
    
    optimal_cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
    pruned_tree = prune(tree, cp=optimal_cp)
    
    predict = predict(pruned_tree, type="class", newdata = validation_set)
    confusion_matrix_pruned = table(predict, validation_set$Survived)
    val_accuracy = sum(diag(confusion_matrix_pruned))/sum(confusion_matrix_pruned)
    
    if(val_accuracy > max_accuracy){
      max_accuracy = val_accuracy
      best_tree = pruned_tree
      param_minbucket = n
      param_minsplit = m
    }
  }
}

#TREE WITH RPART
tree_rpart = rpart(formula, data = training_set,minbucket=param_minbucket, minsplit=param_minsplit, method = "class")
predictionRPart = predict(tree_rpart, type="class", newdata = validation_set)
matrizConfusionRpart = table(predictionRPart, validation_set$Survived)
matrizConfusionRpart
accuracyRpart = sum(diag(matrizConfusionRpart))/sum(matrizConfusionRpart)
accuracyRpart

rpart.plot(tree_rpart)

#PRUNE
opt <- which.min(tree_rpart$cptable[,"xerror"])
cpmin <- tree_rpart$cptable[opt, "CP"]
treerpart_prune <- prune(tree_rpart, cp = cpmin)

predictionRPartPoda = predict(treerpart_prune, type="class", newdata = validation_set)
matrizConfusionRpartPoda = table(predictionRPartPoda, validation_set$Survived)
matrizConfusionRpartPoda
accuracyRpartPoda = sum(diag(matrizConfusionRpartPoda))/sum(matrizConfusionRpartPoda)
accuracyRpartPoda

rpart.plot(tree_rpart)
rpart.plot(treerpart_prune)
