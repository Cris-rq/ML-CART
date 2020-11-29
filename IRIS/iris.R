library(party)
library(rpart)
library(rpart.plot)
set.seed(1234)

#CREAR DATA
indices <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7,0.3)) #Sampleado de 2 indices, tamaño filas de iris, replace=true es que un elem no se repite 2 veces, probabilidad 70 y un 30 en cada indice
train <- iris[indices == 1,] #coge este indice en las filas
validation <- iris[indices == 2,]


#FORMULA
formula <- train$Species ~ .

#TREE PARTY
tree_party <- ctree(formula, data = train)
plot(tree_party)

predictionPARTY <- predict(tree_party, newdata = validation)
ConfusionMatrixPARTY <- table(predictionPARTY, validation$Species)
accuracyParty = sum(diag(ConfusionMatrixPARTY))/sum(ConfusionMatrixPARTY)
accuracyParty

#TREE RPART
rpart_tree <- rpart(formula, data = train, method = "class")
rpart.plot(rpart_tree0)

predictionRPart = predict(rpart_tree, type="class", newdata = validation)
matrizConfusionRpart = table(predictionRPart, validation$Species)

accuracyRpart= sum(diag(matrizConfusionRpart))/sum(matrizConfusionRpart)
accuracyRpart

#FIND MINIM MINBUCKET Y MINSPLIT
limit_param1 = 30
limit_param2 = 30
max_accuracy = 0
for (n in 1:limit_param1) {
  for(m in 1:limit_param2){
    tree = rpart(formula, train, method="class", minbucket=n, minsplit=m)
    
    optimal_cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
    pruned_tree = prune(tree, cp=optimal_cp)
    
    confusion_matrix_pruned = table(predict(pruned_tree, newdata=validation, type="class"), validation$Species)
    val_accuracy = sum(diag(confusion_matrix_pruned))/sum(confusion_matrix_pruned)
    
    if(val_accuracy > max_accuracy){
      max_accuracy = val_accuracy
      best_tree = pruned_tree
      param_minbucket = n
      param_minsplit = m
    }
  }
}
