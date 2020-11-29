library(party)
library(rpart.plot)
library(rpart)
# LOADING DATA
set.seed(1234)
data <- read.table("datos_icb.txt", sep=" ", header=TRUE, stringsAsFactors = TRUE)

# CROSS VALIDATION
ind = sample(1:2, nrow(data), replace=TRUE, prob=(c(0.8,0.2)))
training_set = data[ind == 1,]
validation_set = data[ind == 2,]

# SETTING DEPENDENCIES
formula = training_set$recid ~ .

# PARTY IMPLEMENTATION:

tree_party = ctree(formula, data = training_set)
plot(tree_party)

prediction = predict(tree_party, newdata = validation_set, type = "response")
prediction
matrizConfusionParty = table(prediction, validation_set$recid)
matrizConfusionParty
accuracyParty = sum(diag(matrizConfusionParty))/sum(matrizConfusionParty)
cat("Party: ",accuracyParty)

# RPART IMPLEMENTATION:
#
tree_rpart = rpart(formula, data = training_set,minbucket=1, minsplit=1, method = "class")

predictionRPart = predict(tree_rpart, type="class", newdata = validation_set)
matrizConfusionRpart = table(predictionRPart, validation_set$recid)
matrizConfusionRpart
accuracyRpart = sum(diag(matrizConfusionRpart))/sum(matrizConfusionRpart)
accuracyRpart

opt <- which.min(tree_rpart$cptable[,"xerror"])
cpmin <- tree_rpart$cptable[opt, "CP"]
treerpart_prune <- prune(tree_rpart, cp = cpmin)

predictionRPartPoda = predict(treerpart_prune, type="class", newdata = validation_set)
matrizConfusionRpartPoda = table(predictionRPartPoda, validation_set$recid)
matrizConfusionRpartPoda
accuracyRpartPoda = sum(diag(matrizConfusionRpartPoda))/sum(matrizConfusionRpartPoda)
accuracyRpartPoda

rpart.plot(tree_rpart)
rpart.plot(treerpart_prune)

