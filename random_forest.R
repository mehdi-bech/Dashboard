
                              # Modèle 2 : Random Forest

# Importation de la base de données prétraitée

data = read.csv('Data\\Data_encoded.csv')

# Division de l'ensemble de données en un ensemble d'entraînement et un ensemble de test 

library(caTools)
set.seed(123)
split = sample.split(data$Inflammation.of.urinary.bladder, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Ajustement de la random forest à l'ensemble d'entraînement

library(randomForest)
classifier2 = randomForest( x = training_set[-7],
                            y = training_set$Inflammation.of.urinary.bladder,
                            ntree = 10 )

summary(classifier2)

# Prédiction des résultats de l'ensemble de test

y_pred = predict(classifier2, newdata = test_set[-7])


# Construction de la matrice de confusion 

true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)

true_pos = (true_y==1) & (y_pred==1)
true_neg = (true_y==0) & (y_pred==0)
false_pos = (true_y==0) & (y_pred==1)
false_neg = (true_y==1) & (y_pred==0)

conf_mat_2 = matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)

colnames(conf_mat_2) = c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_2) = c('Y = 1', 'Y = 0')

# Precision (TP / (TP + FP))

precision = conf_mat_2[1, 1] / sum(conf_mat_2[,1])

# Recall (TP / (TP + FN))

recal = conf_mat_2[1, 1] / sum(conf_mat_2[1,])

# F-score (TN / (TN + FP))

f1_score = 2* ((precision*recal) / (precision+recal))

# Accuracy ( (TP + TN) / (TP + FP + TN + FN) )

accuracy = (conf_mat_2[1,1]+conf_mat_2[2,2]) / (conf_mat_2[1,1]+conf_mat_2[2,1]+
                                                  conf_mat_2[2,2]+conf_mat_2[1,2])

# Courbe ROC

library(pROC)
par (pty = "s")
roc.info2 = roc(test_set[,7], y_pred, 
                plot= TRUE, 
                col= "#377eb8", 
                lwd = 3,
                main ="ROC curve -- Random Forest ",
                percent = TRUE) 

# La métrique AUC 

    # Calcul de l'intégration numérique du AUC

roc_df2 = data.frame(
        TP = roc.info2$sensitivities*100,
        FP = (1-roc.info2$sensitivities)*100,
        thresholds = roc.info2$thresholds)

    # Illustration de la valeur AUC

AUC2 = roc(test_set[,7], y_pred, 
           plot= TRUE, 
           col= "#377eb8", 
           lwd = 3,
           print.auc = TRUE,
           print.auc.x= 45,
           main ="AUC illustration",
           percent = TRUE,
           partial.auc = c(100, 0),
           auc.polygon = TRUE,
           auc.polygon.col = "#377eb822")

