
                              # Modèle 3 : SVM

# Importation de la base de données prétraitée

data = read.csv('Data\\Data_encoded.csv')

# Division de l'ensemble de données en un ensemble d'entraînement et un ensemble de test 

library(caTools)
set.seed(123)
split = sample.split(data$Inflammation.of.urinary.bladder, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Ajustement de la SVM à l'ensemble d'entraînement

library(e1071)
classifier3 = svm( formula = Inflammation.of.urinary.bladder ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'polynomial')

summary(classifier3)

# Prédiction des résultats de l'ensemble de test

y_pred = predict(classifier3, newdata = test_set[-7])

# Construction de la matrice de confusion 

true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)

true_pos = (true_y==1) & (y_pred==1)
true_neg = (true_y==0) & (y_pred==0)
false_pos = (true_y==0) & (y_pred==1)
false_neg = (true_y==1) & (y_pred==0)

conf_mat_3 = matrix(c(sum(true_pos), sum(false_pos),
                       sum(false_neg), sum(true_neg)), 2, 2)

colnames(conf_mat_3) = c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_3) = c('Y = 1', 'Y = 0')

# Précision (TP / (TP + FP))

precision = conf_mat_3[1, 1] / sum(conf_mat_3[,1])

# Recall "sensibilité" (TP / (TP + FN))

recal = conf_mat_3[1, 1] / sum(conf_mat_3[1,])

# F-score "specificité" (TN / (TN + FP))

f1_score = 2* ((precision*recal) / (precision+recal))

# Accuracy ( (TP + TN) / (TP + FP + TN + FN) )

accuracy = (conf_mat_3[1,1]+conf_mat_3[2,2]) / (conf_mat_3[1,1]+conf_mat_3[2,1]+
                                                conf_mat_3[2,2]+conf_mat_3[1,2])

# Courbe ROC

library(pROC)
roc_score = roc(test_set[,7], y_pred) 
plot (roc_score ,main ="ROC curve -- SVM")

# La métrique AUC 

    # Calcul de l'intégration numérique du AUC

sum(roc_df_3$recall_3[-1] * diff(1-roc_df_3$specificity_3))
head(roc_df_3)

    # Illustration de la valeur AUC

AUC3 <- ggplot(roc_df_3, aes(specificity_3)) +
  geom_ribbon(aes(ymin=0, ymax=recall_2), fill='blue', alpha=.3) +
  scale_x_reverse(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y='recall') +
  theme_bw() + theme(plot.margin=unit(c(5.5, 10, 5.5, 5.5), "points"))
