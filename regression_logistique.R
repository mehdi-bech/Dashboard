
                              # Modèle 1 : Régression Logistique

# Importation de la base de données prétraitée

data = read.csv('Data\\Data_encoded.csv')

# Division de l'ensemble de données en un ensemble d'entraînement et un ensemble de test 

library(caTools)
set.seed(123)
split = sample.split(data$Inflammation.of.urinary.bladder, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Ajustement de la régression logistique à l'ensemble d'entraînement

library(mgcv)
classifier1 = gam( formula = Inflammation.of.urinary.bladder ~ Temperature.of.patient 
                 + Occurrence.of.nausea + Lumbar.pain + Urine.pushing + Micturition.pains 
                 + Burning.of.urethra,
               family = binomial("logit"),
               data = training_set )

coef(classifier1)

# Prédiction des résultats de l'ensemble de test

prob_pred = predict(classifier1, type="response", newdata = test_set[-7])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Construction de la matrice de confusion 

true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)

true_pos = (true_y==1) & (y_pred==1)
true_neg = (true_y==0) & (y_pred==0)
false_pos = (true_y==0) & (y_pred==1)
false_neg = (true_y==1) & (y_pred==0)

conf_mat_1 = matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)

colnames(conf_mat_1) = c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_1) = c('Y = 1', 'Y = 0')

# Precision (TP / (TP + FP))

precision = conf_mat_1[1, 1] / sum(conf_mat_1[,1])

# Recall (TP / (TP + FN))

recal = conf_mat_1[1, 1] / sum(conf_mat_1[1,])

# F-score (TN / (TN + FP))

f1_score = 2* ((precision*recal) / (precision+recal))

# Accuracy ( (TP + TN) / (TP + FP + TN + FN) )

accuracy = (conf_mat_1[1,1]+conf_mat_1[2,2]) / (conf_mat_1[1,1]+conf_mat_1[2,1]+
                                                  conf_mat_1[2,2]+conf_mat_1[1,2])

# Courbe ROC

library(pROC)
roc_score = roc(test_set[,7], y_pred) 
plot(roc_score ,main ="ROC curve -- Régression Logistique ", col= "blue")

# La métrique AUC 

    # Calcul de l'intégration numérique du AUC

sum(roc_df_1$recall_1[-1] * diff(1-roc_df_1$specificity_1))
head(roc_df_1)

    # Illustration de la valeur AUC

AUC1 <- ggplot(roc_df_1, aes(specificity_1)) +
  geom_ribbon(aes(ymin=0, ymax=recall_1), fill='blue', alpha=.3) +
  scale_x_reverse(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y='recall') +
  theme_bw() + theme(plot.margin=unit(c(5.5, 10, 5.5, 5.5), "points"))

