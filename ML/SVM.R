
                              # Modèle 3 : SVM

# Description Régression logistique

context3 = function(){
  c =   " La colonne de la variable 'Inflammation of urinary bladder' est composée de deux modalités :
      (yes , no). L'objectif ultime est de diagnostiquer l'inflammation de la vessie 
      conditionnée par les symptômes du patient. Pour ce faire, nous allons appliquer
      la méthode Machine à vecteurs de support (SVM) :"
  return(c)
}

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

# Prédiction des résultats de l'ensemble de test

y_pred3 = predict(classifier3, newdata = test_set[-7])

# Construction de la matrice de confusion 

true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)

true_pos = (true_y==1) & (y_pred3 ==1)
true_neg = (true_y==0) & (y_pred3 ==0)
false_pos = (true_y==0) & (y_pred3 ==1)
false_neg = (true_y==1) & (y_pred3 ==0)

conf_mat_3 = matrix(c(sum(true_pos), sum(false_pos),
                       sum(false_neg), sum(true_neg)), 2, 2)

colnames(conf_mat_3) = c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_3) = c('Y = 1', 'Y = 0')

# Fonction matrice de confusion

cm3 = function (){
  return(cbind(' '= c("Y=1", "Y=0"),conf_mat_3))
}

# Les métriques

pre3 = function(){
  # Precision (TP / (TP + FP))
  precision3 = conf_mat_3[1, 1] / sum(conf_mat_3[,1])
  return (precision3) 
}

rec3 = function(){    
  # Recall (TP / (TP + FN))
  recal3 = conf_mat_3[1, 1] / sum(conf_mat_3[1,])
  return (recal3)
}

fsco3 = function(){
  # F-score (2*((precision*recal) / (precision+recal))
  precision3 = conf_mat_3[1, 1] / sum(conf_mat_3[,1])
  recal3 = conf_mat_3[1, 1] / sum(conf_mat_3[1,])
  f1_score3 = 2* ( (precision3 * recal3) / (precision3 + recal3) )
  return (f1_score3)
}

spe3 = function(){
  
  # Spécifité ( TN /  (TN + FP) )
  specificite3 = conf_mat_3[2,2] / (conf_mat_3[2,2]+conf_mat_3[2,1])
  return(specificite3)
}

acc3 = function(){
  # Accuracy ( (TP + TN) / (TP + FP + TN + FN) )
  accuracy3 = (conf_mat_3[1,1]+conf_mat_3[2,2]) / (conf_mat_3[1,1]+conf_mat_3[2,1]+
                                                     conf_mat_3[2,2]+conf_mat_3[1,2])
  return(accuracy3)
}

table_metr3 = function(){
  t = matrix(c('précision','sensibilité','F1-score','spécificité','accuracy', pre3(),rec3(),fsco3(),spe3(),acc3()),5,2)
  colnames(t)= c('Métrique','Valeur')
  return(t)
}

# Fonction ROC

library(pROC)

roc3 = function(){ 

  y_pred3 = predict(classifier3, newdata = test_set[-7])
  y3 = ifelse(as.numeric( y_pred3) > 1.5, 1, 0)

  library(pROC)
  par (pty = "s")
  r3 = roc(test_set[,7], y3, 
           plot= TRUE, 
           col= "#C60800", 
           lwd = 3,
           main ="ROC curve -- SVM ",
           percent = TRUE)
  return(r3)
} 

# Fonction AUC

auc3 = function(){ 
  
  y_pred3 = predict(classifier3, newdata = test_set[-7])
  y3 = ifelse(as.numeric( y_pred3) > 1.5, 1, 0)
  
  par (pty = "s")
  a3 = roc(test_set[,7], y3, 
           plot= TRUE, 
           col= "#C60800", 
           lwd = 3,
           print.auc = TRUE,
           print.auc.x= 45,
           main ="AUC illustration",
           percent = TRUE,
           partial.auc = c(100, 0),
           auc.polygon = TRUE,
           auc.polygon.col = "#C6080022")
  return(a3)
}