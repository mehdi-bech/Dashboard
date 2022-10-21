
                              # Modèle 2 : Random Forest

# Description Random Forest

context2 = function(){
  c =   " La colonne de la variable 'Inflammation of urinary bladder' est composée de deux modalités :
      (yes , no). L'objectif ultime est de diagnostiquer l'inflammation de la vessie 
      conditionnée par les symptômes du patient. Pour ce faire, nous allons appliquer
      la méthode de la Forêt d'arbres de décision (Random Forest)"
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

# Ajustement de la random forest à l'ensemble d'entraînement

# Construction de la matrice de confusion

# Fonction matrice de confusion

library(randomForest)

cm2 = function (k){

  classifierk = randomForest( x = training_set[-7],
                              y = training_set$Inflammation.of.urinary.bladder,
                              ntree = k)
  
  y_predk = predict(classifierk, newdata = test_set[-7])
  yk = ifelse(y_predk > 0.5, 1, 0)
  
  true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)
  
  true_pos = (true_y==1) & (yk==1)
  true_neg = (true_y==0) & (yk==0)
  false_pos = (true_y==0) & (yk==1)
  false_neg = (true_y==1) & (yk==0)
  
  conf_mat_k = matrix(c(sum(true_pos), sum(false_pos),
                        sum(false_neg), sum(true_neg)), 2, 2)
  
  colnames(conf_mat_k) = c('Yhat = 1', 'Yhat = 0')
  rownames(conf_mat_k) = c('Y = 1', 'Y = 0')
  
  return(cbind(' '= c("Y=1", "Y=0"),conf_mat_k))
}

# Fonction des caracteristiques

pre2 = function(k){
  classifierk = randomForest( x = training_set[-7],
                              y = training_set$Inflammation.of.urinary.bladder,
                              ntree = k)
  
  y_predk = predict(classifierk, newdata = test_set[-7])
  yk = ifelse(y_predk > 0.5, 1, 0)
  
  true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)
  
  true_pos = (true_y==1) & (yk==1)
  true_neg = (true_y==0) & (yk==0)
  false_pos = (true_y==0) & (yk==1)
  false_neg = (true_y==1) & (yk==0)
  
  conf_mat_k = matrix(c(sum(true_pos), sum(false_pos),
                        sum(false_neg), sum(true_neg)), 2, 2)
  # Precision (TP / (TP + FP))
  precision2 = conf_mat_k[1, 1] / sum(conf_mat_k[,1])
  return (precision2) 
}

rec2 = function(k){   
  classifierk = randomForest( x = training_set[-7],
                              y = training_set$Inflammation.of.urinary.bladder,
                              ntree = k)
  
  y_predk = predict(classifierk, newdata = test_set[-7])
  yk = ifelse(y_predk > 0.5, 1, 0)
  
  true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)
  
  true_pos = (true_y==1) & (yk==1)
  true_neg = (true_y==0) & (yk==0)
  false_pos = (true_y==0) & (yk==1)
  false_neg = (true_y==1) & (yk==0)
  
  conf_mat_k = matrix(c(sum(true_pos), sum(false_pos),
                        sum(false_neg), sum(true_neg)), 2, 2)
  # Recall (TP / (TP + FN))
  recal2 = conf_mat_k[1, 1] / sum(conf_mat_k[1,])
  return (recal2)
}

fsco2 = function(k){
  classifierk = randomForest( x = training_set[-7],
                              y = training_set$Inflammation.of.urinary.bladder,
                              ntree = k)
  
  y_predk = predict(classifierk, newdata = test_set[-7])
  yk = ifelse(y_predk > 0.5, 1, 0)
  
  true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)
  
  true_pos = (true_y==1) & (yk==1)
  true_neg = (true_y==0) & (yk==0)
  false_pos = (true_y==0) & (yk==1)
  false_neg = (true_y==1) & (yk==0)
  
  conf_mat_k = matrix(c(sum(true_pos), sum(false_pos),
                        sum(false_neg), sum(true_neg)), 2, 2)
  # F-score (2*((precision*recal) / (precision+recal))
  precision2 = conf_mat_k[1, 1] / sum(conf_mat_k[,1])
  recal2 = conf_mat_k[1, 1] / sum(conf_mat_k[1,])
  f1_score2 = 2* ( (precision2 * recal2) / (precision2 + recal2) )
  return (f1_score2)
}

acc2 = function(k){
  classifierk = randomForest( x = training_set[-7],
                              y = training_set$Inflammation.of.urinary.bladder,
                              ntree = k)
  
  y_predk = predict(classifierk, newdata = test_set[-7])
  yk = ifelse(y_predk > 0.5, 1, 0)
  
  true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)
  
  true_pos = (true_y==1) & (yk==1)
  true_neg = (true_y==0) & (yk==0)
  false_pos = (true_y==0) & (yk==1)
  false_neg = (true_y==1) & (yk==0)
  
  conf_mat_k = matrix(c(sum(true_pos), sum(false_pos),
                        sum(false_neg), sum(true_neg)), 2, 2)
  # Accuracy ( (TP + TN) / (TP + FP + TN + FN) )
  accuracy2 = (conf_mat_k[1,1]+conf_mat_k[2,2]) / (conf_mat_k[1,1]+conf_mat_k[2,1]+
                                                    conf_mat_k[2,2]+conf_mat_k[1,2])
  return(accuracy2)
}

table_metr2 = function(k){
  t = matrix(c('précision','sensibilité','F1-score','accuracy',
               pre2(k),rec2(k),fsco2(k),acc2(k)),4,2)
  colnames(t)= c('Métrique','Valeur')
  return(t)
}

# Fonction ROC

library(pROC)

roc2 = function(k){ 
  
      # Ajustement de la random forest à l'ensemble d'entraînement avec k arbres
      
      classifierk = randomForest( x = training_set[-7],
                                  y = training_set$Inflammation.of.urinary.bladder,
                                  ntree = k)
      
      # Prédiction des résultats de l'ensemble de test
      
      y_predk = predict(classifierk, newdata = test_set[-7])
      yk = ifelse(y_predk > 0.5, 1, 0)
      
      # Courbe ROC
      
      par (pty = "s")
      return(roc(test_set[,7], yk, 
                 plot= TRUE, 
                 col= "#C60800", 
                 lwd = 3,
                 main ="ROC curve -- Random Forest ",
                 percent = TRUE))
}

# Fonction illustrant la valeur AUC

auc2 = function(k){ 
  
      # Ajustement de la random forest à l'ensemble d'entraînement avec k arbres
      
      classifierk = randomForest( x = training_set[-7],
                                  y = training_set$Inflammation.of.urinary.bladder,
                                  ntree = k)
      
      # Prédiction des résultats de l'ensemble de test
      
      y_predk = predict(classifierk, newdata = test_set[-7])
      yk = ifelse(y_predk > 0.5, 1, 0)
      
      # Illustration de la valeur AUC
      
      par (pty = "s")
      return(roc(test_set[,7], yk, 
               plot= TRUE, 
               col= "#C60800", 
               lwd = 3,
               print.auc = TRUE,
               print.auc.x= 45,
               main ="AUC illustration",
               percent = TRUE,
               partial.auc = c(100, 0),
               auc.polygon = TRUE,
               auc.polygon.col = "#C6080022"))
}
