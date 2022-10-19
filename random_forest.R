
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

# Prédiction des résultats de l'ensemble de test

y_pred2 = predict(classifier2, newdata = test_set[-7])
y2 = ifelse(y_pred2 > 0.5, 1, 0)

# Construction de la matrice de confusion

true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)

true_pos = (true_y==1) & (y2==1)
true_neg = (true_y==0) & (y2==0)
false_pos = (true_y==0) & (y2==1)
false_neg = (true_y==1) & (y2==0)

conf_mat_2 = matrix(c(sum(true_pos), sum(false_pos),
                      sum(false_neg), sum(true_neg)), 2, 2)

colnames(conf_mat_2) = c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_2) = c('Y = 1', 'Y = 0')

# Fonction matrice de confusion

cm2 = function (){
  return(cbind(' '= c("Y=1", "Y=0"),conf_mat_2))
}

# Fonction des caracteristiques

pre2 = function(){
  # Precision (TP / (TP + FP))
  precision2 = conf_mat_2[1, 1] / sum(conf_mat_2[,1])
  return (precision2) 
}

rec2 = function(){    
  # Recall (TP / (TP + FN))
  recal2 = conf_mat_2[1, 1] / sum(conf_mat_2[1,])
  return (recal2)
}

fsco2 = function(){
  # F-score (2*((precision*recal) / (precision+recal))
  precision2 = conf_mat_2[1, 1] / sum(conf_mat_2[,1])
  recal2 = conf_mat_2[1, 1] / sum(conf_mat_2[1,])
  f1_score2 = 2* ( (precision2 * recal2) / (precision2 + recal2) )
  return (f1_score2)
}

acc2 = function(){
  # Accuracy ( (TP + TN) / (TP + FP + TN + FN) )
  accuracy2 = (conf_mat_2[1,1]+conf_mat_2[2,2]) / (conf_mat_2[1,1]+conf_mat_2[2,1]+
                                                    conf_mat_2[2,2]+conf_mat_2[1,2])
  return(accuracy2)
}

table_metr2 = function(){
  t = matrix(c('précision','sensibilité','F1-score','accuracy', pre2(),rec2(),fsco2(),acc2()),4,2)
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
                 col= "#377eb8", 
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
               col= "#377eb8", 
               lwd = 3,
               print.auc = TRUE,
               print.auc.x= 45,
               main ="AUC illustration",
               percent = TRUE,
               partial.auc = c(100, 0),
               auc.polygon = TRUE,
               auc.polygon.col = "#377eb822"))
}
