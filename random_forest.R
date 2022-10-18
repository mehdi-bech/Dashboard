
                              # Modèle 2 : Random Forest

# Importation de la base de données prétraitée

data = read.csv('Data\\Data_encoded.csv')

# Fonction des caracteristiques

#car2 = function (){
    
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


pre = function(){
  # Precision (TP / (TP + FP))
  return ( precision = conf_mat_2[1, 1] / sum(conf_mat_2[,1])) 
}

rec = function(){    
  # Recall (TP / (TP + FN))
  return ( recal = conf_mat_2[1, 1] / sum(conf_mat_2[1,]))
}

fsco = function(){
  # F-score (TN / (TN + FP))
  return ( f1_score = 2* ((precision*recal) / (precision+recal)))
}

acc = function(){
  # Accuracy ( (TP + TN) / (TP + FP + TN + FN) )
  return( accuracy = (conf_mat_2[1,1]+conf_mat_2[2,2]) / (conf_mat_2[1,1]+conf_mat_2[2,1]+
                      conf_mat_2[2,2]+conf_mat_2[1,2]))
}

# Fonction ROC

library(pROC)

roc2 = function(k){ 
  
      # Ajustement de la random forest à l'ensemble d'entraînement
      
      classifier2 = randomForest( x = training_set[-7],
                                  y = training_set$Inflammation.of.urinary.bladder,
                                  ntree = k)
      
      # Prédiction des résultats de l'ensemble de test
      
      y_pred = predict(classifier2, newdata = test_set[-7])
      
      # Courbe ROC
      
      par (pty = "s")
      return(roc(test_set[,7], y_pred, 
                 plot= TRUE, 
                 col= "#377eb8", 
                 lwd = 3,
                 main ="ROC curve -- Random Forest ",
                 percent = TRUE))
}

# Fonction illustrant la valeur AUC

auc2 = function(k){ 
  
      # Ajustement de la random forest à l'ensemble d'entraînement
      
      classifier2 = randomForest( x = training_set[-7],
                                  y = training_set$Inflammation.of.urinary.bladder,
                                  ntree = k)
      
      # Prédiction des résultats de l'ensemble de test
      
      y_pred = predict(classifier2, newdata = test_set[-7])
      
      # Illustration de la valeur AUC
      
      par (pty = "s")
      return(roc(test_set[,7], y_pred, 
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
