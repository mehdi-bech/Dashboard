
                              # Modèle 1 : Régression Logistique

# Description Régression logistique

context1 = function(){
  c =   " La colonne de la variable 'Inflammation of urinary bladder' est composée de deux modalités :
      (yes , no). L'objectif ultime est de diagnostiquer l'inflammation de la vessie 
      conditionnée par les symptômes du patient. Pour ce faire, nous allons appliquer
      la méthode de la régression logistique"
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

# Ajustement de la régression logistique à l'ensemble d'entraînement

library(mgcv)
classifier1 = gam( formula = Inflammation.of.urinary.bladder ~ Temperature.of.patient 
                 + Occurrence.of.nausea + Lumbar.pain + Urine.pushing + Micturition.pains 
                 + Burning.of.urethra,
               family = binomial("logit"),
               data = training_set )

# Prédiction des résultats de l'ensemble de test

prob_pred1 = predict(classifier1, type="response", newdata = test_set[-7])
y_pred1 = ifelse(prob_pred1 > 0.5, 1, 0)

# Construction de la matrice de confusion 

true_y = as.numeric(test_set$Inflammation.of.urinary.bladder == 1)

true_pos = (true_y==1) & (y_pred1 ==1)
true_neg = (true_y==0) & (y_pred1 ==0)
false_pos = (true_y==0) & (y_pred1 ==1)
false_neg = (true_y==1) & (y_pred1 ==0)

conf_mat_1 = matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)

colnames(conf_mat_1) = c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_1) = c('Y = 1', 'Y = 0')

# Fonction matrice de confusion

cm1 = function (){
  return(cbind(' '= c("Y=1", "Y=0"),conf_mat_1))
}

# Fonction des caracteristiques

pre1 = function(){
  # Precision (TP / (TP + FP))
  precision1 = conf_mat_1[1, 1] / sum(conf_mat_1[,1])
  return (precision1) 
}

rec1 = function(){    
  # Recall (TP / (TP + FN))
  recal1 = conf_mat_1[1, 1] / sum(conf_mat_1[1,])
  return (recal1)
}

fsco1 = function(){
  # F-score (2*((precision*recal) / (precision+recal))
  precision1 = conf_mat_1[1, 1] / sum(conf_mat_1[,1])
  recal1 = conf_mat_1[1, 1] / sum(conf_mat_1[1,])
  f1_score1 = 2* ( (precision1 * recal1) / (precision1 + recal1) )
  return (f1_score1)
}

acc1 = function(){
  # Accuracy ( (TP + TN) / (TP + FP + TN + FN) )
  accuracy1 = (conf_mat_1[1,1]+conf_mat_1[2,2]) / (conf_mat_1[1,1]+conf_mat_1[2,1]+
                                                     conf_mat_1[2,2]+conf_mat_1[1,2])
  return(accuracy1)
}

table_metr1 = function(){
  t = matrix(c('précision','sensibilité','F1-score','accuracy',pre1(),rec1(),fsco1(),acc1()),4,2)
  colnames(t)= c('Métrique','Valeur')
  return(t)
}

# Fonction ROC

library(pROC)

roc1 = function(){ 
  
  # Courbe ROC
  
  library(pROC)
  par (pty = "s")
  r1 = roc(test_set[,7], y_pred1, 
            plot= TRUE, 
            col= "#377eb8", 
            lwd = 3,
            main ="ROC curve -- Régression Logistique ",
            percent = TRUE)
  return(r1)
}

# Fonction Illustration de la valeur AUC

auc1 = function(){ 
    
    # Illustration de la valeur AUC
    
    par (pty = "s")
    a1 = roc(test_set[,7], y_pred1, 
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
    return(a1)
}
