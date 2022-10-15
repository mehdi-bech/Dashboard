# Modèle 1 : Regression Logistique

# Importation de la base de données prétraitée

dataset = read.csv('Data\\Data_encoded.csv')

# Division de l'ensemble de données en un ensemble d'entraînement et un ensemble de test 

      # Rendre cet exemple reproductible

set.seed(1)

      # 70% de BD est l'ensemble d'entraînement et 30% est l'ensemble de test

split = sample.split(dataset, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
head(training_set)

# Ajustement de la régression logistique à l'ensemble d'entraînement

x = model.matrix(Inflammation.of.urinary.bladder ~., training_set)[,-1]
y = model.matrix(training_set['Inflammation.of.urinary.bladder'])


classifier = glm(formula = Inflammation.of.urinary.bladder ~ Temperature.of.patient 
                 + Occurrence.of.nausea + Lumbar.pain + Urine.pushing + Micturition.pains 
                 + Burning.of.urethra,
               family = binomial("logit"),
               data = training_set,
               maxit = 200)

summary(classifier)

# Prédiction des résultats de l'ensemble de test



# Matrice de confusion 



# Precision, Recall, F-score



# Courbe ROC, AUC
