# Importer dataset
df <- read_csv("Data\\Inflammation.csv")

# Encodage des variables qualitatives
df[['Occurrence of nausea']] = factor(df[['Occurrence of nausea']],
                  levels = c('no','yes'),
                  labels = c(0, 1))

df[['Lumbar pain']] = factor(df[['Lumbar pain']],
                  levels = c('no','yes'),
                  labels = c(0, 1))

df[['Urine pushing']] = factor(df[['Urine pushing']],
                  levels = c('no','yes'),
                  labels = c(0, 1))

df[['Micturition pains']] = factor(df[['Micturition pains']],
                  levels = c('no','yes'),
                  labels = c(0, 1))

df[['Burning of urethra']] = factor(df[['Burning of urethra']],
                  levels = c('no','yes'),
                  labels = c(0, 1))

df[['Inflammation of urinary bladder']] = factor(df[['Inflammation of urinary bladder']],
                  levels = c('no','yes'),
                  labels = c(0, 1))

# Vérification des outliers 

boxplot(df['Temperature of patient'])

# Vérification des valeurs manquantes

print(apply(df, 2, anyNA))

#Shapiro-Wilk normality test pour choisir entre la normalisation et la standarisation de la variable 'Temperature of patient' 

shapiro.test(df[['Temperature of patient']])

#p-value = 9.702e-08 < 0.05 donc la variable ne suit pas la loi normale donc on choisit la normalisation

# Normalisation de la variable 'Temperature of patient'

process <- preProcess(as.data.frame(df[['Temperature of patient']])
                      , method=c("range"))
df['Temperature of patient'] <- predict(process
                      , as.data.frame(df[['Temperature of patient']]))

# Vérification du déséquilibre des classes

a <- table(df['Inflammation of urinary bladder'])
lbls <- paste(names(a), "\n", a, sep="")
pie(a, labels = lbls,
    main="Diagramme circulaire de l'inflammation de la vessie ")

# Les deux classes sont équilirées