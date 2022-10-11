# Importer dataset
df <- read_csv("Inflammation.csv")

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


# Vérification des valeurs manquantes


# Normalisation de la variable 'Temperature of patient'

# Vérification du déséquilibre des classes





