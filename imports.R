# Chargement des librairies

library(shiny)
library(markdown)
library(shinythemes)
library(reshape2)
library(ROCR)
library(class)
library(data.table)
library(dplyr)
library(readxl)
library(ROCR)
library(pROC)
library(class)
library(magrittr)
library(factoextra)
library(ggbiplot)
library(ggplot2)
library(ggeasy)
library(reshape2)
library(plotly)
library(bslib)
library(plyr)
library(readr)
library(caret)
library(glmnet)


# Using custom shiny theme

thematic::thematic_shiny(font = "auto")

#Importation des donnees

df <- read.csv("Data\\Inflammation.csv", check.names = FALSE)

# Defining categorical variables
categorical <- c('Occurrence of nausea', 'Lumbar pain', 'Urine pushing (continuous need for urination)',
                 'Micturition pains ', 'Burning of urethra, itch, swelling of urethra outlet ', 
                 'Inflammation of urinary bladder ' )

