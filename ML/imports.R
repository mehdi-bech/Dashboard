# Chargement des librairies

library(shiny)
library(markdown)
library(shinythemes)
library(reshape2)
library(rcompanion)
library(ROCR)
library(psych)
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

a=as.list(names(df))