# Loading libraries
library(shiny)
library(markdown)
library(shinythemes)
library(reshape2)
library(ROCR)
library(class)
library(dplyr)
library(readxl)
library(ROCR)
library(pROC)
library(class)
library(magrittr)
library(factoextra)
library(ggbiplot)
library(plotly)
library(bslib)
library(plyr)
library(readr)
library(caret)
library(caTools)
library(glmnet)

# Using custom shiny theme
thematic::thematic_shiny(font = "auto")

# Defining categorical variables
categorical <- c('Occurrence of nausea', 'Lumbar pain', 'Urine pushing (continuous need for urination)',
                 'Micturition pains ', 'Burning of urethra, itch, swelling of urethra outlet ', 
                 'Inflammation of urinary bladder ' )