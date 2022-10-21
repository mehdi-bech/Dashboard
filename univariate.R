source("imports.R")
# Affichage de la table statistiques pour les données 

statQ <- function(var)
{    
    if (is.numeric(df[, var]))
    {

          temp2=data.frame(
          stat = c('Min', 'Quantile 25%', 'Médiane', 'Quantile 75%', 'Max', 'Moyenne', 'Écart type'),
          values = c(quantile(df[, var]), mean(df[, var]), sd(df[, var])))
          temp2 = setDT(temp2)
          temp2= transpose(temp2)
          temp2=data.frame(temp2)
          colnames(temp2) <- temp2[1,]
          temp2 <- temp2[-1, ]         


        return(setDT(temp2))

    }
    else
    {      temp2 = as.data.frame(table(df[, var]))
           temp2 = cbind(temp2, cumsum(temp2[[2]]))
           temp2 = cbind(temp2,
                       temp2[[2]]/nrow(df)*100,
                       temp2[[3]]/nrow(df)*100)
           colnames(temp2) <- c(var, "Effectifs", "Effectifs Cum.",
                             "Fréquences", "Fréquences Cum.")

        return(setDT(temp2))
    }
}


plot_box_bar <- function(var)
{
    
    if (is.numeric(df[, var]))
    {
        df1=df %>% select(var)
        colnames(df1)[1] = "col"
            fig <- ggplot(df1, aes(x="", y=col)) + 
                geom_boxplot()+
                xlab(var) + ylab("Valeur")+
                ggtitle(paste('Boîte à moustaches de ', var))+
                ggeasy::easy_center_title()
        
        
        return(fig)
    }
    else
    {
        
        temp1 <- as.data.frame(table(df[, var]))
        colnames(temp1)[1] = "Var1"
           fig <- ggplot(temp1, aes(x=Var1, y=Freq, fill=Var1))+
                geom_bar(stat="identity") +
                theme(legend.title=element_blank())+
                scale_fill_brewer(palette="Blues")+
                xlab(var) + ylab("Fréquence")+
                ggtitle(paste('Diagramme en clonnes de ', var))+
                ggeasy::easy_center_title()
    
        return(fig)
    }
}


plot_histo_pie <- function(var)
{
    
    if (is.numeric(df[, var]))
    {
        df1=df %>% select(var)
        colnames(df1)[1] = "col"
        fig <-  ggplot(df1, aes(x=col)) + 
                geom_histogram(aes(y=..density..), colour = 1, fill = "white")+
                geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25)+
                ggtitle(paste('Histogramme de ', var))+
                ggeasy::easy_center_title()
        return(fig)
    }
    else
    {
        temp1 <- as.data.frame(table(df[, var]))
        colnames(temp1)[1] = "Var1"
        fig <-  ggplot(temp1, aes(x="", y=Freq, fill=Var1)) +
                geom_bar(stat="identity", width=1) +
                theme(legend.title=element_blank())+
                coord_polar("y", start=0)+
                guides(fill=guide_legend(title=var))+
                xlab(var) + ylab("")

        return(fig)
    }
}


plot_Cummul <- function(var)
{
    if (is.numeric(df[, var]))
    {
        tmp.hist <- hist( df[, var], plot = FALSE, right = FALSE)
        fig <- plot_ly(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts), mode = 'lines+markers')
        fig <- fig %>% layout(title = paste('Courbe cumulative de \n ', var, sep = ' '))
        return(fig)
    }
    else
    {
        return(NULL)
    }
}