describetable <- function(first_feature, second_feature)
{
    if (! is.numeric(df[, first_feature]) & ! is.numeric(df[, second_feature]))
    {
        p=ftable(df[,first_feature], df[,second_feature],dnn=c(first_feature,second_feature))        
        return(p)        
    }
    else 
    {
        temp <- df %>% select(first_feature, second_feature)
        num_col <- unlist(lapply(temp, is.numeric)) 
        cat_col =! unlist(lapply(temp, is.numeric))

        dd <- describeBy(temp[,num_col],
           group = temp[,cat_col],
           mat=TRUE)
           dd <- rbind(dd)
           dd=setDT(dd %>% select(-c(item,mad, trimmed,skew,kurtosis,se))) 
                     
        return(dd)
    }
    }


plt_box_bar <- function(first_feature, second_feature)
{
    if (! is.numeric(df[, first_feature]) & ! is.numeric(df[, second_feature]))
    {
      temp1 = df %>% select(first_feature,second_feature)
      ggplot(temp1, aes(x = temp1[,1], fill = temp1[,2]))+xlab(first_feature) + geom_bar() + guides(fill=guide_legend(title=second_feature))
    }
    else 
    {
        temp <- df %>% select(first_feature, second_feature)
        num_col <- unlist(lapply(temp, is.numeric)) 
        cat_col =! unlist(lapply(temp, is.numeric))
                     
        return(boxplot(temp[,num_col] ~ temp[,cat_col],
            col="violet", main="Boîtes parallèles",
            xlab = names(temp)[which(cat_col)],
            ylab = names(temp)[which(num_col)],
            horizontal=FALSE) )
    }
    }