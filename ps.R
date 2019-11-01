# df - data frame (two columns with numbers)
# method - pearson or spearman

pearspear <- function(df, method = 'pearson'){
  if (method == 'pearson'){
    return ((sum((df[,2]-mean(df[,2]))*(df[,1]-mean(df[,1])))/(length(df[,1])-1))/(sd(df[,1])*sd(df[,2])))
  } else if (method == 'spearman') {
    df <- df[order(df[,1]),]
    df[,1] <- c(1:length(df[,1]))
    df <- df[order(df[,2]),]
    df[,2] <- c(1:length(df[,2]))
    return (1-((6*(sum((df[,1]-df[,2])^2)))/(length(df[,1])*((length(df[,1])^2)-1))))
  }
}
