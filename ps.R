to_ranked <- function(sorted_v){
  n <- length(sorted_v)
  new_vector <- c()
  start <- 1
  counter <- 0
  for (val in c(2:n)){
    counter <- counter + 1
    if (sorted_v[val] != sorted_v[start]) {
      new_vector <- c(new_vector, rep(((sum(c(start:(start+(counter-1)))))/counter), counter))
      start = start+counter
      counter <- 0
    }
  }
  return (c(new_vector, rep(((sum(c(start:(start+(counter)))))/(counter+1)), (counter+1))))
}

# df - data frame (two columns with numbers)
# method - pearson or spearman

pearspear <- function(df, method = 'pearson'){
  if (method == 'pearson'){
    return ((sum((df[,2]-mean(df[,2]))*(df[,1]-mean(df[,1])))/(length(df[,1])-1))/(sd(df[,1])*sd(df[,2])))
  } else if (method == 'spearman') {
    df <- df[order(df[,1]),]
    df[,1] <- to_ranked(df[,1])
    df <- df[order(df[,2]),]
    df[,2] <- to_ranked(df[,2])
    return (1-((6*(sum((df[,1]-df[,2])^2)))/(length(df[,1])*((length(df[,1])^2)-1))))
  }
}
