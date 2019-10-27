
assemble_df <- function(dir_with_df) {
  files <- list.files(dir_with_df, pattern='*.csv')
  for (file in files){
    if (is.null(result)){
      result <- read.csv(paste('Data/', file, sep = ''))
    } else {
      s <- read.csv(paste('Data/', file, sep = ''))
      result <- rbind(result, s)
    }
  }
  return(result)
}

mollusks <- assemble_df('Data')

