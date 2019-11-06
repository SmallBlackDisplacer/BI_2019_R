
assemble_df <- function(dir_with_df) {
  files <- list.files(dir_with_df, pattern='*.csv')
  result <- c()
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

names(mollusks)[2] <- 'Sex'
mollusks$Sex <- as.factor(mollusks$Sex)
levels(mollusks$Sex) <- c('male','female','juvenile','male','male','juvenile')

mollusks$Rings <- as.numeric(mollusks$Rings)
mollusks$Length <- as.numeric(mollusks$Length)

str(mollusks)

sapply(mollusks, function(y) sum(length(which(is.na(y)))))

#View(mollusks)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

na_rm  <- function(x){
  zzz <- apply(x, 2, function(z) {
    z[is.na(z)] = mean(z, na.rm = TRUE)
    return (z)
  })
  return (as.data.frame(zzz))
}

#dim(mollusks)
mollusks <- mollusks[!is.na(mollusks$Sex),]
mollusks$Rings[is.na(mollusks$Rings)] <- getmode(mollusks$Rings)
mollusks <- cbind(mollusks[,c(1,2)],na_rm(mollusks[,c(3:9)]))

mean(mollusks$Length)

library(dplyr)

mollusks %>% group_by(Sex) %>% summarise(mean = mean(Length), sd = sd(Length))

round((dim(mollusks %>% filter(Height <= 0.165))[1]/dim(mollusks)[1])*100,3)

sort(mollusks$Length)[round(length(mollusks$Length)*0.92)]
max(mollusks$Length)

mollusks <- mollusks %>% mutate(Lenght_z_scores = (Length - mean(Length))/sd(Length))
str(mollusks)

rings_5_and_15 <- mollusks %>% filter(Rings == 5 | Rings == 15)


library(ggplot2)
str(rings_5_and_15)

ggplot(rings_5_and_15, aes(Diameter))+
  geom_density(fill='white')+
  theme_dark()

t.test(Diameter~Rings, data = rings_5_and_15)

rings_5_and_15$Whole_weight

ggplot(mollusks, aes(Diameter))+
  geom_density()+
  theme_dark()

ggplot(mollusks, aes(y=Diameter,x=Whole_weight))+
  geom_point()

ggplot(mollusks, aes(Diameter,sqrt(Whole_weight)))+
  geom_point()

ggplot(mollusks, aes(Diameter,log(Whole_weight)))+
  geom_point()

ggplot(mollusks, aes(Diameter,Whole_weight^(1/3)))+
  geom_point()

cor.test(mollusks$Diameter, mollusks$Whole_weight)

summary(lm(Whole_weight~Diameter, mollusks))

plot(mollusks)
