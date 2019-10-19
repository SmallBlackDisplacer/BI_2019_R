

library(ggplot2)
library(car)
library (carData)

sol <- Soils
str(sol)
sol$N

ggplot (data=sol, aes(N))+
  geom_density(fill='white')

qqPlot(sol$N)

shapiro.test(sol$N)

# 1. распределение не нормальное 