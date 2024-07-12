# Curso udemy regressao linear
# carrega data frame na variavel mola
install.packages("moments")
install.packages("tidyr")
install.packages("rstatix")
install.packages("plotly")
library(moments)
library(tidyr)
library(rstatix)
library(plotly)

mola <- read.csv2(file = "rigidez.csv", sep = ";", dec = ".", header = TRUE)

# analise exploratoria 
View(mola)
glimpse(mola)
summary(mola$forca)

#calculate skewness and kurtosis library (moments)
skewness(mola$forca)
kurtosis(mola$forca)

# renomeia colunas
mola <- rename(mola,compr_inicial = Lo, compr_final = L, deformacao = x, rigidez = K )
# Remove missing values drop_na (diplr)
mola <- drop_na(mola,compr_final,deformacao,rigidez)

# Analise de Outliers
boxplot(mola$forca)
boxplot(mola2$deformacao)
boxplot(mola2$rigidez)

# Tratamento de outliers

mola %>% identify_outliers(deformacao)

# Removendo outliers
outliers <- c(boxplot.stats(mola$deformacao)$out)
mola2 <- mola[-c(which(mola$deformacao %in% outliers)),  ]

# Ajusta a classificacao dos atributos
mola2$cargas <- as.factor(mola2$cargas)

# ANALISE DA CORRELACAO
plot(mola2$deformacao,mola2$forca)

# TESTE DE NORMALIDADE
hist(mola2$forca, probability = TRUE,col = "lightblue")
lines(density(mola2$forca), lwd = 3, col = "red")

hist(mola2$deformacao, probability = TRUE,col = "lightblue")
lines(density(mola2$deformacao), lwd = 3, col = "red")

qqnorm(mola2$forca)
qqline(mola2$forca)

qqnorm(mola2$deformacao)
qqline(mola2$deformacao)

# Roda um shapiro-wilk
# PREMISSAS
# Ho -> DISTRIBUICAO NORMAL PARA p > 0.05
# H1 -> DISTRIBUICAO NAO NORMAL PARA p <= 0.05
shapiro_test(mola2$forca)
shapiro_test(mola2$deformacao)

# Hipoteses para correlacao
# Ho nao existe correlacao
# H1 existe correlacao
cor.test(mola2$deformacao,mola2$forca,method = "pearson")

# Modelo de regressao

modelo <- lm(forca ~ deformacao,mola2)

# graficos
# (01) Linearidade 
# (02) Normalidade dos Residuos
# (03) Hocedasticidade
# (04) Outliers pontos nao podem passar de -3 e 3
plot(modelo) 

#H0 > 0.05 normalidade oK
#H1 <= 0.05 normalidade Nok 
shapiro_test(modelo$residuals)
summary(modelo)
plot(mola2$deformacao,mola2$forca)
abline(modelo,col = "red")


