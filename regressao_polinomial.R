library(dplyr) # so carrego ja tenho instalado
install.packages("readxl")  # instalo pacote para ler planilhas
library(readxl) # carrego pacote

vendas <- read_xlsx("comissao.xlsx") # carrega base
glimpse(vendas) # verifica a estrutura
View(vendas) # visualiza dados

plot(vendas$quantidade,vendas$comissao,col = "red") # plota dispersao

# testa normalidade
qqnorm(vendas$comissao)
qqline(vendas$comissao)

qqnorm(vendas$quantidade)
qqline(vendas$quantidade)

# roda shapiro   # Ho normalidade p > 0.05 H1 alternativa p <= 0.05
shapiro_test(vendas$comissao)
shapiro_test(vendas$quantidade)

# CORRELACAO
#pearson para distribuicao normal
#spearman para distribuicao nao normal
#kendall para distribuicao nao normal com amostra pequena

# Ho p > 0.05 nao existe correlacao
# H1 p <= 0.05 existe correlacao
cor.test(vendas$quantidade,vendas$comissao,method = "spearman")

# fazer modelo com regressao linear 
modelo1 <- lm(vendas$comissao ~ vendas$quantidade, data = vendas)

plot(vendas$quantidade,vendas$comissao,col = "red") 
abline(modelo1)
summary(modelo1)

vendas$previsao1 <- modelo1$fitted.values

#hist(vendas$comissao, probability = TRUE,col = "lightblue")
#lines(density(vendas$comissao),col = "red")

# CONCLUSAO MODELO REGRESSAO LINEAR INADEQUADO

# CONSTRUCAO DE MODELO DE REGRESSAO POLINOMIAL
x <- vendas$quantidade
var_indep <- cbind(x, x^2)

modelo2 <- lm(vendas$comissao ~ var_indep)
summary(modelo2)

vendas$previsao2 <- modelo2$fitted.values

