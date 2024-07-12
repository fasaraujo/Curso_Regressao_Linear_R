
# carrega base
doenca_pre <- read.csv2(file = "casos_obitos_doencas_preexistentes.csv", sep = ";", dec = ".",header = TRUE, encoding = "UTF-8")
glimpse(doenca_pre)
View(doenca_pre)

# ANALISE RESPOSTA EM RELACAO AO SEXO FEMININO OU MASCULINO
table(doenca_pre$cs_sexo)

filtrado <- doenca_pre %>% 
  filter(cs_sexo != "INDEFINIDO")


