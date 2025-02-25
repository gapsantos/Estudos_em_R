###### Pacotes

library(readr)
library(openxlsx) #biblioteca para escrever arquivo em excel
library(haven)
library(readxl)
library(tidyverse)
library(yardstick) #biblioteca para calcular medidas de erro
library(lmtest) # calcula o teste de homogeneidade de variancia
library(car) # calcula vif


# Importar a base de dados 

BodyFat<- read.csv(file = "dados/Bodyfat.csv")

summary(BodyFat)


#Descritiva das variáveis

names(BodyFat)

#Density  - Densidade corporal - Quantitativa Continua 
#bodyfat - Gordura corporal - Quantitativa Continua
#Age - Idade - Quantitativa Discreta
#Weight - Peso - Quantitativa Continua
#Height - Alturra - Quantitativa Continua
#Neck - Pescoço - Quantitativa Continua
#Chest - Peitoral - Quantitativa Continua
#Abdomen - Abdomen - Quantitativa Continua
#Hip - Cintura - Quantitativa Continua
#Thigh - Coxa - Quantitativa Continua
#Knee - Joelho - Quantitativa Continua
#Ankle - Calcanhar - Quantitativa Continua
#Biceps - Biceps - Quantitativa Continua
#Forearm - Antebraço - Quantitativa Continua
#Wrist - Pulso - Quantitativa Continua

#Testando a correlação das variáveis 

cor(BodyFat)

#Analisar quais possuem um correlação com bodyfat acima de 0,6

# Criar um modelo apenas com b0

reg_gordura_nula <- lm(bodyfat ~ 1, data=BodyFat)

summary(reg_gordura_nula)


#Criar um modelo com todas as variáveis 



reg_gordura_full <- lm(bodyfat ~., data = BodyFat)


#Adicionar parâmetros por meio do Forward ( Ou outro método como Backward ou Stepwise)

forw <- step(reg_gordura_nula, scope=list(lower=reg_gordura_nula, upper=reg_gordura_full), direction = "forward")

summary(forw)


#Validando o modelo ao verificar o comportamento dos resíduos 


plot(fitted(forw),residuals(forw),xlab="ValoresAjustados",ylab="Resíduos")
abline(h=0)

# qqplot

qqnorm(residuals(forw), ylab="Resíduos")
qqline(residuals(forw))



#teste de normalidade dos resíduos
#h0: os dados sao normais
#h1: os dados nao sao normais
shapiro.test(forw$residuals)

#teste de homogeneidade de variancia Breusch-Pagan test dos resíduos
#h0: as variâncias dos erros sao iguais (homoscedasticidade)
#h1: as variâncias dos erros nao sao iguais (heteroscedasticidade)

bptest(forw) #library lmtest


#Testando a multicolinearidade 


vif(forw)

#VIF > 5 indica multicolinearidade






