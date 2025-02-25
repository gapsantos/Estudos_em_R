###### Pacotes
library(readr)
library(openxlsx) #biblioteca para escrever arquivo em excel
library(haven)
library(readxl)
library(tidyverse)
library(yardstick) #biblioteca para calcular medidas de erro
library(lmtest) # calcula o teste de homogeneidade de variancia
library(car) # calcula vif


###############################################
########## regressao linear simples ##########
###############################################

# exemplos basicos dos slides

# exemplo pizza
estudante <- c(2,6,8,8,12,16,20,20,22,26)
pizza <- c(55,105,88,118,117,137,157,169,149,202)
dados <- cbind(estudante,pizza)
dados_pizzaria <- as.data.frame(dados)
plot(dados_pizzaria$estudante, dados_pizzaria$pizza)

?cbind

#modelo MRLS
reg_simples <- lm(pizza ~ estudante, data=dados_pizzaria)
summary(reg_simples)


### Conceito de busca pelo otimo no excel

write.xlsx(dados_pizzaria,file = "pizzaria.xlsx", sheetName = "RLSotimizador", append=TRUE )

#calculando mse

dados_pizzaria$resp_modelo <- predict(reg_simples, newdata = dados_pizzaria)

dados_pizzaria$resp_media <- mean(dados_pizzaria$pizza)

dados_pizzaria %>% select(pizza, resp_modelo, resp_media)
#MSE do modelo
mse <- mean((dados_pizzaria$pizza - dados_pizzaria$resp_modelo)^2)
mse
rmse_model <- sqrt(mse)
rmse_model
#MSE usando apenas média ao inves do modelo
mse_usando_media <- mean((dados_pizzaria$pizza - dados_pizzaria$resp_media)^2)
mse_usando_media
rmse_usando_media <- sqrt(mse_usando_media)
rmse_usando_media

# usando a biblioteca yardstick
# Métricas de erro
rmse(dados_pizzaria, truth = pizza, estimate = resp_modelo)
rmse(dados_pizzaria, truth = pizza, estimate = resp_media)




######################################################
# exercício BODYFAT

### dados bodyfat
# importando CSV que foi baixado em: http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.html

BodyFat<- read.csv(file = "dados/Bodyfat.csv")

#Faça uma breve descritiva das variáveis: bodyfat e Wrist

summary(BodyFat$bodyfat)
summary(BodyFat$Wrist)

# variavel bodyfat

boxplot(BodyFat$bodyfat)
hist(BodyFat$bodyfat)

# variavel Wrist

boxplot(BodyFat$Wrist)
hist(BodyFat$Wrist)


# uma contra outra

boxplot(BodyFat$bodyfat,BodyFat$Wrist)


plot(BodyFat$Wrist, BodyFat$bodyfat)

#Calcule a correlação entre as duas variáveis


?cor

cor(BodyFat$bodyfat, BodyFat$Wrist)

#Gere uma equação de regressão linear simples entre bodyfat e Wrist 
#modelo MRLS - nome do objeto reg_gordura_wirst

reg_gordura_wirst <- lm(BodyFat$bodyfat ~ BodyFat$Wrist)
summary(reg_gordura_wirst)





###############################################
########## regressao linear multipla ##########
###############################################
#Density bodyfat   Age Weight Height  Neck Chest Abdomen   Hip Thigh  Knee Ankle Biceps

###### fazendo o nodelo com Abdomen e Biceps
# nome do objeto reg_gordura_abdomen_biceps

reg_gordura_abdomen_biceps <- lm(BodyFat$bodyfat ~ BodyFat$Abdomen + BodyFat$Biceps) 

summary(reg_gordura_abdomen_biceps)

###### fazendo o nodelo com todas as variaveis, exceto Density

# nome do objeto reg_gordura_full

reg_gordura_full <- lm(bodyfat ~. - Density , data = BodyFat)

?lm

summary(reg_gordura_full)

reg_gordura_full <- lm(bodyfat ~. - Density - Knee , data = BodyFat)
summary(reg_gordura_full)


reg_gordura_full <- lm(bodyfat ~. - Density - Knee - Chest  , data = BodyFat)
summary(reg_gordura_full)

reg_gordura_full <- lm(bodyfat ~. - Density - Knee - Chest -Height  , data = BodyFat)
summary(reg_gordura_full)



### testando com selecao de var

#regressao por forward
reg_gordura_nula <- lm(bodyfat ~ 1, data=BodyFat)
summary(reg_gordura_nula)

forw <- step(reg_gordura_nula, scope=list(lower=reg_gordura_nula, upper=reg_gordura_full), direction = "forward")
summary(forw)

 #regressao por backward
backw <-  step(reg_gordura_full, direction = "backward")
summary(backw)

#regressao por stepwise
stepw <- step(reg_gordura_full, direction = "both")
summary(stepw)


#### pegando a medida RMSE de cada modelo
#so estou criando uma nova tabela, para adicionar os preditos
BodyfatPreditos <- BodyFat
BodyfatPreditos$resp_media <- mean(BodyFat$bodyfat)
BodyfatPreditos$resp_modelo_full <- predict(reg_gordura_full, newdata = BodyfatPreditos)
BodyfatPreditos$resp_modelo_forward <- predict(forw, newdata = BodyfatPreditos)
BodyfatPreditos$resp_modelo_backward <- predict(backw, newdata = BodyfatPreditos)
BodyfatPreditos$resp_modelo_stepwise <- predict(stepw, newdata = BodyfatPreditos)

# usando a biblioteca yardstick
# Métricas de erro
rmse(BodyfatPreditos, truth = bodyfat, estimate = resp_media)
rmse(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_full)
rmse(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_forward)
rmse(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_backward)
rmse(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_stepwise)

mae(BodyfatPreditos, truth = bodyfat, estimate = resp_media)
mae(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_full)
mae(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_forward)
mae(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_backward)
mae(BodyfatPreditos, truth = bodyfat, estimate = resp_modelo_stepwise)


#### validando suposicoes modelo stepwise

# ajustado X resíduo
plot(fitted(stepw),residuals(stepw),xlab="ValoresAjustados",ylab="Resíduos")
abline(h=0)
# qqplot
qqnorm(residuals(stepw), ylab="Resíduos")
qqline(residuals(stepw))

#teste de normalidade
#h0: os dados sao normais
#h1: os dados nao sao normais
shapiro.test(stepw$residuals)
#teste de homogeneidade de variancia Breusch-Pagan test
#h0: as variâncias dos erros sao iguais (homoscedasticidade)
#h1: as variâncias dos erros nao sao iguais (heteroscedasticidade)
bptest(stepw) #library lmtest




########### multicolinearidade
## exemplo galoes.xlsx de multicolinearidade

#trazendo o dado em excel
galoes <- read_excel("dados/Galoes.xlsx")

# modelo completo
reg_galoes <- lm(Tempo_viagem ~ Galoes_gastos + Milhas_percorridas, data=galoes)
summary(reg_galoes)


#analise anterior
#existe correlacao entre as vars independentes? + scatter plot tds contra todos
pairs(galoes, col = 2, pch = 19)
cor(galoes)


# calculando VIF (library car)

vif(reg_galoes)

vif(stepw)


###### variavel Dummie

# BASE consumo_oxi
# PREVER consumo oxigenio

# importando os dados
consumo_oxi_bd <- read_sas("dados/consumo_oxi_bd.sas7bdat", 
                           NULL)
View(consumo_oxi_bd)

#dummie Feminino
consumo_dummie <-  consumo_oxi_bd %>% 
  mutate(
    Genero_F = ifelse(Genero == "F", 1, 0)
  ) %>% 
  select(-Genero)

model_full_oxiF <- lm(Consumo_Oxigenio ~ Genero_F, data=consumo_dummie)
summary(model_full_oxiF)

#dummie Masculino
consumo_dummieM <-  consumo_oxi_bd %>% 
  mutate(
    Genero_M = ifelse(Genero == "M", 1, 0)
  ) %>% 
  select(-Genero)

model_full_oxiM <- lm(Consumo_Oxigenio ~ Genero_M, data=consumo_dummieM)
summary(model_full_oxiM)



# e se eu nao criasse a dummie?
model_genero <- lm(Consumo_Oxigenio ~ Genero, data=consumo_oxi_bd)
summary(model_genero)