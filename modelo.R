library(tidyverse)
library(tidymodels)
library(randomForest)
library(readxl)

rm(list = ls())

dados = read_excel("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/WDOLFUT-DIARIO-02_01_2018_a_03-05-2023.xlsx",
                   col_types = c("date", "numeric", "numeric","numeric", "numeric"))

dados = dados  %>%
  select_all() %>%
  mutate(
    Ponto_Medio    = round((Maxima+Minima)/2,2)
  )

dados = dados  %>%
  recipe(~., data = dados) %>%
  step_arrange(Data) %>%
  step_lag(Ponto_Medio, lag = c(1,9,22)) %>% 
  step_lag(Maxima, lag = c(1,9,22)) %>%
  step_lag(Minima, lag = c(1,9,22)) %>%
  step_lag(Fechamento, lag = 1) %>%
  step_arrange(desc(Data)) %>%
  prep(dados) %>%
  bake(dados)%>%
  drop_na() 

#view(dados)

# ********************************************************
#             Modelos Finais
#*********************************************************
# **********************************************
#             Modelo para o Maximo
# **********************************************
modelo.max = dados  %>%
  mutate(
    y  = log(Maxima)-log(lag_1_Maxima),
    x1 = (lag_1_Maxima/lag_22_Maxima)^(1/21),
    x2 = Abertura/lag_1_Maxima,
    .keep = "none"
  )

amostra              = sample(2, nrow(modelo.max), replace = TRUE, prob = c(0.7, 0.3))
conjunto_treinamento = modelo.max[amostra == 1, ]
conjunto_teste       = modelo.max[amostra == 2, ]

# *****************************************************************
maximo               = randomForest(y ~ x1 + x2, data = conjunto_treinamento)
previsoes            = predict(maximo, newdata = conjunto_teste)

rmse   = sqrt(mean((dados$lag_1_Maxima[amostra == 2]*exp(conjunto_teste$y)-as.numeric(dados$lag_1_Maxima[amostra == 2]*exp(previsoes)))^2))
# Para set.seed=58, prob = c(0.7, 0.3), tem-se que rmse = 27,77
# Avaliando em todos o conjunto de dados rmse = 20.5 - rmse no conjunto teste = 27.77

# Salva o modelo final em um arquivo - seed = 58 e prob = c(0.7, 0.3) - rmse = 27.77
saveRDS(maximo, "C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/maximo.rds")


# **********************************************
#            Modelo para o Minimo 
# **********************************************
modelo.min = dados  %>%
  mutate(
    y  = log(Minima)-log(lag_1_Minima),
    x1 = (lag_1_Minima/lag_22_Minima)^(1/21),
    x2 = Abertura/lag_1_Minima,
    .keep = "none"
  )

# Salva o modelo final em um arquivo - seed = 74 e prob = c(0.7, 0.3) rmse = 26,86
set.seed(74)
amostra              = sample(2, nrow(modelo.min), replace = TRUE, prob = c(0.7, 0.3))
conjunto_treinamento = modelo.min[amostra == 1, ]
conjunto_teste       = modelo.min[amostra == 2, ]
minimo    = randomForest(y ~ x1 + x2, data = conjunto_treinamento)
previsoes = predict(minimo, newdata = conjunto_teste)
sqrt(mean((dados$lag_1_Minima[amostra == 2]*exp(conjunto_teste$y)-as.numeric(dados$lag_1_Minima[amostra == 2]*exp(previsoes)))^2))

saveRDS(minimo, "C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/minimo.rds")

# ***************************************************
#   Modelo para o Ponto medio
# ***************************************************
modelo.Ponto.Medio = dados  %>%
  mutate(
    y  = log(Ponto_Medio)-log(lag_1_Ponto_Medio),
    x1 = (lag_1_Ponto_Medio/lag_22_Ponto_Medio)^(1/21),
    x2 = Abertura/lag_1_Ponto_Medio,
    .keep = "none"
  )

# Salva o modelo final em um arquivo - seed = 86 e prob = c(0.7, 0.3) rmse = 26,12
set.seed(86)
amostra              = sample(2, nrow(modelo.Ponto.Medio), replace = TRUE, prob = c(0.7, 0.3))
conjunto_treinamento = modelo.Ponto.Medio[amostra == 1, ]
conjunto_teste       = modelo.Ponto.Medio[amostra == 2, ]
ponto.medio          = randomForest(y ~ x1 + x2, data = conjunto_treinamento)
previsoes            = predict(ponto.medio, newdata = conjunto_teste)
sqrt(mean((dados$lag_1_Ponto_Medio[amostra == 2]*exp(conjunto_teste$y)-as.numeric(dados$lag_1_Ponto_Medio[amostra == 2]*exp(previsoes)))^2))

saveRDS(ponto.medio, "C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/ponto.medio.rds")


# **************************************************************
#               Previsoes para  o dia:
# Carrega o modelo salvo em um arquivo
# **************************************************************

maximo      = readRDS("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/maximo.rds")
minimo      = readRDS("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/minimo.rds")
ponto.medio = readRDS("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/ponto.medio.rds")

# Abertura do Dia:
dia = 1
dados$Data[dia]
Abertura      = 5005


x1 = (dados$Maxima[dia+1]/dados$Maxima[dia+22])^(1/21)
x2 = Abertura/dados$Maxima[dia+1]    
X1 = tibble(x1=x1, x2=x2)

x1 = (dados$Minima[dia+1]/dados$Minima[dia+22])^(1/21)
x2 = Abertura/dados$Minima[dia+1] 
X2 = tibble(x1=x1, x2=x2)

x1 = (dados$Ponto_Medio[dia+1]/dados$Ponto_Medio[dia+22])^(1/21)
x2 = Abertura/dados$Ponto_Medio[dia+1] 
X3 = tibble(x1=x1, x2=x2)

(Minima      = as.numeric(dados$lag_1_Minima[dia]*exp(predict(minimo, newdata = X2))))
(Ponto.Medio = as.numeric(dados$lag_1_Ponto_Medio[dia]*exp(predict(ponto.medio, newdata = X3))))
(Maxima      = as.numeric(dados$lag_1_Maxima[dia]*exp(predict(maximo, newdata = X1))))

