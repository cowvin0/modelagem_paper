library(tidymodels)
library(randomForest)
library(timetk)
library(readxl)
library(visdat)
tidymodels_prefer()

dados <- read_excel("./WDOLFUT-DIARIO-02_01_2018_a_03-05-2023.xlsx",
                   col_types = c("date", "numeric", "numeric","numeric", "numeric")
                   )

dados <- dados |> mutate(Ponto_Medio = log(round((Maxima + Minima)/ 2, 2)))

# modelo ponto médio

# receita ponto médio
dados_rec_medio <- dados  |> 
  recipe(Ponto_Medio ~., data = dados) |> 
  step_arrange(Data) |> 
  step_lag(Ponto_Medio, lag = c(1, 9, 22), role = "lag") |> 
  step_rm(c(Maxima, Minima)) |> 
  step_arrange(desc(Data)) |> 
  step_naomit()

dados_final_medio <- dados_rec_medio |> prep() |> bake(new_data = dados) |> drop_na() |> 
  mutate(y = log(Ponto_Medio) - log(lag_1_Ponto_Medio),
         x1 = (lag_1_Ponto_Medio/lag_22_Ponto_Medio)^(1/21),
         x2 = Abertura/lag_1_Ponto_Medio,
         Data = Data,
         .keep = "none")

rec_ponto_medio <- dados_final_medio |> 
  recipe(y ~., data = dados_final_medio)

# random forest com a engine ranger
rand_model <- rand_forest(min_n = tune(), trees = tune(), mtry = tune()) |> 
  set_engine("ranger", alpha = tune()) |> 
  set_mode("regression")

rand_params <- rand_model |> 
  extract_parameter_set_dials() |> 
  update(trees = trees(c(50, 1000)),
         mtry = mtry(c(1, 3)),
         alpha = mixture(),
         min_n = min_n(c(2, 100))
  )

# criando workflow
ponto_medio_wf <- workflow() |>
  add_recipe(rec_ponto_medio) |> 
  add_model(rand_model)
  
# banco de teste e treinamento

amostra <- sample(2, nrow(dados_final_medio), replace = TRUE, prob = c(0.7, 0.3))
conjunto_treinamento_medio <- dados_final_medio[amostra == 1, ]
conjunto_teste_medio <- dados_final_medio[amostra == 2, ]

cv_ponto_medio <- vfold_cv(data = conjunto_treinamento_medio, v = 20)

tunagem_ponto_medio <- 
  tune_grid(
    ponto_medio_wf,
    resamples = cv_ponto_medio,
    grid = 50L,
    metrics = metric_set(rsq, rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE),
    param_info = rand_params
  )

# modelo maximo

dados_rec_maxima <- dados  |> 
  recipe(Maxima ~., data = dados) |> 
  step_arrange(Data) |> 
  step_lag(Maxima, lag = c(1, 9, 22), role = "lag") |> 
  step_arrange(desc(Data)) |> 
  step_naomit()

dados_final_maxima <- dados_rec_maxima |> prep() |> bake(new_data = dados) |> drop_na() |> 
  mutate(y = log(Maxima) - log(lag_1_Maxima),
         x1 = (lag_1_Maxima/lag_22_Maxima)^(1/21),
         x2 = Abertura/lag_1_Maxima,
         Data = Data,
         .keep = "none")

rec_maxima <- dados_final_minima |> 
  recipe(y ~., data = dados_final_maxima)

conjunto_treinamento_maxima <- dados_final_maxima[amostra == 1, ]
conjunto_teste_maxima <- dados_final_maxima[amostra == 2, ]

cv_maxima <- vfold_cv(data = conjunto_treinamento_maxima, v = 20)

maxima_wf <- workflow() |>
  add_recipe(rec_maxima) |> 
  add_model(rand_model)

tunagem_maxima <- 
  tune_grid(
    maxima_wf,
    resamples = cv_maxima,
    grid = 50L,
    metrics = metric_set(rsq, rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE),
    param_info = rand_params
  )

# modelo minimo

dados_rec_minima <- dados |> 
  recipe(Minima ~., data = dados) |> 
  step_arrange(Data) |> 
  step_lag(Minima, lag = c(1, 9, 22), role = "lag") |> 
  step_arrange(desc(Data)) |> 
  step_naomit()

dados_final_minima <- dados_rec_minima |> prep() |> bake(new_data = dados) |> drop_na() |> 
  mutate(y = log(Minima) - log(lag_1_Minima),
         x1 = (lag_1_Minima/lag_22_Minima)^(1/21),
         x2 = Abertura/lag_1_Minima,
         Data = Data,
         .keep = "none")

rec_minima <- dados_final_minima |> 
  recipe(y ~., data = dados_final_minima)

conjunto_treinamento_minima <- dados_final_minima[amostra == 1, ]
conjunto_teste_minima <- dados_final_minima[amostra == 2, ]

cv_minima <- vfold_cv(data = conjunto_treinamento_minima, v = 20)

minima_wf <- workflow() |>
  add_recipe(rec_minima) |> 
  add_model(rand_model)

tunagem_minima <- 
  tune_grid(
    minima_wf,
    resamples = cv_minima,
    grid = 50L,
    metrics = metric_set(rsq, rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE),
    param_info = rand_params
  )

# ----------------
# Professor Ulisses

# ********************************************************
#             Modelos Finais
#*********************************************************
# **********************************************
#             Modelo para o Maximo
# **********************************************
# modelo.max <- dados |> 
#   mutate(
#     y  = log(Maxima)-log(lag_1_Maxima),
#     x1 = (lag_1_Maxima/lag_22_Maxima)^(1/21),
#     x2 = Abertura/lag_1_Maxima,
#     .keep = "none"
#   )
# 
# amostra <- sample(2, nrow(modelo.max), replace = TRUE, prob = c(0.7, 0.3))
# conjunto_treinamento <- modelo.max[amostra == 1, ]
# conjunto_teste <- modelo.max[amostra == 2, ]
# 
# # *****************************************************************
# maximo <- randomForest(y ~ x1 + x2, data = conjunto_treinamento)
# previsoes <- predict(maximo, newdata = conjunto_teste)
# 
# rmse <- sqrt(mean((dados$lag_1_Maxima[amostra == 2]*exp(conjunto_teste$y)-as.numeric(dados$lag_1_Maxima[amostra == 2]*exp(previsoes)))^2))
# # Para set.seed=58, prob = c(0.7, 0.3), tem-se que rmse = 27,77
# # Avaliando em todos o conjunto de dados rmse = 20.5 - rmse no conjunto teste = 27.77
# 
# # Salva o modelo final em um arquivo - seed = 58 e prob = c(0.7, 0.3) - rmse = 27.77
# saveRDS(maximo, "C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/maximo.rds")
# 
# 
# # **********************************************
# #            Modelo para o Minimo 
# # **********************************************
# modelo.min = dados  %>%
#   mutate(
#     y  = log(Minima)-log(lag_1_Minima),
#     x1 = (lag_1_Minima/lag_22_Minima)^(1/21),
#     x2 = Abertura/lag_1_Minima,
#     .keep = "none"
#   )
# 
# # Salva o modelo final em um arquivo - seed = 74 e prob = c(0.7, 0.3) rmse = 26,86
# set.seed(74)
# amostra              = sample(2, nrow(modelo.min), replace = TRUE, prob = c(0.7, 0.3))
# conjunto_treinamento = modelo.min[amostra == 1, ]
# conjunto_teste       = modelo.min[amostra == 2, ]
# minimo    = randomForest(y ~ x1 + x2, data = conjunto_treinamento)
# previsoes = predict(minimo, newdata = conjunto_teste)
# sqrt(mean((dados$lag_1_Minima[amostra == 2]*exp(conjunto_teste$y)-as.numeric(dados$lag_1_Minima[amostra == 2]*exp(previsoes)))^2))
# 
# saveRDS(minimo, "C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/minimo.rds")
# 
# # ***************************************************
# #   Modelo para o Ponto medio
# # ***************************************************
# modelo.Ponto.Medio = dados  %>%
#   mutate(
#     y  = log(Ponto_Medio)-log(lag_1_Ponto_Medio),
#     x1 = (lag_1_Ponto_Medio/lag_22_Ponto_Medio)^(1/21),
#     x2 = Abertura/lag_1_Ponto_Medio,
#     .keep = "none"
#   )
# 
# # Salva o modelo final em um arquivo - seed = 86 e prob = c(0.7, 0.3) rmse = 26,12
# set.seed(86)
# amostra              = sample(2, nrow(modelo.Ponto.Medio), replace = TRUE, prob = c(0.7, 0.3))
# conjunto_treinamento = modelo.Ponto.Medio[amostra == 1, ]
# conjunto_teste       = modelo.Ponto.Medio[amostra == 2, ]
# ponto.medio          = randomForest(y ~ x1 + x2, data = conjunto_treinamento)
# previsoes            = predict(ponto.medio, newdata = conjunto_teste)
# sqrt(mean((dados$lag_1_Ponto_Medio[amostra == 2]*exp(conjunto_teste$y)-as.numeric(dados$lag_1_Ponto_Medio[amostra == 2]*exp(previsoes)))^2))
# 
# saveRDS(ponto.medio, "C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/ponto.medio.rds")
# 
# 
# # **************************************************************
# #               Previsoes para  o dia:
# # Carrega o modelo salvo em um arquivo
# # **************************************************************
# 
# maximo      = readRDS("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/maximo.rds")
# minimo      = readRDS("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/minimo.rds")
# ponto.medio = readRDS("C:/Users/uliss/OneDrive - UFPB/Dolar/GradientBoosting/ponto.medio.rds")
# 
# # Abertura do Dia:
# dia = 1
# dados$Data[dia]
# Abertura      = 5005
# 
# 
# x1 = (dados$Maxima[dia+1]/dados$Maxima[dia+22])^(1/21)
# x2 = Abertura/dados$Maxima[dia+1]    
# X1 = tibble(x1=x1, x2=x2)
# 
# x1 = (dados$Minima[dia+1]/dados$Minima[dia+22])^(1/21)
# x2 = Abertura/dados$Minima[dia+1] 
# X2 = tibble(x1=x1, x2=x2)
# 
# x1 = (dados$Ponto_Medio[dia+1]/dados$Ponto_Medio[dia+22])^(1/21)
# x2 = Abertura/dados$Ponto_Medio[dia+1] 
# X3 = tibble(x1=x1, x2=x2)
# 
# (Minima      = as.numeric(dados$lag_1_Minima[dia]*exp(predict(minimo, newdata = X2))))
# (Ponto.Medio = as.numeric(dados$lag_1_Ponto_Medio[dia]*exp(predict(ponto.medio, newdata = X3))))
# (Maxima      = as.numeric(dados$lag_1_Maxima[dia]*exp(predict(maximo, newdata = X1))))