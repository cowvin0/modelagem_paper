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

# banco de teste e treinamento

divisao_teste_treinamento <- initial_split(data = dados, prop = 0.7)
conjunto_treinamento <- training(divisao_teste_treinamento)
conjunto_teste <- testing(divisao_teste_treinamento)

# modelo ponto médio

# receita ponto médio
dados_rec_medio <- conjunto_treinamento  |> 
  recipe(Ponto_Medio ~., data = dados) |> 
  step_arrange(Data) |> 
  step_lag(Ponto_Medio, lag = c(1, 22), role = "lag") |> 
  step_rm(c(Maxima, Minima)) |> 
  step_arrange(desc(Data)) |> 
  step_naomit()

dados_final_medio <- dados_rec_medio |> prep() |> bake(new_data = conjunto_treinamento) |> drop_na() |> 
  mutate(y = log(Ponto_Medio) - log(lag_1_Ponto_Medio),
         x1 = (lag_1_Ponto_Medio/lag_22_Ponto_Medio)^(1/21),
         x2 = Abertura/lag_1_Ponto_Medio,
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

# amostra <- initial_split(dados, prop = 0.7)
# conjunto_treinamento_medio <- dados_final_medio[amostra == 1, ]
# conjunto_teste_medio <- dados_final_medio[amostra == 2, ]

cv_ponto_medio <- vfold_cv(data = conjunto_treinamento, v = 20)

tunagem_ponto_medio <- 
  tune_grid(
    ponto_medio_wf,
    resamples = cv_ponto_medio,
    grid = 50L,
    metrics = metric_set(rsq, rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE),
    param_info = rand_params
  )

ponto_medio_wf <- ponto_medio_wf |> finalize_workflow(select_best(tunagem_ponto_medio, "rmse"))
ajuste_final <- last_fit(ponto_medio_wf, divisao_teste_treinamento, metrics = metric_set(rmse))

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

rec_maxima <- dados_final_maxima |> 
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

# salvando modelos

tunagem_maxima |> show_best(metric = "rmse", n = 10)

# atualizar workflow da minima

minima_wf <- minima_wf |> finalize_workflow(select_best(tunagem_minima, "rmse"))
ajuste_final <- last_fit(minima_wf, dados_final_minima, metrics = metric_set(rsq, rmse))
