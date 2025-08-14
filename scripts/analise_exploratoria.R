library(dplyr)


# leitura e tratamento ----------------------------------------------------

caminho <-
  "scripts/bases/Questionário preferência pratos principais RU - Respostas ao formulário 1.csv"

base <- 
  read.csv(caminho) %>% 
  janitor::clean_names() %>% 
  slice(-c(1:4)) 

tirar_coluna <- "ranqueie_de_1_menor_preferencia_a_5_maior_preferencia_as_seguintes_opcoes_de_pratos_principais_nao_vegetarianas_obs_selecione_apenas_uma_alternativa_por_linha_e_apenas_uma_alternativa_por_coluna_ou_seja_nao_repetir_nem_a_coluna_nem_a_linha_"

base <- 
  base %>% 
  rename_with(~ sub(paste0("^", tirar_coluna), "", .), starts_with(tirar_coluna))


# analise descritiva ------------------------------------------------------

n_respostas <- base$nome_completo %>% unique() %>% length()

paste("Quantidade de respostas:", n_respostas)

table(base$voce_se_considera_vegetariano_e_ou_vegano)

table(base$quantas_vezes_em_media_voce_frequenta_o_restaurante_universitario_por_semana)


# teste de page -----------------------------------------------------------

library(DescTools)

rankings <- 
  base %>% 
  select(
    frango_grelhado, 
    carne_moida, nuggets,cozido_misto,peixe_desfiado
    ) %>% 
  as.matrix()

head(rankings)

teste <- PageTest(rankings)

teste

names(teste)

teste$statistic
teste$p
teste$method


