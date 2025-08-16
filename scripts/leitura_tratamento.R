library(dplyr)


# leitura e tratamento ----------------------------------------------------

caminho <-
  "scripts/bases/Questionário preferência pratos principais RU - Respostas ao formulário 1.csv"

base <- 
  read.csv(caminho) %>% 
  janitor::clean_names() %>% 
  slice(-c(1:4), -35) 

tirar_coluna <- "ranqueie_de_1_menor_preferencia_a_5_maior_preferencia_as_seguintes_opcoes_de_pratos_principais_nao_vegetarianas_obs_selecione_apenas_uma_alternativa_por_linha_e_apenas_uma_alternativa_por_coluna_ou_seja_nao_repetir_nem_a_coluna_nem_a_linha_"

base <- 
  base %>% 
  rename_with(~ sub(paste0("^", tirar_coluna), "", .), starts_with(tirar_coluna)) %>% 
  rename(
    autorizacao = voce_autoriza_o_uso_dos_dados_fornecidos_neste_questionario_conforme_descrito_acima,
    vegetariano_vegano = voce_se_considera_vegetariano_e_ou_vegano,
    frequencia_ru = quantas_vezes_em_media_voce_frequenta_o_restaurante_universitario_por_semana
  ) %>% 
  select(-nome_completo) %>% 
  mutate(
    frequencia_ru = case_when(
      frequencia_ru == "Raramente (1 vez por semana)" ~ "Raramente",
      frequencia_ru == "Regularmente (2 a 5 vezes por semana)" ~ "Regularmente",
      frequencia_ru == "Nenhuma vez (0 vezes por semana)" ~ "Nenhuma vez",
      frequencia_ru == "Frequentemente (6 a 10 vezes por semana)" ~ "Frequentemente"
    ),
    frequencia_ru = factor(frequencia_ru, levels = c(
      "Nenhuma vez",
      "Raramente",
      "Regularmente",
      "Frequentemente"
    ))
  )


write.csv2(base, "scripts/bases/base_questionario.csv")
