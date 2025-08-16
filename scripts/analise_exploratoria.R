
base <- read.csv2("scripts/bases/base_questionario.csv")
# analise descritiva ------------------------------------------------------

n_respostas <- base$nome_completo %>% unique() %>% length()

paste("Quantidade de respostas:", n_respostas)

table(base$vegetariano_vegano)

table(base$frequencia_ru)

library(ggplot2)

# Exemplo de dataframe
df <- data.frame(categoria = factor(c("A", "B", "A", "C", "B", "A", "C", "C", "B")))

ggplot(base, aes(x = frequencia_ru)) +
  geom_bar(fill = "#2B4B8F") +                  # muda a cor das barras
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # valores em cima das barras
  labs(title = "Gráfico de barras da frequência semanal de idas ao RU",
       x = "",
       y = "Frequência das respostas") +
  scale_y_continuous(
    limits = c(0, max(table(base$frequencia_ru))),
    breaks = seq(0, max(table(base$frequencia_ru)), by = 2) 
    ) +
  theme_minimal() + 
  theme( 
    panel.grid.major.x = element_blank()
  )
 

# teste de page -----------------------------------------------------------

library(DescTools)

rankings <- 
  base %>% 
  select(
    carne_moida, cozido_misto, frango_grelhado, peixe_desfiado, nuggets,
  ) %>% 
  as.matrix()

head(rankings)

teste <- PageTest(rankings)

teste

names(teste)

teste$statistic
teste$p
teste$method


# teste de page - apenas quem frequenta bastante --------------------------

library(DescTools)

rankings <- 
  base %>% 
  filter(
    frequencia_ru 
      %in% c(
        "Frequentemente"
        )
    ) %>% 
  select(
    carne_moida, cozido_misto, frango_grelhado, peixe_desfiado, nuggets,
  ) %>% 
  as.matrix()

nrow(rankings)

teste <- PageTest(rankings)
teste

data_example <- data.frame(
  y = c(10, 12, 15, 11, 14, 17, 13, 16, 19),
  groups = factor(rep(c("A", "B", "C"), each = 3), levels = c("A", "B", "C")),
  blocks = factor(rep(1:3, times = 3))
); data_example

PageTest()
