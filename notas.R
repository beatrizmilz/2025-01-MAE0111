
library(tidyverse)
library(rvest)

# raspar a tabela que está no site, e abrir em uma tibble ------
notas_url <- "https://www.ime.usp.br/~victorf/mae0111/notas_mae0111.html"

# ler o html
notas_html <- read_html(notas_url)

# extrair a tabela, e pegar o primeiro elemento da lista
notas_df <- html_table(notas_html) |> 
  pluck(1) 

# transformar no formato longo
notas_longo <- notas_df |> 
  pivot_longer(
    cols = -NUSP,
    names_to = "prova",
    values_to = "nota"
  ) |>
  # substituir NA por 0 (NA acontece quando alguém falta na prova)
  mutate(
    nota = replace_na(nota, 0),
  )

# a média é calculada removendo a menor nota.

sem_nota_menor <- notas_longo |> 
  group_by(NUSP) |> 
  arrange(NUSP, nota) |> 
  slice(-1)

media_geral_sem_nota_menor <- sem_nota_menor |> 
  group_by(NUSP) |> 
  summarise(
    media = mean(nota)
  ) |> 
  arrange(desc(media)) |> 
  mutate(
    status = case_when(
      media >= 5 ~ "Aprovado",
      media >= 3 ~ "Recuperação",
      media < 3 ~ "Reprovado",
    )
  )

media_geral_sem_nota_menor |> 
  count(status) 

media_geral_sem_nota_menor |> 
  ggplot() +
  geom_histogram(aes(x = media, fill = status)) +
  scale_fill_manual(values = c("Aprovado" = "darkgreen", "Recuperação" = "yellow", "Reprovado" = "red")) +
  theme_light()
