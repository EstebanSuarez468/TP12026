library(tidyverse)

if (!exists("girai")) source("11-manipulacion-girai.R")

dir.create("figuras", showWarnings = FALSE)

# Selecciono un color por pilar (uso los mismos en todos los graficos)
col_mng <- "#2a6fb0"
col_ag  <- "#c4611f"


# mng y ag: boxplot con los países encima 
g_box <- girai %>%
  pivot_longer(cols = c(mng, ag), names_to = "pilar", values_to = "puntaje") %>%
  mutate(pilar = factor(pilar,
                        levels = c("ag", "mng"),
                        labels = c("Acciones gubernamentales (ag)",
                                   "Marcos normativos (mng)"))) %>%
  ggplot() +
  aes(x = puntaje, y = pilar, color = pilar) +
  geom_jitter(height = 0.2, alpha = 0.3, size = 1) +
  geom_boxplot(width = 0.45, fill = NA, outlier.shape = NA, linewidth = 0.8) +
  scale_color_manual(values = c(col_ag, col_mng), guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = "Puntaje", y = "",
       title = "Distribución de los puntajes por pilar") +
  theme_classic()

ggsave("figuras/01-boxplot-mng-ag.png", g_box, width = 8, height = 4.5, dpi = 300)


# GIRAI_region: barras ordenadas

# Variable nominal: no tiene un orden natural, se ordena por cantidad de paises
g_region <- girai %>%
  count(GIRAI_region) %>%
  ggplot() +
  aes(x = reorder(GIRAI_region, n), y = n) +
  geom_col(width = 0.75, fill = "gray50", alpha = 0.8) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(x = "", y = "Cantidad de países",
       title = "Cantidad de países por región") +
  theme_classic()

ggsave("figuras/02-barras-region.png", g_region, width = 8, height = 4.5, dpi = 300)

# sec_mng: barras en el orden de la escala (anteriormente le di un orden "natural")

g_sec <- ggplot(girai) +
  aes(x = sec_mng) +
  geom_bar(width = 0.75, fill = col_mng, alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Nivel de desarrollo de marcos normativos (fuentes secundarias)",
       y = "Cantidad de países",
       title = "Nivel de desarrollo de los marcos normativos") +
  theme_classic()

ggsave("figuras/03-barras-sec-mng.png", g_sec, width = 8, height = 4.5, dpi = 300)

# areas_mng: gráfico de bastones
# Variable discreta (enteros de 0 a 19): un bastón por valor.

g_areas <- ggplot(girai) +
  aes(x = areas_mng) +
  geom_bar(width = 0.15, fill = col_mng) +
  scale_x_continuous(breaks = 0:19) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Cantidad de áreas temáticas con normativa sobre IA (de 19)",
       y = "Cantidad de países",
       title = "Cobertura normativa de los países") +
  theme_classic()

ggsave("figuras/04-bastones-areas-mng.png", g_areas, width = 8, height = 4.5, dpi = 300)

# p70_*: variable de respuesta múltiple (barras)
# Cada indicador es una columna 0/1. Se cuenta cuántos países cumplen cada área.
# Los porcentajes no suman 100 % porque un país puede cumplir varias áreas.

p70_cant <- girai %>%
  summarize(across(starts_with("p70_"), sum)) %>%
  pivot_longer(cols = everything(), names_to = "indicador", values_to = "paises") %>%
  mutate(area = unname(areas_p70[indicador]))

# Paises que no superan 70 en ninguna área (la categoría "ninguna")
n_ninguna <- girai %>%
  select(starts_with("p70_")) %>%
  rowSums() %>%
  {. == 0} %>%
  sum()

# La categoría "ninguna" se deja afuera del gráfico porque tapa a las demás
# barras (son la gran mayoría): lo informo en el pie del grafico
g_p70 <- ggplot(p70_cant) +
  aes(x = reorder(area, paises), y = paises) +
  geom_col(width = 0.75, fill = "gray50", alpha = 0.8) +
  geom_text(aes(label = paises), hjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(x = "", y = "Cantidad de países que superan 70 puntos",
       title = "Áreas temáticas en las que los países superan 70 puntos",
       caption = paste0(n_ninguna, " de ", nrow(girai),
                        " países no superan 70 puntos en ninguna área.")) +
  theme_classic()

ggsave("figuras/05-barras-p70.png", g_p70, width = 9, height = 5, dpi = 300)
