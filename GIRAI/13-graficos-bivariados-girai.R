library(tidyverse)

if (!exists("girai")) source("11-manipulacion-girai.R")

dir.create("figuras", showWarnings = FALSE)

col_mng <- "#2a6fb0"

# Etiqueta de región con la cantidad de países, para que se vea el tamaño de cada grupo
# (por ejemplo, América del Norte tiene solo 3)
girai <- girai %>%
  add_count(GIRAI_region, name = "n_region") %>%
  mutate(region_n = paste0(GIRAI_region, " (n = ", n_region, ")"))


# Cuantitativa vs. cuantitativa: mng vs. ag
# La diagonal marca los países con mng = ag
# Por encima de la diagonal: más acción que norma. 
# Por debajo: más norma que acción.

n_mas_accion <- sum(girai$brecha < 0)
n_mas_norma  <- sum(girai$brecha > 0)

g_disp <- ggplot(girai) +
  aes(x = mng, y = ag) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(alpha = 0.5, color = col_mng, size = 1.8) +
  annotate("text", x = 2, y = 98, hjust = 0, size = 3.5,
           label = paste0("Más acción que norma: ", n_mas_accion, " países")) +
  annotate("text", x = 98, y = 2, hjust = 1, size = 3.5,
           label = paste0("Más norma que acción: ", n_mas_norma, " países")) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(x = "Marcos normativos (mng)", y = "Acciones gubernamentales (ag)",
       title = "Marcos normativos y acciones gubernamentales") +
  theme_classic()

ggsave("figuras/06-dispersion-mng-ag.png", g_disp, width = 7, height = 7, dpi = 300)


# Cuantitativa vs. categórica: brecha (mng - ag) según la región (boxplot)
# La línea en cero separa "más acción que norma" (izquierda) de "más norma que acción" (derecha).

g_brecha <- ggplot(girai) +
  aes(x = reorder(region_n, brecha, FUN = median), y = brecha) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  coord_flip() +
  labs(x = "", y = "Brecha mng - ag.",
       title = "Brecha entre escribir y hacer, según la región",
       caption = "Cada punto es un país.") +
  theme_light()

ggsave("figuras/07-boxplot-brecha-region.png", g_brecha, width = 9, height = 5, dpi = 300)

# Categórica vs. categórica: región vs. sec_mng (barras apiladas al 100 %)
# Se usa el 100 % porque las regiones tienen tamaños muy distintos (de 3 a 41 países).

g_region_sec <- ggplot(girai) +
  aes(x = region_n, fill = sec_mng) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  coord_flip() +
  labs(x = "", y = "Porcentaje de países",
       fill = "Nivel de desarrollo\n(fuentes secundarias)",
       title = "Nivel de desarrollo de marcos normativos según la región") +
  theme_minimal()

ggsave("figuras/08-barras-region-sec-mng.png", g_region_sec, width = 9, height = 5, dpi = 300)
