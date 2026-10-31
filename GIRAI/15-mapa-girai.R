# Mapa mundial de la brecha entre escribir y hacer (mng - ag)

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

if (!exists("girai")) source("11-manipulacion-girai.R")

dir.create("figuras", showWarnings = FALSE)

# Mismos colores que en las demás figuras: azul = norma (mng), naranja = acción (ag)
col_mng <- "#2a6fb0"
col_ag  <- "#c4611f"


# Datos: agrego el código ISO3 de cada país
# El recorte (girai) no trae la columna ISO3, así que la tomo de datos.
# Como girai se armó a partir de datos sin reordenar filas, coinciden fila a fila:
# lo verifico con el nombre del país.

stopifnot(identical(datos$Pais, girai$Pais))

datos_mapa <- girai %>%
  mutate(ISO3 = datos$ISO3) %>%
  select(ISO3, Pais, brecha)


# Mapa base y unión con los datos

mundo <- ne_countries(scale = "medium", returnclass = "sf")
col_iso <- if ("iso_a3_eh" %in% names(mundo)) "iso_a3_eh" else "iso_a3"

mapa_datos <- mundo %>%
  filter(!(.data[[col_iso]] %in% "ATA")) %>%   # saco la Antártida
  left_join(datos_mapa, by = setNames("ISO3", col_iso))

# Control: países de la base que no se pudieron ubicar en el mapa
# (suelen ser islas pequeñas, que no aparecen a esta escala)
sin_mapa <- datos_mapa %>%
  filter(!(ISO3 %in% mundo[[col_iso]])) %>%
  pull(Pais)
cat("Países de la base que no aparecen en el mapa:", length(sin_mapa), "\n")
print(sin_mapa)


# Mapa

# azul = más norma que acción, naranja = más acción

lim <- max(abs(datos_mapa$brecha))

g_mapa <- ggplot(mapa_datos) +
  geom_sf(aes(fill = brecha), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(low = col_ag, mid = "gray95", high = col_mng,
                       midpoint = 0, limits = c(-lim, lim),
                       na.value = "gray85",
                       name = "Brecha (mng - ag)") +
  labs(title = "¿Escriben más de lo que hacen? La brecha entre norma y acción",
       subtitle = "Azul: más marco normativo que acción. Naranja: más acción que marco normativo.",
       caption = "Gris: país sin dato en la base, o no visible a esta escala del mapa.") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "#1b2030", color = NA))

ggsave("figuras/09-mapa-brecha.png", g_mapa, width = 11, height = 6, dpi = 300)
