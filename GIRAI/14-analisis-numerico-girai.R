library(tidyverse)

if (!exists("girai")) source("11-manipulacion-girai.R")

# mng, ag y brecha: mediana y rango intercuartílico (RIC)
resumen_cuanti <- girai %>%
  pivot_longer(cols = c(mng, ag, brecha), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarize(
    media   = mean(valor),
    mediana = median(valor),
    Q1      = quantile(valor, 0.25, names = FALSE),
    Q3      = quantile(valor, 0.75, names = FALSE),
    RIC     = IQR(valor),
    minimo  = min(valor),
    maximo  = max(valor),
    ceros   = sum(valor == 0)
  )
print(resumen_cuanti, width = Inf)

# GIRAI_region: moda y frecuencias
# Variable nominal: la única medida de posición que corresponde es la moda.
tabla_region <- girai %>% count(GIRAI_region, sort = TRUE)
print(tabla_region)
cat("Moda de GIRAI_region:", as.character(tabla_region$GIRAI_region[1]), "\n")


# sec_mng: frecuencias y categoría mediana (ordinal)
print(table(girai$sec_mng))

codigos <- as.integer(girai$sec_mng)
cat("Categoría mediana de sec_mng:",
    levels(girai$sec_mng)[quantile(codigos, 0.5, type = 1, names = FALSE)], "\n")


# areas_mng: mediana, rango y RIC (discreta)
cat("areas_mng -> mediana:", median(girai$areas_mng),
    "| rango:", min(girai$areas_mng), "a", max(girai$areas_mng),
    "(amplitud", max(girai$areas_mng) - min(girai$areas_mng), ")",
    "| RIC:", IQR(girai$areas_mng), "\n")


# p70_*: cantidad de países por área
p70_cant <- girai %>%
  summarize(across(starts_with("p70_"), sum)) %>%
  pivot_longer(cols = everything(), names_to = "indicador", values_to = "paises") %>%
  mutate(area = unname(areas_p70[indicador])) %>%
  arrange(desc(paises))
print(p70_cant)

n_ninguna <- girai %>% select(starts_with("p70_")) %>% rowSums() %>% {. == 0} %>% sum()
cat("Países que no superan 70 en ninguna área:", n_ninguna, "de", nrow(girai), "\n")

# Bivariado: mng vs. ag y brecha según la región  
cat("Países con más acción que norma:", sum(girai$brecha < 0),
    "| más norma que acción:", sum(girai$brecha > 0),
    "| iguales:", sum(girai$brecha == 0), "\n")

brecha_region <- girai %>%
  group_by(GIRAI_region) %>%
  summarize(
    paises          = n(),
    mediana_brecha  = median(brecha),
    mas_norma       = sum(brecha > 0),
    mas_accion      = sum(brecha < 0)
  ) %>%
  arrange(desc(mediana_brecha))
print(brecha_region)

# Distribuciones condicionales: nivel de desarrollo (sec_mng) dentro de cada región
sec_region <- table(girai$GIRAI_region, girai$sec_mng)
print(round(prop.table(sec_region, margin = 1) * 100, 1))
