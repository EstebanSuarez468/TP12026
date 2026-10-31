library(tidyverse)

if (!exists("datos")) source("10-ingesta-girai.R")

# Le doy un orden "natural" a los valores que usa la variable sec_mng
niveles_sec <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

# Nombre de cada área temática de los indicadores p70_*
areas_p70 <- c(
  p70_sesgo    = "Sesgo y discriminación injusta",
  p70_infancia = "Derechos de la infancia",
  p70_divers   = "Diversidad cultural y lingüística",
  p70_datpers  = "Protección de datos y privacidad",
  p70_genero   = "Igualdad de género",
  p70_suphum   = "Supervisión humana",
  p70_laboral  = "Protección laboral y derecho al trabajo",
  p70_segu     = "Seguridad, precisión y fiabilidad",
  p70_transp   = "Transparencia y explicabilidad"
)

# Recorte de 14 de las 38 columnas
girai <- datos %>%
  select(Pais, GIRAI_region, mng, ag, sec_mng, areas_mng, starts_with("p70_")) %>%
  mutate(
    # Creo variable derivada: brecha entre escribir (mng) y hacer (ag).
    # Positiva: más marco normativo que acción.
    # Negativa: más acción que norma.
    brecha = mng - ag,

    # Variable ordinal: le doy el orden de las categorías
    sec_mng = factor(sec_mng, levels = niveles_sec, ordered = TRUE),

    # Variable nominal
    GIRAI_region = factor(GIRAI_region),

    # Los indicadores p70_* ya vienen como 0 o 1 (no hay NA): los dejo como enteros
    across(starts_with("p70_"), as.integer)
  )

# Controlo que todo este bien y en caso contrario corto la ejecucion del script
stopifnot(
  nrow(girai) == 138,
  !any(is.na(girai$sec_mng)),
  !any(is.na(girai$GIRAI_region)),
  all(names(areas_p70) %in% names(girai))
)
