library(googlesheets4)

url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?usp=sharing"

gs4_deauth()

datos <- read_sheet(url, sheet = "tp", skip = 1)

#deben ser 138 filas y 38 columnas
str(datos)
stopifnot(nrow(datos) == 138, ncol(datos) == 38)