#library(readr)
#ENSSyR_Mujeres_BaseUsuario <- read_delim("C:/Users/Mili/Downloads/ENSSyR_Mujeres_BaseUsuario.txt", 
#  delim = "|",
#  quote = "\"",
#  escape_double = FALSE,
#  trim_ws = TRUE,
  #na = c("", "NA", "9999")               # Ajusta si hay otros códigos de NA
#)
# Explicacion del codigo de arriba: 
#Tipo de archivo: texto plano.
#Delimitador: “|” (pipe, barra vertical, ASCII 124).
#Calificador de texto: comilla doble (ASCII 34).
#Encabezado en la primera línea: sí.
#Codificación: UTF-8.
#Salto de línea: Windows (CRLF)
# Los códigos 9, 99, 999 y 9999 que en general son el valor máximo de cada escala, se corresponden con datos que el encuestado no sabía o no quería contestar



library(rstudioapi)
library(readr)
setwd(dirname(getActiveDocumentContext()$path))
getwd()
data <- read_csv("National.csv")

