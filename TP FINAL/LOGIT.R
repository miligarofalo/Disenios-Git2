install.packages("showtext")
install.packages("sysfonts")

library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(tidyverse)
library(readr)


# Creamos el DAG
causal <- dagitty("dag {
  bb=\"0,0,1,1\"
  \"Actividad Fisica\" [pos=\"0.581,0.104\"]
  \"Alimentacion\" [pos=\"0.416,0.576\"]
  \"Contencion familiar\" [pos=\"0.142,0.073\"]
  \"Intento de suicidio\" [outcome,pos=\"0.791,0.302\"]
  \"Pobreza extrema\" [pos=\"0.489,0.858\"]
  \"Violencia Fisica\" [pos=\"0.587,0.427\"]
  Bullying [pos=\"0.329,0.800\"]
  Consumo [pos=\"0.773,0.619\"]
  Edad [pos=\"0.282,0.443\"]
  IMC [pos=\"0.388,0.121\"]
  Sexo [pos=\"0.162,0.576\"]
  Soledad [exposure,pos=\"0.146,0.262\"]
  
  \"Actividad Fisica\" -> \"Intento de suicidio\"
  \"Actividad Fisica\" -> IMC
  \"Alimentacion\" -> IMC
  \"Contencion familiar\" -> \"Intento de suicidio\"
  \"Contencion familiar\" -> Soledad
  \"Pobreza extrema\" -> \"Alimentacion\"
  \"Pobreza extrema\" -> \"Intento de suicidio\"
  \"Pobreza extrema\" -> Consumo
  \"Violencia Fisica\" -> \"Intento de suicidio\"
  Bullying -> \"Intento de suicidio\"
  Bullying -> \"Violencia Fisica\"
  Bullying -> Soledad
  Consumo -> \"Intento de suicidio\"
  Consumo -> \"Violencia Fisica\"
  Edad -> \"Intento de suicidio\"
  Edad -> Soledad
  IMC -> Bullying
  Sexo -> \"Intento de suicidio\"
  Sexo -> Soledad
  Soledad -> \"Intento de suicidio\"
  Soledad -> Consumo
}")

#Letra
font_add_google("Quicksand", "quicksand")
showtext_auto()

# Colores pastel cute
cute_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", 
                 "#E6CCFF", "#FFCCE5", "#CCE5FF", "#CCFFCC", "#FFE0CC",
                 "#E0CCFF", "#D9F9D9", "#FFD6A5", "#CBAACB")


# Visualización 
ggdag(causal, text = FALSE) +  
  geom_dag_node(aes(fill = factor(name)), shape = 21, size = 30, color = "white", alpha = 0.9) +  # ⬅️ nodos más grandes
  geom_dag_edges(edge_color = "#F9AFAE", edge_alpha = 0.8, edge_width = 1.3) +
  geom_dag_label(color = "black", fill = NA, label.size = 0, size = 7, family = "quicksand") +  # ⬅️ texto más grande
  scale_fill_manual(values = cute_colors) +
  theme_minimal(base_family = "quicksand") +
  theme_dag() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFFDF6", color = NA),
    panel.background = element_rect(fill = "#FFFDF6", color = NA)
  )

causal %>% ggdag_paths_fan (shadow = TRUE, node_size = 18, text_size = 4, spread = 1.5, label_size = text_size, node= TRUE, text_col = "white")

# Obtener todos los caminos entre "Soledad" e "Intento de suicidio"
all_paths <- dag_paths(causal, from = "Soledad", to = "Intento de suicidio", paths_only = FALSE)




# Crear el gráfico del DAG completo
dag_plot <- ggdag(causal, text = FALSE) +  
  geom_dag_node(aes(fill = factor(name)), shape = 21, size = 30, color = "white", alpha = 0.9) +  # Nodos más grandes
  geom_dag_edges(edge_color = "#F9AFAE", edge_alpha = 0.8, edge_width = 1.3) +
  geom_dag_label(color = "black", fill = NA, label.size = 0, size = 7, family = "quicksand") +  # Texto más grande
  scale_fill_manual(values = cute_colors) +
  theme_minimal(base_family = "quicksand") +
  theme_dag() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFFDF6", color = NA),
    panel.background = element_rect(fill = "#FFFDF6", color = NA)
  )



paths(causal, from = "Soledad", to = "Intento de suicidio")



library(ggdag)
library(ggplot2)

dag_plot <- ggdag_paths(causal, from = "Soledad", to = "Intento de suicidio",
                        text_size = 5, node_size = 20) +  # ⬅️ nodos y textos más grandes
  theme_dag() +
  geom_dag_node(color = "pink") + 
  geom_dag_text(color = "black", size = 5) +  # ⬅️ texto legible
  geom_dag_edges(edge_width = 1.2)  # ⬅️ bordes más gruesos

# Guardar con dimensiones grandes para mejor visibilidad
ggsave("dag_paths_soledad_to_suicidio.pdf", dag_plot, width = 20, height = 16)  # ⬅️ más grande



# ------------------------------------------------------------------------------------------
install.packages("MatchIt")
library(MatchIt)


library(rstudioapi)
library(readr)

setwd(dirname(getActiveDocumentContext()$path))
getwd()
data <- read_csv("National.csv")


# ------------------------------------------------------------------------------------------------------------------------------
# FILTRADO DE BASE DE DATOS

# Columnas a excluir explícitamente
excluir <- c("weight", "stratum", "psu", "site", "record")

# Filtrar columnas que NO empiezan con 'qn' y NO están en la lista de exclusión
columnas_filtradas <- setdiff(names(data), c(excluir, grep("^qn", names(data), value = TRUE)))

# Filtrar el DataFrame con las columnas filtradas
data <- data[, columnas_filtradas]

# Variables de interés (similar a lo que tenías en el código Python)
variables_interes <- c(
  'q1', 'q2',  'q22', 
  'q26', 
   'q55', 'q56', 'q57', 'q58','q39', 'q66', 'q67' 
    )

# Eliminar columnas que no están en las variables de interés
data_interes <- data[, variables_interes]

# Variables que NO están en la selección de interés
variables_no_interes <- setdiff(names(data), variables_interes)

# Verificar los resultados
cat("Variables mantenidas:\n")
print(names(data_interes))

cat("\nVariables no de interés:\n")
print(variables_no_interes)

data_clean <- data %>% 
  mutate(across(everything(), as.numeric)) %>%
  drop_na()
# SACAMOS NAN!!!!


# COMBIANCION DE LAS VARIABLES -----------------------------

# A. Bullying: (q66, q67)
data_clean <- data_clean %>%
  mutate(
    q66_bin = if_else(q66 == 1, 1, 0),  # 1 = bullying, 0 = no bullying
    q67_bin = if_else(q67 == 1, 1, 0),
    bullying_score = q66_bin + q67_bin,   # Suma: 0 (no bullying) a 2 (bullying en ambos)
    bullying_bin = if_else(bullying_score > 0, 1, 0)  # Binario: si sufre bullying en alguna variable
  )

# B. Contención familiar: promedio de 4 ítems
data_clean <- data_clean %>%
  mutate(
    contencion_familiar = rowMeans(select(., q55, q56, q57, q58), na.rm = TRUE)
  )

# C. Consumo familiar: binario si q39 tienen valores problemáticos
data_clean <- data_clean %>%
  mutate(
    consumo_familiar = if_else(q39 > 1, 1, 0)
  )

# 🔢  NORMALIZACIÓN (Z-SCORE)
# ================================

data_clean <- data_clean %>%
  mutate(
    bullying_score_z = scale(bullying_score)[,1],
    contencion_familiar_z = scale(contencion_familiar)[,1]
  )



modelo_lineal <- glm(Suicidio ~ bullying_score_z, data = data_clean, family = "binomial")
summary(modelo_lineal)
plot(modelo_lineal)


# recodificamos las variables que quedan 
data_clean <- data_clean %>%
  mutate(
    Sexo = if_else(q2 == 2, 1, 0),      
    Suicidio = if_else(q26 > 1, 1, 0)   # 1 = al menos un intento
  )

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


# 'Suicidio' es la variable binaria
# Ajustar el modelo logit
#modelo_logit <- glm(Suicidio ~ Soledad + Edad + Sexo + Bullying + Consumo_Familiar + Contencion_Familiar, 
#                    family = binomial(link = "logit"), data = data)


# 2. Ajustar modelo logístico
modelo_logit <- glm(Suicidio ~ bullying_score_z + contencion_familiar_z +
                      q1 + Sexo + q22,
                    data = data_clean,
                    family = binomial)

install.packages("modelr")
library(modelr)
modelplot(modelo_logit, coef_omit = 'Interc')

# 3. Ver resultados
summary(modelo_logit)
exp(0.597272)



#  Verificación de multicolinealidad --> debajo de 2
# Asegurate de tener la librería car
install.packages("car")  # Si no la tenés instalada
library(car)

# Ver VIF del modelo
vif(modelo_logit)




# ---------------------------------------------------- MATCHING ---------------------------------------------------------------
library(MatchIt)

data_clean <- data_clean %>%
  mutate(soledad = if_else(q22 >= 4, 1, 0))  # 1 = se siente solo, 0 = no se siente solo
table(data_clean$q22, useNA = "always")

# Crear versión binaria de contención familiar (ejemplo: arriba y abajo mediana)
data_clean <- data_clean %>%
  mutate(
    contencion_bin = if_else(contencion_familiar > median(contencion_familiar, na.rm = TRUE), 1, 0)
  )

# Usamos soledad como una variable ordinal (no binaria)
match <- matchit(soledad ~ q1 + Sexo + bullying_bin + contencion_bin,  # se usan las normalizadas
                 data = data_clean,
                 method = "nearest", distance = "Mahalanobis")


# Usamos soledad como una variable ordinal (no binaria)
match <- matchit(soledad ~ q1 + Sexo + bullying_score_z + contencion_familiar_z,  # se usan las normalizadas
                 data = data_clean,
                 method = "nearest", distance = "Mahalanobis")


# Diagnóstico
summary(match)
plot(match)

modelo_matched <- glm(Suicidio ~ soledad, data = match.data(match), family = binomial)
summary(modelo_matched)
exp(1.30454)





# Guardar el gráfico del DAG (Si lo necesitas)
ggsave("dag_paths_soledad_to_suicidio.pdf", plot = last_plot(), width = 20, height = 16)
