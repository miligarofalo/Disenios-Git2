#install.packages("showtext")
# install.packages("sysfonts")
#showtext_auto()

library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(tidyverse)


# Creamos el DAG
causal <- dagitty("dag {
  bb=\"0,0,1,1\"
  \"Actividad Física\" [pos=\"0.581,0.104\"]
  \"Alimentación\" [pos=\"0.416,0.576\"]
  \"Consumo Familiar\" [pos=\"0.680,0.772\"]
  \"Contención familiar\" [pos=\"0.142,0.073\"]
  \"Intento de suicidio\" [outcome,pos=\"0.791,0.302\"]
  \"Pobreza extrema\" [pos=\"0.489,0.858\"]
  \"Violencia Física\" [pos=\"0.587,0.427\"]
  Bullying [pos=\"0.329,0.800\"]
  Consumo [pos=\"0.773,0.619\"]
  Edad [pos=\"0.282,0.443\"]
  IMC [pos=\"0.388,0.121\"]
  Sexo [pos=\"0.162,0.576\"]
  Soledad [exposure,pos=\"0.146,0.262\"]
  
  \"Actividad Física\" -> \"Intento de suicidio\"
  \"Actividad Física\" -> IMC
  \"Alimentación\" -> IMC
  \"Consumo Familiar\" -> \"Contención familiar\"
  \"Consumo Familiar\" -> \"Violencia Física\"
  \"Consumo Familiar\" -> Consumo
  \"Contención familiar\" -> \"Intento de suicidio\"
  \"Contención familiar\" -> Soledad
  \"Pobreza extrema\" -> \"Alimentación\"
  \"Pobreza extrema\" -> \"Consumo Familiar\"
  \"Pobreza extrema\" -> \"Intento de suicidio\"
  \"Pobreza extrema\" -> Consumo
  \"Violencia Física\" -> \"Intento de suicidio\"
  Bullying -> \"Intento de suicidio\"
  Bullying -> \"Violencia Física\"
  Bullying -> Soledad
  Consumo -> \"Intento de suicidio\"
  Consumo -> \"Violencia Física\"
  Edad -> \"Intento de suicidio\"
  Edad -> Soledad
  IMC -> Bullying
  Sexo -> \"Intento de suicidio\"
  Sexo -> Soledad
  Soledad -> \"Intento de suicidio\"
  Soledad -> Consumo
}")




# Colores pastel cute
cute_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", 
                 "#E6CCFF", "#FFCCE5", "#CCE5FF", "#CCFFCC", "#FFE0CC",
                 "#E0CCFF", "#D9F9D9", "#FFD6A5", "#CBAACB")


# Visualización linda y sin etiquetas duplicadas
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
  'q1', 'q2', 'q4', 'q5', 'q6', 'q10', 'q15', 'q16', 'q22', 
  'q26', 'q27', 'q28', 'q29', 'q30', 'q31', 'q34', 'q35', 'q36',
  'q38', 'q40', 'q41', 'q42', 'q43', 'q49', 'q55', 'q56', 'q57', 
  'q58', 'q61', 'q62', 'q66', 'q67', 'q68'
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



# VARIABLES FINALES
# q1 --> EDAD
# q2 --> SEXO
# q22 --> se sintio solo
# q26 --> intento suicidarse
# q49 --> actividad física

# q66 q67 q68 (ciberbullying) --> bullying AGREGAR TABLA TEORICA
# q55 q56 q57 q58  --> contención familiar
# q39/q33     --> consumo familiar



























# Supongamos que tu dataframe es 'df' y 'Suicidio' es la variable binaria
# Ajustar el modelo logit
modelo_logit <- glm(Suicidio ~ Soledad + Edad + Sexo + Bullying + Consumo_Familiar + Contencion_Familiar, 
                    family = binomial(link = "logit"), data = data)


# Resumen del modelo
summary(modelo_logit)

matching <- matchit(Suicidio ~ Sexo + Edad + Actividad_Fisica, 
                    data = df, 
                    method = "nearest", 
                    distance = "mahalanobis", 
                    discard = "none")  # Usamos la distancia Mahalanobis

# Resumen de los resultados del matching
summary(matching)

# Obtener el conjunto emparejado
matched_data <- match.data(matching)

# Ver los resultados del matching
head(matched_data)

# Verificar el balance de covariables después del matching
plot(matching)


# Ajustar el modelo logit en el conjunto emparejado
modelo_final <- glm(Suicidio ~ Soledad + Sexo + Edad + Actividad_Fisica, 
                    family = binomial(link = "logit"), data = matched_data)

# Resumen del modelo final
summary(modelo_final)




# Guardar el gráfico del DAG (Si lo necesitas)
ggsave("dag_paths_soledad_to_suicidio.pdf", plot = last_plot(), width = 20, height = 16)

# Guardar el conjunto de datos emparejado
write.csv(matched_data, "matched_data.csv")