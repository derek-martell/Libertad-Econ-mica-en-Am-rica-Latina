library(tidyverse)
library(plm)
library(readxl)
library(psych)
library(naniar)
library(car)
library(corrplot)
library(ggplot2)
library(gridExtra)



#EN DATA SE COPIA LA DATA NUEVA
path ="C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/merged_df_sin_ecuador.csv"
df = read.csv(path)










#EN DATA SE COPIA LA DATA NUEVA
path = "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/merged_df.csv"
path2 = "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/merged_df_sin_ecuador.csv"
df = read.csv(path)
df2 = read.csv(path2)

# Verificar nombres de variables
colnames(df)

# Revisar la estructura
glimpse(df)
glimpse(df2)

# Número de países y años
length(unique(df$Country))
length(unique(df2$Country))
length(unique(df$Index.Year))
length(unique(df2$Index.Year))

# Verificar balance del panel
df %>%
  count(Country) %>%
  summarise(min = min(n), max = max(n))

describe(df[, c("capi", "art", "labor", "gdp", "Overall.Score")])

describe(df2[, c("capi", "art", "labor", "gdp", "Overall.Score")])

shapiro.test(df$capi)  # Solo si n < 5000
shapiro.test(df$art)
shapiro.test(df$labor)
shapiro.test(df$gdp)
shapiro.test(df$Overall.Score)

shapiro.test(df2$capi)  # Solo si n < 5000
shapiro.test(df2$art)
shapiro.test(df2$labor)
shapiro.test(df2$gdp)
shapiro.test(df2$Overall.Score)





df %>%
  pivot_longer(cols = c(capi, art, labor, gdp, Overall.Score)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

df2 %>%
  pivot_longer(cols = c(capi, art, labor, gdp, Overall.Score)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

library(dplyr)    # Load the dplyr package
library(tidyr)    # pivot_longer is from tidyr, which is part of the tidyverse, but it's good to explicitly load it.
library(ggplot2)  # Load ggplot2 for plotting


df <- df %>%
  mutate(
    ln_gdp = log(gdp),
    ln_capi = log(capi),
    ln_art = log(art + 1),     # Se suma 1 en caso de ceros
    ln_labor = log(labor),
    ln_Overall.Score = log(Overall.Score)
  )

df2 <- df2 %>%
  mutate(
    ln_gdp = log(gdp),
    ln_capi = log(capi),
    ln_art = log(art + 1),     # Se suma 1 en caso de ceros
    ln_labor = log(labor),
    ln_Overall.Score = log(Overall.Score)
  )


shapiro.test(df$ln_capi)  # Solo si n < 5000
shapiro.test(df$ln_art)
shapiro.test(df$ln_labor)
shapiro.test(df$ln_gdp)
shapiro.test(df$ln_Overall.Score)

shapiro.test(df2$ln_capi)  # Solo si n < 5000
shapiro.test(df2$ln_art)
shapiro.test(df2$ln_labor)
shapiro.test(df2$ln_gdp)
shapiro.test(df2$ln_Overall.Score)



ggplot(df, aes(x = Index.Year, y = ln_gdp, color = Country)) +
  geom_line() +
  labs(title = "Evolución del PIB per cápita (log)") +
  theme_minimal()

ggplot(df, aes(x = Index.Year, y = ln_art, color = Country)) +
  geom_line() +
  labs(title = "Evolución de los artículos per cápita (log)") +
  theme_minimal()

ggplot(df, aes(x = Index.Year, y = ln_labor, color = Country)) +
  geom_line() +
  labs(title = "Evolución del labor per cápita (log)") +
  theme_minimal()

ggplot(df, aes(x = Index.Year, y = ln_capi, color = Country)) +
  geom_line() +
  labs(title = "Evolución del capital per cápita (log)") +
  theme_minimal()

ggplot(df, aes(x = Index.Year, y = ln_Overall.Score, color = Country)) +
  geom_line() +
  labs(title = "Evolución del Overall.Score (log)") +
  theme_minimal()





ggplot(df2, aes(x = Index.Year, y = ln_gdp, color = Country)) +
  geom_line() +
  labs(title = "Evolución del PIB per cápita (log)") +
  theme_minimal()

ggplot(df2, aes(x = Index.Year, y = ln_art, color = Country)) +
  geom_line() +
  labs(title = "Evolución de los artículos per cápita (log)") +
  theme_minimal()

ggplot(df2, aes(x = Index.Year, y = ln_labor, color = Country)) +
  geom_line() +
  labs(title = "Evolución del labor per cápita (log)") +
  theme_minimal()

ggplot(df2, aes(x = Index.Year, y = ln_capi, color = Country)) +
  geom_line() +
  labs(title = "Evolución del capital per cápita (log)") +
  theme_minimal()

ggplot(df2, aes(x = Index.Year, y = ln_Overall.Score, color = Country)) +
  geom_line() +
  labs(title = "Evolución del Overall.Score (log)") +
  theme_minimal()




boxplot(ln_gdp ~ Country, data = df, main = "Distribución del PIB por país", las = 2)
boxplot(ln_art ~ Country, data = df, main = "Distribución del PIB por país", las = 2)
boxplot(ln_labor ~ Country, data = df, main = "Distribución del PIB por país", las = 2)
boxplot(ln_capi ~ Country, data = df, main = "Distribución del PIB por país", las = 2)
boxplot(ln_Overall.Score ~ Country, data = df, main = "Distribución del PIB por país", las = 2)


boxplot(ln_gdp ~ Country, data = df2, main = "Distribución del PIB por país", las = 2)
boxplot(ln_art ~ Country, data = df2, main = "Distribución del PIB por país", las = 2)
boxplot(ln_labor ~ Country, data = df2, main = "Distribución del PIB por país", las = 2)
boxplot(ln_capi ~ Country, data = df2, main = "Distribución del PIB por país", las = 2)
boxplot(ln_Overall.Score ~ Country, data = df2, main = "Distribución del PIB por país", las = 2)

# Matriz de correlación
cor_matrix <- df %>%
  select(ln_gdp, ln_capi, ln_art, ln_labor, Overall.Score) %>%
  cor()



library(corrplot)
library(dplyr) # Make sure dplyr is also loaded for the pipe operator %>%

# Tu matriz de correlación
cor_matrix <- df2 %>%
  select(ln_gdp, capi, labor, Overall.Score) %>%
  cor()

# Gráfico mejorado
corrplot(cor_matrix,
         method = "square",       # Cambia a cuadrados para una apariencia más limpia
         type = "upper",          # Muestra solo la parte superior para evitar redundancia
         col = COL2('RdBu', 200), # Usa una paleta de colores divergente (rojo-azul)
         addCoef.col = "black",   # Añade los coeficientes de correlación en negro
         tl.col = "black",        # Color de las etiquetas de las variables
         tl.srt = 45,             # Rota las etiquetas para que no se superpongan
         diag = FALSE,            # Elimina los valores de la diagonal (siempre 1)
         number.cex = 0.8         # Ajusta el tamaño del número
)




df2 %>%
  select(ln_gdp, capi, art, labor, Overall.Score) %>%
  cor()
as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value") %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  coord_fixed() +
  labs(title = "Heatmap de Correlación", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))



#GRAFICO PARA MOSTRAR LA CORRELACIÓN ENTRE VARIABLES


library(dplyr)   # Para el operador %>% y select
library(tidyr)   # Para pivot_longer
library(ggplot2) # Para graficar


# 1. Tu matriz de correlación (asumiendo que 'df' está disponible)
cor_matrix <- df2 %>%
  select(ln_gdp, capi, labor, Overall.Score) %>%
  cor()

# 2. Convertir la matriz de correlación a formato "largo" para ggplot2
# Esto crea columnas para 'Var1' (variables en Y), 'Var2' (variables en X) y 'value' (los coeficientes de correlación)
cor_long <- cor_matrix %>%
  as.data.frame() %>%               # Convertir la matriz a un data frame
  tibble::rownames_to_column("Var1") %>% # Convertir nombres de filas a una columna
  pivot_longer(
    cols = -Var1,                   # Todas las columnas excepto Var1
    names_to = "Var2",              # Nombre de la nueva columna para los nombres de las variables del eje X
    values_to = "value"             # Nombre de la nueva columna para los valores de correlación
  )

# 3. Crear el heatmap con ggplot2
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") + # Dibuja los cuadrados (tiles) con borde negro
  geom_text(aes(label = round(value, 2)), # Añade el texto (coeficiente redondeado)
            color = "white",
            size = 4) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", # Paleta de colores divergente
                       midpoint = 0, limit = c(-1, 1), # Centra el blanco en 0, límites de -1 a 1
                       name = "Correlación") +
  coord_fixed() + # Asegura que los tiles sean cuadrados
  labs(
    title = "Heatmap de Correlación",
    x = "", # Quita la etiqueta del eje X
    y = ""  # Quita la etiqueta del eje Y
  ) +
  theme_minimal() + # Tema minimalista para un look limpio
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rota las etiquetas del eje X
    axis.text.y = element_text(),                      # Asegura que las etiquetas del eje Y estén visibles
    panel.grid.major = element_blank(),                # Elimina las líneas de la cuadrícula principal
    panel.grid.minor = element_blank(),                # Elimina las líneas de la cuadrícula menor
    plot.title = element_text(hjust = 0.5, face = "bold") # Centra y pone en negrita el título
  )


library(dplyr); library(tidyr); library(ggplot2)
# Asegúrate de que las librerías necesarias estén cargadas



# install.packages("gplots") # Descomenta y ejecuta si no tienes gplots instalado
library(gplots)
library(dplyr) # Para usar el operador %>% si lo necesitas para otras operaciones




# install.packages("gplots") # Descomenta y ejecuta si no tienes gplots instalado
library(gplots)
library(dplyr) # Para usar el operador %>% si lo necesitas para otras operaciones
# Lista de las variables de tu modelo que quieres visualizar


variables_modelo <- c("gdp", "capi", "art", "labor", "Overall.Score")

# Asumiendo que tu variable de tiempo en 'df' se llama 'year'.
# ¡¡¡IMPORTANTE!!! Reemplaza 'df$year' si tu variable de tiempo tiene otro nombre.
time_variable <- df$Index.Year # O df$tu_nombre_de_variable_de_tiempo

# Bucle para generar un gráfico para cada variable
for (var_name in variables_modelo) {
  # Construye la fórmula: variable_actual ~ variable_de_tiempo
  formula_str <- paste(var_name, "~ time_variable")
  
  # Define el título del gráfico
  plot_title <- paste("Heterogeneidad de", var_name, "a lo largo del tiempo")
  
  # Define la etiqueta del eje Y (puedes personalizarla más si lo deseas)
  ylab_text <- paste(var_name)
  
  # Genera el gráfico
  plotmeans(
    as.formula(formula_str), # Convierte la cadena de texto a una fórmula
    data = df,               # Usa tu dataframe df
    main = plot_title,
    xlab = "Período (Año)", # Puedes cambiar "Año" por "Período" si tu variable no son años
    ylab = ylab_text,
    connect = FALSE,         # Para no conectar los puntos con líneas (útil si los años no son consecutivos o si los grupos son discretos)
    barwidth = 0.5,          # Ancho de las barras de los intervalos de confianza
    col = "steelblue",       # Color de los puntos y barras
    ccol = "darkblue"        # Color de las líneas que conectan la media (si connect=TRUE)
  )
}




library(dplyr); library(tidyr); library(ggplot2)


df2 %>%
  select(ln_gdp, capi, labor, Overall.Score) %>%
  cor() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value") %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  coord_fixed() +
  labs(title = "Heatmap de Correlación", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))


# Seleccionar las variables de interés
data_for_pairs <- df2 %>%
  select(ln_gdp, capi, art, labor, Overall.Score)

# Crear el gráfico pairs básico
pairs(data_for_pairs)

















# Matriz de correlación
cor_matrix <- df %>%
  select(ln_gdp, ln_capi, ln_art, ln_labor, ln_Overall.Score) %>%
  cor()

# Matriz de correlación
cor_matrix <- df %>%
  select(ln_gdp, capi, art, labor, Overall.Score) %>%
  cor()




# Matriz de correlación
cor_matrix <- df2 %>%
  select(ln_gdp, ln_capi, ln_art, ln_labor, Overall.Score) %>%
  cor()
# Matriz de correlación
cor_matrix <- df2 %>%
  select(ln_gdp, ln_capi, ln_art, ln_labor, ln_Overall.Score) %>%
  cor()

# Matriz de correlación
cor_matrix <- df2 %>%
  select(ln_gdp, capi, art, labor, Overall.Score) %>%
  cor()





corrplot(cor_matrix, method = "number")

# VIF
modelo_vif <- lm(ln_gdp ~ ln_capi + ln_art + ln_labor + Overall.Score, data = df2)
vif(modelo_vif)


corrplot(cor_matrix, method = "number")

# VIF
modelo_vif <- lm(ln_gdp ~ capi + art + labor + Overall.Score, data = df)
vif(modelo_vif)
