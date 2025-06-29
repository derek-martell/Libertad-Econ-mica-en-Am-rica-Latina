#DATOS PANEL
library(plm)      # Modelos de datos panel
library(readxl)   # Lectura de archivos Excel (aunque no se usa en el código actual)

# Ruta del archivo CSV con los datos
path <- "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/merged_df_sin_ecuador.csv"
df <- read.csv(path)

# Conversión a panel data: identificadores de país y año
df <- pdata.frame(df, index = c("Country", "Index.Year"))

# Vista previa de los datos
View(df)

# Nombres de las variables
colnames(df)

# Histograma del PIB per cápita (gdp)
hist(df$gdp, main = "Distribución de gdp", col = "skyblue")

# Transformación logarítmica del PIB per cápita
df$lngdp <- log(df$gdp)

# Histograma del logaritmo del PIB per cápita
hist(df$lngdp, main = "Distribución de log(gdp)", col = "skyblue")

cor(df[, c("capi", "art", "labor", "Overall.Score")])


reg_pool <- plm(gdp ~ capi + art + Overall.Score,  data = df, model = "pooling")
summary(reg_pool)


reg_fe <- plm(gdp ~ capi + art + Overall.Score,  data = df, model = "within")
summary(reg_fe)

reg_fe <- plm(gdp ~ capi + labor + Overall.Score,  data = df, model = "within")
summary(reg_fe)

reg_re <- plm(gdp ~ capi + art + Overall.Score, 
              data = df, 
              model = "random", 
              random.method = "amemiya")
summary(reg_re)



# H0: Modelo Pooled es adecuado
# H1: Modelo de Efectos Fijos es preferido
pFtest(reg_fe, reg_pool)
# Si se rechaza H0, se prefiere el modelo de efectos fijos




# H0: Efectos Aleatorios (RE) es consistente y eficiente
# H1: Efectos Fijos (FE) es preferible
phtest(reg_re, reg_fe)
# Si se rechaza H0, se prefiere el modelo de efectos fijos




#pairs_data <- as.data.frame(df[, c("capi", "art", "labor", "Overall.Score")])
#graphics::pairs(pairs_data)
#graphics::pairs(df[, c("capi", "art", "labor", "Overall.Score")])



library(ggplot2) # Para gráficos más bonitos

# O con ggplot2 (requiere el paquete 'broom' para facilitar la preparación de datos)
# install.packages("broom")
library(broom)
aug_pool <- augment(reg_pool, df) # 'df' es tu conjunto de datos original
ggplot(aug_pool, aes(.fitted, .resid)) +
geom_point() +
geom_hline(yintercept = 0, lty = 2, color = "red") +
labs(title = "Residuos vs. Valores Ajustados (Pooled)")


aug_pool <- augment(reg_fe, df) # 'df' es tu conjunto de datos original
ggplot(aug_pool, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  labs(title = "Residuos vs. Valores Ajustados (Pooled)")


aug_pool <- augment(reg_re, df) # 'df' es tu conjunto de datos original
ggplot(aug_pool, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  labs(title = "Residuos vs. Valores Ajustados (Pooled)")



# Para el modelo Pooled
plot(df$gdp, reg_pool$fitted.values,
     xlab = "GDP Real", ylab = "GDP Predicho",
     main = "Real vs. Predicho (Pooled)")
abline(a = 0, b = 1, col = "blue", lty = 2) # Línea de 45 grados

# Para el modelo de Efectos Fijos
plot(df$gdp, reg_fe$fitted.values,
     xlab = "GDP Real", ylab = "GDP Predicho",
     main = "Real vs. Predicho (Efectos Fijos)")
abline(a = 0, b = 1, col = "blue", lty = 2)

# Para el modelo de Efectos Aleatorios
plot(df$gdp, reg_re$fitted.values,
     xlab = "GDP Real", ylab = "GDP Predicho",
     main = "Real vs. Predicho (Efectos Aleatorios)")
abline(a = 0, b = 1, col = "blue", lty = 2)



# install.packages("jtools")
# install.packages("broom.mixed") # Útil con plm
library(jtools)
library(ggplot2)

# Graficar los coeficientes de los tres modelos lado a lado
plot_summs(reg_pool, reg_fe, reg_re,
           model.names = c("Pooled", "Efectos Fijos", "Efectos Aleatorios"),
           legend.title = "Modelo",
           coefs = c("CAPI" = "capi", "ART" = "art", "Overall Score" = "Overall.Score"),
           inner_ci_level = .90 # Para un intervalo de confianza del 90%
) +
  labs(title = "Comparación de Coeficientes de Regresión",
       x = "Estimación del Coeficiente",
       y = "Variable Predictora") +
  theme_minimal()





