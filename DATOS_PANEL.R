library(plm)
library(readxl)
    
    
#EN DATA SE COPIA LA DATA NUEVA
path = "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/merged_df_sin_ecuador.csv"
df = read.csv(path)
    
#EN LUGAR DE NR (VA NOMBRE O EL NOMBRE QUE ESTÉ EN LA COLUMNA DE PAÍSES, EN YEAR, VAL EL AÑO)
df <- pdata.frame(df, index = c("Country","Index.Year"))
View(df)
    
colnames(df)
#eN lwage va la endógena, o sea el pbi per capita
# en el resto va el resto de variables
#HACEMOS LO MISMO PARA CADA UNO DEL RESTO DE REgresiones
hist(df$gdp, main = "Distribución de gdp", col = "skyblue")


df$lngdp <- log(df$gdp)


hist(df$lngdp, main = "Distribución de gdp", col = "skyblue")


cor(df[, c("capi", "art", "labor", "Overall.Score")])


pairs(df[, c("capi", "art", "labor", "Overall.Score")])






reg_pool = plm(gdp ~ capi + art + labor+ Overall.Score, data = df, model = "pooling")
summary(reg_pool)

#ESTIMACIÖN DE EFECTOS FIJOS
reg_fe = plm(gdp ~ capi + art + labor+ Overall.Score, data = df, model = "within")
summary(reg_fe)

#ALEATORIOS
reg_re = plm(gdp ~ capi + art + labor+ Overall.Score, data = df, model = "random")
reg_re2 <- plm(gdp ~ capi + art + labor + Overall.Score, 
              data = df, 
              model = "random", 
              random.method = "amemiya")
summary(reg_re2)

#HIpotesis nula = se prefiere el modelo Pooled
#Hipotesis alternativa= se prefiere el modelo de efectod fijos
pFtest(reg_fe, reg_pool)
#Se rechaza la hipótesis nula

#Test de Hausman
phtest(reg_re, reg_fe)
#HA = REF_FE
#HO = REG_R


reg_pool = plm(gdp ~ capi + art + Overall.Score, data = df, model = "pooling")
summary(reg_pool)

#ESTIMACIÖN DE EFECTOS FIJOS
reg_fe = plm(gdp ~ capi + art + Overall.Score, data = df, model = "within")
summary(reg_fe)

#ALEATORIOS
reg_re = plm(gdp ~ capi + art + Overall.Score, data = df, model = "random")
reg_re2 <- plm(gdp ~ capi + art  + Overall.Score, 
               data = df, 
               model = "random", 
               random.method = "amemiya")
summary(reg_re2)

#HIpotesis nula = se prefiere el modelo Pooled
#Hipotesis alternativa= se prefiere el modelo de efectod fijos
pFtest(reg_fe, reg_pool)
#Se rechaza la hipótesis nula

#Test de Hausman
phtest(reg_re, reg_fe)
#HA = REF_FE
#HO = REG_R






library(ggplot2) # Para gráficos más bonitos

# Para el modelo Pooled
plot(reg_pool$fitted.values, reg_pool$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Residuos vs. Valores Ajustados (Pooled)")
abline(h = 0, col = "red", lty = 2) # Agrega una línea en cero

# Para el modelo de Efectos Fijos
plot(reg_fe$fitted.values, reg_fe$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Residuos vs. Valores Ajustados (Efectos Fijos)")
abline(h = 0, col = "red", lty = 2)

# Para el modelo de Efectos Aleatorios
plot(reg_re$fitted.values, reg_re$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Residuos vs. Valores Ajustados (Efectos Aleatorios)")
abline(h = 0, col = "red", lty = 2)

# O con ggplot2 (requiere el paquete 'broom' para facilitar la preparación de datos)
# install.packages("broom")
# library(broom)
# aug_pool <- augment(reg_pool, df) # 'df' es tu conjunto de datos original
# ggplot(aug_pool, aes(.fitted, .resid)) +
#   geom_point() +
#   geom_hline(yintercept = 0, lty = 2, color = "red") +
#   labs(title = "Residuos vs. Valores Ajustados (Pooled)")