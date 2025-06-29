path = "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/datospanelsinNA.xlsx"
df = read_excel(path)
df <- data.frame(df)
View(df)
colnames(df)
colnames(df) <- c("capi", "art","capcorr","labor","gdp", "Index.Year","Country")

df <- df[df$Index.Year >= 1996 & df$Index.Year <= 2022, ]


getwd() 
setwd("C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R")  # Cambia el directorio
# Instalar si no está instalado
install.packages("writexl")

# Cargar librería
library(writexl)

# Exportar
write_xlsx(df, path = "base_de_datos_sin_ecuador.xlsx")
setwd("C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R")  # Cambia el directorio