library(plm)

path="C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/heritage-index-of-economic-freedom-20250602195025.csv"

df = read.csv(path, skip = 4)
View(df)

# Assuming your dataframe is named 'df'
selected_columns <- df[, c("Country", "Index.Year", "Overall.Score")]
filtered_rows <- selected_columns[selected_columns$Index.Year >= 1996 & selected_columns$Index.Year <= 2022, ]


View(filtered_rows)

paises= c("Colombia", "Bolivia", "Peru", "Guatemala", "Mexico")

selected_countries <- filtered_rows[filtered_rows$Country %in% paises, ]

View(selected_countries)

df_1 = data.frame(selected_countries)


library(readxl)
path = "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/base_de_datos_sin_ecuador.xlsx"
df <- read_excel(path)
df <- data.frame(df)
View(df)
colnames(df) <- c("capi", "art","capcorr","labor","gdp", "Index.Year","Country")

# Assuming the column with country names is called 'Country'
# Replace 'Country' with the actual name of your country column
df$Country <- gsub("Perú", "Peru", df$Country)
df$Country <- gsub("México", "Mexico", df$Country)

View(df)

merged_df <- merge(df, df_1, by = c("Country", "Index.Year"))
# Exportar a CSV
write.csv(merged_df, file = "merged_df_sin_ecuador.csv", row.names = FALSE)

getwd()           # Muestra el directorio actua
setwd("C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R")  # Cambia el directorio


#EN CASO DE NECESITARLO
path = "C:/eco-poli/TRABAJO FINAL/CÓDIGO EN R/base_de_datos_sin_ecuador.xlsx"
df = read_excel(path)
df <- data.frame(df)
colnames(df)
colnames(df) <- c("capi", "art","capcorr","labor","gdp", "Index.Year","Country")

merged_df <- merge(df, df_1, by = c("Country", "Index.Year"))
View(merged_df)
