library(tidyverse)
library(readxl)
library(janitor)


# Lista de archivos Excel
archivos <- c("data/Base_de_datos_Polizas_12_Diciembre-2019_Definitivo_Transp.xlsx",
              "data/comsoc-12-dic-2014-definitivo.xlsx",
              "data/Comsoc-12-Diciembre-2012_Definitivo.xlsx",
              "data/Comsoc-12-Diciembre-2015-Definitivo.xlsx",
              "data/Comsoc Pólizas 12 Defin_Diciembre-2017 Transp VF.xlsx",
              "data/comsoc_12_diciembre_2013_definitivo.xlsx",
              "data/Comsoc_P_lizas_12_Diciembre_2020_Trnsp_Def.xlsx",
              "data/Comsoc_Po_lizas_Trans_12_Diciembre_2021.xlsx",
              "data/Comsoc_Po_lizas_Transp_11_NOVIEMBRE_2023.xlsx",
              "data/Comsoc_Po_lizas_Transp_12_DICIEMBRE_2022.xlsx",
              "data/Comsoc_Pólizas_Transp 12_Diciembre-2018_Defin.xlsx",
              "data/ComsocPólizas12Dic-2016Defin.xlsx")

# Función para obtener los nombres de las columnas de un archivo Excel
get_column_names <- function(file_path) {
  hoja <- excel_sheets(file_path)[1]  # Suponiendo que queremos revisar la primera hoja
  nombres <- read_excel(file_path, sheet = hoja, n_max = 1, skip = 5) %>%
    clean_names() |>
    colnames()
  return(nombres)
}

# Usando la función para cada archivo y almacenando los resultados junto con los nombres de los archivos
nombres_columnas <- set_names(map(archivos, get_column_names), archivos)

# Imprimir los resultados
nombres_columnas

# Comparar los nombres de las columnas de cada archivo con un archivo de referencia (por ejemplo, el primer archivo)
comparaciones <- map(nombres_columnas, ~ identical(.x, nombres_columnas[[1]]))
