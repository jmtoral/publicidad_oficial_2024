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
              "data/Comsoc_Po_lizas_Transp_DICIEMBRE_2023_COM.xlsx",
              "data/Comsoc_Po_lizas_Transp_12_DICIEMBRE_2022.xlsx",
              "data/Comsoc_Pólizas_Transp 12_Diciembre-2018_Defin.xlsx",
              "data/ComsocPólizas12Dic-2016Defin.xlsx")

# Función modificada para leer las primeras dos hojas de un archivo Excel
read_filter_remove_last_add_sheet_clean_names_and_file_name <- function(file_path) {
  hojas <- excel_sheets(file_path)[1:2]  # Leer las primeras dos hojas
  datos <- map_dfr(hojas, function(hoja) {  # Iterar sobre cada hoja y combinar los resultados
    datos_hoja <- read_excel(file_path, sheet = hoja, skip = 5)
    file_name <- basename(file_path)  # Obtener solo el nombre del archivo
    datos_hoja %>%
      clean_names() %>%  # Normalizar los nombres de las columnas
      filter(!is.na(fecha_de_gasto)) %>%
      filter(!is.na(cantidad)) %>%
      filter(mes != "Mes") %>%
      select(-ncol(datos_hoja)) %>%  # Eliminar la última columna
      mutate(origin_sheet = hoja, file_name = file_name)  # Agregar columna con el nombre de la hoja y el archivo
  })
  return(datos)
}

# Leyendo, filtrando, quitando la última columna, agregando el nombre de la hoja, limpiando los nombres de las columnas y agregando el nombre del archivo en todos los archivos
datos_procesados <- map(archivos, read_filter_remove_last_add_sheet_clean_names_and_file_name)

# Combinar todos los data frames en uno solo
datos_combinados <- bind_rows(datos_procesados) |> 
  select(1:30) |> 
  rowid_to_column()


# Convertir fechas

datos_limpios <- datos_combinados |> 
  mutate(costo_unitario = as.numeric(costo_unitario)) |> 
  mutate(costo = as.numeric(costo)) |> 
  mutate(iva_del_costo = as.numeric(iva_del_costo)) |> 
  mutate(fecha_de_gasto = as.Date(as.integer(fecha_de_gasto), origin = "1899-12-30")) |> 
  mutate(fecha_de_gasto = case_when(
    year(fecha_de_gasto) == 2026 ~ fecha_de_gasto - years(10),
    year(fecha_de_gasto) == 2105 ~ fecha_de_gasto - years(90),
    year(fecha_de_gasto) == 2103 ~ fecha_de_gasto - years(90),
    TRUE ~ fecha_de_gasto
  ))

#aicm <- datos_limpios |> filter(no_de_contrato_pedido == "AICM-SERV-006-2016")

# 35 observaciones de m*erda
nas <- datos_limpios |>  filter(is.na(fecha_de_gasto))
nas.p <- datos_combinados |> filter(rowid %in% nas$rowid) |> 
  select(rowid, fecha_de_gasto) |> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0014|0214|0201|0204", "2014")) |> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0012|1012", "2012")) |> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0013|1013|0213", "2013"))|> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0216|0216|0016", "2016"))|> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0217|0017|2047", "2017")) |> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0018", "2018"))|> 
  mutate(fecha_de_gasto = str_replace_all(fecha_de_gasto, "0021", "2021")) |> 
  mutate(fecha_de_gasto = as.Date(fecha_de_gasto, "%d/%m/%Y")) |> 
  rename(fecha_de_gasto_m = fecha_de_gasto)


# Añadir observacion3s malas

datos_limpios <- datos_limpios |> 
  left_join(nas.p) |> 
  mutate(fecha_de_gasto = case_when(
    is.na(fecha_de_gasto) ~ fecha_de_gasto_m,
    TRUE ~ fecha_de_gasto
  )) |> 
  select(-fecha_de_gasto_m)


write.csv(datos_limpios, "clean_data/clean_data.csv", row.names = F)
