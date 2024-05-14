library(tidyverse)
library(readxl)
library(janitor)

def <- read_excel("inputs/Deflactor_2023-2024_codigo.xlsx") |> # Deflactor FUNDAR https://fundar.org.mx/wp-content/uploads/2022/04/Base-deflactor-2023.pdf
  clean_names() |> 
  filter(!is.na(deflactor_implicito_del_pib_2020_100)) |> 
  mutate(ciclo = str_remove_all(ciclo, "\\/e"))

clean_data <- read_csv("clean_data/clean_data.csv") |> 
  mutate(ciclo = as.character(year(fecha_de_gasto))) |> 
  left_join(def) |> 
  mutate(costo_total = costo + iva_del_costo) |> 
  mutate(costo_def = costo_total*(100/deflactor_implicito_del_pib_2020_100)) |> 
  mutate(partida = case_when(
    str_detect(origin_sheet, "33605") ~ "33605",
    str_detect(origin_sheet, "3600") ~ "3600",
    TRUE ~ NA_character_ # en caso de que no se encuentre ninguno de los valores
  ))

write.csv(clean_data, "clean_data/deflacted_data.csv", row.names = F)
