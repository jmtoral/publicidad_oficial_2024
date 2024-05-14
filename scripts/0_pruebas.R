library(tidyverse)
library(readxl)

hojas <- excel_sheets("data/Comsoc_Po_lizas_Transp_12_DICIEMBRE_2022.xlsx")

c22_1 <- read_excel("data/Comsoc_Po_lizas_Transp_12_DICIEMBRE_2022.xlsx", sheet = 1, skip = 5) |> 
  filter(!is.na(`Fecha de gasto`)) |> 
  filter(!is.na(Cantidad)) |> 
  filter(Mes != "Mes") |> 
  mutate(Cantidad = as.numeric(Cantidad)) |> 
  mutate(`Costo Unitario` = as.numeric(`Costo Unitario`)) |> 
  mutate(`Costo` = as.numeric(`Costo`)) |> 
  mutate(`IVA del Costo` = as.numeric(`IVA del Costo`)) |> 
  mutate(hoja = hojas[1])

c22_2 <- read_excel("data/Comsoc_Po_lizas_Transp_12_DICIEMBRE_2022.xlsx", sheet = 2, skip = 5) |> 
  filter(!is.na(`Fecha de gasto`)) |> 
  filter(!is.na(Cantidad)) |> 
  filter(Mes != "Mes") |> 
  mutate(Cantidad = as.numeric(Cantidad)) |> 
  mutate(`Costo Unitario` = as.numeric(`Costo Unitario`)) |> 
  mutate(`Costo` = as.numeric(`Costo`)) |> 
  mutate(`IVA del Costo` = as.numeric(`IVA del Costo`))|> 
  mutate(hoja = hojas[2])



hojas <- excel_sheets("data/Comsoc-12-Diciembre-2012_Definitivo.xlsx")

c12_1 <- read_excel("data/Comsoc-12-Diciembre-2012_Definitivo.xlsx", sheet = 1, skip = 5) |> 
  filter(!is.na(`Fecha de gasto`)) |> 
  filter(!is.na(Cantidad)) |> 
  filter(Mes != "Mes") |> 
  mutate(Cantidad = as.numeric(Cantidad)) |> 
  mutate(`Costo Unitario` = as.numeric(`Costo Unitario`)) |> 
  mutate(`Costo` = as.numeric(`Costo`)) |> 
  mutate(`IVA del Costo` = as.numeric(`IVA del Costo`)) |> 
  mutate(hoja = hojas[1])

c12_2 <- read_excel("data/Comsoc_Po_lizas_Transp_12_DICIEMBRE_2022.xlsx", sheet = 2, skip = 5) |> 
  filter(!is.na(`Fecha de gasto`)) |> 
  filter(!is.na(Cantidad)) |> 
  filter(Mes != "Mes") |> 
  mutate(Cantidad = as.numeric(Cantidad)) |> 
  mutate(`Costo Unitario` = as.numeric(`Costo Unitario`)) |> 
  mutate(`Costo` = as.numeric(`Costo`)) |> 
  mutate(`IVA del Costo` = as.numeric(`IVA del Costo`))|> 
  mutate(hoja = hojas[2])


c20_1 <- read_excel("data/Comsoc_P_lizas_12_Diciembre_2020_Trnsp_Def.xlsx", sheet = 1, skip = 5)
