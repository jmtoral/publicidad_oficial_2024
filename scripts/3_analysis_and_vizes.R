
# Bibliotecas -------------------------------------------------------------


library(tidyverse)
library(ggbump)
library(showtext)
library(rcartocolor)
library(ggraph)
library(igraph)
library(tidygraph)

# Temas -------------------------------------------------------------------

showtext_auto()
font_add_google("Anton", family = "Anton")
font_add_google("Mulish", family = "Mulish")

tema <- theme(
  plot.title = element_text(size = 24, face = "bold", family = "Anton", color = "grey15"),
  plot.subtitle = element_text(size = 20, colour = "grey30", family = "Mulish"),
  text = element_text(family = "Mulish", color = "grey20"),
  plot.title.position = 'plot',
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(size = 0.1),
  axis.title.y = element_text(angle = 0, vjust = 0.5),
  axis.title = element_text(family = "Mulish", color = "grey50", size = 18),
  axis.text = element_text(family = "Mulish", size = 14),
  legend.text = element_text(family = "Mulish", size = 14),
  legend.title = element_text(family = "Mulish", size = 16),
  legend.position="bottom"
)

# Gasto total por año -----------------------------------------------------

clean_data |> 
  mutate(yr = floor_date(fecha_de_gasto, "year")) |> 
  group_by(yr) |> 
  summarise(costo_def = sum(costo_def)) |> 
  filter(yr >= as.Date('2012-01-01') & yr <= as.Date('2023-01-01') ) |> 
  ggplot(aes(yr, costo_def/1000000000, fill = costo_def,
             label = round(costo_def/1000000000,1))) +
  geom_col() +
  geom_text(vjust = 2, color = "white", fontface = "bold", size = 5)+
  scale_x_date(breaks = seq(as.Date("2012-01-01"), as.Date("2023-12-31"), by="1 years"),
               date_labels = "%Y") +
  theme_minimal() +
  tema +
  labs(title = "Gasto total por año de pago",
       subtitle = "Miles de millones de pesos de 2020",
       y= "$", x= "\nAño de pago",
       caption = "Fuente: Secretaría de la Función Pública\nPara el año 2023, las cifras no son definitivas.\n@jmtoralc") +
  scale_fill_distiller(palette="Spectral", direction =-1)+
  guides(fill = "none")

ggsave("graphs/gasto_total.png", width = 5, height = 4, dpi = 180)


# Gasto total por beneficiario --------------------------------------------



rank_bene_tot <- clean_data %>%
  mutate(beneficiario = str_to_lower(beneficiario)) |> 
  mutate(beneficiario = str_trim(beneficiario)) |> 
  mutate(beneficiario = str_replace_all(beneficiario, ", s\\.a\\. de c\\.v\\.| sa de cv| s\\.a\\. de c\\.v\\.|, s\\.a\\. de c\\.v\\.|", "")) %>%
  mutate(beneficiario = case_when(
    str_detect(beneficiario, "(?i)televisa") ~ "Televisa",
    str_detect(beneficiario, "(?i)demos|jornada") ~ "La Jornada",
    str_detect(beneficiario, "(?i)azteca") ~ "TV Azteca",
    str_detect(beneficiario, "(?i)especializado en economía|el economista") ~ "El Economista",
    str_detect(beneficiario, "el financiero") ~ "El Financiero",
    str_detect(beneficiario, "(?i)agencia digital|milenio") ~ "Milenio",
    str_detect(beneficiario, "(?i)imagen|image|cadena tres i") ~ "Imagen",
    str_detect(beneficiario, "(?i)universal") ~ "El Universal",
    str_detect(beneficiario, "(?i)5m2|cinco m") ~ "5M2",
    str_detect(beneficiario, "(?i)formula|fórmula|pm onstreet") ~ "Grupo Fórmula",
    str_detect(beneficiario, "(?i)radiodifusores mexicanos indep") ~ "Radiodifusores Mexicanos Ind.",
    str_detect(beneficiario, "organizacion editorial mexicana") ~ "OEM",
    str_detect(beneficiario, "mm on line") ~ "MM Online",
    str_detect(beneficiario, "reforma") ~ "Reforma / El Norte",
    str_detect(beneficiario, "editorial del golfo") ~ "Org. Editorial del Golfo",
    str_detect(beneficiario, "radio publicidad xhmexico|radio centro") ~ "Grupo Radio Centro",
    str_detect(beneficiario, "gim compañia editorial|excelsior") ~ "Excélsior",
    str_detect(beneficiario, "compañia editora del mayab|centro de cultura nuestra america") ~ "Cía. Ed. del Mayab",
    str_detect(beneficiario, "ap&h communication group") ~ "AP&H Comm. Group",
    str_detect(beneficiario, "b grande") ~ "La B Grande",
    str_detect(beneficiario, "stereorey méxico") ~ "Stereorey",
    str_detect(beneficiario, "radio y television de hidalgo") ~ "Radio y Televisión de Hidalgo",
    str_detect(beneficiario, "isa ") ~ "ISA Corporativo",
    str_detect(beneficiario, "organizacion editorial acuario") ~ "Org. Editorial Acuario",
    str_detect(beneficiario, "medios masivos mexicanos") ~ "Medios Masivos Mexicanos",
    str_detect(beneficiario, "imu") ~ "Comercializadora IMU",
    str_detect(beneficiario, "edicion y publicidad de medios de los estados") ~ "Edición y Publicidad en Medios de los Estados",
    str_detect(beneficiario, "operadora mexicana de television") ~ "Canal 40",
    str_detect(beneficiario, "promotur|starcom") ~ "PROMOTUR / STARCOM",
    str_detect(beneficiario, "pergom") ~ "PERGOM",
    str_detect(beneficiario, "radio mil") ~ "Núcleo Radio Mil",
    str_detect(beneficiario, "sistema público de radio") ~ "SPR",
    str_detect(beneficiario, "instituto mexicano de la radio") ~ "IMER",
    str_detect(beneficiario, "sociedad mexicana") ~ "Soc. Mex. de la Radio",
    str_detect(beneficiario, "radio y televisión de hidalgo") ~ "Radioy TV de Hgo.",
    str_detect(beneficiario, "edición y publicidad en medios de los estados") ~ "EyPME",
    str_detect(beneficiario, "operadora y administradora de infor mación y editorial|heraldo") ~ "El Heraldo",
    str_detect(beneficiario, "capital news|mac ediciones") ~ "Capital News",
    str_detect(beneficiario, "radios comunitarias") ~ "AMARC",
    TRUE ~ str_to_title(beneficiario)
  )) %>%
  mutate(beneficiario = str_replace(beneficiario, "Televisión", "TV")) |>
  mutate(beneficiario = str_replace(beneficiario, "Edición y Publicidad en Medios de los Estados", "EyPME")) |>
  mutate(yr = floor_date(fecha_de_gasto, "year")) %>%
  filter(yr >= as.Date('2012-01-01') & yr <= as.Date('2023-01-01')) %>%
  group_by(yr, beneficiario) %>%
  summarise(monto = sum(costo_def), .groups = 'drop') %>%
  arrange(yr, desc(monto)) %>%
  group_by(yr) %>%
  slice_max(order_by = monto, n = 25) %>%
  ungroup() %>%
  mutate(yr = as.character(yr),
         yr = substr(yr, 1, 4)) %>%
  group_by(yr) %>%
  mutate(rank = rank(monto)) %>%
  ungroup() %>%
  mutate(yr = as.integer(yr)) %>%
  mutate(beneficiario = str_wrap(beneficiario, 30)) 


rank_bene <- rank_bene_tot |> 
  filter(yr %in% c(2012, 2018, 2023))

#write.csv(rank_bene, "rank_medios.csv")

ggplot(rank_bene, aes(x = yr, y = rank, color = beneficiario)) +
  geom_bump(size = 1) +
  geom_point(aes(size = monto)) +
  guides(color = "none")+
  geom_text(data = rank_bene %>% filter(yr == min(yr)),
            aes(x = yr - 1.2, label = beneficiario),
            size = 7, hjust = 0.5) +
  geom_text(data = rank_bene %>% filter(yr == max(yr)),
            aes(x = yr + 1.2, label = beneficiario),
            size = 7, hjust = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2010,2025),
                     breaks=c(2012,2018,2023),
                     labels=c(2012,2018,2023))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish"),
        axis.text.x = element_text(size = 24)
  ) +
  guides(size = "none")+
  #scale_color_manual(values = c(carto_pal(12, "Vivid"), "navyblue", "darkred", "red")) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  labs(title = "Top 25 medios beneficiados por gastos en comunicación social",
       subtitle = "por año de gasto y monto, de 2012, 2018 y 2023",
       y = "",
       x = "Año",
       caption = "Fuente: Secretaría de la Función Pública\n@jmtoralc")

ggsave("graphs/rank_beneficiario.png", width = 15, height = 12, dpi = 120, bg = "white")

rank_bene_amlo <- rank_bene_tot |> 
  filter(yr %in% c(2019, 2023))

ggplot(rank_bene_amlo, aes(x = yr, y = rank, color = beneficiario)) +
  geom_bump(size = 1) +
  geom_point(aes(size = monto)) +
  guides(color = "none")+
  geom_text(data = rank_bene_amlo %>% filter(yr == min(yr)),
            aes(x = yr - 1.2, label = beneficiario),
            size = 7, hjust = 0.5) +
  geom_text(data = rank_bene_amlo %>% filter(yr == max(yr)),
            aes(x = yr + 1.2, label = beneficiario),
            size = 7, hjust = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2017,2025),
                     breaks=seq(2019, 2023, 4),
                     labels=seq(2019, 2023, 4))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish"),
        axis.text.x = element_text(size = 24)
  ) +
  guides(size = "none")+
  #scale_color_manual(values = c(carto_pal(12, "Vivid"), "navyblue", "darkred", "red")) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  labs(title = "Top 25 medios beneficiados por gastos en comunicación social",
       subtitle = "por año de gasto y monto, de 2019 a 2023",
       y = "",
       x = "Año",
       caption = "Fuente: Secretaría de la Función Pública\n@jmtoralc")

ggsave("graphs/rank_beneficiario_amlo.png", width = 15, height = 12, dpi = 120, bg = "white")


# Emisores ----------------------------------------------------------------

rank_nom <- clean_data |> 
  mutate(nombre = str_trim(nombre)) |> 
  mutate(nombre = str_replace_all(nombre, "ADMINISTRACIÓN DEL SISTEMA PORTUARIO NACIONAL", "ASIPONA")) |> 
  mutate(nombre = str_replace_all(nombre, ", S.A. DE C.V.|, S.A DE C.V.", "")) |> 
  mutate(nombre = str_replace_all(nombre, "LÁZARO CÁRDENAS", "LC")) |> 
  mutate(nombre = case_when(
    str_detect(nombre, "BANSEFI|BANCO DEL BIENESTAR") ~ "BANCO DEL BIENESTAR",
    str_detect(nombre, "SECRETARIA DE SALUD|S. SALUD") ~ "SSA",
    str_detect(nombre, "PRON(Ó|O)STICOS") ~ "Pronósticos",
    str_detect(nombre, "CENTRO NACIONAL PARA LA SALUD DE LA INFANCIA") ~ "CENSIA",
    str_detect(nombre, "CENTRO NACIONAL DE EQUIDAD DE GÉNERO Y SALUD") ~ "CNEGSR",
    str_detect(nombre, "PUEBLO LO ROBADO") ~ "INDEP/SAE",
    str_detect(nombre, "COORDINACION NACIONAL DEL PROGRAMA DE DESARRO") ~ "OPORTUNIDADES",
    str_detect(nombre, "COMISIÓN NACIONAL PARA LA PROTECCIÓN Y DEFEN") ~ "CONDUSEF",
    str_detect(nombre, "DICONSA") ~ "DICONSA",
    str_detect(nombre, "FONACOT") ~ "FONACOT",
    str_detect(nombre, "SECRETARÍA DE ECONOMÍA|SECRETARÍA DE ECONOMIA") ~ "SE",
    str_detect(nombre, "COMISIÓN NACIONAL PARA PREVENIR Y ERRADICAR") ~ "CONAVIM",
    str_detect(nombre, "FINANCIERA NACIONAL DE DESARROLLO") ~ "FND",
    str_detect(nombre, "GRUPO AEROPORTUARIO DE LA CIUDAD DE MÉXICO") ~ "GACM",
    str_detect(nombre, "CORREDOR INTEROCEÁNICO DEL ISTMO DE TEHUANTEPEC") ~ "CORREDOR INTEROCEÁNICO",
    str_detect(nombre, "SECRETARÍA DE CULTURA") ~ "CULTURA",
    str_detect(nombre, "LOTER(Í|I)A NACIONAL") ~ "LOTENAL",
    str_detect(nombre, "CONAGUA") ~ "CONAGUA",
    str_detect(nombre, "SEDESOL") ~ "SEDESOL",
    str_detect(nombre, "SEDATU") ~ "SEDATU",
    str_detect(nombre, "SGM") ~ "SGM",
    TRUE ~ nombre
  )) |> 
  mutate(yr = floor_date(fecha_de_gasto, "year")) %>%
  filter(yr >= as.Date('2012-01-01') & yr <= as.Date('2023-01-01') ) |> 
  group_by(yr, nombre) %>%
  summarise(monto = sum(costo_def), .groups = 'drop') %>%
  arrange(yr, desc(monto)) %>%
  group_by(yr) %>%
  slice_max(order_by = monto, n = 25) %>%
  ungroup() |> 
  mutate(yr = as.character(yr), 
         yr = substr(yr, 1, 4))  |>
  group_by(yr) %>%
  mutate(rank = rank(monto)) |> 
  ungroup() |>
  mutate(yr = as.integer(yr)) |>
  mutate(
    nombre = ifelse(str_detect(nombre, "\\("), 
                    str_extract(nombre, "(?<=\\().+?(?=\\))"), 
                    nombre)
  ) |>
  mutate(nombre = str_wrap(nombre, 30))


rank_nom_tot <- rank_nom |> 
  filter(yr %in% c(2012, 2018, 2023))

#write.csv(rank_bene, "rank_medios.csv")

ggplot(rank_nom_tot, aes(x = yr, y = rank, color = nombre)) +
  geom_bump(size = 1) +
  geom_point(aes(size = monto)) +
  guides(color = "none")+
  geom_text(data = rank_nom_tot %>% filter(yr == min(yr)),
            aes(x = yr - 1.2, label = nombre),
            size = 7, hjust = 0.5) +
  geom_text(data = rank_nom_tot %>% filter(yr == max(yr)),
            aes(x = yr + 1.2, label = nombre),
            size = 7, hjust = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2010,2025),
                     breaks=c(2012,2018,2023),
                     labels=c(2012,2018,2023))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish"),
        axis.text.x = element_text(size = 24)
  ) +
  guides(size = "none")+
  #scale_color_manual(values = c(carto_pal(12, "Vivid"), "navyblue", "darkred", "red")) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  labs(title = "Top 25 organismos por gastos en comunicación social",
       subtitle = "por año de gasto y monto, de 2012, 2018 y 2023",
       y = "",
       x = "Año",
       caption = "Fuente: Secretaría de la Función Pública\n@jmtoralc")

ggsave("graphs/rank_emisor.png", width = 15, height = 12, dpi = 120, bg = "white")



rank_nom_amlo <- rank_nom |> 
  filter(yr %in% c(2019, 2023))

ggplot(rank_nom_amlo , aes(x = yr, y = rank, color = nombre)) +
  geom_bump(size = 1) +
  geom_point(aes(size = monto)) +
  guides(color = "none")+
  geom_text(data = rank_nom_amlo  %>% filter(yr == min(yr)),
            aes(x = yr - 1.2, label = nombre),
            size = 7, hjust = 0.5) +
  geom_text(data = rank_nom_amlo  %>% filter(yr == max(yr)),
            aes(x = yr + 1.2, label = nombre),
            size = 7, hjust = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2017,2025),
                     breaks=seq(2019, 2023, 4),
                     labels=seq(2019, 2023, 4))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish"),
        axis.text.x = element_text(size = 24)
  ) +
  guides(size = "none")+
  #scale_color_manual(values = c(carto_pal(12, "Vivid"), "navyblue", "darkred", "red")) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  labs(title = "Top 25 medios beneficiados por gastos en comunicación social",
       subtitle = "por año de gasto y monto, de 2019 a 2023",
       y = "",
       x = "Año",
       caption = "Fuente: Secretaría de la Función Pública\n@jmtoralc")

ggsave("graphs/rank_emisor_amlo.png", width = 15, height = 12, dpi = 120, bg = "white")


# La relación más fructífera ----------------------------------------------

rel <- clean_data %>%
  mutate(beneficiario = str_to_lower(beneficiario)) |> 
  mutate(beneficiario = str_trim(beneficiario)) |> 
  mutate(beneficiario = str_replace_all(beneficiario, ", s\\.a\\. de c\\.v\\.| sa de cv| s\\.a\\. de c\\.v\\.|, s\\.a\\. de c\\.v\\.|", "")) %>%
  mutate(beneficiario = case_when(
    str_detect(beneficiario, "(?i)televisa") ~ "Televisa",
    str_detect(beneficiario, "(?i)demos|jornada") ~ "La Jornada",
    str_detect(beneficiario, "(?i)azteca") ~ "TV Azteca",
    str_detect(beneficiario, "(?i)especializado en economía|el economista") ~ "El Economista",
    str_detect(beneficiario, "el financiero") ~ "El Financiero",
    str_detect(beneficiario, "(?i)agencia digital|milenio") ~ "Milenio",
    str_detect(beneficiario, "(?i)imagen|image|cadena tres i") ~ "Imagen",
    str_detect(beneficiario, "(?i)universal") ~ "El Universal",
    str_detect(beneficiario, "(?i)5m2|cinco m") ~ "5M2",
    str_detect(beneficiario, "(?i)formula|fórmula|pm onstreet") ~ "Grupo Fórmula",
    str_detect(beneficiario, "(?i)radiodifusores mexicanos indep") ~ "Radiodifusores Mexicanos Ind.",
    str_detect(beneficiario, "organizacion editorial mexicana") ~ "OEM",
    str_detect(beneficiario, "mm on line") ~ "MM Online",
    str_detect(beneficiario, "reforma") ~ "Reforma / El Norte",
    str_detect(beneficiario, "editorial del golfo") ~ "Org. Editorial del Golfo",
    str_detect(beneficiario, "radio publicidad xhmexico|radio centro") ~ "Grupo Radio Centro",
    str_detect(beneficiario, "gim compañia editorial|excelsior") ~ "Excélsior",
    str_detect(beneficiario, "compañia editora del mayab|centro de cultura nuestra america") ~ "Cía. Ed. del Mayab",
    str_detect(beneficiario, "ap&h communication group") ~ "AP&H Comm. Group",
    str_detect(beneficiario, "b grande") ~ "La B Grande",
    str_detect(beneficiario, "stereorey méxico") ~ "Stereorey",
    str_detect(beneficiario, "radio y television de hidalgo") ~ "Radio y Televisión de Hidalgo",
    str_detect(beneficiario, "isa ") ~ "ISA Corporativo",
    str_detect(beneficiario, "organizacion editorial acuario") ~ "Org. Editorial Acuario",
    str_detect(beneficiario, "medios masivos mexicanos") ~ "Medios Masivos Mexicanos",
    str_detect(beneficiario, "imu") ~ "Comercializadora IMU",
    str_detect(beneficiario, "edicion y publicidad de medios de los estados") ~ "Edición y Publicidad en Medios de los Estados",
    str_detect(beneficiario, "operadora mexicana de television") ~ "Canal 40",
    str_detect(beneficiario, "promotur|starcom") ~ "PROMOTUR / STARCOM",
    str_detect(beneficiario, "pergom") ~ "PERGOM",
    str_detect(beneficiario, "radio mil") ~ "Núcleo Radio Mil",
    str_detect(beneficiario, "sistema público de radio") ~ "SPR",
    str_detect(beneficiario, "instituto mexicano de la radio") ~ "IMER",
    str_detect(beneficiario, "sociedad mexicana") ~ "Soc. Mex. de la Radio",
    str_detect(beneficiario, "radio y televisión de hidalgo") ~ "Radioy TV de Hgo.",
    str_detect(beneficiario, "edición y publicidad en medios de los estados") ~ "EyPME",
    str_detect(beneficiario, "operadora y administradora de infor mación y editorial|heraldo") ~ "El Heraldo",
    str_detect(beneficiario, "capital news|mac ediciones") ~ "Capital News",
    str_detect(beneficiario, "radios comunitarias") ~ "AMARC",
    TRUE ~ str_to_title(beneficiario)
  )) %>%
  mutate(beneficiario = str_replace(beneficiario, "Televisión", "TV")) |>
  mutate(beneficiario = str_replace(beneficiario, "Edición y Publicidad en Medios de los Estados", "EyPME")) |>
  mutate(nombre = str_trim(nombre)) |> 
  mutate(nombre = str_replace_all(nombre, "ADMINISTRACIÓN DEL SISTEMA PORTUARIO NACIONAL", "ASIPONA")) |> 
  mutate(nombre = str_replace_all(nombre, ", S.A. DE C.V.|, S.A DE C.V.", "")) |> 
  mutate(nombre = str_replace_all(nombre, "LÁZARO CÁRDENAS", "LC")) |> 
  mutate(nombre = case_when(
    str_detect(nombre, "BANSEFI|BANCO DEL BIENESTAR") ~ "BANCO DEL BIENESTAR",
    str_detect(nombre, "SECRETARIA DE SALUD|S. SALUD") ~ "SSA",
    str_detect(nombre, "PRON(Ó|O)STICOS") ~ "Pronósticos",
    str_detect(nombre, "CENTRO NACIONAL PARA LA SALUD DE LA INFANCIA") ~ "CENSIA",
    str_detect(nombre, "CENTRO NACIONAL DE EQUIDAD DE GÉNERO Y SALUD") ~ "CNEGSR",
    str_detect(nombre, "PUEBLO LO ROBADO") ~ "INDEP/SAE",
    str_detect(nombre, "COORDINACION NACIONAL DEL PROGRAMA DE DESARRO") ~ "OPORTUNIDADES",
    str_detect(nombre, "COMISIÓN NACIONAL PARA LA PROTECCIÓN Y DEFEN") ~ "CONDUSEF",
    str_detect(nombre, "DICONSA") ~ "DICONSA",
    str_detect(nombre, "FONACOT") ~ "FONACOT",
    str_detect(nombre, "SECRETARÍA DE ECONOMÍA|SECRETARÍA DE ECONOMIA") ~ "SE",
    str_detect(nombre, "COMISIÓN NACIONAL PARA PREVENIR Y ERRADICAR") ~ "CONAVIM",
    str_detect(nombre, "FINANCIERA NACIONAL DE DESARROLLO") ~ "FND",
    str_detect(nombre, "GRUPO AEROPORTUARIO DE LA CIUDAD DE MÉXICO") ~ "GACM",
    str_detect(nombre, "CORREDOR INTEROCEÁNICO DEL ISTMO DE TEHUANTEPEC") ~ "CORREDOR INTEROCEÁNICO",
    str_detect(nombre, "SECRETARÍA DE CULTURA") ~ "CULTURA",
    str_detect(nombre, "LOTER(Í|I)A NACIONAL") ~ "LOTENAL",
    str_detect(nombre, "CONAGUA") ~ "CONAGUA",
    str_detect(nombre, "SEDESOL") ~ "SEDESOL",
    str_detect(nombre, "SEDATU") ~ "SEDATU",
    str_detect(nombre, "SGM") ~ "SGM",
    TRUE ~ nombre
  )) |> 
  mutate(yr = floor_date(fecha_de_gasto, "year")) %>%
  filter(yr >= as.Date('2012-01-01') & yr <= as.Date('2023-01-01') ) |> 
  group_by(yr, nombre, beneficiario) |> 
  summarise(monto = sum(costo_def), .groups = 'drop') %>%
  arrange(yr, desc(monto)) %>%
  group_by(yr) %>%
  slice_max(order_by = monto, n = 25) %>%
  ungroup() |> 
  mutate(yr = as.character(yr), 
         yr = substr(yr, 1, 4))  |>
  group_by(yr) %>%
  mutate(rank = rank(monto)) |> 
  ungroup() |>
  mutate(yr = as.integer(yr)) |>
  mutate(
    nombre = ifelse(str_detect(nombre, "\\("), 
                    str_extract(nombre, "(?<=\\().+?(?=\\))"), 
                    nombre)
  )


rel23 <- rel |> 
  filter(yr >= 2019) |>
  group_by(nombre, beneficiario) |> 
  summarise(monto = sum(monto), .groups = 'drop') |> 
  select(source = nombre, target = beneficiario, monto)

edges <-rel23 %>%
  select(source, target, monto) |> 
  mutate(monto = monto/1000000)

# Crear el objeto graph
graph <- graph_from_data_frame(edges, directed = FALSE)

# Calcular el grado de los nodos
V(graph)$degree <- degree(graph)

# Convertir a tbl_graph para usar ggraph
tbl_graph <- as_tbl_graph(graph)

# Añadir una columna para diferenciar source y target
tbl_graph <- tbl_graph %>%
  mutate(type = ifelse(name %in% edges$source, 'source', 'target'))

# Crear el gráfico de red utilizando ggraph
ggraph(tbl_graph, layout = 'kk') + 
  geom_edge_fan(aes(width = monto), alpha = 0.8, color = "grey50",
                #curvature = 0.03, # Ajustar la curvatura de las líneas
                arrow = arrow(length = unit(4, 'mm'), type = "closed"), 
                end_cap = circle(5, 'mm')) +  # Separar la punta de la flecha del nodo
  geom_node_point(aes(size = degree, color = type), alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 8, fontface = "bold", nudge_y = 0.05) + 
  theme_void() +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(3, 20)) +
  scale_color_manual(values = c('source' = 'skyblue', 'target' = 'red')) +
  guides(edge_width = guide_legend("Millones de\npesos de 2020"), size = "none",
         color = "none") +
  labs(title = "Las 25 relaciones de gasto más grandes de 2019 a 2023") +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave("graphs/red_amlo.png", width = 15, height = 15, dpi = 120, bg = "white")








rel23 <- rel |> 
  filter(yr >= 2012 & yr <=2018) |>
  group_by(nombre, beneficiario) |> 
  summarise(monto = sum(monto), .groups = 'drop') |> 
  select(source = nombre, target = beneficiario, monto)


edges <-rel13 %>%
  select(source, target, monto) |> 
  mutate(monto = monto/1000000)

# Crear el objeto graph
graph <- graph_from_data_frame(edges, directed = FALSE)

# Calcular el grado de los nodos
V(graph)$degree <- degree(graph)

# Convertir a tbl_graph para usar ggraph
tbl_graph <- as_tbl_graph(graph)

# Añadir una columna para diferenciar source y target
tbl_graph <- tbl_graph %>%
  mutate(type = ifelse(name %in% edges$source, 'source', 'target'))

# Crear el gráfico de red utilizando ggraph
ggraph(tbl_graph, layout = 'kk') + 
  geom_edge_fan(aes(width = monto), alpha = 0.8, color = "grey50",
                #curvature = 0.03, # Ajustar la curvatura de las líneas
                arrow = arrow(length = unit(4, 'mm'), type = "closed"), 
                end_cap = circle(5, 'mm')) +  # Separar la punta de la flecha del nodo
  geom_node_point(aes(size = degree, color = type), alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 8, fontface = "bold", nudge_y = 0.05) + 
  theme_void() +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(3, 20)) +
  scale_color_manual(values = c('source' = 'skyblue', 'target' = 'red')) +
  guides(edge_width = guide_legend("Millones de\npesos de 2020"), size = "none",
         color = "none") +
  labs(title = "Las 25 relaciones de gasto más grandes de 2012 a 2018") +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave("graphs/red_epn.png", width = 15, height = 12, dpi = 120, bg = "white")
