###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



## Librerías
library(tidyverse)
library(lubridate)


## Adquisición de datos
df_00 <- read_csv("raw/IDVFC_NM_dic22.csv",
                  locale = locale(encoding = "latin1"))


glimpse(df_00)


## Limpieza de datos para HOMICIDIO DOLOSO
df_01 <- df_00 %>%
  filter(`Tipo de delito` == "Homicidio",
         `Subtipo de delito` == "Homicidio doloso") %>% 
  select(-`Bien jurídico afectado`,
         - `Tipo de delito`,
         -`Subtipo de delito`) %>%
  janitor::clean_names() %>% 
  mutate(clave_ent = as.factor(clave_ent),
         entidad = as.factor(entidad),
         modalidad = as.factor(modalidad)) %>% 
  select(año = ano,
         everything())

#glimpse(df_01)


# Convertir a tidy format
df_02 <- df_01 %>% 
  pivot_longer(cols = enero:diciembre,
               names_to = "mes",
               values_to = "cuenta") %>% 
  mutate(mes = factor(mes,
                      levels = c("enero", "febrero", "marzo", "abril",
                                 "mayo", "junio", "julio", "agosto",
                                 "septiembre", "octubre", "noviembre",
                                 "diciembre"))) %>% 
  group_by(año, clave_ent, entidad, mes) %>% 
  summarize(homicidios = sum(cuenta)) %>% 
  ungroup()

#glimpse(df_02)


df_03 <- df_02 %>% 
  mutate(id_entidad = recode(clave_ent,
                             "1" = "01",
                             "2" = "02",
                             "3" = "03",
                             "4" = "04",
                             "5" = "05",
                             "6" = "06",
                             "7" = "07",
                             "8" = "08",
                             "9" = "09",
                             "10" = "10",
                             "11" = "11",
                             "12" = "12",
                             "13" = "13",
                             "14" = "14",
                             "15" = "15",
                             "16" = "16",
                             "17" = "17",
                             "18" = "18",
                             "19" = "19",
                             "20" = "20",
                             "21" = "21",
                             "22" = "22",
                             "23" = "23",
                             "24" = "24",
                             "25" = "25",
                             "26" = "26",
                             "27" = "27",
                             "28" = "28",
                             "29" = "29",
                             "30" = "30",
                             "31" = "31",
                             "32" = "32",)) %>% 
  relocate(id_entidad, .before = entidad) %>% 
  select(-clave_ent)

# Adquisición de base de datos de nomenclatura de estados
estados <- read_csv("raw/code_ENTIDAD.csv")

# Preparación de data frame para integración
estados <- estados %>% 
  mutate(ENTIDAD_RES = as.factor(CLAVE_ENTIDAD),
         ENTIDAD_FEDERATIVA = as.factor(ENTIDAD_FEDERATIVA)) %>% 
  select(-CLAVE_ENTIDAD,
         -ABREVIATURA,
         ENTIDAD_RES,
         estado = ENTIDAD_FEDERATIVA)

# Integración de dataframes
df_04 <- df_03 %>% 
  left_join(estados, by = c("id_entidad" = "ENTIDAD_RES")) %>%
  relocate(estado, .after = entidad) %>% 
  select(-entidad) %>% 
  rowid_to_column()

#levels(df_04$mes)


mes_key = c("enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4,
            "mayo" = 5, "junio" = 6, "julio" = 7, "agosto" = 8,
            "septiembre" = 9, "octubre" = 10, "noviembre" = 11,
            "diciembre" = 12)


df_05 <- df_04 %>% 
  mutate(num_mes = recode(mes, !!!mes_key)) %>% 
  mutate(fecha = as_date(paste(año, num_mes, 1, sep = "-")))


rm(df_00, df_01, df_02, df_03, df_04)
