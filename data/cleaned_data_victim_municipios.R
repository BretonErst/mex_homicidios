###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



## Librerías
library(tidyverse)
library(lubridate)


## Adquisición de datos
df_mun_00 <- read_csv("raw/IDM_NM_ago22.csv",
                      locale = locale(encoding = "latin1"))


## Limpieza de datos
# Estado == Guanajuato
df_mun_01 <- df_mun_00 %>% 
  filter(Entidad == "Guanajuato")

# Homicidio doloso
df_mun_02 <- df_mun_01 %>% 
  filter(`Tipo de delito` == "Homicidio",
         `Subtipo de delito` == "Homicidio doloso") %>% 
  select(- Clave_Ent,
         - `Bien jurídico afectado`,
         - `Tipo de delito`,
         - `Subtipo de delito`,
         - Entidad) %>% 
  janitor::clean_names() %>% 
  mutate(cve_municipio = as_factor(cve_municipio),
         municipio = as_factor(municipio),
         modalidad = as_factor(modalidad)) %>% 
  select(año = ano,
         everything()) 
  

glimpse(df_mun_02)

# Convertir en tidy data
df_mun_03 <- df_mun_02 %>% 
  pivot_longer(cols = enero:diciembre,
               names_to = "mes",
               values_to = "cuenta") %>% 
  mutate(mes = as_factor(mes),
         municipio = as_factor(str_to_upper(municipio))) %>% 
  group_by(año, cve_municipio, municipio, mes) %>% 
  summarize(homicidios = sum(cuenta)) %>% 
  ungroup()


mes_key = c("enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4,
            "mayo" = 5, "junio" = 6, "julio" = 7, "agosto" = 8,
            "septiembre" = 9, "octubre" = 10, "noviembre" = 11,
            "diciembre" = 12)


df_mun_04 <- df_mun_03 %>% 
  mutate(num_mes = recode(mes, !!!mes_key),
         fecha = as_date(paste(año, num_mes, 1, sep = "-")))



rm(df_mun_00, df_mun_01, df_mun_02, df_mun_03)







  



