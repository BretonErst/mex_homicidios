# librerias
library(tidyverse)


# importación de datos
dt_00 <- 
  readxl::read_xlsx("raw/0_Pob_inicio_1950_2070.xlsx") |> 
  janitor::clean_names() |> 
  rename(año = ano) |> 
  select(-renglon)



# limpieza de datos
dt_01 <- 
  dt_00 |> 
  filter(año %in% c(2016:2024)) |> 
  filter(entidad != "República Mexicana") |> 
  summarize(tot_pobla = sum(poblacion),
            .by = c(año, entidad, cve_geo)) |> 
  mutate(cve_geo = factor(str_pad(cve_geo, 2, pad = "0")))



# exportación de datos
write_rds(dt_01, "data/mx_pob.rds")


df_05 <- 
  read_rds("data/df_05.rds")

# merge
homic_tasa <- 
  df_05 |> 
  filter(año > 2015) |> 
  left_join(dt_01,
            by = join_by(id_entidad == cve_geo, año == año)) |> 
  mutate(tasa_mes = homicidios / tot_pobla * 1e5)

write_rds(homic_tasa, "data/homic_tasa.rds")
