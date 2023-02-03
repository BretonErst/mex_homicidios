###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



## Librerías
library(tidyverse)
library(ggtext)
library(sf)
library(slider)


## Source
suppressWarnings(source("data/cleaned_data_victim_municipios.R")) 



## Gráfica de calor
df_mun_04 %>%
  filter(año > 2015) %>%
  na.omit() %>% 
  mutate(municipio = factor(municipio,
                            levels = rev(sort(unique(municipio))))) %>%
  ggplot(aes(x = fecha,
             y = municipio, 
             fill = homicidios)) +
  geom_tile(color = "grey70", 
            alpha = 0.85) +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#C0392B",
                      expand = c(0,0)) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.caption = element_markdown(color = "grey45", hjust = 0),
        plot.title = element_text(size = 18, face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Patrones de la Violencia Homicida en Guanajuato",
       subtitle = "Víctimas de homicidos dolosos acumuladas por mes en cada Municipio, de enero de 2016 a diciembre de 2022.",
       x = NULL,
       y = NULL,
       caption = "Fuente: Reportes de Incidencia Delictiva
           2022; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, Gobierno de México.<br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp",
       fill = "Víctimas de Homicidios por Mes") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) -> mune03

mune03 +
  geom_vline(aes(xintercept = as_date("2018-12-01"), 
                 alpha = 0.15), 
             size = 3, 
             color = "darkgrey") +
  guides(alpha = guide_legend("Inicio de Sexenio", 
                              label = FALSE,
                              title.vjust =  -0.01)) #-> be03


## gráfica de calor por tasa
# carga de archivo de población de municipios
gto_municip <- readxl::read_excel("raw/pob_mex.xlsx",
                                  sheet = "edo",
                                  trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  mutate(CVEGEO = as_factor(sprintf("11%03d", 
                                    clave_del_municipio))) %>% 
  select(CVEGEO,
         municipio, 
         habitantes_2020)

# integrar columna de población con archivo de datos original
df_gto_tasa <- df_mun_04 %>% 
  filter(año > 2019) %>% 
  left_join(gto_municip, by = c("cve_municipio" = "CVEGEO")) %>% 
  select(municipio = municipio.x,
         fecha,
         mes, 
         homicidios,
         hab = habitantes_2020) %>% 
  mutate(tasa = homicidios / hab * 1e5)

# visualizacion
df_gto_tasa %>%
  na.omit() %>% 
  mutate(municipio = factor(municipio,
                            levels = rev(sort(unique(municipio))))) %>%
  ggplot(aes(x = fecha,
             y = municipio, 
             fill = tasa)) +
  geom_tile(color = "grey70", 
            alpha = 0.85) +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#C0392B",
                      expand = c(0,0)) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.caption = element_markdown(color = "grey45", hjust = 0),
        plot.title = element_text(size = 18, face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Patrones de la Violencia Homicida en Guanajuato",
       subtitle = "Víctimas por cada 100,000 habitantes por mes.",
       x = NULL,
       y = NULL,
       caption = "Fuente: Reportes de Incidencia Delictiva
           2022; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
           Gobierno de México.<br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp",
       fill = "Tasa mensual") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) #-> gto_mun_01



## mapa
# carga de archivo shape
gto_mapa <- st_read("conjunto_de_datos/gto/11mun.shp")


# archivo de datos por municipio filtrado 
homi_mun_gto <- df_mun_04 %>% 
  filter(año == 2022) %>% 
  group_by(cve_municipio) %>% 
  summarize(total_homicidios = sum(homicidios, na.rm = TRUE)) %>% 
  left_join(gto_municip, by = c("cve_municipio" = "CVEGEO")) %>% 
  mutate(tasa_22 = total_homicidios / habitantes_2020 * 1e5)


# integración con archivo shape
gto_map_inte <- gto_mapa %>% 
  left_join(homi_mun_gto, by = c("CVEGEO" = "cve_municipio"))


# visualización de mapa
gto_map_inte %>% 
  ggplot(aes(fill = tasa_22)) +
    geom_sf(color = "grey60",
            size = 0.7) +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title = element_text(face = "bold", size = 16),
          panel.background = element_rect(fill = "#FFFFFF"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.title.position = "plot",
          plot.caption.position = "plot",) +
    labs(title = "Patrones de Violencia Homicida",
         subtitle = "Víctimas de homicidio por 100,000 habitantes en 2022",
         caption = "Fuente: Reportes de Incidencia Delictiva
           2022; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
           Gobierno de México.<br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp") +
    scale_fill_gradient(name = "Tasa de\nvíctimas", 
                        low = "#FFFFFF",
                        high = "#C0392B")




