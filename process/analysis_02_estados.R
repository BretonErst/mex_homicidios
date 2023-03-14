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
suppressWarnings(source("data/cleaned_data_victim.R")) 


## Gráfica de calor
df_05 %>%
  filter(año > 2015) %>%
  na.omit() %>% 
  mutate(estado = factor(estado, 
                         levels = rev(sort(unique(estado)))),
         texto = paste0(estado, "\n",
                        año, mes, "\n",
                        homicidios)) %>%
  ggplot(aes(x = fecha,
             y = estado, 
             fill = homicidios)) +
    geom_tile(color = "grey70", 
              alpha = 0.85) +
    scale_fill_gradient(low = "#FFFFFF", 
                        high = "#7B0303",
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
    labs(title = "Patrones de la Violencia Homicida en México",
         subtitle = "Víctimas de homicidos dolosos acumuladas por mes en cada Entidad Federativa, de enero de 2016 a enero de 2023.",
         x = NULL,
         y = NULL,
         caption = "Fuente: Reportes de Incidencia Delictiva
           2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
           Gobierno de México.<br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp",
         fill = "Víctimas de Homicidios por Mes") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_date(expand = c(0, 0)) -> be03

be03 +
  geom_vline(aes(xintercept = as_date("2018-12-01"), 
                 alpha = 0.15), 
             linewidth = 3, 
             color = "darkgrey") +
  guides(alpha = guide_legend("Inicio de Sexenio", 
                              label = FALSE,
                              title.vjust =  -0.01)) #-> be03


# plotly::ggplotly(be03, tooltip = "texto")

ggsave(filename = "graficas/be03.jpg", device = "jpeg", dpi = "retina")


## gráfica de calor por tasas
# carga archivo de población por estado
nal_estados <- readxl::read_excel("raw/pob_mex.xlsx",
                                  sheet = "muni",
                                  trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  select(estado = entidad_federativa,
         pob = poblacion_total_2020) %>% 
  mutate(estado = str_to_upper(estado)) %>% 
  mutate(estado = if_else(estado == "ESTADO DE MÉXICO", "MÉXICO", estado))

# integrar columna de población con archivo de datos original
nal_tasa <- df_05 %>% 
  filter(año > 2019) %>% 
  left_join(nal_estados, by = "estado") %>% 
  mutate(tasa_mes = homicidios / pob * 1e5) %>% 
  select(-año)

# visualización
nal_tasa %>% 
  na.omit() %>% 
  mutate(estado = factor(estado, 
                         levels = rev(sort(unique(estado))))) %>%
  ggplot(aes(x = fecha,
             y = estado, 
             fill = tasa_mes)) +
    geom_tile(color = "grey70", 
              alpha = 0.85) +
    scale_fill_gradient(low = "#FFFFFF", 
                        high = "#7B0303",
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
    labs(title = "¿En Qué Estados Ocurren, Proporcionalmente, Más Homicidios?",
         subtitle = "Víctimas de homicidio doloso por cada 100,000 habitantes por mes",
         x = NULL,
         y = NULL,
         caption = "Fuente: Reportes de Incidencia Delictiva
             2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
             Gobierno de México. INEGI, Censo de Población y Vivienda 2020<br>
             Visualización: Juan L. Bretón, PMP | @BretonPmp",
         fill = "Proporción de víctimas respecto a la población") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_date(expand = c(0, 0))


## datos de población 2022
pob_2022 <- readxl::read_xlsx("raw/mex_pob2022.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(estado = str_to_upper(estado))

nal_tasa_ult <- df_05 %>% 
  filter(año > 2021) %>% 
  left_join(pob_2022, by = "estado")


## mapa por tasa 2022
# carga de archivo shape
nal_mapa <- st_read("conjunto_de_datos/nal/00ent.shp")

# summarize para total año 2022
nal_tasa_2022 <- nal_tasa_ult %>% 
  filter(fecha >= "2022-01-01") %>% 
  group_by(id_entidad, estado, pob2022) %>% 
  summarize(total_2022 = sum(homicidios, na.rm = TRUE)) %>% 
  mutate(tasa_2022 = total_2022 / pob2022 * 1e5) %>% 
  ungroup() %>% 
  select(-c(pob2022, total_2022))

nal_tasa_2022 %>% 
  summarize(media = mean(tasa_2022))

# mínimo y máximo
val_limite <- round(c(min(nal_tasa_2022$tasa_2022), 
                      max(nal_tasa_2022$tasa_2022)), 
                    digits = 1)

# unir con el archivo shape
nal_mapa_2022 <- nal_mapa %>% 
  left_join(nal_tasa_2022, by = c("CVEGEO" = "id_entidad"))

# visualización de mapa
nal_mapa_2022 %>% 
  ggplot(aes(fill = tasa_2022)) +
    geom_sf(color = "grey60",
            size = 0.7) +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title = element_text(face = "bold", 
                                    size = 16),
          panel.background = element_rect(fill = "#FFFFFF"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.title.position = "plot",
          plot.caption.position = "plot") +
    labs(title = "¿En Qué Entidades Ocurrieron Proporcionalmente Más Homidicios?",
         subtitle = "Víctimas de homicidio por cada 100,000 habitantes registradas en el año 2022",
         caption = "Fuente: Reportes de Incidencia Delictiva
             2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
             Gobierno de México. Estimación de población de la ENOE, INEGI.<br>
             Visualización: Juan L. Bretón, PMP | @BretonPmp") +
    scale_fill_gradient(name = "Tasa de\nvíctimas por\n100,000 habs", 
                        low = "#FFFFFF",
                        high = "#7B0303",
                        limits = val_limite,
                        breaks = round(seq(val_limite[1],
                                           val_limite[2],
                                           length.out = 5),
                                       digits = 1))



ggsave(filename = "graficas/mex_nal_01.jpg", device = "jpeg", dpi = "retina")

# slider
valores_medios <- df_05 %>% 
  arrange(fecha)

calcula_mediana <- function(df){
  df %>% 
    summarize(fecha = min(fecha),
              median_homicidios = median(homicidios))
}

homicidios_mediana <- slide_period_dfr(valores_medios,
                                       valores_medios$fecha,
                                       "month",
                                       calcula_mediana)


# Gráfica de líneas
df_05 %>% 
  ggplot(aes(x = fecha, 
             y = homicidios, 
             group = estado)) +
    geom_line(alpha = 0.15) +
    geom_line(data = df_05 %>% filter(estado == "GUANAJUATO"),
              aes(y = homicidios),
              color = "darkred") +
    geom_line(data = df_05 %>% filter(estado == "JALISCO"),
              aes(y = homicidios),
              color = "darkgreen") +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.caption = element_markdown(color = "grey45", hjust = 0),
          plot.title = element_text(size = 18, face = "bold"),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.background = element_rect(fill = "#FFFFFF"),
          panel.grid = element_line(color = "grey97")) +
    labs(title = "Patrones de la Violencia Homicida en México",
         subtitle = "Víctimas de homicidos dolosos acumuladas por mes en cada Entidad Federativa, de enero de 2016 a julio de 2022.",
         x = NULL,
         y = "Homicidios",
         caption = "Fuente: Reportes de Incidencia Delictiva al mes de julio
             2022; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, Gobierno de México.<br>
             Visualización: Juan L. Bretón, PMP | @BretonPmp") #-> po1


homicidios_mediana %>%  
  ggplot(aes(fecha, median_homicidios)) +
  geom_line(color = "steelblue") 


homicidios_mediana %>% nrow()

df_05 %>% nrow()








