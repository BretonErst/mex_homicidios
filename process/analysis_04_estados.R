###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



## Librerías
library(tidyverse)
library(ggtext)
library(sf)
library(styleBreton)


## Source
suppressWarnings(source("data/cleaned_data_victim.R")) 


## Gráfica de calor cuenta absoluta de homicidios por estado
df_05  |> 
  filter(año > 2015) |> 
  na.omit() |>  
  mutate(estado = factor(estado, 
                         levels = rev(sort(unique(estado)))),
         texto = paste0(estado, "\n",
                        año, mes, "\n",
                        homicidios)) |> 
  ggplot(aes(x = fecha,
             y = estado, 
             fill = homicidios)) +
  geom_tile(color = "grey70", 
            alpha = 0.85) +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#7B0303",
                      expand = c(0,0)) +
  geom_vline(aes(xintercept = as_date("2018-12-01"), 
                 alpha = 0.15), 
             linewidth = 3, 
             color = "darkgrey") +
  theme_breton() +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        legend.position = "top") +
  labs(title = "Patrones de la Violencia Homicida en México",
       subtitle = "Víctimas de homicidios dolosos acumuladas cada mes por Entidad Federativa, de enero de 2016 a abril de 2024.",
       x = NULL,
       y = NULL,
       caption = "Fuente: Reportes de Incidencia Delictiva; 
         Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
         Gobierno de México.<br>
         Visualización: Juan L. Bretón, PMP | @juanlbreton",
       fill = "Víctimas por Mes") +
  guides(alpha = guide_legend("Inicio de Sexenio", 
                              label = FALSE,
                              title.vjust =  -0.01)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) #-> ab1


ggsave(filename = "graficas/heatmap_abso.jpg", device = "jpeg", dpi = "retina")

# plotly::ggplotly(ab1)


## Cuenta relativa de homicidios por estado

# importacion de datos de poblacion por estado
mx_pob <- 
  read_rds("data/mx_pob.rds")


# merge con datos de homicidios mensual
homic_tasa_mes <- 
  df_05 |> 
  na.omit() |> 
  filter(año > 2015) |> 
  left_join(mx_pob,
            by = join_by(id_entidad == cve_geo, año == año)) |> 
  mutate(tasa_mes = homicidios / tot_pobla * 1e5)


# gráfica de calor tasa por mes
homic_tasa_mes  |> 
  filter(año > 2015) |> 
  na.omit() |>  
  mutate(estado = factor(estado, 
                         levels = rev(sort(unique(estado)))),
         texto = paste0(estado, "\n",
                        año, mes, "\n",
                        tasa_mes)) |> 
  ggplot(aes(x = fecha,
             y = estado, 
             fill = tasa_mes)) +
  geom_tile(color = "grey70", 
            alpha = 0.85) +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#7B0303",
                      expand = c(0,0)) +
  geom_vline(aes(xintercept = as_date("2018-12-01"), 
                 alpha = 0.15), 
             linewidth = 3, 
             color = "darkgrey") +
  theme_breton() +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        legend.position = "top") +
  labs(title = "Patrones de la Violencia Homicida en México",
       subtitle = "Víctimas de homicidios dolosos acumuladas cada mes por Entidad Federativa, de enero de 2016 a abril de 2024.",
       x = NULL,
       y = NULL,
       caption = "Fuente: Reportes de Incidencia Delictiva; 
         Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
         Gobierno de México.<br>
         Visualización: Juan L. Bretón, PMP | @juanlbreton",
       fill = "Víctimas por Mes") +
  guides(alpha = guide_legend("Inicio de Sexenio", 
                              label = FALSE,
                              title.vjust =  -0.01)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) #-> ts1


ggsave(filename = "graficas/heatmap_tasa.jpg", device = "jpeg", dpi = "retina")


# plotly::ggplotly(ts1)



## mapa de tasa por año
# archivo shape de estados
mapa_nal <- 
  st_read("conjunto_de_datos/nal/00ent.shp")


# tasa anual
homic_tasa_anual <- 
  homic_tasa_mes |> 
  select(-tasa_mes) |> 
  summarize(homicidios = sum(homicidios, na.rm. =TRUE),
            poblacion = mean(tot_pobla),
            .by = c(año, id_entidad, estado)) |> 
  mutate(tasa = homicidios / poblacion * 1e5)


# datos tasa anual
homic_tasa_2023 <- 
  homic_tasa_anual |> 
  filter(año == 2023) |> 
  na.omit()

# limites
vlim <- 
  c(min(homic_tasa_2023$tasa),
    max(homic_tasa_2023$tasa))


# merge con archivo shape
mapa_nal_tasa_2023 <- 
  mapa_nal |> 
  left_join(homic_tasa_2023,
            by = join_by(CVEGEO == id_entidad))

write_rds(mapa_nal_tasa_2023, "data/mapa_nal_tasa_2023.rds")


# plot de mapa
mapa_nal_tasa_2023 |> 
  ggplot(aes(fill = tasa)) +
  geom_sf(color = "grey60",
          size = 0.7) +
  theme_breton() +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Geografía de la Violencia Homicida",
       subtitle = "Víctimas de homicidio por cada 100,000 habitantes en el año 2023",
       caption = "Fuente: Reportes de Incidencia Delictiva; 
       Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
             Gobierno de México. 
             <br>CONAPO Conciliaciones de población.<br>
             Visualización: Juan L. Bretón, PMP | @BretonPmp") +
  scale_fill_gradient(name = "Tasa de\nvíctimas por\n100,000 habs", 
                      low = "#FFFFFF",
                      high = "#7B0303",
                      limits = vlim,
                      breaks = round(seq(vlim[1],
                                         vlim[2],
                                         length.out = 5),
                                     digits = 1))
















