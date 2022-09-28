###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



## Librerías
library(tidyverse)
library(ggtext)
library(slider)


## Source
suppressWarnings(source("data/cleaned_data_victim_municipios.R")) 


## Gráfica de calor
df_mun_04 %>%
  filter(año > 2015) %>%
  na.omit() %>% 
  mutate(estado = factor(municipio, 
                         levels = rev(sort(unique(municipio)))),
         texto = paste0(municipio, "\n",
                        año, mes, "\n",
                        homicidios)) %>%
  ggplot(aes(x = fecha,
             y = municipio, 
             fill = homicidios)) +
  geom_tile(color = "grey70", 
            alpha = 0.85) +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#FF0000",
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
       subtitle = "Víctimas de homicidos dolosos acumuladas por mes en cada Municipio, de enero de 2016 a agosto de 2022.",
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
