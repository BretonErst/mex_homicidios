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
    labs(title = "Patrones de la Violencia Homicida en México",
         subtitle = "Víctimas de homicidos dolosos acumuladas por mes en cada Entidad Federativa, de enero de 2016 a octubre de 2022.",
         x = NULL,
         y = NULL,
         caption = "Fuente: Reportes de Incidencia Delictiva
           2022; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, Gobierno de México.<br>
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

# ggsave(filename = "be03", plot = be03, path = "graficas", device = "tiff")


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








