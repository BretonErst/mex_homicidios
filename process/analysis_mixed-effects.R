###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(lme4)
library(ggtext)
library(sf)


## Source
suppressWarnings(source("data/cleaned_data_victim.R")) 

# data
df_06 <- 
  df_05 %>% 
  select(rowid, id_entidad, año, num_mes, mes, fecha, estado, homicidios) %>% 
  filter(fecha > as_date("2018-12-01")) %>%
  drop_na(homicidios) %>% 
  nest(data = -estado)


# función indice mes
indice_mes <- function(base){
  seq_along(along.with = base$mes)
}

# integrar indice mes por estado
df_07 <- 
  df_06 %>% 
  mutate(mes_sexenio = map(data, indice_mes)) %>% 
  unnest(everything()) %>% 
  relocate(mes_sexenio, .before = fecha)


morena <- 
  c("MORELOS", "TAMAULIPAS", "GUERRERO", "CIUDAD DE MÉXICO", "SONORA",
    "SINALOA", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "TABASCO",
    "CAMPECHE", "NAYARIT", "VERACRUZ DE IGNACIO DE LA LLAVE", "COLIMA", "MICHOACÁN DE OCAMPO",
    "QUINTANA ROO", "PUEBLA", "CHIAPAS", "OAXACA", "TLAXCALA", "SAN LUIS POTOSÍ")

# Plot of homicidios por mes por entidad federativa
df_07 %>% 
  # filter(estado %in% morena) %>%
  ggplot(aes(x = mes_sexenio, 
             y = homicidios,
             group = estado)) +
    geom_line(alpha = 0.35) +
    # poisson trend line
    geom_smooth(method = "glm",
                method.args = c("poisson"),
                se = FALSE, 
                linewidth = 0.28,
                color = "darkred") +
    labs(title = "Comportamiento del Número de Homicidios Dolosos durante el Sexenio",
         subtitle = "Cifras mensuales por estado. La línea roja representa la tendencia simple.",
         x = "Mes del sexenio",
         y = "Víctimas de homicidio doloso por mes",
         caption = "Fuente: Reportes de Incidencia Delictiva
           2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
           Gobierno de México.<br>
           Visualización: Juan L. Bretón, PMP | @juanlbreton") +
    theme(text = element_text(family = "Encode Sans Condensed"),
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          panel.grid = element_line(color = "grey95"),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 16),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = "darkgrey", 
                                          hjust = 0),
          legend.position = "top",
          strip.text.x.top = element_text(size = 7.5)) +
    scale_x_continuous(breaks = c(min(df_07$mes_sexenio), 
                                  (max(df_07$mes_sexenio) / 2) * 1,
                                  max(df_07$mes_sexenio))) +
    facet_wrap(vars(estado))

ggsave(filename = "graficas/mixed_01.jpg", device = "jpeg", dpi = "retina")


# different trend across estados implies
# different random-effect slopes for each estado


## Modeling
# Poisson regression
# homicidios predicted by mes, año and estado
glm(homicidios ~  mes_sexenio + estado,
    data = df_07,
    family = "poisson")

# glmer
glmer(homicidios ~ mes_sexenio + (mes_sexenio | estado),
      data = df_07,
      family = "poisson")

# scale año
df_08 <- 
  df_07 %>% 
  mutate(mes_sexe_esc = (mes_sexenio - mean(mes_sexenio)) / sd(mes_sexenio))

model_01 <- 
  glmer(homicidios ~ mes_sexe_esc + (mes_sexe_esc | estado),
        data = df_08,
        family = "poisson")

# model summary
summary(model_01)

# fixed-effect slope for año_esc
mes_slope <- 
  fixef(model_01)[2]

# random-effect slopes for estado
estado_slope <- 
  ranef(model_01)$estado

# new column for slope by adding fixed-effect and random-effect slopes
estado_slope_fill <- 
  estado_slope %>% 
  mutate(slope = mes_slope + mes_sexe_esc,
         estado = rownames(.)) %>% 
  as_tibble() %>%  
  select(estado, intercept = `(Intercept)`, everything()) %>% 
  mutate(estado = as_factor(estado)) %>% 
  rowid_to_column(var = "id_estado") %>% 
  mutate(id_estado = if_else(id_estado < 10,
                             paste0("0", id_estado),
                             as.character(id_estado)))


# Plot results
estado_slope_fill %>% 
  # filter(estado %in% morena) %>% 
  group_by(slope > 0) %>% 
  ggplot(aes(x = slope, 
             y = fct_reorder(estado, slope),
             color = slope > 0)) +
  geom_point(alpha = 0.65,
             size = 2) +
  geom_segment(aes(x = 0,
                   xend = slope,
                   yend = fct_reorder(estado, slope)),
               alpha = 0.65) +
  labs(title = "Cambio en la Tendencia de Ocurrencia de Homicidios Dolosos",
       subtitle = "Datos de dicembre de 2018 a noviembre 2023",
       y = NULL,
       x = "Tasa de cambio en la tendencia",
       caption = "Fuente: Reportes de Incidencia Delictiva
           2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
           Gobierno de México.<br>
           Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton") +
  scale_color_manual(name = "Orientación de la tendencia",
                    values = c("darkcyan", "darkred"),
                    labels = c("Decrece",
                               "Incrementa")) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 16),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey", 
                                        hjust = 0),
        legend.position = "top") 

ggsave(filename = "graficas/mixed_01.jpg", device = "jpeg", dpi = "retina")


# shape file for states
mapa_estados <- 
  st_read("conjunto_de_datos/nal/00ent.shp")

# join with data file
slope_estados <- 
  mapa_estados %>% 
  left_join(estado_slope_fill, by = c("CVEGEO" = "id_estado"))

# plot map
slope_estados %>% 
  ggplot(aes(fill = slope)) +
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
  labs(title = "Tendencia de Homicidios Dolosos",
       subtitle = "Por entidad",
       caption = "Fuente: Reportes de Incidencia Delictiva
             2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
             Gobierno de México.<br>
             Visualización: Juan L. Bretón, PMP | @juanlbreton") +
  scale_fill_gradient2(name = "Orientación de\nla tendencia", 
                      low = "darkgreen",
                      mid = "#F0F0F0",
                      high = "#7B0303")


## map showing change YTY
# latest month in record
last_reg <- 
  max(df_07$mes_sexenio)

# aggregated homicides per state last 12 months
cuenta_presente <- 
  df_07 %>% 
  filter(mes_sexenio <= last_reg & mes_sexenio >= last_reg - 12) %>% 
  group_by(id_entidad) %>% 
  summarize(homicidios_presente = sum(homicidios))

# aggregated homicides per state previos 12 months
cuenta_base <- 
  df_07 %>% 
  filter(mes_sexenio <= last_reg - 12 & mes_sexenio >= last_reg - 24) %>% 
  group_by(id_entidad) %>% 
  summarize(homicidios_base = sum(homicidios))

# change last 12 months
cambio <- 
  cuenta_base %>% 
  left_join(cuenta_presente, by = "id_entidad") %>% 
  mutate(pct_cambio = (homicidios_presente - homicidios_base) / homicidios_base)

# join with shape file
cambio_mapa <- 
  mapa_estados %>% 
  left_join(cambio, by = c("CVEGEO" = "id_entidad"))

# map of change YTY
cambio_mapa %>% 
  ggplot(aes(fill = pct_cambio)) +
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
  labs(title = "Cambio en el Acumulado de Homicidios de los Últimos 12 Meses",
       subtitle = "Por entidad federativa",
       caption = "Fuente: Reportes de Incidencia Delictiva
             2023; Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, 
             Gobierno de México.<br>
             Visualización: Juan L. Bretón, PMP | @juanlbreton") +
  scale_fill_gradient2(name = "Magnitud\n del cambio", 
                       low = "darkgreen",
                       mid = "#F0F0F0",
                       high = "#7B0303")














