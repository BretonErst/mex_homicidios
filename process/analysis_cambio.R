###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(lme4)


## Source
suppressWarnings(source("data/cleaned_data_victim.R")) 

# data
df_06 <- 
  df_05 |>
  select(rowid, id_entidad, año, num_mes, mes, fecha, estado, homicidios)
 

# Plot of homicidios por mes por entidad federativa
df_06 %>% 
  filter(!fecha > as_date("2023-05-01")) %>% 
  ggplot(aes(x = fecha, 
             y = homicidios,
             group = estado)) +
  geom_line(alpha = 0.3) +
  # poisson trend line
  geom_smooth(method = "glm",
              method.args = c("poisson"),
              se = FALSE, 
              linewidth = 0.25)

# different trend across estados implies
# different random-effect slopes for each estado


## Modeling
# Poisson regression
# homicidios predicted by mes, año and estado
glm(homicidios ~  año + estado,
    data = df_06,
    family = "poisson")

# glmer
glmer(homicidios ~ año + (año | estado),
      data = df_06,
      family = "poisson")

# scale año
df_07 <- 
  df_06 |>
  mutate(año_esc = año - min(año))

model_01 <- 
  glmer(homicidios ~ año_esc + (año_esc | estado),
        data = df_07,
        family = "poisson")

# model summary
summary(model_01)

# fixed-effect slope for año_esc
año_slope <- 
  fixef(model_01)[2]

# random-effect slopes for estado
estado_slope <- 
  ranef(model_01)$estado

# new column for slope by adding fixed-effect and random-effect slopes
estado_slope_fill <- 
  estado_slope %>% 
  mutate(slope = año_slope + año_esc,
         estado = rownames(.)) %>% 
  as_tibble() %>%  
  select(estado, intercept = `(Intercept)`, everything()) %>% 
  mutate(estado = as_factor(estado))


# Plot results
estado_slope_fill %>% 
  ggplot(aes(x = slope, y = fct_reorder(estado, slope))) +
  geom_point()
















