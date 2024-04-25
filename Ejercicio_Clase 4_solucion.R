# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                 Introducción al Análisis de bases de datos                     #
#                    Operacionalización: Construcción de NBI                     #
#                                  Clases 4 y 5                                  #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Carga de paquetes ###

library(rio)
library(osfr)
library(here)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Carga de base Censo 2011 (INE) ###

## Opción 1: cargar archivo desde ruta

df  <- rio::import("MI RUTA/Censo 2011.Rdata") # Base competa (descargar de web INE u OSF)
df  <- rio::import("MI RUTA/Censo2011_muestra.Rdata") # Base reducida solo para ejercicios (descargar de EVA)


## Opción 2: descargar desde web OSF - UMAD

if (!dir.exists(here::here("Censo"))) {dir.create(here::here("Censo"))}

osfr::osf_retrieve_file("4jczy") %>% 
  osfr::osf_download(here("Censo"), conflicts = "overwrite")

df <- rio::import(here::here("Censo", "Censo 2011.Rdata"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Transformaciones generales ###

# Me quedo solo con las viviendas particulares

df <- subset(df, VIVVO01<9)

# Genero una variable de identificación del hogar en cada vivienda

df <-df %>% mutate(NUMERO = paste(ID_VIVIENDA, HOGID))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### CONSTRUCCIÓN DE VARIABLES DE NBI ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Vivienda decorosa ~~~ #

# Materialidad vivienda
# El hogar habita una vivienda con techos o paredes construidas
# predominantemente con materiales de desecho, o piso de tierra sin
# piso ni contrapiso.


df <- df %>% 
  dplyr::mutate(NBI_materialidad = ifelse(VIVDV02 == 5 | 
                                          VIVDV01 == 6 | 
                                          VIVDV03 == 4, 1, 
                                   ifelse(VIVDV02 == 0, 99, 0)))

prop.table(table(df$NBI_materialidad))*100


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Espacio habitable (Hacinamiento)
# Umbral: Más de dos miembros del hogar por habitación en la vivienda
# (excluyendo baño y cocina).

df <- df %>% 
  mutate(unos = 1) # Variable que vale 1 para toda las personas
df <- df %>% 
  group_by(NUMERO) %>% 
  mutate(nint = sum(unos)) %>% 
  as.data.frame() # Cuenta cantidad de personas por hogar

df <- df %>% 
  dplyr::mutate(NBI_hacinamiento  = ifelse(nint / HOGHD00 > 2, 1,  # Cantidad de habitaciones residenciales mayor estricto que dos
                                               ifelse(HOGHD00 == 88, 
                                                      99, 
                                                      0)))

prop.table(table(df$NBI_hacinamiento))*100


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Espacio apropiado para cocinar
# Umbral: El hogar habita una vivienda que no cuenta con un espacio para
# cocinar con canilla y pileta.

df <- df %>% dplyr::mutate(NBI_cocina = ifelse(HOGSC01 == 3, 
                                               1, 
                                        ifelse(HOGSC01 == 8,
                                               99,
                                               0)))

prop.table(table(df$NBI_cocina))*100


df <- df %>% dplyr::mutate(NBI_vivienda = ifelse(NBI_hacinamiento == 1 |
                                                 NBI_cocina       == 1 |
                                                 NBI_materialidad == 1, 
                                                 1,
                                          ifelse(NBI_hacinamiento == 99 |
                                                 NBI_cocina       == 99 |
                                                 NBI_materialidad == 99, 
                                                 99,
                                                 0)))

prop.table(table(df$NBI_vivienda))*100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Abastecimiento de agua potable ~~~ #
# Umbral: El agua no llega por cañería dentro de la vivienda que habita el
# hogar, o su origen no es red general o pozo surgente protegido.

df <- df %>% dplyr::mutate(NBI_agua = ifelse(VIVDV05 == 0,
                                             99,
                                      ifelse(VIVDV06 != 1 | VIVDV05 >= 3, 
                                             1, 
                                             0)))

prop.table(table(df$NBI_agua))*100


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Servicio higiénico de calidad ~~~ #
# El hogar no accede a baño de uso exclusivo o la evacuación del
# servicio sanitario no es a través de la red general, fosa séptica o pozo
# negro.

df <- df %>% dplyr::mutate(NBI_servhigien = ifelse(HOGSH01 == 8, 
                                                   99,
                                            ifelse(HOGSH01 == 3 |
                                                     HOGSH02 == 2 |
                                                     HOGSH03 > 2, 
                                                   1,
                                                   0)))

prop.table(table(df$NBI_servhigien))*100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Energía eléctrica ~~~ #
# El hogar no cuenta con energía eléctrica en la vivienda que habita.

df <- df %>% dplyr::mutate(NBI_energia = ifelse(VIVDV07 == 6, 
                                                1, 
                                         ifelse(VIVDV07 == 0,
                                                99,
                                                0)))

prop.table(table(df$NBI_energia))*100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Artefactos básicos de cofnort ~~~ #
# El hogar no cuenta con ningún medio para calefaccionar la vivienda que habita. 
# El hogar no cuenta con heladera o freezer.
# El hogar no posee calefón, termofón, caldereta o calentador instantáneo.

df <- df %>% dplyr::mutate(NBI_artefactos = ifelse(HOGCE01 == 8, 99,
                                            ifelse(HOGCA01 == 9 | 
                                                   HOGCE03 == 2 | 
                                                  (HOGCE01 == 2 &
                                                     HOGCE02 == 2), 1, 0)))

prop.table(table(df$NBI_artefactos))*100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Educación ~~~ #
# Al menos un integrante del hogar con edad comprendida entre los
# 4 y los 17 años no se encuentra asistiendo a un centro educativo
# formal, no habiendo finalizado enseñanza secundaria.


df <- df %>% 
  dplyr::mutate(tramoed = ifelse(PERNA01 > 3 & PERNA01 < 18, 1, 0),
                asiste  = ifelse((PERED00 >= 1 & PERED00 <= 3) | 
                                             PERED01 >= 1 & PERED01 <= 2, 1, 0))

df <- df %>% 
  dplyr::mutate(finaliza = ifelse(asiste == 0 & 
                                    PERED03_1_r >= 6 & 
                                    PERED04_r == 1, 1, 0))
                           
df <- df %>% 
  dplyr::mutate(nbiedupers = ifelse(tramoed == 1 & 
                                      asiste == 0 & 
                                      finaliza == 0, 1, 0))

                                   
df <- df %>% group_by(NUMERO) %>% 
  mutate(NBI_educacion = max(nbiedupers))

prop.table(table(df$NBI_educacion))*100


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Total NBI ~~~ #

df <- df %>% 
  dplyr::mutate(NBI_2011= ifelse(NBI_vivienda   == 1 | 
                                 NBI_agua       == 1 | 
                                 NBI_servhigien == 1 |
                                 NBI_energia    == 1 | 
                                 NBI_artefactos == 1 | 
                                 NBI_educacion  == 1, 
                                 1, 
                                 ifelse(NBI_vivienda     == 99 | 
                                          NBI_agua       == 99 | 
                                          NBI_servhigien == 99 |
                                          NBI_energia    == 99 | 
                                          NBI_artefactos == 99 | 
                                          NBI_educacion  == 99, 99, 0)))

prop.table(table(df$NBI_2011))*100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
