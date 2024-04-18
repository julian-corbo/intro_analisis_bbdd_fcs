# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
library(rio)
library(osfr)
library(here)
library(tidyverse)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de censo ###

#df  <- rio::import("Auxiliares/Clase 4/Ejercicio/Base unificada Viv_Hog_Pers.sav")
### Lo guardo en RData ### 
#save(df, file = "Auxiliares/Clase 4/Ejercicio/Censo 2011.RData")


if (!dir.exists(here::here("Censo"))) {dir.create(here::here("Censo"))}

osfr::osf_retrieve_file("4jczy") %>% 
  osfr::osf_download(here("Censo"), conflicts = "overwrite")
df <- rio::import(here::here("Censo", "Censo 2011.Rdata"))


#Me quedo solo con las viviendas particulares
df <- subset (df, VIVVO01<9)

#Identifico hogar en cada vivienda

df <- df %>% mutate(NUMERO = paste(ID_VIVIENDA, HOGID))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###


###### NBI - Vivienda decorosa


## Materialidad vivienda
df <- df %>% dplyr::mutate(NBI_materialidad = ifelse(VIVDV02==5 | VIVDV01==6 | VIVDV03==4, 1, 
                                              ifelse(VIVDV02==0, 99, 0)))


## Espacio habitable - Hacinamiento

#Genero cantidad de personas en el hogar
df <- df %>% mutate(unos = 1) 
df <- df %>% group_by(NUMERO) %>% mutate(nint = sum(unos)) %>% as.data.frame()

df <- df %>% dplyr::mutate(NBI_HACINAMIENTO  = ifelse(nint/HOGHD00>2, 1,
                                               ifelse(HOGHD00==88, 99, 0)))

## Espacio apropiado para cocinar
df <- df %>% dplyr::mutate(NBI_cocina = ifelse(HOGSC01==3, 1, 
                                        ifelse(HOGSC01==8,99,0)))


df <- df %>% dplyr::mutate(NBI_VIVIENDA = ifelse(NBI_HACINAMIENTO==1|
                                                 NBI_cocina==1|
                                                 NBI_materialidad==1, 1,
                                          ifelse(NBI_HACINAMIENTO==99|
                                                 NBI_cocina==99|
                                                 NBI_materialidad==99, 99,0)))


###### NBI - Abastecimiento de agua potable

df <- df %>% dplyr::mutate(NBI_agua = ifelse(VIVDV05==0,99,
                                      ifelse(VIVDV06!=1 | VIVDV05>=3, 1,0)))


###### NBI - Servicio higiénico de calidad

df <- df %>% dplyr::mutate(NBI_servhigien = ifelse(HOGSH01==8,99,
                                            ifelse(HOGSH01==3|HOGSH02==2|HOGSH03>2, 1,0)))


###### NBI - Energía eléctrica
df <- df %>% dplyr::mutate(NBI_energia = ifelse(VIVDV07==6, 1, 
                                         ifelse(VIVDV07==0,99,0)))


###### NBI - Artefactos básicos de cofnort

df <- df %>% dplyr::mutate(NBI_artefactos = ifelse(HOGCE01==8,99,
                                            ifelse(HOGCA01==9 | 
                                                   HOGCE03==2 | 
                                                  (HOGCE01==2&HOGCE02==2), 1, 0)))


###### NBI - Educación

df <- df %>% dplyr::mutate(EDUC = ifelse(PERNA01>3&PERNA01<18 &  
                                         PERED01>2 &
                                        (PERED03_1_r<6 | 
                                        ((PERED03_1_r==6|PERED03_1_r==7)&PERED04_r==2)),1,0))



df <- df %>% group_by(NUMERO) %>% mutate(NBI_EDUCACION11 = max(EDUC)) %>% as.data.frame()


###### NBI Total

df <- df %>% dplyr::mutate(NBI_2011= ifelse(NBI_vivienda11==1 | NBI_agua11==1 | NBI_servhigien11==1 | NBI_energia11==1 | NBI_artefactos11==1 | NBI_educacion11==1, 1, 0))
df <- df %>% dplyr::mutate(NBI_cant= NBI_vivienda11 + NBI_agua11 + NBI_servhigien11 + NBI_energia11 + NBI_artefactos11 + NBI_educacion11)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
