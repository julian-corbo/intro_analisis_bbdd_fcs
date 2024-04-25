
install.packages("rio")

library(rio)
library(tidyverse)

#df <- rio::import("C:/Users/Julian/Desktop/JULIAN/R/FCS/Base unificada Viv_Hog_Pers.dbf")

df <- rio::import("C:/Users/Julian/Desktop/JULIAN/R/FCS/bbdd/Censo2011_muestra.RData")

df <- subset(df, VIVVO01<9)


# Segun material de construccion de piso techo y paredes. 
df <- df %>% 
  dplyr::mutate(NBI_materialidad = ifelse(
                                      VIVDV01 == 6 | VIVDV02 == 5 | VIVDV03 == 4,
                                      1,
                                      ifelse(VIVDV02 == 0, 99, 0)))

table(df$NBI_MAT,df$NBI_materialidad)
table(df$NBI_materialidad)

# Espacio apropiado para cocina 

df <- df %>% 
  dplyr::mutate(NBI_cocina = ifelse(
                                HOGSC01 == 3, 1,
                                ifelse(HOGSC01 == 8, 99, 0)))


table(df$NBI_cocina)


# hacinamiento 



df <- df %>%
  dplyr::mutate(NBI_hacinamiento = case_when(HOGPR01/HOGHD00 > 2 ~ 1, TRUE ~ 0)) #Me falta 99 

table(df$NBI_cocina)

df <- df %>% 
  dplyr::mutate(NBI_vivienda_dec = ifelse(NBI_materialidad == 1| NBI_hacinamiento == 1 | NBI_cocina == 1, 1, 0))


table(df$NBI_HAC, df$NBI_hacinamiento)

#agua (No se si esta bien)

df <- df %>% 
  dplyr::mutate(NBI_agua = ifelse(
                              VIVDV05 >= 3 | VIVDV06 >3, 1,
                              ifelse(VIVDV05 == 8 | VIVDV05 == 0 | VIVDV06 == 0, 99, 0)))
  

round(prop.table(table(df$NBI_agua))*100, 1)

#higiene

df <- df %>% 
  dplyr::mutate(NBI_higiene = ifelse(HOGSH03 == 8 | HOGSH02 == 8, 99,
                                     ifelse(HOGSH03 > 2 | HOGSH02 !=1, 1, 0 )))

round(prop.table(table(df$NBI_higiene))*100, 1)


df <- df %>% 
  dplyr::mutate(NBI_electrico = ifelse(VIVDV07 == 6, 1,
                                       ifelse(VIVDV07 == 8 | VIVDV07 == 0, 99, 0)))
                                            

round(prop.table(table(df$NBI_electrico))*100, 1)    


df <- df %>% 
  dplyr::mutate(NBI_calefaccion = ifelse(HOGCA01 == 9, 1, 
                                      ifelse(HOGCA01 == 8 | HOGCA01 == 0 , 99, 0)))

                                      

round(prop.table(table(df$NBI_calefaccion))*100, 1)  


df <- df %>% 
  dplyr::mutate(NBI_alimentos = ifelse(HOGCE03 == 2, 1, 
                                       ifelse(HOGCE03 == 8 | HOGCE03 == 0, 99, 0)))

round(prop.table(table(df$NBI_alimentos))*100, 1)  



df <- df %>% 
  dplyr::mutate(NBI_cal_banio = ifelse(HOGCE02 == 2 & HOGCE01 == 2, 1, 
                                       ifelse(HOGCE02 == 8 | HOGCE02 == 0 |
                                              HOGCE01 == 8 | HOGCE01 == 0, 99, 
                                              0)))


round(prop.table(table(df$NBI_cal_banio))*100, 1)

df <- df %>% 
  dplyr::mutate(NBI_artefactos2 = dplyr::case_when(NBI_alimentos == 1 |
                                                   NBI_calefaccion == 1 |
                                                   NBI_cal_banio == 1 ~1,
                                                   NBI_alimentos == 99 |
                                                   NBI_calefaccion == 99 |
                                                   NBI_cal_banio == 99 ~ 99,
                                                   TRUE ~ 0))

round(prop.table(table(df$NBI_artefactos2))*100, 1)


#EDUCACION







