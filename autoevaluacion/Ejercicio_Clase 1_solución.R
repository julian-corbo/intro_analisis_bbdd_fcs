

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#         Introducción al Análisis de bases de datos         #
#          Ejercicio de auto-evaluación de uso de R          #
#                           Clase 1                          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Actividad 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Cargar el paquete tidyverse y readxl (usar funcion read_excel()  
# para importar la base de datos)

library(tidyverse)
library(readxl)

# Abra la base de datos llamada dptos_2022.xlsx que se
# encuentra en la carpeta "Ejercicios/data" en la plataforma
# EVA o el Git-Hub del curso. 

df <- read_excel("Ejercicios/data/dptos_2022.xlsx")

# Identifique la cantidad de observaciones y variables 
# que contiene. Observe qué tipo de variables son. 

glimpse(df)

# Actividad 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Conteste las siguientes preguntas:

# ¿Cuál es el promedio de población por departamento? 

mean(df$pob_tot, na.rm = TRUE)

# ¿Qué departamento tiene mayor nivel de desempleo? 

max(df$td, na.rm = TRUE)
range(df$td, na.rm = TRUE)

# ¿Qué departamento tiene menor proporción de personas en hogares 
# en situación de pobreza? 

min(df$pers_pobres, na.rm = TRUE)
range(df$pers_pobres, na.rm = TRUE)


# Actividad 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Genere una nueva variable que indique la cantidad de niños/as de 0 a 14 años.

df <- df %>% mutate(ninos = pob_0a4 + pob_5a9 + pob_10a14)

# La tasa de dependencia es un índice que expresa la proporción existente 
# entre la población dependiente y la activa. Se calcula como la cantidad 
# de personas menores de 15 años y mayores de 65 años, dividido la cantidad
# de personas de 15 a 65 años, multiplicado por 100. Calcule la tasa de 
# dependencia de cada departamento. 

df <- df %>% mutate(tdep = (ninos + pob_65ymas) / pob_15a64 * 100)

# Genere una nueva variable que recodifique la tasa de dependencia en tres
# tramos: Baja (Menor a 55%); Media (Entre 55% y 60%); y Alta (60% y más)

df <- df %>% mutate(tdep_rec = case_when(tdep < 55 ~ 1, 
                                         tdep >= 55 & tdep < 60 ~ 2, 
                                         tdep >= 60 ~ 3))


# Genere una nueva variable que recodifique la proporción de personas 
# en hogares en situación de pobreza en tres tramos: Baja (menor a 5%);
# Media (Entre 5% y 10%); y Alta (10% y más)

df <- df %>% mutate(pobre_rec = case_when(pers_pobres < 5 ~ 1, 
                                          pers_pobres >= 5 & pers_pobres < 10 ~ 2, 
                                          pers_pobres >= 10 ~ 3))

# Actividad 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Conteste: ¿Qué departamento tiene mayor tasa de dependencia? 

max(df$tdep)

# Construya una tabla de contingencia que contenga en las 
# columnas las categorías de la variable tasa de dependencia 
# recodificada y en las filas los niveles de pobreza recodificados. 

table(df$pobre_rec, df$tdep_rec)

# Porcentualice la tabla construida en el punto anterior por filas 
# y columnas. 

prop.table(table(df$pobre_rec, df$tdep_rec), margin = 1)
prop.table(table(df$pobre_rec, df$tdep_rec), margin = 2)

# Realice una breve lectura de la tabla porcentualizada. 


# Actividad 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Complete el siguiente formulario de Google:
# https://forms.gle/1fnfJCQfaUS77G5V7

