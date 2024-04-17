library(here)
library(readxl)

df <- read_excel(here("ejercicio","consistencia.xlsx"))



table(is.na(df$PerEC03))
table(is.na(df$PerEC01))
table(is.na(df$PerNa01))

table(df$PerNa01) #Edad Un caso 999

table(is.na(df$PerPa01))
table(df$PerPa01) 

table(df$PerEC01)
table(df$PerEC03)

library(dplyr)
df2 <- df %>% dplyr::filter(MA != 1)

table(df2$ID)

table(df2$DPTO)

table(df2$MA)

table(df2$HOGID)

table(df2$PERID)

table(df2$HogPR01)

table(df2$HogPR02)
table(df2$HogPR03)
table(df2$PerPH02)
table(df2$PerNa01)
table(df2$PerPa01)
table(df2$PerEC01)
table(df2$PerEC03)
