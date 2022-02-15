# Packages upload ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)


# 1- Building Seniority by plot (Antigüedad por parcela) --------------------------------------------------

#From table construcciones we grab fields "FECHA" and "SUP" for the formula
#to create "Antigüedad por parcela" 

antiguedad <- construcciones %>%
  group_by(REFCAT) %>%
  mutate(numerador = sum(SUP*FECHA), denominador = sum(SUP)) %>%
  transmute(antiguedad = numerador / denominador) %>%
  count(antiguedad) %>%
  transmute(REFCAT, antiguedad)

#result check
glimpse(antiguedad)

#We provide geometry to the "Antiguedad" table by joining it with table "parcelas"
#but only keeping the necessary field "REFCAT"
#TO DO SO declare a new variable "parcelas_antigüedad"
#and begin to add "REFCAT" and the geometry field
parcelas_antiguedad <- parcelas %>%
  transmute(REFCAT)

#THEN joining with the table "antiguedad"
#which contains the field "antiguedad" with yearly value
parcelas_antiguedad <- parcelas_antiguedad %>% 
  left_join(antiguedad, by = "REFCAT")

#Export to shapefile
write_sf(parcelas_antiguedad, "datos_badalona/output_layers/parcelas_antiguedad.shp")


#Plotting the data on a map (See Map5)
#by "antiguedad" from 1900 to 2020
parcelas_antiguedad %>%
  select(antiguedad) %>%
  plot(nbreaks = 10,
    breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2010, 2020),
    border = FALSE,
    main = "Antigüedad media de la edificación"
    )

