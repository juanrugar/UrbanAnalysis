# Packages upload ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)


# Conversion of table "Construcciones" ----------------------------------------
construcciones_14 <- read_excel("datos_badalona/tablas/tipo14.xlsx", col_types = "text")

#Check result
glimpse(construcciones_14)

#Declaring new variable "construcciones" 
#for storing field filtering and renaming from the initial file "construcciones_14"  
construcciones <- construcciones_14 %>%
  filter(`1_tipo_reg`=='14') %>%
  select(REFCAT = `31_pc`,
         PLANTA = `65_pt`,
         FECHA = `79_aec`,
         SUP = `84_stl`,
         UCMC = `105_tip`) %>%
  mutate(FECHA = as.integer(FECHA),
         SUP = as.integer(SUP))

#Result check
glimpse(construcciones)

# Calculation of total ground floor (Planta Baja) surface ---------------------------------------------------

#Gathering ground floor (filtering by "PLANTA")
#and grouping by cadastral reference (REFCAT)
#adding up all
locales_pb_total <- construcciones %>%
  filter(PLANTA %in% c("0","00","BJ")) %>%
  group_by(REFCAT) %>%
  mutate(sup_pb_total = sum(SUP)) %>%
  count(sup_pb_total) %>%
  transmute(REFCAT, sup_pb_total)

#Result check
glimpse(locales_pb_total)  

#Sum of total ground floor
sum(locales_pb_total$sup_pb_total)


# Calculation of total ground floor for commercial purposes (Comercial) -----------------------------------------------

#Gathering ground floor (filtering by "PLANTA" 
#and using a Regex targeted to select all records with 04 and 072 values) 
#and grouping all by cadastral reference (REFCAT)
#adding up all
locales_pb_comercial <- construcciones %>%
  filter(PLANTA %in% c('0','00','BJ'), str_detect(UCMC, "^04.*|^072.*")) %>% 
  group_by(REFCAT) %>%
  mutate(sup_pb_comercial = sum(SUP)) %>%
  count(sup_pb_comercial) %>%
  transmute(REFCAT, sup_pb_comercial)

#Result check
glimpse(locales_pb_comercial)

#Sum of total ground floor for commercial purposes
sum(locales_pb_comercial$sup_pb_comercial)


# Tables' JOIN ---------------------------------------------------------
#Here we join the two tables with total ground floor and commercial floor
#with the carto so as to calculate the commercial potential of land plots
#and its percentage
#and then output the results to a map

#New variable declaration to store the data join ("parcelas_locales")
#from the carto file "parcelas"
#keeping the key field (REFCAT)
parcelas_locales <- parcelas %>%
  transmute(REFCAT)

#Join 1: parcelas + locales_pb_total
parcelas_locales <- parcelas_locales %>%
  left_join(locales_pb_total, by = "REFCAT")

#result check
glimpse(parcelas_locales)
  
#join 2: parcelas_locales + locales_pb_comercial
parcelas_locales <- parcelas_locales %>%
  left_join(locales_pb_comercial, by = "REFCAT")

#result check
glimpse(parcelas_locales)

#Checking union through a map (Map2)
plot(parcelas_locales, border= NA)


# Calculation of Commercial Potential (POtencial comercial) ----------------------------------------------------

#From the existing table "parcelas_locales" 
#coalescing NA values from the data fields
parcelas_locales  <- parcelas_locales %>%
  mutate(sup_pb_comercial =  coalesce(sup_pb_comercial, 0), 
         sup_pb_total = coalesce(sup_pb_total, 0))

#Adding new fields "potencial" and "por_comercial" to the 
parcelas_locales  <- parcelas_locales %>%
  mutate(potencial = sup_pb_total-sup_pb_comercial, 
         por_comercial = coalesce((sup_pb_comercial/sup_pb_total), 0)
  )
  
#result check
glimpse(parcelas_locales)  

#Export to shapefile
#Preparation of field names length for ESRI Shapefile driver:
parc_loc <- parcelas_locales %>%
  transmute(REFCAT,
         totalpb = sup_pb_total,
         totalcom = sup_pb_comercial,
         potencial,
         percom = por_comercial)
write_sf(parc_loc, "datos_badalona/output_layers/parcelas_locales.shp")

#plotting map of "Potencial Comercial (log)", (See Map3)
parcelas_locales %>%
  select(potencial) %>%
  mutate(potencial = potencial/10000) %>%
  plot(main ="Potencial Comercial (log)",
      nbreaks=10,
      breaks="pretty",
      border= NA
      )

#plotting map of "por_comercial", (See Map4)
parcelas_locales %>%
  select(por_comercial) %>%
  plot(main ="Porcentaje Comercial",
       breaks = "pretty",
       border = FALSE
       )
