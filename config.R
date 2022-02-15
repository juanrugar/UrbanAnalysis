
# Packages upload  ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

# Data upload  -------------------------------------------------------------

# Cadastral data upload - Type 11
fincas_11 <- read_excel("datos_badalona/tablas/tipo11.xlsx", col_types = "text")

#table contents check OK
glimpse(fincas_11)

#Cadastral data upload - Type 14
construcciones_14 <- read_excel("datos_badalona/tablas/tipo14.xlsx", col_types = "text")

#table contents check OK
glimpse(construcciones_14)

# Cartography upload
#land plot layer upload (Shapefile)
parcelas <- read_sf("datos_badalona/capas/PARCELA.SHP")

#Upload check (OK)
parcelas %>%
  select(geometry) %>%
  plot(lwd=0.5)


# Example - Calculation of lands stats per plot (Type 11) ------------------

#Filtering table by target field
fincas <- fincas_11 %>%
  filter(`1_tipo_reg` == '11')

#Data check for both tables (origin and filtered)
nrow(fincas_11)
nrow(fincas)

#Filtering and renaming fields
fincas <- fincas_11 %>%
  filter(`1_tipo_reg`=='11') %>%
  select(REFCAT = `31_pc`,
         SUELO = `296_sup`,
         TOTAL = `306_sct`,
         SOBRE = `313_ssr`,
         BAJO = `320_sbr`,
         CUBIERTA = `327_sc`)

#Filtering and renaming check (OK)
glimpse(fincas)

#Parsing fields' type to integers
fincas <- fincas_11 %>%
  filter(`1_tipo_reg`=='11') %>%
  select(REFCAT = `31_pc`,
         SUELO = `296_sup`,
         TOTAL = `306_sct`,
         SOBRE = `313_ssr`,
         BAJO = `320_sbr`,
         CUBIERTA = `327_sc`) %>%
  mutate(SUELO = as.integer(SUELO),
         TOTAL = as.integer(TOTAL),
         SOBRE = as.integer(SOBRE),
         BAJO = as.integer(BAJO),
         CUBIERTA = as.integer(CUBIERTA))

#Checking results
glimpse(fincas)

#Exporting results to a table (csv file)
write_csv(fincas, "datos_badalona/output_tables/fincas.csv")

#IDEM to a DBF file
foreign::write.dbf(fincas, "datos_badalona/output_tables/fincas.dbf")
##ERROR MESSAGE: unknown column type in data frame
##I could not perform this operation

#Export to SHP file:
#Step 1-Creating new variable for the JOIN 
#between data table ("fincas")and shp ("parcelas)
parcelas_fincas <- parcelas %>%
  select(REFCAT) %>%
  left_join(fincas, by="REFCAT")

#Step 1 result's check
glimpse(parcelas_fincas)

#Step 2- Export to shapefile
write_sf(parcelas_fincas, "datos_badalona/output_layers/parcelas_fincas.shp")


#Map of "Densidad sobre rasante" plotting (See Map1)
#calculating density field for the map
#Adding some 
parcelas_fincas %>%
  transmute(densidad = SOBRE / SUELO) %>%
  plot(pal = NULL,
    breaks = "pretty",
    border = NA,
    main = "Densidad sobre rasante (m2 techo / m2 suelo)")
       


