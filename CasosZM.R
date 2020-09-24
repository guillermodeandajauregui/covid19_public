###############################################################################
#Distribución de casos en Zonas Metropolitanas 
#Guillermo de Anda-Jáuregui
#gdeanda at inmegen dot edu dot mx 
###############################################################################

###############################################################################
#libraries ----
###############################################################################

library(tidyverse)
library(vroom)
library(lubridate)
library(janitor)
###############################################################################
#download data ----
###############################################################################

el_path <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

temp <- tempfile()
download.file(url = el_path, destfile = temp)
el_file <- unzip(zipfile = temp, list = T)
el_file <- unz(description = temp, filename = el_file[1])
el_data <- vroom::vroom(el_file)
unlink(temp)
rm(temp, el_file)

claves_estados    <- vroom::vroom("https://raw.githubusercontent.com/guillermodeandajauregui/covid19_public/master/claves_estados.txt")
claves_municipios <- vroom::vroom("https://raw.githubusercontent.com/guillermodeandajauregui/covid19_public/master/claves_municipios.txt")

#zonas metropolitanas, definidas de https://www.gob.mx/conapo/documentos/delimitacion-de-las-zonas-metropolitanas-de-mexico-2015
datos_metropolis <- vroom::vroom("https://raw.githubusercontent.com/coronamex/datos/master/util/zonas_metropolitanas_2015.csv")

###############################################################################
#Armar dataset ----
###############################################################################

carga_casos <- 
el_data %>% 
  #limit to confirmed
  filter(RESULTADO=="1") %>% 
  #some selects to better handle data
  select(contains("FECH"), ENTIDAD_RES, MUNICIPIO_RES, TIPO_PACIENTE) %>% 
  #join with metropolis data
  mutate(CVE_MUN = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  left_join(y = select(datos_metropolis, 
                       NOM_ZM, CVE_MUN)
            ) %>% 
  #add everything happening outside the metro zones to "otra"
  mutate(NOM_ZM = ifelse(is.na(NOM_ZM), "otra", NOM_ZM)) %>% 
  #make a is ZM or not variable
  mutate(es_zm = ifelse(NOM_ZM=="otra", "NO", "SI"))

###############################################################################
#plots ----
###############################################################################

#plot: Dentro o fuera
carga_casos %>% 
  group_by(es_zm) %>% 
  tally() %>% 
  mutate(porcentaje = 100*n/sum(n)) %>% 
  mutate(es_zm = as_factor(es_zm)) %>% 
  mutate(es_zm = fct_relevel(.f = es_zm, "SI", "NO")) %>% 
  ggplot(aes(x = es_zm, y = porcentaje, fill = es_zm)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name = "", values = c("darkred", "black")) + 
  theme_minimal() + 
  xlab("¿Zona Metropolitana?") + 
  ggtitle("Porcentaje de casos confirmados", 
          subtitle = "Residencia dentro o fuera de una Zona Metropolitana")
 

 #plot - cuales Z.M. concentran más casos
carga_casos %>% 
  mutate(NOM_ZM = as_factor(NOM_ZM)) %>% 
  group_by(NOM_ZM) %>% 
  tally() %>% 
  mutate(porcentaje = 100*n/sum(n)) %>% 
  arrange(desc(porcentaje)) %>% 
  filter(porcentaje >= 1) %>% 
  mutate(NOM_ZM = fct_recode(NOM_ZM, fuera_zm = "otra")) %>% 
  mutate(NOM_ZM = fct_reorder(.f = NOM_ZM, .x = porcentaje, .desc = T)) %>% 
  ggplot(aes(x = NOM_ZM, y = porcentaje, fill = NOM_ZM)) + 
  geom_bar(stat="identity") + 
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggsci::scale_fill_d3(palette = "category20b") + 
  xlab("Zona Metropolitana") + 
  ggtitle(label = "Porcentaje de casos confirmados", 
          subtitle = ("Z.M. con más de 1% de los casos o casos fuera de Z.M.")
          ) 
  
