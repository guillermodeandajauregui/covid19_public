library(tidyverse)
library(vroom)
library(lubridate)
###############################################################################
#read data
###############################################################################

el_path <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

temp <- tempfile()
download.file(url = el_path, destfile = temp)
el_file <- unzip(zipfile = temp, list = T)
el_file <- unz(description = temp, filename = el_file[1])
el_data <- vroom::vroom(el_file)
unlink(temp)
rm(temp, el_file)

dict_estados <- vroom::vroom(file = "https://raw.githubusercontent.com/guillermodeandajauregui/covid19_public/master/claves_estados.txt")


# CFR ambulatorio ----

el_data %>% 
  mutate(semana_epi_sintomas = epiweek(FECHA_SINTOMAS)) %>%
  mutate(semana_epi_sintomas = ifelse(year(FECHA_SINTOMAS)==2021 & semana_epi_sintomas!=53, semana_epi_sintomas+53, semana_epi_sintomas)) %>% 
  filter(semana_epi_sintomas > 10) %>%
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(TIPO_PACIENTE%in%1) %>% 
  mutate(deceased = !is.na(FECHA_DEF)) %>% 
  group_by(semana_epi_sintomas, ENTIDAD_UM, deceased) %>% 
  tally() %>% 
  mutate(total = sum(n)) %>% 
  mutate(case_fatality_rate = 100*n/total) %>% 
  filter(deceased) %>% 
  left_join(dict_estados, by=c("ENTIDAD_UM"="CLAVE_ENTIDAD")) %>% 
  ggplot() + 
  aes(x = semana_epi_sintomas, y = case_fatality_rate) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept = 53, linetype=3) + 
  facet_wrap(facets = vars(ENTIDAD_FEDERATIVA), scales = "free_y") + 
  theme_minimal() + 
  ylab("Porcentaje de casos confirmados que fallecen") + 
  xlab("Semana epidemiológica del inicio de síntomas") + 
  ggtitle("Evolucion del Case Fatality Rate", 
          "Pacientes ambulatorios") 
  
# CFR hospitalizado ----
el_data %>% 
  mutate(semana_epi_sintomas = epiweek(FECHA_SINTOMAS)) %>%
  mutate(semana_epi_sintomas = ifelse(year(FECHA_SINTOMAS)==2021 & semana_epi_sintomas!=53, semana_epi_sintomas+53, semana_epi_sintomas)) %>% 
  filter(semana_epi_sintomas > 10) %>%
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(TIPO_PACIENTE%in%2) %>% 
  mutate(deceased = !is.na(FECHA_DEF)) %>% 
  group_by(semana_epi_sintomas, ENTIDAD_UM, deceased) %>% 
  tally() %>% 
  mutate(total = sum(n)) %>% 
  mutate(case_fatality_rate = 100*n/total) %>% 
  filter(deceased) %>% 
  left_join(dict_estados, by=c("ENTIDAD_UM"="CLAVE_ENTIDAD")) %>% 
  ggplot() + 
  aes(x = semana_epi_sintomas, y = case_fatality_rate) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept = 53, linetype=3) + 
  facet_wrap(facets = vars(ENTIDAD_FEDERATIVA), scales = "free_y") + 
  theme_minimal() + 
  ylab("Porcentaje de casos confirmados que fallecen") + 
  xlab("Semana epidemiológica del inicio de síntomas") + 
  ggtitle("Evolucion del Case Fatality Rate", 
          "Pacientes hospitalizados") 


# Número casos confirmados ----

el_data %>% 
  mutate(semana_epi_sintomas = epiweek(FECHA_SINTOMAS)) %>%
  mutate(semana_epi_sintomas = ifelse(year(FECHA_SINTOMAS)==2021 & semana_epi_sintomas!=53, semana_epi_sintomas+53, semana_epi_sintomas)) %>% 
  filter(semana_epi_sintomas > 10) %>%
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(TIPO_PACIENTE%in%1:2) %>%
  group_by(semana_epi_sintomas, ENTIDAD_UM, TIPO_PACIENTE) %>% 
  tally() %>% 
  mutate(TIPO_PACIENTE = ifelse(TIPO_PACIENTE==1, "ambulatorio", "hospitalizado")) %>% 
  left_join(dict_estados, by=c("ENTIDAD_UM"="CLAVE_ENTIDAD")) %>% 
  ggplot() + 
  aes(semana_epi_sintomas, n, color=as.factor(TIPO_PACIENTE)) + 
  geom_line() + 
  facet_wrap(facets = vars(ENTIDAD_FEDERATIVA), scales = "free_y") + 
  theme_minimal() + 
  ylab("Número de casos confirmados registrados") + 
  xlab("Semana epidemiológica de inicio de síntomas") + 
  scale_color_manual(name="", values = c("black", "red"))

# pruebas realizadas ----

el_data %>% 
  filter(!(RESULTADO_LAB==97 &  RESULTADO_ANTIGENO==97)) %>% 
  mutate(semana_epi_sintomas = epiweek(FECHA_SINTOMAS)) %>%
  mutate(semana_epi_sintomas = ifelse(year(FECHA_SINTOMAS)==2021 & semana_epi_sintomas!=53, semana_epi_sintomas+53, semana_epi_sintomas)) %>% 
  filter(semana_epi_sintomas > 10) %>%
  group_by(semana_epi_sintomas, ENTIDAD_UM) %>% 
  tally() %>% 
  left_join(dict_estados, by=c("ENTIDAD_UM"="CLAVE_ENTIDAD")) %>% 
  ggplot() + 
  aes(semana_epi_sintomas, n) + 
  geom_line() + 
  facet_wrap(facets = vars(ENTIDAD_FEDERATIVA), scales = "free_y") + 
  theme_minimal() + 
  ylab("Número de pruebas realizadas") + 
  xlab("Semana epidemiológica de inicio de síntomas") 
