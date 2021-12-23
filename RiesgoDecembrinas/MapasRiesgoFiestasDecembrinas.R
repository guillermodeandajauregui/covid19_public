################################################################################
# Adaptación del trabajo de estimación de riesgo COVID-19 de

# https://github.com/kaitejohnson 
# PandemicPreventionInstitute 
# https://github.com/PandemicPreventionInstitute/holiday_risk_estimator/blob/main/code/holiday_risk_map_Delta.R

# al escenario Mexicano 

# Guillermo de Anda Jáuregui

# Este material es informativo, consulte las recomendaciones de personas expertas locales 


################################################################################
library(tidyverse)
library(rvest)
library(sf)

################################################################################

#leer lo necesario ----

# necesitas para reproducir: 
# - datos de sisver
# - shp files entidades y municipales del inegi https://www.inegi.org.mx/app/ageeml/ 
# - conteos de poblacion del inegi https://www.inegi.org.mx/programas/ccpv/2020/#Datos_abiertos 

#leer datos de casos ----
#connect to sql----
remota <- readr::read_table("datos_sisver.txt")

#big block of wrangling to get vax data ----
#bajar las vacunas
vacunacovid <- rvest::read_html("http://vacunacovid.gob.mx/wordpress/")
vacunacovid %>% 
  html_elements("path") %>% 
  #html_attr("data-title")
  html_attr("data-vacunados")

vax <- tibble(Entidad = vacunacovid %>% html_elements("path") %>% html_attr("data-title"),
              Aplicadas = vacunacovid %>% html_elements("path") %>% html_attr("data-vacunados") %>% parse_number()
) %>% 
  mutate(Entidad = case_when(Entidad=="Coahuila" ~ "Coahuila de Zaragoza",
                             Entidad=="CDMX" ~ "Ciudad de México",
                             Entidad=="Estado de México" ~ "México",
                             Entidad=="Michoacán" ~ "Michoacán de Ocampo",
                             Entidad=="Veracruz" ~ "Veracruz de Ignacio de la Llave",
                             TRUE ~ Entidad
  )) %>% 
  drop_na()

###########
path_marco.ent = "donde/tienes/tus/shapes.shp"
path_marco.mun = "donde/tienes/tus/shapes.shp"
shp_ent <- read_sf(path_marco.ent)
shp_mun <- read_sf(path_marco.mun)

vax <-
  shp_ent %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  left_join(vax, ., by=c("Entidad"="NOMGEO")) 

###########
path.pop <- "donde/tienes/tabla/inegi.csv"
pop <- vroom::vroom("path.pop")
pop.ent <- 
  pop %>% 
  filter(NOM_LOC == "Total de la Entidad") %>% 
  select(ENTIDAD, POBTOT) %>% 
  rename(CVEGEO = ENTIDAD, 
         Population = POBTOT)

vax <- 
  left_join(vax, pop.ent) %>% 
  mutate(vax_pc  = 100*Aplicadas/Population) 

###########

pop.mun <- 
  pop %>% 
  filter(MUN!="000") %>% 
  filter(LOC == "0000") %>% 
  select(ENTIDAD, MUN, POBTOT) %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, everything()) %>% 
  rename(Population = POBTOT)

################################################################################


#parameters from original repo ---- 
yesterday <- lubridate::today('America/Mexico_City')- days(1)

DUR_INF<- 7 
reporting_rate<- 1/10 # https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/burden.html
test_sens <-0.786 # https://www.cdc.gov/mmwr/volumes/70/wr/mm7003e3.htm, https://www.medrxiv.org/content/10.1101/2020.10.30.20223198v1.full.pdf
VE_inf<- 0.53 # 0.3 for omicron # https://www.cdc.gov/mmwr/volumes/70/wr/mm7034e3.htm 
event_size<-c(10,20,30)
event_size<-sort(event_size)
prop_vax_event<-1

#repo functions ----

calc_risk_inf_at_event <- function(p_I, n) {
  r<-1-(1-p_I)**n
  round(100 * r, 1)
}

#get active count ----


early_date <- yesterday-DUR_INF-1

activos <- 
  remota %>% 
  filter(FECHA_SINTOMAS>=early_date) %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES) %>% 
  tally() %>% 
  collect()

activos <- 
  activos %>% 
  ungroup() %>% 
  mutate(ENTIDAD_RES= str_pad(ENTIDAD_RES, 2, "left", 0), 
         MUNICIPIO_RES= str_pad(MUNICIPIO_RES, 3, "left", 0), 
         n = as.numeric(n)
  ) %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  select(CVEGEO, cases = n)

activos <- 
  activos %>% 
  mutate(CVE_ENT = str_sub(CVEGEO, end = 2))

full_data <-
  left_join(activos, vax, by="CVE_ENT") %>% 
  select(CVEGEO = CVEGEO.x, 
         cases, 
         CVE_ENT, 
         vax_pc)

full_data <-
  left_join(full_data, pop.mun)


################################################################################

# Calculate the effect of vaccination and  day of testing on 
mun_risk <- 
  full_data %>% 
  mutate(sum_cases_7_days = cases, 
         pop = Population,
         vax_rate = vax_pc/100
  ) %>% 
  mutate(cases_last_7_days_per_100k=round((sum_cases_7_days/pop)*100000,0),
         prev_per_100k = (1/reporting_rate)*sum_cases_7_days/pop*100000,
         prev = ((1/reporting_rate)*sum_cases_7_days)/pop,
         m_test = (1-test_sens),
         m_v = (1-VE_inf*prop_vax_event)/(1-VE_inf*vax_rate),
  ) 

mun_risk <-
  mun_risk %>% 
  mutate(
    risk_inf_no_mitig_small = calc_risk_inf_at_event(prev, event_size[1]),
    risk_inf_testing_no_vax_small = calc_risk_inf_at_event(m_test*prev, event_size[1]),
    risk_inf_fully_vax_no_test_small = calc_risk_inf_at_event(m_v*prev, event_size[1]),
    risk_inf_fully_vax_and_test_small = calc_risk_inf_at_event(m_test*m_v*prev, event_size[1]),
    risk_inf_no_mitig_med = calc_risk_inf_at_event(prev, event_size[2]),
    risk_inf_testing_no_vax_med = calc_risk_inf_at_event(m_test*prev, event_size[2]),
    risk_inf_fully_vax_no_test_med = calc_risk_inf_at_event(m_v*prev, event_size[2]),
    risk_inf_fully_vax_and_test_med = calc_risk_inf_at_event(m_test*m_v*prev, event_size[2]),
    risk_inf_no_mitig_big = calc_risk_inf_at_event(prev, event_size[3]),
    risk_inf_testing_no_vax_big = calc_risk_inf_at_event((m_test*prev), event_size[3]),
    risk_inf_fully_vax_no_test_big = calc_risk_inf_at_event(m_v*prev, event_size[3]),
    risk_inf_fully_vax_and_test_big = calc_risk_inf_at_event(m_test*m_v*prev, event_size[3])
  )



################################################################################
#la omicroniña ----

VE_inf.omi <- 0.7

mun_risk.omi <- 
  full_data %>% 
  mutate(sum_cases_7_days = cases, 
         pop = Population,
         vax_rate = vax_pc/100
  ) %>% 
  mutate(cases_last_7_days_per_100k=round((sum_cases_7_days/pop)*100000,0),
         prev_per_100k = (1/reporting_rate)*sum_cases_7_days/pop*100000,
         prev = ((1/reporting_rate)*sum_cases_7_days)/pop,
         m_test = (1-test_sens),
         m_v = (1-VE_inf.omi*prop_vax_event)/(1-VE_inf.omi*vax_rate),
  ) 

mun_risk.omi <-
  mun_risk.omi %>% 
  mutate(
    risk_inf_no_mitig_small = calc_risk_inf_at_event(prev, event_size[1]),
    risk_inf_testing_no_vax_small = calc_risk_inf_at_event(m_test*prev, event_size[1]),
    risk_inf_fully_vax_no_test_small = calc_risk_inf_at_event(m_v*prev, event_size[1]),
    risk_inf_fully_vax_and_test_small = calc_risk_inf_at_event(m_test*m_v*prev, event_size[1]),
    risk_inf_no_mitig_med = calc_risk_inf_at_event(prev, event_size[2]),
    risk_inf_testing_no_vax_med = calc_risk_inf_at_event(m_test*prev, event_size[2]),
    risk_inf_fully_vax_no_test_med = calc_risk_inf_at_event(m_v*prev, event_size[2]),
    risk_inf_fully_vax_and_test_med = calc_risk_inf_at_event(m_test*m_v*prev, event_size[2]),
    risk_inf_no_mitig_big = calc_risk_inf_at_event(prev, event_size[3]),
    risk_inf_testing_no_vax_big = calc_risk_inf_at_event((m_test*prev), event_size[3]),
    risk_inf_fully_vax_no_test_big = calc_risk_inf_at_event(m_v*prev, event_size[3]),
    risk_inf_fully_vax_and_test_big = calc_risk_inf_at_event(m_test*m_v*prev, event_size[3])
  )


################################################################################

#para mapear ---- 

left_join(shp_mun, mun_risk) %>% 
  ggplot() + 
  geom_sf(aes(fill = risk_inf_fully_vax_no_test_big), lwd=0) + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "A")

left_join(shp_mun, mun_risk.omi) %>% 
  ggplot() + 
  geom_sf(aes(fill = risk_inf_fully_vax_no_test_big), lwd=0) + 
  theme_minimal() + 
  scale_fill_viridis_c("Riesgo", option = "A") 


################################################################################

# pruebas ----

pruebas_realizadas <- 
  remota %>% 
  filter(FECHA_SINTOMAS>=early_date) %>% 
  filter(CLASIFICACION_FINAL%in%c(3,7)) %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES) %>% 
  tally() %>% 
  collect()

pruebas_realizadas <- 
  pruebas_realizadas %>% 
  ungroup() %>% 
  mutate(ENTIDAD_RES= str_pad(ENTIDAD_RES, 2, "left", 0), 
         MUNICIPIO_RES= str_pad(MUNICIPIO_RES, 3, "left", 0), 
         n = as.numeric(n)
  ) %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  select(CVEGEO, cases = n)

pruebas_realizadas <- 
  pruebas_realizadas %>% 
  mutate(CVE_ENT = str_sub(CVEGEO, end = 2)) %>% 
  rename(pruebas = cases)

pruebas.full <- 
  left_join(pop.mun, pruebas_realizadas) %>% 
  mutate(pruebas_100k_hab = 100000*pruebas/Population) 

left_join(shp_mun, pruebas.full) %>% 
  ggplot() + 
  geom_sf(aes(fill = pruebas_100k_hab), lwd=0) + 
  theme_minimal() + 
  scale_fill_viridis_c("Pruebas 100k", option = "A")

left_join(shp_mun, pruebas.full) %>% 
  arrange(desc(pruebas_100k_hab))  

################################################################################

#contact risk write out
mun_risk %>% vroom_write(file = "results/riesgos_reuniones_delta_20211223.txt")
mun_risk.omi %>% vroom_write(file = "results/riesgos_reuniones_omicron_20211223.txt")
