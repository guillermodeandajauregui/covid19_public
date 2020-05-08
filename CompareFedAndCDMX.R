################################################################################
#libraries
################################################################################
library(tidyverse)
library(vroom)

###############################################################################
#read data
###############################################################################

#federal 
el_path <- "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

temp <- tempfile()
download.file(url = el_path, destfile = temp)
el_file <- unzip(zipfile = temp, list = T)
el_file <- unz(description = temp, filename = el_file[1])
fedr_data <- vroom::vroom(el_file)
unlink(temp)
rm(temp, el_file)

#cdmx 
## descargada de https://datos.cdmx.gob.mx/explore/dataset/base-covid-sinave/export/

cdmx_data <- vroom::vroom("data/open_cdmx/base-covid-sinave_20200508.csv")


################################################################################
#tally deaths
################################################################################

#### cdmx, by federal 
death_fedr <- 
  fedr_data %>% 
  filter(ENTIDAD_UM == "09") %>% 
    filter(RESULTADO==1) %>% 
  filter(!is.na(FECHA_DEF)) %>%
  group_by(FECHA_DEF) %>% 
  tally() %>% 
  mutate(running_sum = cumsum(n))  




#### cdmx, by cdmx
death_cdmx <- 
  cdmx_data %>% 
    filter(resultado_definitivo=="SARS-CoV-2") %>% 
  filter(!is.na(fecha_defuncion)) %>% 
  group_by(fecha_defuncion) %>% 
  tally() %>% 
  mutate(running_sum = cumsum(n)) 


################################################################################
#join and plot, side by side
################################################################################

full_join(x = death_fedr, 
          y = death_cdmx, 
          by = c("FECHA_DEF" = "fecha_defuncion"), 
          suffix = c(".fedr", ".cdmx")
          ) %>% 
  select(FECHA_DEF, running_sum.fedr, running_sum.cdmx) %>% 
  rename(conteo_federal = running_sum.fedr, 
         conteo_local   = running_sum.cdmx) %>% 
  pivot_longer(cols = -FECHA_DEF, 
               names_to = "registro") %>% 
  ggplot(mapping = aes(x = FECHA_DEF, y = value, colour = as.factor(registro))) + 
  geom_point() + 
  scale_colour_manual(values = c("black", "red")) +
  theme_minimal() + 
  theme(legend.title=element_blank()) + 
  xlab("fecha defunción") + 
  ylab("suma acumulada") + 
  ggtitle(label = "Muertes con resultado SARS-CoV-2 positivo, Federal y CDMX",
          subtitle = paste0(Sys.Date()))
