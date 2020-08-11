###############################################################################
#Monitor covid 
#Guillermo de Anda-Jáuregui
#gdeanda at inmegen dot edu dot mx 
###############################################################################

###############################################################################
#libraries ----
###############################################################################

library(tidyverse)
library(vroom)
library(lubridate)

###############################################################################
#download data ----
###############################################################################

el_path <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
#el_path <- "http://epidemiologia.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
#el_path <- "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

temp <- tempfile()
download.file(url = el_path, destfile = temp)
el_file <- unzip(zipfile = temp, list = T)
el_file <- unz(description = temp, filename = el_file[1])
el_data <- vroom::vroom(el_file)
unlink(temp)
rm(temp, el_file)

claves_estados    <- vroom::vroom("data/claves_estados.txt")
claves_municipios <- vroom::vroom("data/claves_municipios.txt")

###############################################################################
#Funcion plots monitoras ----
###############################################################################

monitor_covid <-function(clave_entidad, claves_de_municipos = NULL){
  
  #get names 
  mi_estado = as.character(clave_entidad)
  
  nombre_estado <- 
    claves_estados %>% 
    filter(CLAVE_ENTIDAD == as.character(mi_estado)) %>% 
    pull(ENTIDAD_FEDERATIVA)
  
  
  if(!is.null(claves_de_municipos)){
    mis_municipios = claves_de_municipos
    
    nombres_municipios <- 
      claves_municipios %>% 
      filter(CLAVE_ENTIDAD==as.character(mi_estado)) %>% 
      filter(CLAVE_MUNICIPIO%in%mis_municipios) %>% 
      pull(MUNICIPIO)
    
    
    if(length(nombres_municipios)==0){
      stop("municipios no válidos")
     
    }
    
    string_municipios <- paste(nombres_municipios, sep = ";", collapse = ";")
    #filter data 
    
    mis_datos <- 
      el_data %>% 
      filter(ENTIDAD_RES == mi_estado) %>% 
      filter(MUNICIPIO_RES%in%mis_municipios) %>%
      filter(RESULTADO%in%c(1,3)) 
    
  }else{
    
    mis_datos <- 
      el_data %>% 
      filter(ENTIDAD_RES == mi_estado) %>% 
      filter(RESULTADO%in%c(1,3)) 
    
    string_municipios <- "todos los municipios"
  }
  
  #casos del 23 de marzo
  
  casos_mar23 <-   
    mis_datos %>% 
      group_by(FECHA_SINTOMAS) %>% 
      tally() %>% 
      filter(FECHA_SINTOMAS == "2020-03-23") %>% 
      pull(n)
  
  #max de casos nuevos en JNSD
  max_casos_JNSD <-   
    mis_datos %>% 
    group_by(FECHA_SINTOMAS) %>% 
    tally() %>% 
    filter(FECHA_SINTOMAS > "2020-03-23" & FECHA_SINTOMAS < "2020-06-01") %>% 
    pull(n) %>% 
    max()
  
  #min de casos nuevos en JNSD
  min_casos_JNSD <-   
    mis_datos %>% 
    group_by(FECHA_SINTOMAS) %>% 
    tally() %>% 
    filter(FECHA_SINTOMAS > "2020-03-23" & FECHA_SINTOMAS < "2020-06-01") %>% 
    pull(n) %>% 
    min()
  
  #mean de casos nuevos en JNSD
  mean_casos_JNSD <-   
    mis_datos %>% 
    group_by(FECHA_SINTOMAS) %>% 
    tally() %>% 
    filter(FECHA_SINTOMAS > "2020-03-23" & FECHA_SINTOMAS < "2020-06-01") %>% 
    pull(n) %>% 
    mean()
  
  ##poner 0 si no hay casos de ese día
  casos_mar23 <- ifelse(length(casos_mar23)==0, 0, casos_mar23)
  
  
  #graficar ----
  
  #casos nuevos ---- 
  casos_nuevos <- 
  mis_datos %>%   
    group_by(FECHA_SINTOMAS) %>% 
    tally() %>% 
    #remove data from two days ago; very immature
    filter(FECHA_SINTOMAS < max(FECHA_SINTOMAS) -2 ) %>% 
    ggplot(mapping = aes(FECHA_SINTOMAS, n)) +
    geom_line() + 
    geom_smooth() + 
    theme_minimal() + 
    #mark Susana Distancia
    geom_vline(xintercept = as.Date("2020-03-23"), linetype = 2, color = "purple") + 
    geom_text(aes(x=as.Date("2020-03-23"), 
                  label="Susana Distancia", 
                  y=(1+casos_mar23)*10), 
              colour="purple", 
              angle=90, 
              text=element_text(size=11)) +
    #lines for mean and min in JNSD
    geom_hline(yintercept = mean_casos_JNSD, linetype = 2, color = "darkred") + 
    geom_hline(yintercept = min_casos_JNSD, linetype = 2, color = "darkgreen") + 
    #mark Nueva Normalidad
    geom_vline(xintercept = as.Date("2020-06-01"), linetype = 2, color = "Orange") + 
    geom_text(aes(x=as.Date("2020-06-01"), 
                  label="Nueva Normalidad", 
                  y=(1+casos_mar23)*10), 
              colour="orange", 
              angle=90, 
              text=element_text(size=11)) +
    #mark area of data delay
    annotate("rect", xmin = (Sys.Date() - 15), 
             xmax = Sys.Date(), 
             ymin = 0, 
             ymax = Inf,
             alpha = .5) + 
    geom_text(aes(x = (Sys.Date()-15), 
                  y = casos_mar23, 
                  label = "Zona de datos\n no maduros",
                  #angle = 90
    )
    ) + 
    ylim(0, NA) +
    ylab("Casos Nuevos") + 
    xlab("Fecha de inicio de síntomas") +
    ggtitle(label = nombre_estado, 
            subtitle = string_municipios)
  
  
    #casos activos ----  
  
  #sacar los días de mis datos 
  my_days <- mis_datos %>% pull(FECHA_SINTOMAS) %>% unique() %>% sort() 
  names(my_days) <- my_days
  
  #sacar conteo de activos por día
  datos_activos <-
    lapply(my_days, function(jour){
      
      mis_datos %>% 
        filter(FECHA_SINTOMAS <= jour) %>% 
        mutate(status = case_when(!is.na(FECHA_DEF) ~ "R",
                                  FECHA_SINTOMAS < (as.Date(jour)-14) ~ "R",
                                  FECHA_SINTOMAS >= (as.Date(jour) -14) ~ "I"
        )
        ) %>% 
        group_by(status) %>% 
        tally
    }) %>% 
    bind_rows(.id = "fecha") %>% 
    mutate(fecha = ymd(fecha)) %>% 
    filter(status == "I") 
  
  #contar activos de marzo 23; si no hay, poner cero
  activos_mar23 <-
    datos_activos %>% 
    filter(fecha == "2020-03-23") %>% 
    pull(n)
  
  activos_mar23 <- ifelse(length(activos_mar23)==0, 0, activos_mar23)
  
  #Maximo de la JNSD
  max_activos_jnsd <-
    datos_activos %>% 
    filter(fecha > "2020-03-23" & fecha < "2020-06-01") %>% 
    pull(n) %>% 
    max()
  
  #Minimo de la JNSD
  min_activos_jnsd <-
    datos_activos %>% 
    filter(fecha > "2020-03-23" & fecha < "2020-06-01") %>% 
    pull(n) %>% 
    min()
  
  #Mean de la JNSD
  mean_activos_jnsd <-
    datos_activos %>% 
    filter(fecha > "2020-03-23" & fecha < "2020-06-01") %>% 
    pull(n) %>% 
    mean()
  
  casos_activos <- 
  datos_activos %>% 
    ggplot(mapping = aes(x = fecha, y = n)) + 
    geom_line(colour = "black") +
    geom_point(colour = "black") +
    theme_minimal() + 
    #mark Susana Distancia
    geom_vline(xintercept = as.Date("2020-03-23"), linetype = 2, color = "purple") + 
    geom_text(aes(x=as.Date("2020-03-23"), 
                  label="Susana Distancia", 
                  #y=casos_mar23*10),
                  y=max(n)/2), 
              
              colour="purple", 
              angle=90, 
              text=element_text(size=11)) +
    #mark Nueva Normalidad
    geom_vline(xintercept = as.Date("2020-06-01"), linetype = 2, color = "Orange") + 
    geom_text(aes(x=as.Date("2020-06-01"), 
                  label="Nueva Normalidad", 
                  #y=casos_mar23*10), 
                  y=max(n)/2), 
              colour="orange", 
              angle=90, 
              text=element_text(size=11)) +
    #mark area of data delay
    annotate("rect", xmin = (Sys.Date() - 15), 
             xmax = Sys.Date(), 
             ymin = 0, 
             ymax = Inf,
             alpha = .5) + 
    geom_text(aes(x = (Sys.Date()-15), 
                  y = 10+activos_mar23*10, 
                  label = "Zona de datos\n no maduros",
                  #angle = 90
    )
    ) + 
    #mark target value 
    geom_hline(yintercept = max_activos_jnsd, color = "darkred", linetype = 2, size = 0.5) +
    geom_hline(yintercept = mean_activos_jnsd, color = "darkblue", linetype = 2, size = 0.5) + 
    geom_hline(yintercept = min_activos_jnsd, color = "darkgreen", linetype = 2, size = 0.5) +
    ylim(0, NA) +
    ylab("Casos Activos") + 
    xlab("Fecha") +
    ggtitle(label = nombre_estado, 
            subtitle = string_municipios)
  
  
 
  
  
  #return
  return(
  list(casos_nuevos = casos_nuevos,
       casos_activos = casos_activos)
  )
}


###############################################################################
#Ejemplo ----
###############################################################################

#prueba_monitor <- monitor_covid(clave_entidad = "09", claves_de_municipos = "008")
#prueba_monitor$casos_nuevos
#prueba_monitor$casos_activos
