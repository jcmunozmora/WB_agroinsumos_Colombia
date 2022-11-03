#-------------------------------------------------------#
# Priorización ----
# Ultima fecha de modificacion: Nov 1, 2022
# Priorización por participación 
#-------------------------------------------------------#

#--------------------------#
# packages and paths ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf)
# .rs.restartR()

#--------------------------#
# A. Mapas (shp) ----
#--------------------------#

# Hay municipios con igual nombre, pegamos la abrevitatura del dpto para crear identificadores unicos
dptos <- sf::st_read(glue("07_Priorizacion/mapas/mapa_departamentos_colombia.shp"))
sf::st_geometry(dptos) <- NULL

dptos <- dptos %>% 
  dplyr::select(nivl_vl, nvl_lbl) %>%
  dplyr::rename(cod_dpto = nivl_vl, nom_dpto = nvl_lbl) %>%
  mutate(nom_dpto = substr(nom_dpto, 1, 3), nom_dpto = str_to_upper(nom_dpto))

# Shapefiles para mapa
map_dptos <- sf::st_read(glue("07_Priorizacion/mapas/mapa_departamentos_colombia.shp")) %>% 
  dplyr::select(nivl_vl, nvl_lbl, X, Y) %>%
  dplyr::rename(cod_dpto = nivl_vl, nom_dpto = nvl_lbl) %>%
  dplyr::filter(cod_dpto != 88)

dpto_label <- map_dptos
sf::st_geometry(dpto_label) <- NULL

map_mpio <- sf::st_read(glue("07_Priorizacion/mapas/mapa_municipios_colombia.shp")) %>% 
  dplyr::select(nivl_vl, nvl_lbl) %>%
  dplyr::rename(codmpio = nivl_vl, nom_mpio = nvl_lbl)

#--------------------------#
# Producción (EVA - 2021) ----
#--------------------------#
prod_eva <- haven::read_stata("07_Priorizacion/data/CULTIVOS_PRIORIZADOS_EVA_2021.dta") %>%
            dplyr::transmute(p_munic=cod_mpio,p_s6p46=cod,area_eva=area_sembrada)

#--------------------------#
# Componente Producto ----
#--------------------------#

#####----- Componente Productivo ----- #######
### Read XLSX
ds_raw <- readxl::read_xlsx("07_Priorizacion/data/PROP_MUN_1.xlsx",
                          sheet="Base de datos")

### Cultivos Priorizados
  ds_raw$prio[ds_raw$p_s6p46 == "112201001"] <- "Maíz"
  ds_raw$prio[ds_raw$p_s6p46 == "112201002"] <- "Maíz"
  ds_raw$prio[ds_raw$p_s6p46 == "113202001"] <- "Arroz"
  ds_raw$prio[ds_raw$p_s6p46 == "123401001"] <- "Tomate"
  ds_raw$prio[ds_raw$p_s6p46 == "125101001"] <- "Zanahoria"
  ds_raw$prio[ds_raw$p_s6p46 == "125301001"] <- "Cebolla"
  ds_raw$prio[ds_raw$p_s6p46 == "125401001"] <- "Cebolla"
  ds_raw$prio[ds_raw$p_s6p46 == "125402001"] <- "Cebolla"
  ds_raw$prio[ds_raw$p_s6p46 == "131201001"] <- "Banano"
  ds_raw$prio[ds_raw$p_s6p46 == "131301001"] <- "Platano"
  ds_raw$prio[ds_raw$p_s6p46 == "131601001"] <- "Mango"
  ds_raw$prio[ds_raw$p_s6p46 == "131801001"] <- "Piña"
  ds_raw$prio[ds_raw$p_s6p46 == "131910001"] <- "Tomate de árbol"
  ds_raw$prio[ds_raw$p_s6p46 == "132201001"] <- "Limón"
  ds_raw$prio[ds_raw$p_s6p46 == "132301002"] <- "Naranja"
  ds_raw$prio[ds_raw$p_s6p46 == "134401001"] <- "Fresas"
  ds_raw$prio[ds_raw$p_s6p46 == "151001001"] <- "Papas"
  ds_raw$prio[ds_raw$p_s6p46 == "151002002"] <- "Papas"
  ds_raw$prio[ds_raw$p_s6p46 == "159201001"] <- "Yuca"
  ds_raw$prio[ds_raw$p_s6p46 == "159301001"] <- "Ñame"
  ds_raw$prio[ds_raw$p_s6p46 == "180201002"] <- "Caña panelera"
  ds_raw$prio[ds_raw$p_s6p46 == "191999056"] <- "Frijol"
  ds_raw$prio[ds_raw$p_s6p46 == "196104075"] <- "Mora"
  ds_raw$prio[ds_raw$p_s6p46 == "134301001"] <- "Mora"
  ds_raw$prio[ds_raw$p_s6p46 == "134301004"] <- "Mora"
  

  #### Participación número de UPAS x Cultivo
  part_upa <- ds_raw[!is.na(ds_raw$prio),] %>% ungroup() %>%  
    dplyr::group_by(prio,tamano) %>%
    dplyr::summarise(n_upa=sum(n)) %>%
    tidyr::pivot_wider(names_from=tamano,
                       values_from = n_upa,
                       values_fill = 0) %>%
    dplyr::mutate(peq=`1`,peqmed=`1`+`2`,peqmedgd=`1`+`2`+`3`) %>%
    dplyr::select(prio,matches("peq")) %>% ungroup() %>%
    mutate_at(vars(matches("peq")),
              function(x) { (x)/(sum(x))}) %>%
    ### Finalize Data
    tidyr::pivot_longer(cols=!prio,
                        names_to="cat",
                        values_to = "participacion")  %>%
    
    group_by(cat) %>%
    ### 50 % de la producción total
    dplyr::mutate(nq=ntile(participacion,4)) %>% 
    dplyr::filter(nq %in% c(3,4))
    
  ### Participación de Cultivos
  ds_pro <- merge(ds_raw[!is.na(ds_raw$prio),],prod_eva) %>%
                        ### Delete crops without Eva
                        dplyr::filter(area_eva>0) %>%
                        dplyr::group_by(p_munic,prio,tamano) %>%
                        dplyr::summarise(n=sum(n),area_s=sum(area_sembrada),area_eva=mean(area_eva)) %>%
                        tidyr::pivot_wider(names_from=tamano,
                                           values_from = c(n,area_s),
                              values_fill = 0) %>%
                        ### Def. Value
                        dplyr::transmute(codmpio=p_munic,prio,
                                         ### Tamano 1
                                         area_eva=area_eva,
                                         peq_n=n_1,
                                         peq_area=area_s_1,
                                         ### Tamano 2
                                         peqmed_n=n_1+n_2,
                                         peqmed_area=area_s_1+area_s_2,
                                         ## Tamano 3
                                         peqmedgd_n=n_1+n_2+n_3,
                                         peqmedgd_area=area_s_1+area_s_2+area_s_3) %>%
                        ### Percentage
                        ungroup() 
  
  ### Data Production
  data_prod0 <- ds_pro %>% dplyr::select(codmpio,area_eva,prio,matches("_n")) %>%
                        tidyr::pivot_longer(cols=c(matches("_n")),
                                              names_to="cat",
                                              values_to = "n_upa") %>%
                        dplyr::mutate(cat=gsub("_n","",cat))
  
  
  data_prod1 <- ds_pro %>% dplyr::select(codmpio,prio,matches("_area")) %>%
    tidyr::pivot_longer(cols=c(matches("_area")),
                        names_to="cat",
                        values_to = "area") %>%
    dplyr::mutate(cat=gsub("_area","",cat))
  
  data_prod <- merge(data_prod0,data_prod1)
  
  rm(data_prod0,data_prod1)
  
  data_prod <- merge(data_prod,part_upa) %>% dplyr::select(-participacion,-nq)
  
  ### 00 - Municipality data ------
  ds_pro <- ds_pro %>%  dplyr::mutate_at(vars(matches("_n")),
                                         function(x) { (x)/(sum(x))})
  d1 <- ds_pro  %>%
    ### Estandarizamos
    dplyr::select(codmpio,prio,matches("_n")) %>%
                        tidyr::pivot_longer(cols=matches("_n"),
                                            names_to="cat",
                                            values_to = "estd_upa") %>%
                        dplyr::mutate(cat=gsub("_n","",cat))
  
  d2 <- ds_pro %>% dplyr::select(codmpio,prio,matches("_area")) %>%
    tidyr::pivot_longer(cols=matches("_area"),
                        names_to="cat",
                        values_to = "area") %>%
    dplyr::mutate(cat=gsub("_area","",cat))
  
  ds_pro <- merge(d1,d2)
  ds_pro <- merge(ds_pro,part_upa) %>% dplyr::mutate(upa_stad=estd_upa*participacion) %>%
                    ### Datos a nivel municipal
                    dplyr::group_by(codmpio,cat) %>%
                    dplyr::summarise(ind_prod=sum(upa_stad),area_per=sum(area)) %>%
                    dplyr::mutate(area_per=area_per/sum(area_per)) 
  
  ## Borramos las df no necesarios
  rm(d1,d2,part_upa,ds_raw)
  
#####----- Componente Vulnerabilidad ----- #######
  
  ds_raw_v <- readxl::read_xlsx("07_Priorizacion/data/PROP_MUN_1.xlsx",
                              sheet="Ind. vulnerabilidad",range = "A4:G1126")
  colnames(ds_raw_v) <- c("codmpio","pov_ind","pov_est","autob_ind","autob_est",
                           "fert_ind","fert_est")
  ds_raw_v <- ds_raw_v %>% select(codmpio,pov_est,autob_est,fert_est)
  
  #### Weights
  wght2 <- c(0.5,0.5)
  wght3 <- c(0.3,0.3,0.3)

  ### Estandarización -- De los Indece
  idx_v <- ds_raw_v %>% dplyr::transmute(codmpio,
                            # ind1 --> Poverty -- Fertilizantes           
                            ind1=wght2[1]*pov_est+wght2[2]*fert_est,
                            # ind2 --> Pobreza -- Auto-abastecimiento -- Fertilizantes
                            ind2=wght3[1]*pov_est+wght3[2]*autob_est+wght3[3]*fert_est,
                            # ind3 --> Pobreza -- Auto-abastecimiento
                            ind3=wght2[1]*pov_est+wght2[2]*autob_est,
                            # ind4 --> Autoabastecimeinto -- Fertilizantes 
                            ind4=wght2[1]*autob_est+wght2[2]*fert_est) %>%
                            #### -- Estandarizamos los índices --- ####
                            mutate_at(vars(matches("id")),
                                function(x) { (x - min(x))/(max(x)-min(x))})
  rm(ds_raw_v,wght2,wght3)          
                            
#####----- Ds - Final Priorización ----- #######
  ### names dptos
  dptos_nm <- map_dptos %>% dplyr::select(cod_dpto,nom_dpto)
    
  wgt <- c(0.5,0.5)
  priorizacion <- merge(ds_pro,idx_v) %>%
          dplyr::mutate(prior1=wgt[1]*ind_prod+wgt[1]*ind1,
                        prior2=wgt[1]*ind_prod+wgt[1]*ind2,
                        prior3=wgt[1]*ind_prod+wgt[1]*ind3,
                        prior4=wgt[1]*ind_prod+wgt[1]*ind4) %>%
          #### -- Estandarizamos los índices --- ####
          dplyr::group_by(cat) %>%
           mutate_at(vars(matches("prior")),
                function(x) { (x - min(x))/(max(x)-min(x))}) %>%
          ### depto
    dplyr::mutate(cod_dpto = as.character(codmpio), 
           cod_dpto = ifelse(nchar(cod_dpto) == 4, glue("0{cod_dpto}"), cod_dpto),
           cod_dpto = substr(cod_dpto, 1, 2), cod_dpto = as.numeric(cod_dpto)) %>%
          left_join(dptos_nm,by="cod_dpto") %>%
          left_join(map_mpio,by="codmpio") %>%
          relocate(cod_dpto,nom_dpto,codmpio,nom_mpio)
          
  #### putnames
  #Cultivos y Regiones
  c_f <- "peq"
  
  main <- priorizacion %>% dplyr::filter(cat==c_f) %>% ungroup() %>%
    dplyr::transmute(order=prior4,cod_dpto,nom_dpto,codmpio,nom_mpio) %>% 
    dplyr::arrange(-order) %>%
    dplyr::mutate(nq=ntile(order,4)) %>%
    dplyr::filter(nq==4) %>%
    dplyr::select(-nq)
  
  prod <- data_prod %>% dplyr::filter(cat==c_f) %>% select(-cat)
  
  ### Get data
  ds_end <- merge(prod,main,by=c("codmpio")) 
  
  crp <- ds_end %>%
            group_by(cod_dpto,prio) %>%
            dplyr::summarise(area_eva=sum(area_eva)) %>% ungroup() %>%
            ### data %>% 
            group_by(cod_dpto) %>% 
            dplyr::mutate(crop_are=paste0(prio," (",area_eva," ha)")) %>%
            mutate(crops = paste0(crop_are, collapse = " , "))
  
  crp <- crp[!duplicated(crp$cod_dpto),] %>%
            dplyr::select(cod_dpto,crops)
  
  ds_fin <- ds_end %>% dplyr::group_by(cod_dpto,nom_dpto) %>%
                       dplyr::summarise(n_upa=sum(n_upa),
                                        area=sum(area_eva),order=mean(order)) %>%
                      ungroup() %>%
                      dplyr::transmute(order,rk=22-rank(order,ties.method= "max"),
                                       cod_dpto,nom_dpto,
                                       n_upa=n_upa,
                                       sh_upa=n_upa/sum(n_upa),
                                       sh_are=area/sum(area))
  
  ds_fin_mpio <- ds_end[!duplicated(ds_end$codmpio),] %>%
                      dplyr::group_by(cod_dpto) %>%
                      dplyr::summarize(n_mpio=n())
  
  ds_fin <- merge(ds_fin,ds_fin_mpio)
  ds_fin <- merge(ds_fin,crp,by=c("cod_dpto")) %>% arrange()
  
  
  writexl::write_xlsx(ds_fin,"07_Priorizacion/WB_priorizacionNov2.xlsx")
  


  
  
  
  
  