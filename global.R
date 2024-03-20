#setwd("/srv/shiny-server/sihc")
#setwd("~/shiny_online")
library(DT)
library(shiny)
library(shinycssloaders)
library(rgdal)
library(scales)
library(tmap)
library(htmltools)
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(shinyBS)
library(bslib)
# ###################################
library(lme4)#pour la modelisation
library(lmerTest)
library(nlme)
library(blme)
library(grid)
library(gtable)
###############################
library(rstatix)
library(base)
library(ggpubr)
library(ggplus)
library(tidyverse)
library(tidyr)
library(stats)
library(car)
library(lubridate)
library(gridExtra)
library(cowplot)
library(MuMIn)
library(phia)
library(flextable)
library(multcomp)
library(readr)
library(merTools)
library(emmeans)
library(glmmTMB)
library(lsmeans)
library(effects)
library(MCMCglmm)
library(arm)
library(AICcmodavg)
library(sjstats)
library(blme)
library(nlme)
library(RColorBrewer)
#library(R2jags)
library(readxl)
library(lattice)
library(predictmeans)
library(predictionInterval)
library(visreg)
library(shinycssloaders)
library(pool)
require(RPostgreSQL)
library(RPostgres)
library(shinymanager)
library(DBI)
library(glue)
library(sf)
library(mapview)
library(rsq)
library(htmlwidgets)
library(slickR)

load_dot_env(file = "C:/Users/Herinomena/Desktop/.env")
Sys.getenv("wdir")

dbname = Sys.getenv("DB_NAME")
host = Sys.getenv("DB_HOST")
port = Sys.getenv("DB_PORT")
user = Sys.getenv("DB_USER")
password = Sys.getenv("DB_PWD")

my_custom_check_creds <- function(dbname, host, port, db_user, db_password) {
  # retourne une fonction de user et password
  function(user, password) {
    con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname,
                     host = host, port = port,
                     user = db_user, password = db_password)
    on.exit(dbDisconnect(con))
    req <- glue_sql("SELECT * FROM login_shiny WHERE \"user\" = ({user}) AND \"password\" = ({password})",
                    user = user, password = password, .con = con
    )
    req <- dbSendQuery(con, req)
    res <- dbFetch(req)
    # qui retourne une liste
    if (nrow(res) > 0) {
      list(result = TRUE, user_info = list(user = user, name = res$nom, email = res$email))
    } else {
      list(result = FALSE)
    }
  }
}
linebreaks <- function(n){HTML(strrep(br(), n))}
annee = 20
mois_abbr = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
filtre_mois <- c()
for(annee in c(20:25)){
  i<-1
  for(i in c(1:12)){
    filtre_mois <- append(filtre_mois,paste(mois_abbr[i],annee,sep = " "))
    i=i+1
  }
  annee=annee+1
}
#CHARGEMENT DES ELEMENTS DE LA CARTE SUR LA PAGE D'ACCUEIL
region = readOGR(dsn ="./region", layer = "Region_CORECRABE")
mada = readOGR(dsn ="./mada", layer = "Madagascar")
mangrove = readOGR(dsn ="./Mangrove_Mada", layer = "Mangroves_IEFN")
sous_zone = readOGR(dsn ="./sous_zone", layer = "Zone-production_CET-2022")
projRegion <- spTransform(region, CRS("+proj=longlat +datum=WGS84 +no_defs"))
projMada <- spTransform(mada, CRS("+proj=longlat +datum=WGS84 +no_defs"))
projMangrove <- spTransform(mangrove, CRS("+proj=longlat +datum=WGS84 +no_defs"))
projSous_zone <- spTransform(sous_zone, CRS("+proj=longlat +datum=WGS84 +no_defs"))

corecrabe <-read_delim("./corecrabe.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
corecrabe_avec_taille <-read_delim("./corecrabe_avec_taille.csv", 
                       delim = ",", escape_double = FALSE, trim_ws = TRUE)

  donnees_dist_sexe <-read_delim("./donnees_dist_sexe.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  donnees_ensenble_destination <-read_delim("./donnees_ensenble_destination.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  corecrabe_taux <-read_delim("./corecrabe_taux.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  recensement_complet <-read_delim("./recensement_complet.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  corecrabe_cpue <-read_delim("./corecrabe_cpue.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  Prix_corecrabe <-read_delim("./Prix_corecrabe.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  Donnee_taille_d <-read_delim("./Donnee_taille_d.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  Donnee_taille <-read_delim("./Donnee_taille.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  Donnees_dist_taille <-Donnee_taille

  infos_geo <-read_delim("./infos_geo.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  production_geo <-read_delim("./production_geo.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
  point_zone <-read_delim("./point_zone.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

  nb_enq_renafep <-read_delim("./nb_enq_renafep.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
  production_par_zone <-read_delim("./production_par_zone.csv", 
                              delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
  corecrabe_sans_taille <- corecrabe
  
  corecrabe_avec_taille=subset(corecrabe_avec_taille,corecrabe_avec_taille$nombre_premier_engin_sortie_de_peche_1>0)
  
  ######## Dater
  corecrabe_avec_taille$Mois<-format(as.Date(corecrabe_avec_taille$date_premiere_sortie_de_peche, format="%Y-%m-%d"),"%h %y") 
  corecrabe_avec_taille$coef_extr=corecrabe_avec_taille$poids_crabe_capture/corecrabe_avec_taille$poids_total_de_l_echantillon
  summary(corecrabe_avec_taille$coef_extr)
  ####colonne effectif mesure pour cet individu à mettre 1
  corecrabe_avec_taille$Eff_mesure = c(1)
  #Estimation des individus mésurés dans la capture suivant les échantillons
  corecrabe_avec_taille$Eff_estime = corecrabe_avec_taille$Eff_mesure*corecrabe_avec_taille$coef_extr
  # arrondir l'effectif estimé en nombre entier
  corecrabe_avec_taille$Eff_estime=round(corecrabe_avec_taille$Eff_estime,1)
  
  #####subdiviser les effectif estime
  corecrabe_avec_taille$Engin <- corecrabe_avec_taille$premier_engin_sortie_de_peche_1
  #######classer les taille suivant la taille-age selon la fonction de croissance
  corecrabe_avec_taille$classe_de_taille <-cut(corecrabe_avec_taille$taille_de_crabe,
                                               c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220))
  
  
