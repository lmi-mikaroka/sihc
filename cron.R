###################################################### EMPLACEMENT ############################################
#setwd("/srv/shiny-server/sihc")
#setwd("~/cron")
#setwd("~/shiny_online")
##################################################### <- Fin Emplacement -> ###################################
library(dotenv)
load_dot_env(file = "C:/Users/Herinomena/Desktop/.env")
Sys.getenv("wdir")

#################################### CHARGEMENT DES LIBRAIRIES ###############################################
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
library(lme4)
library(lmerTest)
library(nlme)
library(blme)
library(grid)
#library(gtable)
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
################################### <- FIN DE CHARGEMENT DE LIBRAIRIES -> ############################


#################################### CONNEXION A LA BASE DE DONNEE ####################################

pool <- dbPool(RPostgres::Postgres(), #max.con = 100,
               dbname = Sys.getenv("DB_NAME"),
               host = Sys.getenv("DB_HOST"),
               port = Sys.getenv("DB_PORT"),
               user = Sys.getenv("DB_USER"),
               password = Sys.getenv("DB_PWD"))


################################# <- FIN DE LA CONNEXION A LA BASE DE DONNEE -> #######################

# initialisation, pour l'exemple
#con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname , host = host, port = port ,
#                 user = user, password = password )
#on.exit(dbDisconnect(con))



######################### PRODUCTION PAR ZONE 2021 ############################
prod_carte_zone <-read_delim("./Tab_cumule_result_zone1.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE)
######################## FIN PRODUCTION PAR ZONE 2021 #########################

######################### PRODUCTION  2021 ############################
prod_carte <-read_delim("./production_annuelle_village.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)
######################## FIN PRODUCTION PAR ZONE 2021 #########################

######################## REQUETE DONNEE SANS TAILLE ############################
corecrabe_sans_taille<-dbGetQuery(pool,"SELECT * FROM corecrabe_sans_taille")
######################## FIN REQUETE SANS TAILLE ###############################

######################## REQUETE DONNEE AVEC TAILLE ############################
corecrabe_avec_taille<-dbGetQuery(pool,"SELECT * FROM corecrabe_avec_taille")
######################## FIN REQUETE AVEC TAILLE ###############################



###################### REQUETE NB ENQUETEUR RENAFEP ###################
nb_enq_renafep<-dbGetQuery(pool,"SELECT * FROM nb_enq_renafep")
##Export nb_enq_renafep
write.csv(nb_enq_renafep, "nb_enq_renafep.csv", row.names=FALSE)

##################### FIN REQUETE RENAFEP #############################


##################### REQUETE RECENSEMENT ############################
recensement_complet <-dbGetQuery(pool,"SELECT * FROM recensement_avec_coeff")
##################### FIN REQUETE RECENSEMENT ########################


################### REQUETE DONNEE DIST. TAILLE #############################################################################################################################################################################################################################################################################################################################################################################################################
Donnees_dist_taille <- dbGetQuery(pool, "select fiche_enqueteur.id,crabe.destination,crabe.sexe,crabe.taille,echantillon.poids,fiche_enqueteur.date,fiche_enqueteur.capture_poids,round(coalesce(fiche_enqueteur.capture_poids/nullif(echantillon.poids,0),0)) as coefficient_extrapolation, fiche_enqueteur.nombre_sortie_capture,engin.nom as engin, coordonnees.village as Village ,coordonnees.zone as Zone, coordonnees.sous_zone as Sous_zone from sortie_de_peche_enqueteur
                                        join engin_sortie_de_peche_enqueteur on engin_sortie_de_peche_enqueteur.sortie_de_peche_enqueteur=sortie_de_peche_enqueteur.id
                                        join engin on engin.id= engin_sortie_de_peche_enqueteur.engin
                                        join fiche_enqueteur on fiche_enqueteur.id=sortie_de_peche_enqueteur.fiche_enqueteur
                                    join echantillon on echantillon.fiche_enqueteur=fiche_enqueteur.id
                                        join crabe on crabe.echantillon=echantillon.id
                                        join pecheur on fiche_enqueteur.pecheur=pecheur.id
                                    join coordonnees on pecheur.village=coordonnees.idvillage
                                        where engin.id!=1")

################## REQUETE INFOS GEO ET COORDONNEES ##########################################
infos_geo <- dbGetQuery(pool,"SELECT * FROM coordonnees")
##Export infos_geo
write.csv(infos_geo, "infos_geo.csv", row.names=FALSE)
point_zone <- dbGetQuery(pool,"SELECT * FROM zone")
##Export infos_geo
write.csv(point_zone, "point_zone.csv", row.names=FALSE)
################## FIN REQUETE COORDONNEES ###################################################
# dbDisconnect(pool)
################# REQUETE RECENSEMENT AVEC COEFF #############################################
#recensement_avec_coefficient <- dbGetQuery(pool,"SELECT * FROM recensement_avec_coeff")
################ FIN REQUETE RECENSEMENT AVEC COEFF ##########################################


################################## TRAITEMENT DONNEES ###################################                             

##### TRAITEMENT RECENSEMENT COMPLET #################################################

#convertir en numérique les coefficient considérer pour avoir N/A sur les NULL
recensement_complet$coeff_considere <- as.numeric(recensement_complet$coeff_considere)
recensement_complet=recensement_complet%>%drop_na(coeff_considere)
#Régulariser à 0.8 les coefficients de Mahajanga et Ambanja pour le mois d'octobre pour cause de données incorrectes
row <- nrow(recensement_complet)
i<-1
for (i in c(1:row)) {
  if((recensement_complet$zone[i]=='Mahajanga' || recensement_complet$zone[i] == 'Ambanja') & recensement_complet$mois[i]==10 & recensement_complet$coeff_considere[i]<0.8 ){
    recensement_complet$coeff_considere[i]<-0.8
  }
}
#Formater les mois pour avoir les mêmes formats que le reste des données utilisées dans ce système
recensement_complet$Mois <- format(as.Date(paste(paste(recensement_complet$annee,recensement_complet$mois,sep = "-"),"01",sep = "-")),"%h %y")
#Enlever les mois de novembre (mois de fermeture)
recensement_complet <- recensement_complet%>%filter(mois!=11)

##Export recensement_complet
write.csv(recensement_complet, "recensement_complet.csv", row.names=FALSE)

##### FIN TRAITEMENT RECENSEMENT COMPLET ###############################################

###### TRAITEMENT CORECRABE SANS TAILLE ###############################################

#Utiliser un nouveau dataframe pour conserver les données brutes
corecrabe <- corecrabe_sans_taille
#Formater les mois pour avoir les mêmes formats que le reste des données utilisées dans ce système
corecrabe$Mois<-format(as.Date(corecrabe$date_premiere_sortie_de_peche,  format="%d/%m/%Y"),"%h %y")
#Dégager les lignes avec nombre_premier_engin inférieur à zéro
corecrabe=subset(corecrabe, corecrabe$nombre_premier_engin_sortie_de_peche_1>0)
#Formater les dates pour avoir un format de date standard
corecrabe$date_standard <- as.Date(format(as.Date(paste("01 ",corecrabe$Mois),  format="%d %h %y"),"%d/%m/%Y"),format="%d/%m/%Y")
#Arranger le dataframe du plus ancien au plus récent
permutation <- order(corecrabe$date_standard)
head(corecrabe[permutation,])

##Export corecrabe
write.csv(corecrabe, "corecrabe.csv", row.names=FALSE)

##### FIN TRAITEMENT CORECRABE SANS TAILLE ############################################

#### TRAITEMENT CORECRABE POUR LES CALCULS DE CPUE ####################################

#utiliser un nouveau dataframe
corecrabe_cpue <- corecrabe
#ajouter les champs utilisés pour calculer le cpue
corecrabe_cpue$cpue_sortie <- (corecrabe_cpue$poids_crabe_capture/corecrabe_cpue$nombre_de_sortie)
corecrabe_cpue$cpue_engin <- (corecrabe_cpue$poids_crabe_capture/corecrabe_cpue$nombre_de_sortie/corecrabe_cpue$nombre_premier_engin_sortie_de_peche_1)
#Dégager maintenant les sortie et engin null
corecrabe_cpue=subset(corecrabe_cpue, corecrabe_cpue$cpue_sortie>0)
corecrabe_cpue=subset(corecrabe_cpue, corecrabe_cpue$cpue_engin>0)
#transformation en logha
corecrabe_cpue$logCPUE_sortie <- log(corecrabe_cpue$cpue_sortie)
corecrabe_cpue$logCPUE <- log(corecrabe_cpue$cpue_engin)
corecrabe_cpue$Engin <- corecrabe_cpue$premier_engin_sortie_de_peche_1

##Export corecrabe_cpue
write.csv(corecrabe_cpue, "corecrabe_cpue.csv", row.names=FALSE)

#### FIN TRAITEMENT CORECRABE POUR LES CALCULS DE CPUE ################################

#### TRAITEMENT CORECRABE POUR LE TAUX D'ACTIVITE #####################################

#Stocker dans des nouveaux variables les données utilisées
id=corecrabe$id_enquete
Zone=corecrabe$zone
Sous_zone=corecrabe$sous_zone
Region=corecrabe$region
District=corecrabe$district
Commune=corecrabe$commune
Fokontany=corecrabe$fokontany
Village=corecrabe$village
Date_1=corecrabe$date_premiere_sortie_de_peche
Date_2=corecrabe$date_deuxieme_sortie_de_peche
Date_3=corecrabe$date_troisieme_sortie_de_peche
Date_4=corecrabe$date_quatrieme_sortie_de_peche
Activite_1=corecrabe$nombre_sortie_de_peche_1
Activite_2=corecrabe$nombre_sortie_de_peche_2
Activite_3=corecrabe$nombre_sortie_de_peche_3
Activite_4=corecrabe$nombre_sortie_de_peche_4
Engin_1=corecrabe$premier_engin_sortie_de_peche_1
Engin_2=corecrabe$premier_engin_sortie_de_peche_2
Engin_3=corecrabe$premier_engin_sortie_de_peche_3
Engin_4=corecrabe$premier_engin_sortie_de_peche_4
Nombre.engin_1=corecrabe$nombre_premier_engin_sortie_de_peche_1
Nombre.engin_2=corecrabe$nombre_premier_engin_sortie_de_peche_2
Nombre.engin_3=corecrabe$nombre_premier_engin_sortie_de_peche_3
Nombre.engin_4=corecrabe$nombre_premier_engin_sortie_de_peche_4
#Construire un nouveau dataframe avec les variables créés
corecrabe_taux=data.frame(Zone,Sous_zone,Region,District,Commune,Fokontany,Village,id,
                          Date_1,Activite_1,Engin_1,Nombre.engin_1,
                          Date_2,Activite_2,Engin_2,Nombre.engin_2,
                          Date_3, Activite_3,Engin_3,Nombre.engin_3,
                          Date_4,Activite_4,Engin_4,Nombre.engin_4)

##Export corecrabe_taux
write.csv(corecrabe_taux, "corecrabe_taux.csv", row.names=FALSE)

####### FIN TRAITEMENT CORECRABE POUR LE TAUX D'ACTIVITE ####################################



###### TRAITEMENT DE DONNEES POUR LA DISTRIBUTION DE TAILLE #################################

#Paramètres d'affichage
options(digits = 3)
set.seed(1234)
theme_set(theme_minimal())
#Créer un nouveau variable à partir du précédent en enlevant les coefficient égale à zéro
donnees_dist_taille_coeff0=subset(Donnees_dist_taille, Donnees_dist_taille$coefficient_extrapolation==0,)
#Isoler les données par engin
donnees_dist_taille_angady=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Angady",)
donnees_dist_taille_nasse=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Nasse_(Treko)",)
donnees_dist_taille_crochet=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Crochet_(Fingavitra,_Kavitry,_Fiavitry)",)
donnees_dist_taille_raquette=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Raquette_(Kipaoky,_Fisahoky,_Fisaoka)",)
donnees_dist_taille_balance=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Balance_(Garigary,_Kiriry)",)
donnees_dist_taille_canne=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Ligne_a_la_canne_(Vintana,_Vinta,Kibitsoka)",)
donnees_dist_taille_hazo=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Crochet_Hazo_(Fingavitra,_Kavitry,_Fiavitry)",)
donnees_dist_taille_Vy=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Crochet_Vy_(Fingavitra,_Kavitry,_Fiavitry)",)
donnees_dist_taille_bycatch=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Bycatch(harato)",)
donnees_dist_taille_Moillage=subset(Donnees_dist_taille, Donnees_dist_taille$engin=="Ligne_au_mouillage_(Topitopy)",)
#grouper les données de l'engins angady, crochet, hazo et vy
Donnee_ensemble_dist_taille_crochet<-rbind(donnees_dist_taille_angady,donnees_dist_taille_crochet,donnees_dist_taille_hazo,donnees_dist_taille_Vy)
#grouper les données des engins canne et mouillage
Donnee_ensemble_dist_taille_ligne<-rbind(donnees_dist_taille_canne,donnees_dist_taille_Moillage)
#changer les engins d'engin qui commence par Ang en crochet
Donnee_ensemble_dist_taille_crochet$engin <- sub("^Ang.*", "Crochet", Donnee_ensemble_dist_taille_crochet$engin)
#changer les nom d'engin qui commence par crochet en crochet
Donnee_ensemble_dist_taille_crochet$engin <- sub("^Crochet.*", "Crochet", Donnee_ensemble_dist_taille_crochet$engin)
#changer les engins d'engin qui commence par ligne en ligne, nasse, raquette,balance,bycatch
Donnee_ensemble_dist_taille_ligne$engin<- sub("^Ligne.*", "Ligne", Donnee_ensemble_dist_taille_ligne$engin)
donnees_dist_taille_nasse$engin <- sub("^Nasse.*", "Nasse", donnees_dist_taille_nasse$engin)
donnees_dist_taille_raquette$engin <- sub("^Raquette_.*", "Raquette", donnees_dist_taille_raquette$engin)
donnees_dist_taille_balance$engin <- sub("^Balance_.*", "Balance", donnees_dist_taille_balance$engin)
donnees_dist_taille_bycatch$engin <- sub("^Bycatch.*", "Bycatch", donnees_dist_taille_bycatch$engin)
#Re-associer les données en un seul dataframe
Donnee_ensemble_dist_taille_engin<-rbind(Donnee_ensemble_dist_taille_crochet,Donnee_ensemble_dist_taille_ligne,donnees_dist_taille_nasse,donnees_dist_taille_raquette,donnees_dist_taille_balance,donnees_dist_taille_bycatch)
#Dissocier le dataframe par destination
donnees_dist_destination_1=subset(Donnee_ensemble_dist_taille_engin, Donnee_ensemble_dist_taille_engin$destination==1,)
donnees_dist_destination_2=subset(Donnee_ensemble_dist_taille_engin, Donnee_ensemble_dist_taille_engin$destination==2,)
donnees_dist_destination_3=subset(Donnee_ensemble_dist_taille_engin, Donnee_ensemble_dist_taille_engin$destination==3,)
#renommer les destination 1.2.3
donnees_dist_destination_1$destination <- sub("^1.*", "Collecte", donnees_dist_destination_1$destination)
donnees_dist_destination_2$destination <- sub("^2.*", "Marche local", donnees_dist_destination_2$destination)
donnees_dist_destination_3$destination <- sub("^3.*", "Autoconsommation", donnees_dist_destination_3$destination)
#Re-associer les données
donnees_ensenble_destination <- rbind(donnees_dist_destination_1,donnees_dist_destination_2,donnees_dist_destination_3)

##Export donnees_ensenble_destination
write.csv(donnees_ensenble_destination, "donnees_ensenble_destination.csv", row.names=FALSE)
#subdivision selon le sexe
donnees_dist_sexe_FO=subset(donnees_ensenble_destination, donnees_ensenble_destination$sexe=="FO",)
donnees_dist_sexe_NO=subset(donnees_ensenble_destination, donnees_ensenble_destination$sexe=="NO",)
donnees_dist_sexe_M=subset(donnees_ensenble_destination, donnees_ensenble_destination$sexe=="M",)
#renomer les FO et Les NO en F
donnees_dist_sexe_FO$sexe <- sub("^FO.*", "Femelle", donnees_dist_sexe_FO$sexe)
donnees_dist_sexe_NO$sexe <- sub("^NO.*", "Femelle", donnees_dist_sexe_NO$sexe)
donnees_dist_sexe_M$sexe <- sub("^M.*", "Male", donnees_dist_sexe_M$sexe)
#grouper les donnes FO et No en une seule données F
donnees_dist_sexe_F <- rbind(donnees_dist_sexe_FO,donnees_dist_sexe_NO)
#grouper les donnes F et M en une seule données sexe
donnees_dist_sexe<- rbind(donnees_dist_sexe_F,donnees_dist_sexe_M)

##Export donnees_dist_sexe
write.csv(donnees_dist_sexe, "donnees_dist_sexe.csv", row.names=FALSE)

#subdiviser les effectif par coefficient d'extrapolation 
donnees_avec_coeff_extrapole_0=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==0,)
donnees_avec_coeff_extrapole_1=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==1,)
donnees_avec_coeff_extrapole_2=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==2,)
donnees_avec_coeff_extrapole_3=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==3,)
donnees_avec_coeff_extrapole_4=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==4,)
donnees_avec_coeff_extrapole_5=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==5,)
donnees_avec_coeff_extrapole_6=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==6,)
donnees_avec_coeff_extrapole_7=subset(donnees_dist_sexe, donnees_dist_sexe$coefficient_extrapolation==7,)
#repeter ce qui ont un coefficient extapolation <= 2 
donnees_repeter_x2<- donnees_avec_coeff_extrapole_2[rep(1:nrow(donnees_avec_coeff_extrapole_2), each = 2),]
donnees_repeter_x3<- donnees_avec_coeff_extrapole_3[rep(1:nrow(donnees_avec_coeff_extrapole_3), each = 3),]
donnees_repeter_x4<- donnees_avec_coeff_extrapole_4[rep(1:nrow(donnees_avec_coeff_extrapole_4), each = 4),]
donnees_repeter_x5<- donnees_avec_coeff_extrapole_5[rep(1:nrow(donnees_avec_coeff_extrapole_5), each = 5),]
donnees_repeter_x6<- donnees_avec_coeff_extrapole_6[rep(1:nrow(donnees_avec_coeff_extrapole_6), each = 6),]
donnees_repeter_x7<- donnees_avec_coeff_extrapole_7[rep(1:nrow(donnees_avec_coeff_extrapole_7), each = 7),]
#mettre dans un meme tableau les effectifs Multiplier par coefficient traspolation
Donnee_taille<-rbind(donnees_avec_coeff_extrapole_0,
                     donnees_avec_coeff_extrapole_1,
                     donnees_repeter_x2,
                     donnees_repeter_x3,
                     donnees_repeter_x4,
                     donnees_repeter_x5,
                     donnees_repeter_x6,
                     donnees_repeter_x7)

##Export Donnee_taille
#write.csv(Donnee_taille, "Donnee_taille.csv", row.names=FALSE)
write.csv(Donnees_dist_taille, "Donnee_taille.csv", row.names=FALSE)


###### FIN TRAITEMENT DISTRIBUTION DE TAILLE ####################################################################################


##### TRAITEMENT PRODUCTION SUR LA CARTE ########################################################################################

#Renommer le dataframe pour le left_join
# names(prod_carte)[names(prod_carte) == 'Zone'] <- 'zone'
# names(prod_carte)[names(prod_carte) == 'Sous_zone'] <- 'sous_zone'
names(prod_carte)[names(prod_carte) == 'Village'] <- 'village'
#combiner les données de production avec les données géographique
production_geo <- left_join(infos_geo,prod_carte, by=c("village"))
##Export production_geo
write.csv(production_geo, "production_geo.csv", row.names=FALSE)


##### FIN TRAITEMENT PRODUCTION SUR LA CARTE ####################################################################################


#### TRAITEMENT VALEUR ##########################################################################################################

#nouveau dataframe depuis les données cpue
corecrabe_valeur <- corecrabe_cpue
#enlever les prix de collecte 0
corecrabe_valeur <- subset(corecrabe_valeur,corecrabe_valeur$prix_destination_collecte_1>0)
#calculer la moyenne des prix pour chaque destination par village, mois et engin
prix_destination <-corecrabe_valeur%>%
  dplyr::group_by(zone, sous_zone, village,Engin, Mois)%>%
  dplyr::summarise(prix_collecte1=mean(prix_destination_collecte_1, na.rm = TRUE), prix_collecte2=mean(prix_destination_collecte_2, na.rm = TRUE), prix_local=mean(prix_destination_marche_local, na.rm = TRUE))
#enlever les NA
prix_destination=prix_destination%>%drop_na(Mois)
prix_destination$prix_local[is.nan(prix_destination$prix_local)]<-NA
prix_destination$prix_collecte1[is.nan(prix_destination$prix_collecte1)]<-NA
prix_destination$prix_collecte2[is.nan(prix_destination$prix_collecte2)]<-NA
prix_destination[is.na(prix_destination)] <- 0
#renommer les variables 
names(prix_destination)[names(prix_destination) == 'zone'] <- 'Zone'
names(prix_destination)[names(prix_destination) == 'sous_zone'] <- 'Sous_zone'
names(prix_destination)[names(prix_destination) == 'village'] <- 'Village'
names(corecrabe_valeur)[names(corecrabe_valeur) == 'zone'] <- 'Zone'
names(corecrabe_valeur)[names(corecrabe_valeur) == 'sous_zone'] <- 'Sous_zone'
names(corecrabe_valeur)[names(corecrabe_valeur) == 'village'] <- 'Village'
#calucler les part pour chaque destination
corecrabe_valeur$part_collecte1=corecrabe_valeur$poids_destination_collecte_1/corecrabe_valeur$poids_crabe_capture
corecrabe_valeur$part_collecte2=corecrabe_valeur$poids_destination_collecte_2/corecrabe_valeur$poids_crabe_capture
corecrabe_valeur$part_local=corecrabe_valeur$poids_destination_marche_local/corecrabe_valeur$poids_crabe_capture
#calculer les moyennes des part pour chaque destination  pour avoir le revenu
Revenu_corecrabe <-corecrabe_valeur%>%
  dplyr::group_by(Zone,Sous_zone, Village,Engin,Mois)%>%
  dplyr::summarise(part_collecte1=mean(part_collecte1),
                   part_collecte2=mean(part_collecte2),part_local=
                     mean(part_local))
#produire un nouveau tableau pour le prix à partir des revenus combinés avec les prix pour chaque destination
Prix_corecrabe <-left_join(Revenu_corecrabe,prix_destination, by=c("Zone","Sous_zone","Village","Engin", "Mois"))
Prix_corecrabe=Prix_corecrabe%>%drop_na(Mois)

##Export Prix_corecrabe
write.csv(Prix_corecrabe, "Prix_corecrabe.csv", row.names=FALSE)

#### FIN TRAITEMENT VALEUR ####################################################################################################

#### TRAITEMENT DONNEES AVEC TAILLE ##########################################################################################

#Nouveau tableau pour conserver les données brutes
donnees_avec_pecheur <-corecrabe_avec_taille
#creer une colonne
#calcul facteur d'extrapolation
donnees_avec_pecheur$coef_extr=donnees_avec_pecheur$poids_crabe_capture/donnees_avec_pecheur$poids_total_de_l_echantillon
#colonne effectif mesure pour cet individu à mettre 1
donnees_avec_pecheur$Eff_mesure = c(1)
#Estimation des individus mésurés dans la capture suivant les échantillons
donnees_avec_pecheur$Eff_estime = donnees_avec_pecheur$Eff_mesure*donnees_avec_pecheur$coef_extr
#arrondir l'effectif estimé en nombre entier
donnees_avec_pecheur$Eff_estime=round(donnees_avec_pecheur$Eff_estime,0)
#subdiviser les effectifs estimés
donnees_avec_pecheur_0=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="0",)
donnees_avec_pecheur_1=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="1",)
donnees_avec_pecheur_2=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="2",)
donnees_avec_pecheur_3=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="3",)
donnees_avec_pecheur_4=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="4",)
donnees_avec_pecheur_5=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="5",)
donnees_avec_pecheur_6=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="6",)
donnees_avec_pecheur_7=subset(donnees_avec_pecheur, donnees_avec_pecheur$Eff_estime=="7",)
#repeter ce qui sont ?gale ou plus de 2 
donnees_avec_pecheur_2 <- donnees_avec_pecheur_2[rep(1:nrow(donnees_avec_pecheur_2), each = 2),]
donnees_avec_pecheur_3 <- donnees_avec_pecheur_3[rep(1:nrow(donnees_avec_pecheur_3), each = 3),]
donnees_avec_pecheur_4 <- donnees_avec_pecheur_4[rep(1:nrow(donnees_avec_pecheur_4), each = 4),]
donnees_avec_pecheur_5 <- donnees_avec_pecheur_5[rep(1:nrow(donnees_avec_pecheur_5), each = 5),]
donnees_avec_pecheur_6 <- donnees_avec_pecheur_6[rep(1:nrow(donnees_avec_pecheur_6), each = 6),]
donnees_avec_pecheur_7 <- donnees_avec_pecheur_7[rep(1:nrow(donnees_avec_pecheur_7), each = 7),]
#mettre dans un meme tableau les effectifs estimes
Donnee_taille_d<-rbind(donnees_avec_pecheur_0,donnees_avec_pecheur_1,donnees_avec_pecheur_2,
                       donnees_avec_pecheur_3,
                       donnees_avec_pecheur_4,donnees_avec_pecheur_5,
                       donnees_avec_pecheur_6,donnees_avec_pecheur_7)
#variable mois à introduire mais on change le format de la date d'abord
Donnee_taille_d$Mois<-format(as.Date(Donnee_taille_d$date_premiere_sortie_de_peche,  format="%d/%m/%Y"),"%h %y")
#changer les noms des engins
Donnee_taille_d$Engin <- Donnee_taille_d$premier_engin_sortie_de_peche_1

##Export Donnee_taille_d
write.csv(Donnee_taille_d, "Donnee_taille_d.csv", row.names=FALSE)

##Export Donnee_taille_d
write.csv(corecrabe_avec_taille, "corecrabe_avec_taille.csv", row.names=FALSE)

#### FIN TRAITEMENT DONNEES AVEC TAILLE ######################################################################################
