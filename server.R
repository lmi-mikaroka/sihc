server <- function(input, output, session) {
  
  
  res_auth <- secure_server(
    check_credentials = my_custom_check_creds(
      dbname = dbname,
      host = host,
      port = port,
      db_user = user,
      db_password = password
    )
  )
  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })
   #access info
  observe({
    print(auth_output())
  })
  
  
  
  
  
  
  
  ############################################## <- FONCTIONS D'OBSERVATION -> ############################################################
  
    ################################# EFFORT DE PECHE #################################################################################
  
      ####### OPERATION SOUS_ZONE ########
  
      observe({
        if(("Selectionnez tous" %in% input$zone || is.null(input$zone))){
          selected_choices=(c("Selectionnez tous",unique(corecrabe$zone))[-1])# choose all the choices _except_ "Select All"
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }else{
          selected_choices=input$zone 
          # update the select input with choice selected by user
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }
        updatePickerInput(session,"zone","Zone d'études",choices = choices,selected = selected_choices)
      })
  
      observe({
        x <- corecrabe$sous_zone[corecrabe$zone %in% input$zone]
        if(("Selectionnez tous" %in% input$sous_zone || is.null(input$sous_zone) || "Choisir" %in% input$sous_zone)){
          selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }else if("Tou-te-s" %in% input$sous_zone){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
        }else{
          selected_choices=input$sous_zone 
          # update the select input with choice selected by user
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }
        
        updatePickerInput(session,"sous_zone","Sous_zones",choices = choices,selected = selected_choices)
      })
  
      ####### OPERATION VILLAGE ########
  
      observe({
        x <- corecrabe$village[corecrabe$sous_zone %in% input$sous_zone]
        
        if("Tou-te-s" %in% input$sous_zone){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s")
        }else{
          if(("Selectionnez tous" %in% input$village || is.null(input$village) || "Choisir" %in% input$village)){
            selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }else if("Tou-te-s" %in% input$village){
            selected_choices="Tou-te-s"
            choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
          }else{
            selected_choices=input$village 
            # update the select input with choice selected by user
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }
        }
        updatePickerInput(session,"village","Villages",choices = choices,selected = selected_choices)
      })

    ################################# INDICE D'ABONDANCE #################################################################################
  
      ####### OPERATION SOUS_ZONE ########
      observe({
        if(("Selectionnez tous" %in% input$zone_cpue || is.null(input$zone_cpue))){
          selected_choices=(c("Selectionnez tous",unique(corecrabe$zone))[-1])# choose all the choices _except_ "Select All"
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }else{
          selected_choices=input$zone_cpue
          # update the select input with choice selected by user
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }
        updatePickerInput(session,"zone_cpue","Zone d'études",choices = choices,selected = selected_choices)
      })
      observe({
        x <- corecrabe$sous_zone[corecrabe$zone %in% input$zone_cpue]
        if(("Selectionnez tous" %in% input$sous_zone_cpue || is.null(input$sous_zone_cpue) || "Choisir" %in% input$sous_zone_cpue)){
          selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }else if("Tou-te-s" %in% input$sous_zone_cpue){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
        }else{
          selected_choices=input$sous_zone_cpue 
          # update the select input with choice selected by user
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }
        updatePickerInput(session,"sous_zone_cpue","Sous_zones",choices = choices,selected = selected_choices)
      })
  
      ####### OPERATION VILLAGE ########
      observe({
        x <- corecrabe$village[corecrabe$sous_zone %in% input$sous_zone_cpue]
        if("Tou-te-s" %in% input$sous_zone_cpue){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s")
        }else{
          if(("Selectionnez tous" %in% input$village_cpue || is.null(input$village_cpue) || "Choisir" %in% input$village_cpue)){
            selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }else if("Tou-te-s" %in% input$village_cpue){
            selected_choices="Tou-te-s"
            choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
          }else{
            selected_choices=input$village_cpue 
            # update the select input with choice selected by user
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }
        }
        updatePickerInput(session,"village_cpue","Villages",choices = choices,selected = selected_choices)
      })
  
  
    ################################# CAPTURES #################################################################################
  
      ####### OPERATION SOUS_ZONE ########
      observe({
        if(("Selectionnez tous" %in% input$zone_dist || is.null(input$zone_dist))){
          selected_choices=(c("Selectionnez tous",unique(corecrabe$zone))[-1])# choose all the choices _except_ "Select All"
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }else{
          selected_choices=input$zone_dist 
          # update the select input with choice selected by user
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }
        updatePickerInput(session,"zone_dist","Zone d'études",choices = choices,selected = selected_choices)
      })
      observe({
        x <- corecrabe$sous_zone[corecrabe$zone%in%input$zone_dist]
        if(("Selectionnez tous" %in% input$sous_zone_dist || is.null(input$sous_zone_dist) || "Choisir" %in% input$sous_zone_dist)){
          selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }else if("Tou-te-s" %in% input$sous_zone_dist){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
        }else{
          selected_choices=input$sous_zone_dist 
          # update the select input with choice selected by user
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }
        updatePickerInput(session,"sous_zone_dist","Sous_zones",choices = choices,selected = selected_choices)
      })

      ####### OPERATION VILLAGE ########
      observe({
        x <- corecrabe$village[corecrabe$sous_zone%in%input$sous_zone_dist]
        
        if("Tou-te-s" %in% input$sous_zone_dist){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s")
        }else{
          if(("Selectionnez tous" %in% input$village_dist || is.null(input$village_dist) || "Choisir" %in% input$village_dist)){
            selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }else if("Tou-te-s" %in% input$village_dist){
            selected_choices="Tou-te-s"
            choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
          }else{
            selected_choices=input$village_dist 
            # update the select input with choice selected by user
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }
        }
        updatePickerInput(session,"village_dist","Villages",choices = choices,selected = selected_choices)
      })
  
      #********************************** SEXE ************************************************
      observe({
        if("Selectionnez tous" %in% input$sexe)
          selected_choices=choices=c("Selectionnez tous",unique(donnees_dist_sexe$sexe))[-1] # choose all the choices _except_ "Select All"
        else
          selected_choices=input$sexe # update the select input with choice selected by user
        updateSelectInput(session, "sexe", selected = selected_choices)
      })
      #************************************* DESTINATION ********************************************
      observe({
        if("Selectionnez tous" %in% input$destination)
          selected_choices=c("Selectionnez tous",unique(donnees_ensenble_destination$destination))[-1] # choose all the choices _except_ "Select All"
        else
          selected_choices=input$destination # update the select input with choice selected by user
        updateSelectInput(session, "destination", selected = selected_choices)
      })

    ################################# VALEUR #################################################################################
  
      ####### OPERATION SOUS_ZONE ########
      observe({
        if(("Selectionnez tous" %in% input$zone_valeur || is.null(input$zone_valeur))){
          selected_choices=(c("Selectionnez tous",unique(corecrabe$zone))[-1])# choose all the choices _except_ "Select All"
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }else{
          selected_choices=input$zone_valeur
          # update the select input with choice selected by user
          choices = c("Selectionnez tous",unique(corecrabe$zone))
        }
        updatePickerInput(session,"zone_valeur","Zone d'études",choices = choices,selected = selected_choices)
      })
      observe({
        x <- corecrabe$sous_zone[corecrabe$zone%in%input$zone_valeur]
        if(("Selectionnez tous" %in% input$sous_zone_valeur || is.null(input$sous_zone_valeur) || "Choisir" %in% input$sous_zone_valeur)){
          selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }else if("Tou-te-s" %in% input$sous_zone_valeur){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
        }else{
          selected_choices=input$sous_zone_valeur 
          # update the select input with choice selected by user
          choices = c("Tou-te-s","Selectionnez tous",unique(x))
        }
        updatePickerInput(session,"sous_zone_valeur","Sous_zones",choices = choices,selected = selected_choices)
      })

      ####### OPERATION VILLAGE ########
      observe({
        x <- corecrabe$village[corecrabe$sous_zone%in%input$sous_zone_valeur]
        if("Tou-te-s" %in% input$sous_zone_valeur){
          selected_choices="Tou-te-s"
          choices=c("Tou-te-s")
        }else{
          if(("Selectionnez tous" %in% input$village_valeur || is.null(input$village_valeur) || "Choisir" %in% input$village_valeur)){
            selected_choices=(c("Tou-te-s","Selectionnez tous",unique(x))[-1])[-1]# choose all the choices _except_ "Select All"
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }else if("Tou-te-s" %in% input$village_valeur){
            selected_choices="Tou-te-s"
            choices=c("Tou-te-s","Choisir")# update the select input with choice selected by user
          }else{
            selected_choices=input$village_valeur
            # update the select input with choice selected by user
            choices = c("Tou-te-s","Selectionnez tous",unique(x))
          }
        }
        updatePickerInput(session,"village_valeur","Villages",choices = choices,selected = selected_choices)
      })

    ############################################# OBSERVE MAP CLICK ##################################################
    observe({
      click = input$map_shape_click
      if(is.null(click))
        return()
      else
        leafletProxy("map") %>%
        setView(lng = click$lng, lat = click$lat, zoom=10)
    })


  ################################################ <- FONCTIONS REACTIVES -> ##############################################################

    #### TAUX D'ACTIVITE ###################################################

      #Déclaration de la fonction
      func_taux <- function(zone,village,date,engin,sous_zone) {
        #filtrer le dataframe corecrabe_taux par zone
        corecrabe_taux_filtre <- corecrabe_taux[corecrabe_taux$Zone%in%zone, ]
        #filtre par sous_zone, village par rapport au choix dans les filtres
        if("Tou-te-s" %in% village){
          if("Tou-te-s" %in% sous_zone){
            corecrabe_taux_filtre <- corecrabe_taux_filtre%>%filter(Zone%in%zone)
          }else{
            corecrabe_taux_filtre <- corecrabe_taux_filtre%>%filter(Sous_zone%in%sous_zone)
          }
        }else{
          corecrabe_taux_filtre <- corecrabe_taux_filtre%>%filter(Village%in%village)
        }
        #transposition des donnees
        corecrabe_taux_filtre <-corecrabe_taux_filtre%>%
        pivot_longer(cols = -c(Zone,Sous_zone,Region,District,Commune,Fokontany, Village, id),
                    names_to = c(".value", "date_number"),names_sep = "_", 
                    values_drop_na = TRUE)
        corecrabe_taux_filtre$Mois<-format(as.Date(corecrabe_taux_filtre$Date,  format="%d/%m/%Y"),"%h %y")
        c=nrow(corecrabe_taux_filtre)
        for(i in c(1:c)){
          if(corecrabe_taux_filtre$Activite[i]>1){
            corecrabe_taux_filtre$Activite[i]=1
          }
          i=i+1
        }
        #filter les Mois
        #corecrabe_taux_filtre <- filter(corecrabe_taux_filtre, Mois%in%filtre_mois[match(format(as.Date(date[1],  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date(date[2],  format="%Y/%m/%d"),"%h %y"),filtre_mois)])
        #Filtre par rapport à l'engin
        if("Tou-te-s" %in% engin){
          taux_filtre<-corecrabe_taux_filtre%>%
            dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
            dplyr::summarise(somme_tac=sum(Activite), nb=(length(unique(id))*4),
                            nb_engin_moyen=mean(Nombre.engin, na.rm = TRUE), sd_engin=sd(Nombre.engin, na.rm = TRUE),
                            N_engin=length(Nombre.engin))
          taux_filtre$tac=taux_filtre$somme_tac/taux_filtre$nb
        }else{
          taux_filtre<-corecrabe_taux_filtre%>%
            dplyr::group_by(Zone,Sous_zone,Village,Mois,Engin)%>%
            dplyr::summarise(somme_tac=sum(Activite), nb=(length(unique(id))*4),
                            nb_engin_moyen=mean(Nombre.engin, na.rm = TRUE), sd_engin=sd(Nombre.engin, na.rm = TRUE),
                            N_engin=length(Nombre.engin))
          taux_filtre$tac=taux_filtre$somme_tac/taux_filtre$nb
          taux_filtre=taux_filtre %>% filter(Engin%in%engin)
        }
        #Dégager les lignes aves taux inférieur à 0.2
        taux_filtre=subset(taux_filtre, taux_filtre$tac>=0.2)
        #Valeur de retour (un tableau formaté et traité)
        taux_filtre
      }
    
    #### FIN TAUX D'ACTIVITE #######################################################


    #### PECHEUR ACTIF #############################################################

      #Déclaration de la fonction
      func_pecheur <- function(zone,village,date,engin,sous_zone){
        #filtrages
        recensement <- recensement_complet[recensement_complet$zone%in%zone, ]
        if("Tou-te-s" %in% village){
          if("Tou-te-s" %in% sous_zone){
            recensement_filtre <- recensement[recensement$zone%in%zone,]
          }else{
            recensement_filtre <- recensement[recensement$sous_zone%in%sous_zone,]
          }
        }else{
          recensement_filtre <- recensement[recensement$village%in%village,]
        }
        
        #filter les Mois
        #recensement_filtre <- filter(recensement_filtre, Mois%in%filtre_mois[match(format(as.Date(date[1],  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date(date[2],  format="%Y/%m/%d"),"%h %y"),filtre_mois)])
        #Renommer les variables
        names(recensement_filtre)[names(recensement_filtre) == 'zone'] <- 'Zone'
        names(recensement_filtre)[names(recensement_filtre) == 'sous_zone'] <- 'Sous_zone'
        names(recensement_filtre)[names(recensement_filtre) == 'village'] <- 'Village'
        names(recensement_filtre)[names(recensement_filtre) == 'Mois'] <- 'Mois'
        names(recensement_filtre)[names(recensement_filtre) == 'engin'] <- 'Engin'
        #Filtre engin
        if("Tou-te-s" %in% engin){
          recensement_corecrabe <-recensement_filtre%>%
            dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
            dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coeff_considere=mean(coeff_considere,na.rm=TRUE))
        }else{
          recensement_corecrabe <-recensement_filtre%>%
            dplyr::group_by(Zone,Sous_zone,Village,Mois,Engin)%>%
            dplyr::summarise(pecheur_total=pecheur_total,coeff_considere=mean(coeff_considere,na.rm=TRUE))
          recensement_corecrabe=recensement_corecrabe %>% filter(Engin%in%engin)
        }
        #Valeur de retour
        recensement_corecrabe
      }
    
    #### FIN PECHEUR ACTIF ##########################################
  
  
    #### CPUE ###########################################################

      #Déclaration de la fonction
      func_cpue <- function(zone,village,date,engin,sous_zone){
        #Filtrage
        corecrabe_cpue_filtre <- corecrabe_cpue[corecrabe_cpue$zone%in%zone, ]
        if("Tou-te-s" %in% village){
          if("Tou-te-s" %in% sous_zone){
            corecrabe_cpue_filtre <- corecrabe_cpue_filtre[corecrabe_cpue_filtre$zone%in%zone, ]
          }else{
            corecrabe_cpue_filtre <- corecrabe_cpue_filtre[corecrabe_cpue_filtre$sous_zone%in%sous_zone, ]
          }
        }else{
          corecrabe_cpue_filtre <- corecrabe_cpue_filtre[corecrabe_cpue_filtre$village%in%village, ]
        }

        corecrabe_cpue_filtre <- corecrabe_cpue_filtre[is.finite(corecrabe_cpue_filtre$logCPUE_sortie), ]
        #filter les Mois
        #corecrabe_cpue_filtre <- filter(corecrabe_cpue_filtre, Mois%in%filtre_mois[match(format(as.Date(date[1],  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date(date[2],  format="%Y/%m/%d"),"%h %y"),filtre_mois)])
        mixed <-  lmer(logCPUE_sortie ~ Mois*Engin+Engin:nombre_premier_engin_sortie_de_peche_1+nombre_premier_engin_sortie_de_peche_1+sous_zone+(1|village), data = corecrabe_cpue_filtre, REML=FALSE)
        #prédire les reponses de CPUE selon le modele GLMM
        #creer une valeur prédiction de CPUE
        prediction_cpue <- predict(mixed,newdata = corecrabe_cpue_filtre,type="response")
        corecrabe_cpue_filtre$cpue_predite <- exp(prediction_cpue)
        if("Tou-te-s" %in% engin){
          
        }else{
          #Filtre engin
          corecrabe_cpue_filtre=corecrabe_cpue_filtre%>% filter(Engin%in%engin)
        }
        #Renommer les variables 
        names(corecrabe_cpue_filtre)[names(corecrabe_cpue_filtre) == 'sous_zone'] <- 'Sous_zone'
        names(corecrabe_cpue_filtre)[names(corecrabe_cpue_filtre) == 'zone'] <- 'Zone'
        names(corecrabe_cpue_filtre)[names(corecrabe_cpue_filtre) == 'village'] <- 'Village'
        #Valeur de retour
        corecrabe_cpue_filtre
      }

    ##### FIN CPUE #######################################################################

    #### RENDEMENT ###########################################################
      
      #Déclaration de la fonction
      func_rendement <- function(zone,village,date,engin,sous_zone){
        #Filtrage
        corecrabe_cpue_filtre <- corecrabe_cpue[corecrabe_cpue$zone%in%zone, ]
        if("Tou-te-s" %in% village){
          if("Tou-te-s" %in% sous_zone){
            corecrabe_cpue_filtre <- corecrabe_cpue_filtre[corecrabe_cpue_filtre$zone%in%zone, ]
          }else{
            corecrabe_cpue_filtre <- corecrabe_cpue_filtre[corecrabe_cpue_filtre$sous_zone%in%sous_zone, ]
          }
        }else{
          corecrabe_cpue_filtre <- corecrabe_cpue_filtre[corecrabe_cpue_filtre$village%in%village, ]
        }
        
        if("Tou-te-s" %in% engin){
          
        }else{
          #Filtre engin
          corecrabe_cpue_filtre=corecrabe_cpue_filtre%>% filter(Engin%in%engin)
        }
        #Renommer les variables 
        names(corecrabe_cpue_filtre)[names(corecrabe_cpue_filtre) == 'sous_zone'] <- 'Sous_zone'
        names(corecrabe_cpue_filtre)[names(corecrabe_cpue_filtre) == 'zone'] <- 'Zone'
        names(corecrabe_cpue_filtre)[names(corecrabe_cpue_filtre) == 'village'] <- 'Village'
        #Valeur de retour
        corecrabe_cpue_filtre
      }
      
      ##### FIN RENDEMENT #######################################################################

    ##### EFFORT DE PECHE ################################################################
  
      #Déclaration de la fonction
      func_effort <- function(zone,village,date,engin,sous_zone){
        #Filtre
        if("Tou-te-s" %in% village){
          if("Tou-te-s" %in% sous_zone){
            taux_filtre <- func_taux(zone,village,date,engin,sous_zone)%>%
              dplyr::group_by(Zone,Mois)%>%
              dplyr::summarise(tac=mean(tac))
            recensement_corecrabe <-func_pecheur(zone,village,date,engin,sous_zone)%>%
              dplyr::group_by(Zone,Mois)%>%
              dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
            recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
            #calcul du taux d'actifs par mois
            recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
            #joindre les tableaux ouu il y a les taux d'activité et le recensement
            tableau_effort <-left_join(taux_filtre,recensement_corecrabe, by=c("Zone","Mois"))
            tableau_effort$jour=c(30.41)
            #changer les engins d'engin qui commence par ligne en ligne
            tableau_effort$effort <- tableau_effort$tac*tableau_effort$actifs*tableau_effort$jour
            tableau_effort
          }else{
            taux_filtre <- func_taux(zone,village,date,engin,sous_zone)%>%
              dplyr::group_by(Zone,Mois,Sous_zone)%>%
              dplyr::summarise(tac=mean(tac))
            recensement_corecrabe <-func_pecheur(zone,village,date,engin,sous_zone)%>%
              dplyr::group_by(Zone,Mois,Sous_zone)%>%
              dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
            recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
            #calcul du taux d'actifs par mois
            recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
            #joindre les tableaux ouu il y a les taux d'activité et le recensement
            tableau_effort <-left_join(taux_filtre,recensement_corecrabe, by=c("Zone","Mois","Sous_zone"))
            tableau_effort$jour=c(30.41)
            #changer les engins d'engin qui commence par ligne en ligne
            tableau_effort$effort <- tableau_effort$tac*tableau_effort$actifs*tableau_effort$jour
            tableau_effort
          }
        }else{
          taux_filtre <- func_taux(zone,village,date,engin,sous_zone)%>%
            dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
            dplyr::summarise(tac=mean(tac))
          recensement_corecrabe <-func_pecheur(zone,village,date,engin,sous_zone)%>%
            dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
            dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
          recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
          #calcul du taux d'actifs par mois
          recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
          #joindre les tableaux ouu il y a les taux d'activité et le recensement
          tableau_effort <-left_join(taux_filtre,recensement_corecrabe, by=c("Zone","Mois","Sous_zone","Village"))
          tableau_effort$jour=c(30.41)
          #changer les engins d'engin qui commence par ligne en ligne
          tableau_effort$effort <- tableau_effort$tac*tableau_effort$actifs*tableau_effort$jour
          tableau_effort
        }
      }

    #### FIN EFFORT DE PECHE ##############################################################

    #### PRODUCTION TOTALE ###############################################################
  
  
  func_prod_totale <- function(zone,village,date,engin,sous_zone){
    
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        Ensemble <- left_join(func_effort(zone,village,date,engin,sous_zone),T_glm, by=c("Zone","Mois"))
        ####calcul de la production
        Ensemble$production <- Ensemble$effort*Ensemble$mean
        Ensemble <- Ensemble%>%drop_na(production)
        
        Ensemble
        
      }else{
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Mois,Sous_zone)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        Ensemble <- left_join(func_effort(zone,village,date,engin,sous_zone),T_glm, by=c("Zone","Mois","Sous_zone"))
        ####calcul de la production
        Ensemble$production <- Ensemble$effort*Ensemble$mean
        Ensemble <- Ensemble%>%drop_na(production)
        
        Ensemble
      }
    }else{
      T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
        dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
        dplyr::summarise(mean=mean(cpue_predite))
      
      Ensemble <- left_join(func_effort(zone,village,date,engin,sous_zone),T_glm, by=c("Zone","Mois","Sous_zone","Village"))
      ####calcul de la production
      Ensemble$production <- Ensemble$effort*Ensemble$mean
      Ensemble <- Ensemble%>%drop_na(production)
      
      Ensemble
    }
  }
  
  
  func_prod_cumule <- function(zone,village,date,engin,sous_zone){
    
    production_totale <- func_prod_totale(zone,village,date,engin,sous_zone)
    production_totale$annee <- format(as.Date(paste("01 ",production_totale$Mois),  format="%d %h %y"),"%Y")
    
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        
        Tab_cumule <-production_totale%>%
          dplyr::group_by(Zone,Mois,annee)%>%
          dplyr::summarise(production=sum(production))
        
        Tab_cumule$date_standard <- as.Date(format(as.Date(paste("01 ",Tab_cumule$Mois),  format="%d %h %y"),"%d/%m/%Y"),format="%d/%m/%Y")
        Tab_cumule<-Tab_cumule[with(Tab_cumule, order(Zone, date_standard)), ] 
        #permutation <- order(Tab_cumule$annee)
        #head(Tab_cumule[permutation,])
        ###############################################################################################################
        annee2021 <- filter(Tab_cumule,((annee==2021 & Mois != format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2020/12/01",  format="%Y/%m/%d"), "%h %y")))
        annee2022 <- filter(Tab_cumule,((annee==2022 & Mois != format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")))
        annee2023 <- filter(Tab_cumule,((annee==2023 & Mois != format(as.Date("2023/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")))
        ###############cumuler mois par mois
        
        zone_individuel <- as.array(unique(Tab_cumule$Zone))
        i<-1
        Tab_cumule_result <- NULL
        for(i in c(1:nrow(zone_individuel))){
          cumul_result_2021 <- annee2021[annee2021$Zone==zone_individuel[i], ]
          cumul_result_2022 <- annee2022[annee2022$Zone==zone_individuel[i], ]
          cumul_result_2023 <- annee2023[annee2023$Zone==zone_individuel[i], ]
          cumul_result_2021$cumul<-cumsum(cumul_result_2021$production)
          cumul_result_2022$cumul<-cumsum(cumul_result_2022$production)
          cumul_result_2023$cumul<-cumsum(cumul_result_2023$production)
          Tab_cumule_result<-rbind(Tab_cumule_result,cumul_result_2021,cumul_result_2022,cumul_result_2023)
        }
        #annee2021$cumul <-cumsum(annee2021$production)
        #annee2022$cumul <-cumsum(annee2022$production)
        
        ######################################################rbindeva
        #Tab_cumule<-rbind(annee2021,annee2022)
        #write.csv(Tab_cumule_result, file = "Tab_cumule_result_zone1.csv")
        
        Tab_cumule_result
        
      }else{
        Tab_cumule <-production_totale%>%
          dplyr::group_by(Zone,Sous_zone,Mois,annee)%>%
          dplyr::summarise(production=sum(production))
        
        Tab_cumule$date_standard <- as.Date(format(as.Date(paste("01 ",Tab_cumule$Mois),  format="%d %h %y"),"%d/%m/%Y"),format="%d/%m/%Y")
        Tab_cumule<-Tab_cumule[with(Tab_cumule, order(Sous_zone, date_standard)), ] 
        #permutation <- order(Tab_cumule$annee)
        #head(Tab_cumule[permutation,])
        ###############################################################################################################
        annee2021 <- filter(Tab_cumule,((annee==2021 & Mois != format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2020/12/01",  format="%Y/%m/%d"), "%h %y")))
        annee2022 <- filter(Tab_cumule,((annee==2022 & Mois != format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")))
        annee2023 <- filter(Tab_cumule,((annee==2023 & Mois != format(as.Date("2023/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")))
        ###############cumuler mois par mois
        
        sous_zone_individuel <- as.array(unique(Tab_cumule$Sous_zone))
        i<-1
        Tab_cumule_result <- NULL
        for(i in c(1:nrow(sous_zone_individuel))){
          cumul_result_2021 <- annee2021[annee2021$Sous_zone==sous_zone_individuel[i], ]
          cumul_result_2022 <- annee2022[annee2022$Sous_zone==sous_zone_individuel[i], ]
          cumul_result_2023 <- annee2023[annee2023$Sous_zone==sous_zone_individuel[i], ]
          cumul_result_2021$cumul<-cumsum(cumul_result_2021$production)
          cumul_result_2022$cumul<-cumsum(cumul_result_2022$production)
          cumul_result_2023$cumul<-cumsum(cumul_result_2023$production)
          
          Tab_cumule_result<-rbind(Tab_cumule_result,cumul_result_2021,cumul_result_2022,cumul_result_2023)
        }
        #annee2021$cumul <-cumsum(annee2021$production)
        #annee2022$cumul <-cumsum(annee2022$production)
        
        ######################################################rbindeva
        #Tab_cumule<-rbind(annee2021,annee2022)
        
        
        Tab_cumule_result
      }
    }else{
      Tab_cumule <-production_totale%>%
        dplyr::group_by(Zone,Sous_zone,Village,Mois,annee)%>%
        dplyr::summarise(production=sum(production))
      
      Tab_cumule$date_standard <- as.Date(format(as.Date(paste("01 ",Tab_cumule$Mois),  format="%d %h %y"),"%d/%m/%Y"),format="%d/%m/%Y")
      Tab_cumule<-Tab_cumule[with(Tab_cumule, order(Village, date_standard)), ] 
      #permutation <- order(Tab_cumule$annee)
      #head(Tab_cumule[permutation,])
      ###############################################################################################################
      annee2021 <- filter(Tab_cumule,((annee==2021 & Mois != format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2020/12/01",  format="%Y/%m/%d"), "%h %y")))
      annee2022 <- filter(Tab_cumule,((annee==2022 & Mois != format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")))
      annee2023 <- filter(Tab_cumule,((annee==2023 & Mois != format(as.Date("2023/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")))
      ###############cumuler mois par mois
      
      village_individuel <- as.array(unique(Tab_cumule$Village))
      i<-1
      Tab_cumule_result <- NULL
      for(i in c(1:nrow(village_individuel))){
        cumul_result_2021 <- annee2021[annee2021$Village==village_individuel[i], ]
        cumul_result_2022 <- annee2022[annee2022$Village==village_individuel[i], ]
        cumul_result_2023 <- annee2023[annee2023$Village==village_individuel[i], ]
        
        cumul_result_2021$cumul<-cumsum(cumul_result_2021$production)
        cumul_result_2022$cumul<-cumsum(cumul_result_2022$production)
        cumul_result_2023$cumul<-cumsum(cumul_result_2023$production)
        
        Tab_cumule_result<-rbind(Tab_cumule_result,cumul_result_2021,cumul_result_2022,cumul_result_2023)
      }
      #annee2021$cumul <-cumsum(annee2021$production)
      #annee2022$cumul <-cumsum(annee2022$production)
      
      ######################################################rbindeva
      #Tab_cumule<-rbind(annee2021,annee2022)
      #write.csv(Tab_cumule_result, file = "Tab_cumule_result1.csv")
      
      Tab_cumule_result
    }
  }
  
  func_valeur <- function(zone,village,date,engin,sous_zone){
    prix_corecrabe_filtre <- Prix_corecrabe[Prix_corecrabe$Zone%in%zone, ]
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        prix_corecrabe_filtre <- prix_corecrabe_filtre%>%filter(Zone%in%zone)
        valeur <- left_join(prix_corecrabe_filtre, func_prod_totale(zone,village,date,engin,sous_zone)  , by=c("Zone","Mois"))
        valeur$revenus <- valeur$mean*((valeur$part_collecte1*valeur$prix_collecte1)+(valeur$part_collecte2*valeur$prix_collecte2)+(valeur$part_local*valeur$prix_local))
        valeur=valeur%>%drop_na(revenus)
        Revenu_pecheur_valeur <-valeur%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(Revenu_pecheur=mean(revenus), effort=sum(effort))
        Revenu_pecheur_valeur$valeur <- Revenu_pecheur_valeur$Revenu_pecheur*Revenu_pecheur_valeur$effort
        
        Revenu_pecheur_valeur
      }else{
        prix_corecrabe_filtre <- prix_corecrabe_filtre%>%filter(Sous_zone%in%sous_zone)
        valeur <- left_join(prix_corecrabe_filtre, func_prod_totale(zone,village,date,engin,sous_zone)  , by=c("Zone","Sous_zone","Mois"))
        valeur$revenus <- valeur$mean*((valeur$part_collecte1*valeur$prix_collecte1)+(valeur$part_collecte2*valeur$prix_collecte2)+(valeur$part_local*valeur$prix_local))
        valeur=valeur%>%drop_na(revenus)
        Revenu_pecheur_valeur <-valeur%>%
          dplyr::group_by(Zone,Sous_zone,Mois)%>%
          dplyr::summarise(Revenu_pecheur=mean(revenus), effort=sum(effort))
        Revenu_pecheur_valeur$valeur <- Revenu_pecheur_valeur$Revenu_pecheur*Revenu_pecheur_valeur$effort
        
        Revenu_pecheur_valeur
      }
    }else{
      prix_corecrabe_filtre <- prix_corecrabe_filtre%>%filter(Village%in%Village)
      valeur <- left_join(prix_corecrabe_filtre, func_prod_totale(zone,village,date,engin,sous_zone)  , by=c("Zone","Sous_zone","Village","Mois"))
        valeur$revenus <- valeur$mean*((valeur$part_collecte1*valeur$prix_collecte1)+(valeur$part_collecte2*valeur$prix_collecte2)+(valeur$part_local*valeur$prix_local))
        valeur=valeur%>%drop_na(revenus)
        Revenu_pecheur_valeur <-valeur%>%
          dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
          dplyr::summarise(Revenu_pecheur=mean(revenus), effort=sum(effort))
        Revenu_pecheur_valeur$valeur <- Revenu_pecheur_valeur$Revenu_pecheur*Revenu_pecheur_valeur$effort
        
        Revenu_pecheur_valeur
    }
    
  }
  

  func_prix <- function(zone,village,date,engin,sous_zone){
    prix_corecrabe_filtre <- Prix_corecrabe[Prix_corecrabe$Zone%in%zone, ]
    #prix_corecrabe_filtre <- filter(prix_corecrabe_filtre, Mois%in%filtre_mois[match(format(as.Date(date[1],  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date(date[2],  format="%Y/%m/%d"),"%h %y"),filtre_mois)])
    
    if("Tou-te-s" %in% engin){
      
    }else{
      prix_corecrabe_filtre <- prix_corecrabe_filtre[prix_corecrabe_filtre$Engin%in%engin, ]
    }
    
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        prix_moyenne <-prix_corecrabe_filtre%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(prix_collecte1=mean(prix_collecte1), prix_local=mean(prix_local))
        
        prix_moyenne
      }else{
        prix_moyenne <-prix_corecrabe_filtre%>%
          dplyr::group_by(Zone,Sous_zone,Mois)%>%
          dplyr::summarise(prix_collecte1=mean(prix_collecte1), prix_local=mean(prix_local))
        
        prix_moyenne
      }
    }else{
      prix_moyenne <-prix_corecrabe_filtre%>%
        dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
        dplyr::summarise(prix_collecte1=mean(prix_collecte1), prix_local=mean(prix_local))
      
      prix_moyenne
    }
  }

  func_taille_moyenne <- function(zone,village,date,engin,sous_zone){
    
    Donnee <- Donnee_taille_d[Donnee_taille_d$zone%in%zone, ]
    
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        Donnee <- Donnee[Donnee$zone%in%zone, ]
        glm_taille <-  glm(formula =taille_de_crabe ~ Mois+Engin+sous_zone+village,data=Donnee, family = poisson)
        #Pseudo-R2
        R2_1 <- 1.0 - glm_taille$deviance / glm_taille$null.deviance
        ######prediction
        pred_glm_taille <- predict(object = glm_taille, newdata = Donnee, type = "response")
        Donnee$taille_predite_glm <-pred_glm_taille
        Donnee=Donnee%>%drop_na(taille_predite_glm)
        
        
        if("Tou-te-s" %in% engin){
          
        }else{
          Donnee=Donnee[Donnee$Engin%in%engin, ]
        }
        ###Group by
        taille_moyenne <-Donnee%>%
          dplyr::group_by(zone,Mois)%>%
          dplyr::summarise(mean=mean(taille_predite_glm))
      }else{
        Donnee <- Donnee[Donnee$sous_zone%in%sous_zone, ]
        glm_taille <-  glm(formula =taille_de_crabe ~ Mois+Engin+sous_zone+village,data=Donnee, family = poisson)
        #Pseudo-R2
        R2_1 <- 1.0 - glm_taille$deviance / glm_taille$null.deviance
        ######prediction
        pred_glm_taille <- predict(object = glm_taille, newdata = Donnee, type = "response")
        Donnee$taille_predite_glm <-pred_glm_taille
        Donnee=Donnee%>%drop_na(taille_predite_glm)
        
        
        if("Tou-te-s" %in% engin){
          
        }else{
          Donnee=Donnee[Donnee$Engin%in%engin, ]
        }
        ###Group by
        taille_moyenne <-Donnee%>%
          dplyr::group_by(zone,sous_zone,Mois)%>%
          dplyr::summarise(mean=mean(taille_predite_glm))
      }
    }else{
      Donnee <- Donnee[Donnee$village%in%village, ]
      glm_taille <-  glm(formula =taille_de_crabe ~ Mois+Engin+sous_zone+village,data=Donnee, family = poisson)
      #Pseudo-R2
      R2_1 <- 1.0 - glm_taille$deviance / glm_taille$null.deviance
      ######prediction
      pred_glm_taille <- predict(object = glm_taille, newdata = Donnee, type = "response")
      Donnee$taille_predite_glm <-pred_glm_taille
      Donnee=Donnee%>%drop_na(taille_predite_glm)
      
      
      if("Tou-te-s" %in% engin){
        
      }else{
        Donnee=Donnee[Donnee$Engin%in%engin, ]
      }
      ###Group by
      taille_moyenne <-Donnee%>%
        dplyr::group_by(zone,sous_zone,village,Mois)%>%
        dplyr::summarise(mean=mean(taille_predite_glm))
    }
    
    
    names(taille_moyenne)[names(taille_moyenne) == 'sous_zone'] <- 'Sous_zone'
    names(taille_moyenne)[names(taille_moyenne) == 'zone'] <- 'Zone'
    names(taille_moyenne)[names(taille_moyenne) == 'village'] <- 'Village'
    
    taille_moyenne
  }
  
  func_cpue_nombre <- function(zone,village,date,engin,sous_zone){
    Donnee_filtre <- corecrabe_avec_taille[corecrabe_avec_taille$zone%in%zone, ]
    if("Tou-te-s" %in% engin){
      
    }else{
      Donnee_filtre=Donnee_filtre%>% filter(Engin%in%engin)
    }
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        #####calcul cpue en nombre par sortie globale####
        Donnee_globale <- Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,id_enquete)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon),Nombre=sum(Eff_mesure))
        
        Donnee_globale1<-Donnee_globale%>%
          dplyr::group_by(zone,Mois,Engin)%>%
          dplyr::summarise(capture_kg=sum(capture_kg),Nombre=sum(Nombre))
        
        Donnee_globale1$nb_par_kg<-Donnee_globale1$Nombre/Donnee_globale1$capture_kg
        names(Donnee_globale1)[names(Donnee_globale1)=='zone'] <- 'Zone'
        
        
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Mois,Engin)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        donnee_cpue_nb_globale<-left_join(Donnee_globale1,T_glm,by=c("Zone","Mois","Engin"))
        donnee_cpue_nb_globale$nb_global_par_sortie<-donnee_cpue_nb_globale$nb_par_kg*donnee_cpue_nb_globale$mean
        
        donnee_cpue_nb_globale
        
      }else{
        #####calcul cpue en nombre par sortie globale####
        Donnee_globale <- Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,id_enquete)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon),Nombre=sum(Eff_mesure))
        
        Donnee_globale1<-Donnee_globale%>%
          dplyr::group_by(zone,sous_zone,Mois,Engin)%>%
          dplyr::summarise(capture_kg=sum(capture_kg),Nombre=sum(Nombre))
        
        Donnee_globale1$nb_par_kg<-Donnee_globale1$Nombre/Donnee_globale1$capture_kg
        names(Donnee_globale1)[names(Donnee_globale1)=='zone'] <- 'Zone'
        names(Donnee_globale1)[names(Donnee_globale1)=='sous_zone'] <- 'Sous_zone'
        
        
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Sous_zone,Mois,Engin)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        donnee_cpue_nb_globale<-left_join(Donnee_globale1,T_glm,by=c("Zone","Sous_zone","Mois","Engin"))
        donnee_cpue_nb_globale$nb_global_par_sortie<-donnee_cpue_nb_globale$nb_par_kg*donnee_cpue_nb_globale$mean
        
        donnee_cpue_nb_globale
      }
    }else{
      #####calcul cpue en nombre par sortie globale####
      Donnee_globale <- Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,id_enquete)%>%
        dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon),Nombre=sum(Eff_mesure))
      
      Donnee_globale1<-Donnee_globale%>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin)%>%
        dplyr::summarise(capture_kg=sum(capture_kg),Nombre=sum(Nombre))
      
      Donnee_globale1$nb_par_kg<-Donnee_globale1$Nombre/Donnee_globale1$capture_kg
      names(Donnee_globale1)[names(Donnee_globale1)=='zone'] <- 'Zone'
      names(Donnee_globale1)[names(Donnee_globale1)=='sous_zone'] <- 'Sous_zone'
      names(Donnee_globale1)[names(Donnee_globale1)=='village'] <- 'Village'
      
      
      T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
        dplyr::group_by(Zone,Sous_zone,Village,Mois,Engin)%>%
        dplyr::summarise(mean=mean(cpue_predite))
      
      donnee_cpue_nb_globale<-left_join(Donnee_globale1,T_glm,by=c("Zone","Sous_zone","Village","Mois","Engin"))
      donnee_cpue_nb_globale$nb_global_par_sortie<-donnee_cpue_nb_globale$nb_par_kg*donnee_cpue_nb_globale$mean
      
      donnee_cpue_nb_globale
    }
    
  }
  
  
  func_cpue_taille <- function(zone,village,date,engin,sous_zone){
    Donnee_filtre <- corecrabe_avec_taille[corecrabe_avec_taille$zone%in%zone, ]
    if("Tou-te-s" %in% engin){
      
    }else{
      Donnee_filtre=Donnee_filtre%>% filter(Engin%in%engin)
    }
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        
        ##### calcul cpue en nombre par sortie par classe de taille ####
        Donnee_groupe <- Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,id_enquete,classe_de_taille)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupex <- Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,id_enquete)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupe1<-Donnee_groupe%>%
          dplyr::group_by(zone,Mois,Engin)%>%
          dplyr::summarise(capture_kg=sum(capture_kg))
        
        Donnee_groupe2 <-Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,classe_de_taille)%>%
          dplyr::summarise(Nombre=sum(Eff_mesure))
        
        Donnee_groupe3 <- left_join(Donnee_groupe1,Donnee_groupe2, by=c("zone","Engin","Mois"))
        
        Donnee_groupe3$nb_par_kg <-Donnee_groupe3$Nombre/Donnee_groupe3$capture_kg
        summary(Donnee_groupe3$nb_par_kg)
        names(Donnee_groupe3)[names(Donnee_groupe3)=='zone'] <- 'Zone'
        
        
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Mois,Engin)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        donnee_cpue_nb<-left_join(Donnee_groupe3,T_glm,by=c("Zone","Mois","Engin"))
        donnee_cpue_nb$nb_par_sortie=donnee_cpue_nb$nb_par_kg*donnee_cpue_nb$mean
        
        donnee_cpue_nb
        
        
      }else{
        
        ##### calcul cpue en nombre par sortie par classe de taille ####
        Donnee_groupe <- Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,id_enquete,classe_de_taille)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupex <- Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,id_enquete)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupe1<-Donnee_groupe%>%
          dplyr::group_by(zone,sous_zone,Mois,Engin)%>%
          dplyr::summarise(capture_kg=sum(capture_kg))
        
        Donnee_groupe2 <-Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,classe_de_taille)%>%
          dplyr::summarise(Nombre=sum(Eff_mesure))
        
        Donnee_groupe3 <- left_join(Donnee_groupe1,Donnee_groupe2, by=c("zone","sous_zone","Engin","Mois"))
        
        Donnee_groupe3$nb_par_kg <-Donnee_groupe3$Nombre/Donnee_groupe3$capture_kg
        summary(Donnee_groupe3$nb_par_kg)
        names(Donnee_groupe3)[names(Donnee_groupe3)=='zone'] <- 'Zone'
        names(Donnee_groupe3)[names(Donnee_groupe3)=='sous_zone'] <- 'Sous_zone'
        
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Sous_zone,Mois,Engin)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        donnee_cpue_nb<-left_join(Donnee_groupe3,T_glm,by=c("Zone","Sous_zone","Mois","Engin"))
        donnee_cpue_nb$nb_par_sortie=donnee_cpue_nb$nb_par_kg*donnee_cpue_nb$mean
        
        donnee_cpue_nb
      }
    }else{
      
      ##### calcul cpue en nombre par sortie par classe de taille ####
      Donnee_groupe <- Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,id_enquete,classe_de_taille)%>%
        dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
      
      Donnee_groupex <- Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,id_enquete)%>%
        dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
      
      Donnee_groupe1<-Donnee_groupe%>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin)%>%
        dplyr::summarise(capture_kg=sum(capture_kg))
      
      Donnee_groupe2 <-Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,classe_de_taille)%>%
        dplyr::summarise(Nombre=sum(Eff_mesure))
      
      Donnee_groupe3 <- left_join(Donnee_groupe1,Donnee_groupe2, by=c("zone","sous_zone","village","Engin","Mois"))
      
      Donnee_groupe3$nb_par_kg <-Donnee_groupe3$Nombre/Donnee_groupe3$capture_kg
      summary(Donnee_groupe3$nb_par_kg)
      names(Donnee_groupe3)[names(Donnee_groupe3)=='zone'] <- 'Zone'
      names(Donnee_groupe3)[names(Donnee_groupe3)=='sous_zone'] <- 'Sous_zone'
      names(Donnee_groupe3)[names(Donnee_groupe3)=='village'] <- 'Village'
      
      T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
        dplyr::group_by(Zone,Sous_zone,Village,Mois,Engin)%>%
        dplyr::summarise(mean=mean(cpue_predite))
      
      donnee_cpue_nb<-left_join(Donnee_groupe3,T_glm,by=c("Zone","Sous_zone","Village","Mois","Engin"))
      donnee_cpue_nb$nb_par_sortie=donnee_cpue_nb$nb_par_kg*donnee_cpue_nb$mean
      
      donnee_cpue_nb
      
    }
    
  }

  func_calibre_taille <- function(zone,village,date,engin,sous_zone){
    corecrabe_avec_taille$calibre_de_taille <-cut(corecrabe_avec_taille$taille_de_crabe,
                                               c(0,70,100,250))
    Donnee_filtre <- corecrabe_avec_taille[corecrabe_avec_taille$zone%in%zone, ]
    if("Tou-te-s" %in% engin){
      
    }else{
      Donnee_filtre=Donnee_filtre%>% filter(Engin%in%engin)
    }
    if("Tou-te-s" %in% village){
      if("Tou-te-s" %in% sous_zone){
        
        ##### calcul cpue en nombre par sortie par calibre_de_taille ####
        Donnee_groupe <- Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,id_enquete,calibre_de_taille)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupex <- Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,id_enquete)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupe1<-Donnee_groupe%>%
          dplyr::group_by(zone,Mois,Engin)%>%
          dplyr::summarise(capture_kg=sum(capture_kg))
        
        Donnee_groupe2 <-Donnee_filtre %>%
          dplyr::group_by(zone,Mois,Engin,calibre_de_taille)%>%
          dplyr::summarise(Nombre=sum(Eff_mesure))
        
        Donnee_groupe3 <- left_join(Donnee_groupe1,Donnee_groupe2, by=c("zone","Engin","Mois"))
        
        Donnee_groupe3$nb_par_kg <-Donnee_groupe3$Nombre/Donnee_groupe3$capture_kg
        summary(Donnee_groupe3$nb_par_kg)
        names(Donnee_groupe3)[names(Donnee_groupe3)=='zone'] <- 'Zone'
        
        
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Mois,Engin)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        donnee_cpue_nb<-left_join(Donnee_groupe3,T_glm,by=c("Zone","Mois","Engin"))
        donnee_cpue_nb$nb_par_sortie=donnee_cpue_nb$nb_par_kg*donnee_cpue_nb$mean
        
        donnee_cpue_nb
        
        
      }else{
        
        ##### calcul cpue en nombre par sortie par calibre_de_taille ####
        Donnee_groupe <- Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,id_enquete,calibre_de_taille)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupex <- Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,id_enquete)%>%
          dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
        
        Donnee_groupe1<-Donnee_groupe%>%
          dplyr::group_by(zone,sous_zone,Mois,Engin)%>%
          dplyr::summarise(capture_kg=sum(capture_kg))
        
        Donnee_groupe2 <-Donnee_filtre %>%
          dplyr::group_by(zone,sous_zone,Mois,Engin,calibre_de_taille)%>%
          dplyr::summarise(Nombre=sum(Eff_mesure))
        
        Donnee_groupe3 <- left_join(Donnee_groupe1,Donnee_groupe2, by=c("zone","sous_zone","Engin","Mois"))
        
        Donnee_groupe3$nb_par_kg <-Donnee_groupe3$Nombre/Donnee_groupe3$capture_kg
        summary(Donnee_groupe3$nb_par_kg)
        names(Donnee_groupe3)[names(Donnee_groupe3)=='zone'] <- 'Zone'
        names(Donnee_groupe3)[names(Donnee_groupe3)=='sous_zone'] <- 'Sous_zone'
        
        T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
          dplyr::group_by(Zone,Sous_zone,Mois,Engin)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        donnee_cpue_nb<-left_join(Donnee_groupe3,T_glm,by=c("Zone","Sous_zone","Mois","Engin"))
        donnee_cpue_nb$nb_par_sortie=donnee_cpue_nb$nb_par_kg*donnee_cpue_nb$mean
        
        donnee_cpue_nb
      }
    }else{
      
      ##### calcul cpue en nombre par sortie par calibre_de_taille ####
      Donnee_groupe <- Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,id_enquete,calibre_de_taille)%>%
        dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
      
      Donnee_groupex <- Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,id_enquete)%>%
        dplyr::summarise(capture_kg=mean(poids_total_de_l_echantillon))
      
      Donnee_groupe1<-Donnee_groupe%>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin)%>%
        dplyr::summarise(capture_kg=sum(capture_kg))
      
      Donnee_groupe2 <-Donnee_filtre %>%
        dplyr::group_by(zone,sous_zone,village,Mois,Engin,calibre_de_taille)%>%
        dplyr::summarise(Nombre=sum(Eff_mesure))
      
      Donnee_groupe3 <- left_join(Donnee_groupe1,Donnee_groupe2, by=c("zone","sous_zone","village","Engin","Mois"))
      
      Donnee_groupe3$nb_par_kg <-Donnee_groupe3$Nombre/Donnee_groupe3$capture_kg
      summary(Donnee_groupe3$nb_par_kg)
      names(Donnee_groupe3)[names(Donnee_groupe3)=='zone'] <- 'Zone'
      names(Donnee_groupe3)[names(Donnee_groupe3)=='sous_zone'] <- 'Sous_zone'
      names(Donnee_groupe3)[names(Donnee_groupe3)=='village'] <- 'Village'
      
      T_glm <- func_cpue(zone,village,date,engin,sous_zone)%>%
        dplyr::group_by(Zone,Sous_zone,Village,Mois,Engin)%>%
        dplyr::summarise(mean=mean(cpue_predite))
      
      donnee_cpue_nb<-left_join(Donnee_groupe3,T_glm,by=c("Zone","Sous_zone","Village","Mois","Engin"))
      donnee_cpue_nb$nb_par_sortie=donnee_cpue_nb$nb_par_kg*donnee_cpue_nb$mean
      
      donnee_cpue_nb
      
    }
    
  }
  #func_dist <- function(zone,village,date,engin,sous_zone,dest,sex,rng){
    
  #  Donnee = Donnee_taille
    
   # if("Tou-te-s" %in% village){
    #  if("Tou-te-s" %in% sous_zone){
     #   Donnee <- Donnee%>%filter(zone%in%zone)
     # }else{
     #   Donnee <- Donnee%>%filter(sous_zone%in%sous_zone)
     # }
    #}else{
    #  Donnee <- Donnee%>%filter(village%in%Village)
    #}
    
   # Donnee%>% 
  #    filter(date >= date[1] & date <= date[2]) %>%
 #     filter(taille >= rng[1] & taille <= rng[2]) %>% 
  #    filter(sexe %in% c(sex)) %>% 
    #  filter(destination %in% c(dest))
  #}
  
  func_download <- function(donnee,zone,sous_zone,village){
    jeu_de_donnee <- NULL
    if(donnee == 1){
      jeu_de_donnee <- corecrabe_avec_taille
    }else if(donnee == 2){
      jeu_de_donnee <- corecrabe_sans_taille
    }else if(donnee == 3){
      jeu_de_donnee <- recensement_complet
    }
    
    jeu_de_donnee <- jeu_de_donnee[jeu_de_donnee$zone %in% zone,]
    jeu_de_donnee <- jeu_de_donnee[jeu_de_donnee$sous_zone %in% sous_zone,]
    
    jeu_de_donnee
  }


  ############################################# <- GRAPHE OUTPUT (RESULTATS) -> #############################################################
  
output$prix <- renderPlotly({
    prix_moyenne <- func_prix(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur)
    if("Tou-te-s" %in% input$village_valeur){
      if("Tou-te-s" %in% input$sous_zone_valeur){
        p <- ggplot(prix_moyenne, aes(x = Mois, y =prix_collecte1, group=Zone, color=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Prix à la collecte (Ariary)",font.axis=88,face="bold")+ylim(0,10000)
      }else{
        p <- ggplot(prix_moyenne, aes(x = Mois, y =prix_collecte1, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Prix à la collecte (Ariary)",font.axis=88,face="bold")+ylim(0,10000)
      }
    }else{
      p <- ggplot(prix_moyenne, aes(x = Mois, y =prix_collecte1, group=Village, color=Village)) +
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="Prix à la collecte (Ariary)",font.axis=88,face="bold")+ylim(0,10000)
    }
    
  }) %>% bindCache(input$zone_valeur,input$sous_zone_valeur,input$village_valeur,input$engin_valeur)

  
  output$download_prix_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_valeur){
        if("Tou-te-s" %in% input$sous_zone_valeur){
          if(length(input$zone_valeur) > 1){
            paste0("corecrabe_prix_au_pecheur_par_zone_", Sys.Date(),"_",input$engin_valeur, ".csv")
          }else{
            paste0("corecrabe_prix_au_pecheur_par_zone_", Sys.Date(), "_", input$zone_valeur,"_",input$engin_valeur, ".csv")
          }
        }else{
          if(length(input$sous_zone_valeur) > 1){
            paste0("corecrabe_prix_au_pecheur_par_sous_zone_", Sys.Date(),"_",input$engin_valeur, ".csv")
          }else{
            paste0("corecrabe_prix_au_pecheur_par_sous_zone_", Sys.Date(), "_", input$sous_zone_valeur,"_",input$engin_valeur, ".csv")
          }
        }
      }else{
        if(length(input$village_valeur) > 1){
          paste0("corecrabe_prix_au_pecheur_par_village_", Sys.Date(),"_",input$engin_valeur, ".csv")
        }else{
          paste0("corecrabe_prix_au_pecheur_par_village_", Sys.Date(), "_", input$village_valeur,"_",input$engin_valeur, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        
        prix_moyenne <- func_prix(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur)
        
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(prix_moyenne, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )


  output$taux <- renderPlotly({
    
    if("Tou-te-s" %in% input$village){
      if("Tou-te-s" %in% input$sous_zone){
        taux_filtre <- func_taux(input$zone,input$village,input$date,input$engin,input$sous_zone)%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(tac=mean(tac))
        
        p <- ggplot(taux_filtre, aes(x = Mois, y =tac, group=Zone, color=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="taux d'activité (%)",font.axis=88,face="bold")+ylim(0,1)
      }else{
        taux_filtre <- func_taux(input$zone,input$village,input$date,input$engin,input$sous_zone)%>%
          dplyr::group_by(Zone,Mois,Sous_zone)%>%
          dplyr::summarise(tac=mean(tac))
        
        p <- ggplot(taux_filtre, aes(x = Mois, y =tac, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="taux d'activité (%)",font.axis=88,face="bold")+ylim(0,1)
      }
    }else{
      taux_filtre <- func_taux(input$zone,input$village,input$date,input$engin,input$sous_zone)%>%
        dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
        dplyr::summarise(tac=mean(tac))
      
      p <- ggplot(taux_filtre, aes(x = Mois, y =tac, group=Village, color=Village)) +
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="taux d'activité (%)",font.axis=88,face="bold")+ylim(0,1)
    }
    suppressWarnings(ggplotly(p))
  })
  
  
  output$download_taux_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village){
        if("Tou-te-s" %in% input$sous_zone){
          if(length(input$zone) > 1){
            paste0("corecrabe_frequence_activite_par_zone_", Sys.Date(),"_",input$engin, ".csv")
          }else{
            paste0("corecrabe_frequence_activite__par_zone_", Sys.Date(), "_", input$zone,"_",input$engin, ".csv")
          }
        }else{
          if(length(input$sous_zone) > 1){
            paste0("corecrabe_frequence_activite_par_sous_zone_", Sys.Date(),"_",input$engin, ".csv")
          }else{
            paste0("corecrabe_frequence_activite_par_sous_zone_", Sys.Date(), "_", input$sous_zone,"_",input$engin, ".csv")
          }
        }
      }else{
        if(length(input$village) > 1){
          paste0("corecrabe_frequence_activite_par_village_", Sys.Date(),"_",input$engin, ".csv")
        }else{
          paste0("corecrabe_frequence_activite_par_village_", Sys.Date(), "_", input$village,"_",input$engin, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        taux_filtre <- func_taux(input$zone,input$village,input$date,input$engin,input$sous_zone)
        if("Tou-te-s" %in% input$village){
          if("Tou-te-s" %in% input$sous_zone){
            taux_filtre <- taux_filtre%>%
              dplyr::group_by(Zone,Mois)%>%
              dplyr::summarise(tac=mean(tac))
          }else{
            taux_filtre <- taux_filtre%>%
              dplyr::group_by(Zone,Sous_zone,Mois)%>%
              dplyr::summarise(tac=mean(tac))
          }
        }else{
          taux_filtre <- taux_filtre%>%
            dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
            dplyr::summarise(tac=mean(tac))
        }
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(taux_filtre, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  
  
  output$pecheur <- renderPlotly({
    
    if("Tou-te-s" %in% input$village){
      if("Tou-te-s" %in% input$sous_zone){
        recensement_corecrabe <-func_pecheur(input$zone,input$village,input$date,input$engin,input$sous_zone)%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
        
        recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
        ####calcul du taux d'actifs par mois
        recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
        
        p <- ggplot(recensement_corecrabe, aes(x = Mois, y =actifs, group=Zone, color=Zone))+
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Pêcheurs actifs (nombre)",font.axis=88,face="bold")+ylim(0,max(recensement_corecrabe$actifs))
      }else{
        recensement_corecrabe <-func_pecheur(input$zone,input$village,input$date,input$engin,input$sous_zone)%>%
          dplyr::group_by(Zone,Sous_zone,Mois)%>%
          dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
        
        
        recensement_corecrabe <- recensement_corecrabe%>%drop_na(Sous_zone)
        recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
        ####calcul du taux d'actifs par mois
        recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
        
        p <- ggplot(recensement_corecrabe, aes(x = Mois, y =actifs, group=Sous_zone, color=Sous_zone))+
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Pêcheurs actifs (nombre)",font.axis=88,face="bold")+ylim(0,max(recensement_corecrabe$actifs))
      }
    }else{
      recensement_corecrabe <-func_pecheur(input$zone,input$village,input$date,input$engin,input$sous_zone)%>%
        dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
        dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
      
      recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
      ####calcul du taux d'actifs par mois
      recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
      
      p <- ggplot(recensement_corecrabe, aes(x = Mois, y =actifs, group=Village, color=Village))+
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="Pêcheurs actifs (nombre)",font.axis=88,face="bold")+ylim(0,max(recensement_corecrabe$actifs))
    }
    suppressWarnings(ggplotly(p))
  }) %>% bindCache(input$zone,input$sous_zone,input$village,input$engin)

  output$download_pecheur_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village){
        if("Tou-te-s" %in% input$sous_zone){
          if(length(input$zone) > 1){
            paste0("corecrabe_pecheur_actif_par_zone_", Sys.Date(),"_",input$engin, ".csv")
          }else{
            paste0("corecrabe_pecheur_actif__par_zone_", Sys.Date(), "_", input$zone,"_",input$engin, ".csv")
          }
        }else{
          if(length(input$sous_zone) > 1){
            paste0("corecrabe_pecheur_actif_par_sous_zone_", Sys.Date(),"_",input$engin, ".csv")
          }else{
            paste0("corecrabe_pecheur_actif_par_sous_zone_", Sys.Date(), "_", input$sous_zone,"_",input$engin, ".csv")
          }
        }
      }else{
        if(length(input$village) > 1){
          paste0("corecrabe_pecheur_actif_par_village_", Sys.Date(),"_",input$engin, ".csv")
        }else{
          paste0("corecrabe_pecheur_actif_par_village_", Sys.Date(), "_", input$village,"_",input$engin, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        recensement_corecrabe <-func_pecheur(input$zone,input$village,input$date,input$engin,input$sous_zone)
        if("Tou-te-s" %in% input$village){
          if("Tou-te-s" %in% input$sous_zone){
            recensement_corecrabe <-recensement_corecrabe%>%
              dplyr::group_by(Zone,Mois)%>%
              dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
            recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
            ####calcul du taux d'actifs par mois
            recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
          }else{
            recensement_corecrabe <-recensement_corecrabe%>%
              dplyr::group_by(Zone,Sous_zone,Mois)%>%
              dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
            recensement_corecrabe <- recensement_corecrabe%>%drop_na(Sous_zone)
            recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
            ####calcul du taux d'actifs par mois
            recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
          }
        }else{
          recensement_corecrabe <-recensement_corecrabe%>%
            dplyr::group_by(Zone,Sous_zone,Village,Mois)%>%
            dplyr::summarise(pecheur_total=sum(pecheur_total,na.rm=TRUE),coef=mean(coeff_considere,na.rm=TRUE))
          recensement_corecrabe <- recensement_corecrabe%>%drop_na(coef)
          ####calcul du taux d'actifs par mois
          recensement_corecrabe$actifs <- recensement_corecrabe$pecheur_total*recensement_corecrabe$coef
        }
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(recensement_corecrabe, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  output$cpue <- renderPlotly({
    if("Tou-te-s" %in% input$village_cpue){
      if("Tou-te-s" %in% input$sous_zone_cpue){
        
        T_glm <- func_cpue(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        p <- ggplot(T_glm, aes(x = Mois, y =mean, group=Zone, color=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Indice de biomasse (kg)",font.axis=88,face="bold")+ylim(0,15)
      }else{
        T_glm <- func_cpue(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)%>%
          dplyr::group_by(Zone,Mois,Sous_zone)%>%
          dplyr::summarise(mean=mean(cpue_predite))
        
        p <- ggplot(T_glm, aes(x = Mois, y =mean, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Indice de biomasse (kg)",font.axis=88,face="bold")+ylim(0,15)
      }
    }else{
      T_glm <- func_cpue(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)%>%
        dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
        dplyr::summarise(mean=mean(cpue_predite))
      
      p <- ggplot(T_glm, aes(x = Mois, y =mean, group=Village, color=Village)) +
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="Indice de biomasse (kg)",font.axis=88,face="bold")+ylim(0,15)
    }
    suppressWarnings(ggplotly(p))
  })
  
  
  
  
  output$download_cpue_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_cpue){
        if("Tou-te-s" %in% input$sous_zone_cpue){
          if(length(input$zone_cpue) > 1){
            paste0("corecrabe_cpue_standardise_par_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_standardise_par_zone_", Sys.Date(), "_", input$zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }else{
          if(length(input$sous_zone_cpue) > 1){
            paste0("corecrabe_cpue_standardise_par_sous_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_standardise_par_sous_zone_", Sys.Date(), "_", input$sous_zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }
      }else{
        if(length(input$village_cpue) > 1){
          paste0("corecrabe_cpue_standardise_par_village_", Sys.Date(),"_",input$engin_cpue, ".csv")
        }else{
          paste0("corecrabe_cpue_standardise_par_village_", Sys.Date(), "_", input$village_cpue,"_",input$engin_cpue, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        T_glm <- func_cpue(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
        if("Tou-te-s" %in% input$village_cpue){
          if("Tou-te-s" %in% input$sous_zone_cpue){
            
            T_glm <- T_glm%>%
              dplyr::group_by(Zone,Mois)%>%
              dplyr::summarise(mean=mean(cpue_predite))
          }else{
            T_glm <- T_glm%>%
              dplyr::group_by(Zone,Mois,Sous_zone)%>%
              dplyr::summarise(mean=mean(cpue_predite))
          }
        }else{
          T_glm <- T_glm%>%
            dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
            dplyr::summarise(mean=mean(cpue_predite))
        }
        
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(T_glm, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )


  output$rendement <- renderPlotly({
    if("Tou-te-s" %in% input$village_dist){
      if("Tou-te-s" %in% input$sous_zone_dist){
        
        T_glm <- func_rendement(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)%>%
          dplyr::group_by(Zone,Mois)%>%
          dplyr::summarise(mean=mean(cpue_sortie))
        
        p <- ggplot(T_glm, aes(x = Mois, y =mean, group=Zone, color=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")+ylim(0,30)
      }else{
        T_glm <- func_rendement(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)%>%
          dplyr::group_by(Zone,Mois,Sous_zone)%>%
          dplyr::summarise(mean=mean(cpue_sortie))
        
        p <- ggplot(T_glm, aes(x = Mois, y =mean, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")+ylim(0,30)
      }
    }else{
      T_glm <- func_rendement(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)%>%
        dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
        dplyr::summarise(mean=mean(cpue_sortie))
      
      p <- ggplot(T_glm, aes(x = Mois, y =mean, group=Village, color=Village)) +
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="",font.axis=88,face="bold")+ylim(0,30)
    }
    suppressWarnings(ggplotly(p))
  })
  
  
  
  
  output$download_rend_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          if(length(input$zone_dist) > 1){
            paste0("corecrabe_rendement_par_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_rendement_par_zone_", Sys.Date(), "_", input$zone_dist,"_",input$engin_dist, ".csv")
          }
        }else{
          if(length(input$sous_zone_dist) > 1){
            paste0("corecrabe_rendement_par_sous_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_rendement_par_sous_zone_", Sys.Date(), "_", input$sous_zone_dist,"_",input$engin_dist, ".csv")
          }
        }
      }else{
        if(length(input$village_dist) > 1){
          paste0("corecrabe_rendement_par_village_", Sys.Date(),"_",input$engin_dist, ".csv")
        }else{
          paste0("corecrabe_rendement_par_village_", Sys.Date(), "_", input$village_dist,"_",input$engin_dist, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        T_glm <- func_rendement(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)
        if("Tou-te-s" %in% input$village_dist){
          if("Tou-te-s" %in% input$sous_zone_dist){
            
            T_glm <- T_glm%>%
              dplyr::group_by(Zone,Mois)%>%
              dplyr::summarise(mean=mean(cpue_sortie))
          }else{
            T_glm <- T_glm%>%
              dplyr::group_by(Zone,Mois,Sous_zone)%>%
              dplyr::summarise(mean=mean(cpue_sortie))
          }
        }else{
          T_glm <- T_glm%>%
            dplyr::group_by(Zone,Mois,Sous_zone,Village)%>%
            dplyr::summarise(mean=mean(cpue_sortie))
        }
        
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(T_glm, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  output$effort_par_sortie <- renderPlotly({
    
    if("Tou-te-s" %in% input$village){
      
      if("Tou-te-s" %in% input$sous_zone){
        
        p <- ggplot(func_effort(input$zone,input$village,input$date,input$engin,input$sous_zone), aes(x = Mois, y =effort, group=Zone, color=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Effort nominal (nombre de sorties)",font.axis=88,face="bold")
        
      }else{
        
        
        p <- ggplot(func_effort(input$zone,input$village,input$date,input$engin,input$sous_zone), aes(x = Mois, y =effort, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Effort nominal (nombre de sorties)",font.axis=88,face="bold")+ylim(0,30000)
        
      }
    }else{
      
      
      p <- ggplot(func_effort(input$zone,input$village,input$date,input$engin,input$sous_zone), aes(x = Mois, y =effort, group=Village, color=Village)) +
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="Effort nominal (nombre de sorties)",font.axis=88,face="bold")+ylim(0,10000)
      
    }
    suppressWarnings(ggplotly(p))
    
  })
  
  output$download_effort_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village){
        if("Tou-te-s" %in% input$sous_zone){
          if(length(input$zone) > 1){
            paste0("corecrabe_effort_par_sortie_par_zone_", Sys.Date(),"_",input$engin, ".csv")
          }else{
            paste0("corecrabe_effort_par_sortie_par_zone_", Sys.Date(), "_", input$zone,"_",input$engin, ".csv")
          }
        }else{
          if(length(input$sous_zone) > 1){
            paste0("corecrabe_effort_par_sortie_par_sous_zone_", Sys.Date(),"_",input$engin, ".csv")
          }else{
            paste0("corecrabe_effort_par_sortie_par_sous_zone_", Sys.Date(), "_", input$sous_zone,"_",input$engin, ".csv")
          }
        }
      }else{
        if(length(input$village) > 1){
          paste0("corecrabe_effort_par_sortie_par_village_", Sys.Date(),"_",input$engin, ".csv")
        }else{
          paste0("corecrabe_effort_par_sortie_par_village_", Sys.Date(), "_", input$village,"_",input$engin, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        effort <- func_effort(input$zone,input$village,input$date,input$engin,input$sous_zone)
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(effort, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  output$cpue_en_nombre <- renderPlotly({
    
    graph <- func_cpue_nombre(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
    if("Tou-te-s" %in% input$village_cpue){
      if("Tou-te-s" %in% input$sous_zone_cpue){
        p <- ggplot(graph, aes(x = Mois, y =nb_global_par_sortie, group=Zone, color=Zone, fill=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")
      }else{
        p <- ggplot(graph, aes(x = Mois, y =nb_global_par_sortie, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")
      }
    }else{
      p <- ggplot(graph, aes(x = Mois, y =nb_global_par_sortie, group=Village, color=Village)) +
        geom_line(aes(color=Village))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="",font.axis=88,face="bold")
    }
    ggplotly(p)
  })
  
  output$download_cpue_nb_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_cpue){
        if("Tou-te-s" %in% input$sous_zone_cpue){
          if(length(input$zone_cpue) > 1){
            paste0("corecrabe_cpue_en_nombre_par_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_en_nombre_par_zone_", Sys.Date(), "_", input$zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }else{
          if(length(input$sous_zone_cpue) > 1){
            paste0("corecrabe_cpue_en_nombre_par_sous_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_en_nombre_par_sous_zone_", Sys.Date(), "_", input$sous_zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }
      }else{
        if(length(input$village_cpue) > 1){
          paste0("corecrabe_cpue_en_nombre_par_village_", Sys.Date(),"_",input$engin_cpue, ".csv")
        }else{
          paste0("corecrabe_cpue_en_nombre_par_village_", Sys.Date(), "_", input$village_cpue,"_",input$engin_cpue, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        cpue_nb <- func_cpue_nombre(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(cpue_nb, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  output$cpue_en_taille <- renderPlotly({
    
    graph <- func_cpue_taille(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
    graph <- graph[graph$classe_de_taille%in%cut(corecrabe_avec_taille$taille_de_crabe, seq(input$classe_taille[1],input$classe_taille[2],by=10)), ]
    if("Tou-te-s" %in% input$village_cpue){
      if("Tou-te-s" %in% input$sous_zone_cpue){
        p <- ggplot(graph, aes(x = Mois, y =nb_par_sortie, group=classe_de_taille, color=classe_de_taille, fill=classe_de_taille)) +
          geom_line(aes(color=classe_de_taille))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")+ facet_wrap(~ Zone)
      }else{
        p <- ggplot(graph, aes(x = Mois, y =nb_par_sortie, group=classe_de_taille, color=classe_de_taille, fill=classe_de_taille)) +
          geom_line(aes(color=classe_de_taille))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")+ facet_wrap(~ Sous_zone)
      }
    }else{
      p <- ggplot(graph, aes(x = Mois, y =nb_par_sortie, group=classe_de_taille, color=classe_de_taille, fill=classe_de_taille)) +
        geom_line(aes(color=classe_de_taille))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="",font.axis=88,face="bold")+ facet_wrap(~ Village)
    }
    ggplotly(p)
  })
  
  
  output$download_cpue_taille_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_cpue){
        if("Tou-te-s" %in% input$sous_zone_cpue){
          if(length(input$zone_cpue) > 1){
            paste0("corecrabe_cpue_par_classe_de_taille_par_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_par_classe_de_taille_par_zone_", Sys.Date(), "_", input$zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }else{
          if(length(input$sous_zone_cpue) > 1){
            paste0("corecrabe_cpue_par_classe_de_taille_par_sous_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_par_classe_de_taille_par_sous_zone_", Sys.Date(), "_", input$sous_zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }
      }else{
        if(length(input$village_cpue) > 1){
          paste0("corecrabe_cpue_par_classe_de_taille_par_village_", Sys.Date(),"_",input$engin_cpue, ".csv")
        }else{
          paste0("corecrabe_cpue_par_classe_de_taille_par_village_", Sys.Date(), "_", input$village_cpue,"_",input$engin_cpue, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        cpue_taille <- func_cpue_taille(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
        cpue_taille <- cpue_taille[cpue_taille$classe_de_taille%in%cut(corecrabe_avec_taille$taille_de_crabe, seq(input$classe_taille[1],input$classe_taille[2],by=10)), ]
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(cpue_taille, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )

  output$cpue_en_cal_taille <- renderPlotly({
    
    graph <- func_calibre_taille(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
    graph <- graph[graph$calibre_de_taille%in%cut(corecrabe_avec_taille$taille_de_crabe, c(0,70,100,250)), ]
    if("Tou-te-s" %in% input$village_cpue){
      if("Tou-te-s" %in% input$sous_zone_cpue){
        p <- ggplot(graph, aes(x = Mois, y =nb_par_sortie, group=calibre_de_taille, color=calibre_de_taille, fill=calibre_de_taille)) +
          geom_line(aes(color=calibre_de_taille))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")+ facet_wrap(~ Zone)
      }else{
        p <- ggplot(graph, aes(x = Mois, y =nb_par_sortie, group=calibre_de_taille, color=calibre_de_taille, fill=calibre_de_taille)) +
          geom_line(aes(color=calibre_de_taille))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="",font.axis=88,face="bold")+ facet_wrap(~ Sous_zone)
      }
    }else{
      p <- ggplot(graph, aes(x = Mois, y =nb_par_sortie, group=calibre_de_taille, color=calibre_de_taille, fill=calibre_de_taille)) +
        geom_line(aes(color=calibre_de_taille))+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="",font.axis=88,face="bold")+ facet_wrap(~ Village)
    }
    ggplotly(p)
  })
  
  
  output$download_cpue_cal_taille_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_cpue){
        if("Tou-te-s" %in% input$sous_zone_cpue){
          if(length(input$zone_cpue) > 1){
            paste0("corecrabe_cpue_par_calibre_de_taille_par_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_par_calibre_de_taille_par_zone_", Sys.Date(), "_", input$zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }else{
          if(length(input$sous_zone_cpue) > 1){
            paste0("corecrabe_cpue_par_calibre_de_taille_par_sous_zone_", Sys.Date(),"_",input$engin_cpue, ".csv")
          }else{
            paste0("corecrabe_cpue_par_calibre_de_taille_par_sous_zone_", Sys.Date(), "_", input$sous_zone_cpue,"_",input$engin_cpue, ".csv")
          }
        }
      }else{
        if(length(input$village_cpue) > 1){
          paste0("corecrabe_cpue_par_calibre_de_taille_par_village_", Sys.Date(),"_",input$engin_cpue, ".csv")
        }else{
          paste0("corecrabe_cpue_par_calibre_de_taille_par_village_", Sys.Date(), "_", input$village_cpue,"_",input$engin_cpue, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        cpue_taille <- func_calibre_taille(input$zone_cpue,input$village_cpue,input$date_cpue,input$engin_cpue,input$sous_zone_cpue)
        cpue_taille <- cpue_taille[cpue_taille$calibre_de_taille%in%cut(corecrabe_avec_taille$taille_de_crabe, c(0,70,100,250)), ]
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(cpue_taille, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  
  output$prod_totale <- renderPlotly({
    
    if("Tou-te-s" %in% input$village_dist){
      
      if("Tou-te-s" %in% input$sous_zone_dist){
        ##production totale
        p<-ggplot(func_prod_totale(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist), aes(x = Mois, y =production/1000, group=Zone, fill=Zone)) +
          geom_bar(stat="identity")+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Captures estimées (t)",font.axis=88,face="bold")+ylim(0,500)
        
      }else{
        ##production totale
        p<-ggplot(func_prod_totale(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist), aes(x = Mois, y =production/1000, group=Sous_zone, fill=Sous_zone)) +
          geom_bar(stat="identity")+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Captures estimées (t)",font.axis=88,face="bold")+ylim(0,500)
      }
    }else{
      p<-ggplot(func_prod_totale(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist), aes(x = Mois, y =production/1000, group=Village, fill=Village)) +
        geom_bar(stat="identity")+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="Captures estimées (t)",font.axis=88,face="bold")+ylim(0,500)
    }
    suppressWarnings(ggplotly(p))
  })
  
  
  output$download_prod_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          if(length(input$zone_dist) > 1){
            paste0("corecrabe_production_totale_par_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_production_totale_par_zone_", Sys.Date(), "_", input$zone_dist,"_",input$engin_dist, ".csv")
          }
        }else{
          if(length(input$sous_zone_dist) > 1){
            paste0("corecrabe_production_totale_par_sous_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_production_totale_par_sous_zone_", Sys.Date(), "_", input$sous_zone_dist,"_",input$engin_dist, ".csv")
          }
        }
      }else{
        if(length(input$village_dist) > 1){
          paste0("corecrabe_production_totale_par_village_", Sys.Date(),"_",input$engin_dist, ".csv")
        }else{
          paste0("corecrabe_production_totale_par_village_", Sys.Date(), "_", input$village_dist,"_",input$engin_dist, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        prod_tot<-func_prod_totale(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)
        
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(prod_tot, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
  
  output$prod_cumule <- renderPlotly({
    
    if("Tou-te-s" %in% input$village_dist){
      
      if("Tou-te-s" %in% input$sous_zone_dist){
        
        ##production totale
        p<-ggplot(func_prod_cumule(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist))+ 
          geom_bar(aes(x = Mois, y = cumul/1000,group=Zone, fill=Zone), position = "stack", stat = "identity")+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Captures estimées cumulées (t)",font.axis=88,face="bold")+ylim(0,5000)
        
      }else{
        
        ##production totale
        p<-ggplot(func_prod_cumule(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist))+ 
          geom_bar(aes(x = Mois, y = cumul/1000,group=Sous_zone, fill=Sous_zone), position = "stack", stat = "identity")+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Captures estimées cumulées (t)",font.axis=88,face="bold")+ylim(0,5000)
      }
    }else{
      
      ##production totale
      p<-ggplot(func_prod_cumule(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist))+ 
        geom_bar(aes(x = Mois, y = cumul/1000,group=Village, fill=Village), position = "stack", stat = "identity")+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        labs(x="",y="Captures estimées cumulées (t)",font.axis=88,face="bold")+ylim(0,5000)
    }
    suppressWarnings(ggplotly(p))
  })
  
  
  output$download_prod_cum_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          if(length(input$zone_dist) > 1){
            paste0("corecrabe_production_cumule_par_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_production_cumule_par_zone_", Sys.Date(), "_", input$zone_dist,"_",input$engin_dist, ".csv")
          }
        }else{
          if(length(input$sous_zone_dist) > 1){
            paste0("corecrabe_production_cumule_par_sous_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_production_cumule_par_sous_zone_", Sys.Date(), "_", input$sous_zone_dist,"_",input$engin_dist, ".csv")
          }
        }
      }else{
        if(length(input$village_dist) > 1){
          paste0("corecrabe_production_cumule_par_village_", Sys.Date(),"_",input$engin_dist, ".csv")
        }else{
          paste0("corecrabe_production_cumule_par_village_", Sys.Date(), "_", input$village_dist,"_",input$engin_dist, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        prod_cumule<-func_prod_cumule(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(prod_cumule, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )


  output$valeur_plot <- renderPlotly({
    if("Tou-te-s" %in% input$village_valeur){
      if("Tou-te-s" %in% input$sous_zone_valeur){
        p <- ggplot(func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur), aes(x = Mois, y =valeur/1000000, group=Zone, fill=Zone,color=Zone)) +
          geom_line(aes(color=Zone))+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Valeur Totale en Million d'Ariary",font.axis=88,face="bold")
      }else{
        p <- ggplot(func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur), aes(x = Mois, y =valeur/1000000, group=Sous_zone, fill=Sous_zone,color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Valeur Totale en Million d'Ariary",font.axis=88,face="bold")
      }
    }else{
      p <- ggplot(func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur), aes(x = Mois, y =valeur/1000000, group=Village, fill=Village,color=Village)) +
          geom_line(aes(color=Village))+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Valeur Totale en Million d'Ariary",font.axis=88,face="bold")
    }
  })
  
  output$revenu_plot <- renderPlotly({
    if("Tou-te-s" %in% input$village_valeur){
      if("Tou-te-s" %in% input$sous_zone_valeur){
        p <- ggplot(func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur), aes(x = Mois, y =Revenu_pecheur, group=Zone, fill=Zone,color=Zone)) +
          geom_line(aes(color=Zone))+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Revenu moyen par sortie (Ariary)",font.axis=88,face="bold")+ylim(0,30000)
      }else{
        p <- ggplot(func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur), aes(x = Mois, y =Revenu_pecheur, group=Sous_zone, fill=Sous_zone,color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+geom_line(size = 1)+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Revenu moyen par sortie (Ariary)",font.axis=88,face="bold")+ylim(0,30000)
      }
    }else{
      p <- ggplot(func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur), aes(x = Mois, y =Revenu_pecheur, group=Village, fill=Village,color=Village)) +
          geom_line(aes(color=Village))+geom_line(size = 1)+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          labs(x="",y="Revenu moyen par sortie (Ariary)",font.axis=88,face="bold")+ylim(0,30000)
    }
  })
  
  output$download_revenu_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_valeur){
        if("Tou-te-s" %in% input$sous_zone_valeur){
          if(length(input$zone_valeur) > 1){
            paste0("corecrabe_revenu_et_valeur_par_zone_", Sys.Date(),"_",input$engin_valeur, ".csv")
          }else{
            paste0("corecrabe_revenu_et_valeur_par_zone_", Sys.Date(), "_", input$zone_valeur,"_",input$engin_valeur, ".csv")
          }
        }else{
          if(length(input$sous_zone_valeur) > 1){
            paste0("corecrabe_revenu_et_valeur_par_sous_zone_", Sys.Date(),"_",input$engin_valeur, ".csv")
          }else{
            paste0("corecrabe_revenu_et_valeur_par_sous_zone_", Sys.Date(), "_", input$sous_zone_valeur,"_",input$engin_valeur, ".csv")
          }
        }
      }else{
        if(length(input$village_valeur) > 1){
          paste0("corecrabe_revenu_et_valeur_par_village_", Sys.Date(),"_",input$engin_valeur, ".csv")
        }else{
          paste0("corecrabe_revenu_et_valeur_par_village_", Sys.Date(), "_", input$village_valeur,"_",input$engin_valeur, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        valeur<-func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur)
        
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(valeur, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )

  output$download_valeur_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_valeur){
        if("Tou-te-s" %in% input$sous_zone_valeur){
          if(length(input$zone_valeur) > 1){
            paste0("corecrabe_revenu_et_valeur_par_zone_", Sys.Date(),"_",input$engin_valeur, ".csv")
          }else{
            paste0("corecrabe_revenu_et_valeur_par_zone_", Sys.Date(), "_", input$zone_valeur,"_",input$engin_valeur, ".csv")
          }
        }else{
          if(length(input$sous_zone_valeur) > 1){
            paste0("corecrabe_revenu_et_valeur_par_sous_zone_", Sys.Date(),"_",input$engin_valeur, ".csv")
          }else{
            paste0("corecrabe_revenu_et_valeur_par_sous_zone_", Sys.Date(), "_", input$sous_zone_valeur,"_",input$engin_valeur, ".csv")
          }
        }
      }else{
        if(length(input$village_valeur) > 1){
          paste0("corecrabe_revenu_et_valeur_par_village_", Sys.Date(),"_",input$engin_valeur, ".csv")
        }else{
          paste0("corecrabe_revenu_et_valeur_par_village_", Sys.Date(), "_", input$village_valeur,"_",input$engin_valeur, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        valeur<-func_valeur(input$zone_valeur,input$village_valeur,input$date_valeur,input$engin_valeur,input$sous_zone_valeur)
        
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(valeur, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )




  output$download_prod_an_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          if(length(input$zone_dist) > 1){
            paste0("corecrabe_production_annuelle_par_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_production_annuelle_par_zone_", Sys.Date(), "_", input$zone_dist,"_",input$engin_dist, ".csv")
          }
        }else{
          if(length(input$sous_zone_dist) > 1){
            paste0("corecrabe_production_annuelle_par_sous_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_production_annuelle_par_sous_zone_", Sys.Date(), "_", input$sous_zone_dist,"_",input$engin_dist, ".csv")
          }
        }
      }else{
        if(length(input$village_dist) > 1){
          paste0("corecrabe_production_annuelle_par_village_", Sys.Date(),"_",input$engin_dist, ".csv")
        }else{
          paste0("corecrabe_production_annuelle_par_village_", Sys.Date(), "_", input$village_dist,"_",input$engin_dist, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        prod_annuel<-func_prod_cumule(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)

        annee2021 <- filter(prod_annuel,((annee==2021 & Mois != format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2020/12/01",  format="%Y/%m/%d"), "%h %y")))
        annee2022 <- filter(prod_annuel,((annee==2022 & Mois != format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")))
        annee2023 <- filter(prod_annuel,((annee==2023 & Mois != format(as.Date("2023/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")))

    if("Tou-te-s" %in% input$village_dist){
      
      if("Tou-te-s" %in% input$sous_zone_dist){
        prod_2021 <- annee2021%>%
        dplyr::group_by(Zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2021$annee<-2021
        prod_2022 <- annee2022%>%
        dplyr::group_by(Zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2022$annee<-2022
        prod_2023 <- annee2023%>%
        dplyr::group_by(Zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2023$annee<-2023
      }else{
        prod_2021 <- annee2021%>%
        dplyr::group_by(Sous_zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2021$annee<-2021
        prod_2022 <- annee2022%>%
        dplyr::group_by(Sous_zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2022$annee<-2022
        prod_2023 <- annee2023%>%
        dplyr::group_by(Sous_zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2023$annee<-2023
      }
    }else{
      prod_2021 <- annee2021%>%
        dplyr::group_by(Village)%>%
        dplyr::summarise(production=max(cumul))
        prod_2021$annee<-2021
        prod_2022 <- annee2022%>%
        dplyr::group_by(Village)%>%
        dplyr::summarise(production=max(cumul))
        prod_2022$annee<-2022
        prod_2023 <- annee2023%>%
        dplyr::group_by(Village)%>%
        dplyr::summarise(production=max(cumul))
        prod_2023$annee<-2023
    }

    production <- rbind(prod_2021,prod_2022,prod_2023)

        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(production, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )

  output$prod_ann_plot <- renderPlotly({

    prod_annuel <- func_prod_cumule(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)

    annee2021 <- filter(prod_annuel,((annee==2021 & Mois != format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2020/12/01",  format="%Y/%m/%d"), "%h %y")))
    annee2022 <- filter(prod_annuel,((annee==2022 & Mois != format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2021/12/01",  format="%Y/%m/%d"), "%h %y")))
    annee2023 <- filter(prod_annuel,((annee==2023 & Mois != format(as.Date("2023/12/01",  format="%Y/%m/%d"), "%h %y")) || Mois == format(as.Date("2022/12/01",  format="%Y/%m/%d"), "%h %y")))

    


    if("Tou-te-s" %in% input$village_dist){
      
      if("Tou-te-s" %in% input$sous_zone_dist){
        
        prod_2021 <- annee2021%>%
        dplyr::group_by(Zone)%>%
        dplyr::summarise(production=max(cumul))

        prod_2021$annee<-2021
        prod_2022 <- annee2022%>%
        dplyr::group_by(Zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2022$annee<-2022
        prod_2023 <- annee2023%>%
        dplyr::group_by(Zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2023$annee<-2023

        production <- rbind(prod_2021,prod_2022,prod_2023)
        


      p <- ggplot(production, aes(x = factor(annee), y = round(production/1000, digits = 0), fill = Zone)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Année", y = "Production estimée annuelle (t)")
      }else{
        prod_2021 <- annee2021%>%
        dplyr::group_by(Sous_zone)%>%
        dplyr::summarise(production=max(cumul))

        prod_2021$annee<-2021
        prod_2022 <- annee2022%>%
        dplyr::group_by(Sous_zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2022$annee<-2022
        prod_2023 <- annee2023%>%
        dplyr::group_by(Sous_zone)%>%
        dplyr::summarise(production=max(cumul))
        prod_2023$annee<-2023

        production <- rbind(prod_2021,prod_2022,prod_2023)


      p <- ggplot(production, aes(x = factor(annee), y = round(production/1000, digits = 0), fill = Sous_zone)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Année", y = "Production estimée annuelle (t)")
      }
    }else{
      prod_2021 <- annee2021%>%
        dplyr::group_by(Village)%>%
        dplyr::summarise(production=max(cumul))

        prod_2021$annee<-2021
        prod_2022 <- annee2022%>%
        dplyr::group_by(Village)%>%
        dplyr::summarise(production=max(cumul))
        prod_2022$annee<-2022
        prod_2023 <- annee2023%>%
        dplyr::group_by(Village)%>%
        dplyr::summarise(production=max(cumul))
        prod_2023$annee<-2023

        production <- rbind(prod_2021,prod_2022,prod_2023)


      p <- ggplot(production, aes(x = factor(annee), y = round(production/1000, digits = 0), fill = Village)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Année", y = "Production estimée annuelle (t)")
    }
    suppressWarnings(ggplotly(p))

  }
  )
  
  
  output$table_prod_carte <- renderTable({
    tableau_production <-read_csv("./production_annuelle_zone.csv")
    names(tableau_production)[names(tableau_production) == 'production'] <- 'Production (kg)'  
    names(tableau_production)[names(tableau_production) == 'production_estimee'] <- 'Production annuelle estimée (t)'  
    names(tableau_production)[names(tableau_production) == 'surface'] <- 'Surface de Mangrove (km²)' 
    names(tableau_production)[names(tableau_production) == 'prod_par_km'] <- 'Production annuelle par km² (t/km²)'
    tableau_production <- tableau_production[, c("Zone", "Production annuelle estimée (t)","Surface de Mangrove (km²)","Production annuelle par km² (t/km²)")]
    tableau_production
  })%>% bindCache(infos_geo)

  # output$pecheur_total_carte <- renderTable({
  #   df <- recensement_complet%>%
  #     dplyr::group_by(zone,engin,village)%>%
  #     dplyr::summarise(nb_pecheur=mean(pecheur_total))
    
  #   df2 <- df%>%
  #     dplyr::group_by(zone,engin)%>%
  #     dplyr::summarise(nb_pecheur=as.integer(round(sum(nb_pecheur),0 )))


    

  #   df2=subset(df2, df2$nb_pecheur>10)

  #   names(df2)[names(df2) == 'nb_pecheur'] <- 'Nombre de pêcheurs total'
  #   df2=subset(df2, df2$zone!="Maintirano")
    
  #   df2
  # })
  
###################################################################################################
#  output$table_cpue_standard <- DT::renderDataTable({                                            #
#    ######creer un data frame groupé par engin et par mois de la zone                            #
#    T_Zone_glm <-cpue_standardise()%>%                                                           #
#      dplyr::group_by(zone,sous_zone,Mois,Engin)%>%                                              #
#      dplyr::summarise(mean=mean(cpue_predite))                                                  #
#                                                                                                 #
#     names(T_Zone_glm)[names(T_Zone_glm) == 'sous_zone'] <- 'Sous_zone'                          #
#    names(T_Zone_glm)[names(T_Zone_glm) == 'zone'] <- 'Zone'                                     #
#                                                                                                 #
#    T_Zone_glm=T_Zone_glm%>% filter(Engin%in%input$engin_cpue)                                   #
#    T_Zone_glm=T_Zone_glm%>% filter(Sous_zone%in%input$sous_zone_cpue)                           #
#    df <- T_Zone_glm                                                                             #
#    DT::datatable(df)                                                                            #
#  })                                                                                             #
###################################################################################################  
  
 
  
  
  
  ####################################################### <- CARTES OUTPUT -> ###############################################################
  output$map <- renderLeaflet({
    #leaflet() %>% addTiles() %>% addPolygons(data= projRegion,stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    #color = 'gray', group = 'region'
    #)
   
   
    leaflet() %>%
      setView( lng = 44.3957836, lat = -18.8442811, zoom = 6 )%>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Fond de carte")%>%
      addPolygons(data = projMada, color = 'black', fillOpacity = 0,  weight = 1) %>%
      addPolygons(data = projRegion[projRegion$NOM %in% c("Atsimo Andrefana", "Boeny", "Menabe","Diana"), ],weight = 2,
                  opacity = 1,
                  fillColor = projRegion$NOM,
                  dashArray = "3",
                  fillOpacity = 0.02,
                  highlight = highlightOptions(weight = 4, color = "red", fillOpacity = 0.1, bringToFront = FALSE),
                  label = ~NOM,
                  layerId = ~NOM,
                  popup = ~as.character(NOM),
                  group = "Zone Corecrabe"
      )%>%
      addPolygons(data = projMangrove, color = 'green',weight = 5,fillOpacity = 1, group = "Mangroves")%>%





      
      #addPolygons(data = projSous_zone[projSous_zone$Type == "fluviale",],weight = 3,
      #           opacity = 2,
      #           color = "blue",
      #           dashArray = "3",
      #           fillOpacity = 0,group = "Sous_zones de type fluvial"
      #)%>%
      #addPolygons(data = projSous_zone[projSous_zone$Type == "cotiere",],weight = 3,
      #           opacity = 2,
      #           color = "red",
      #           dashArray = "3",
      #           fillOpacity = 0,group = "Sous_zones de type cotiere"
      #)%>%
      #addCircles(data= filter(point_zone,nom != "Maintirano"),weight = 0, popup = ~as.character(nom), group = "Zone Corecrabe")%>%
      #addCircles(data= filter(point_zone, nom != "Maintirano"),weight = 0, label = ~as.character(nom),labelOptions = labelOptions(noHide = T,direction = "right", offset = c(20,0) ))%>%
      addCircles(data= filter(infos_geo,!is.na(lng)),opacity = 1, radius = ~pecheur*50, color = 'black',  group = 'Nombre de pêcheurs par village', popup = ~as.character(
        paste(sep = "<br/>",
              paste(sep = " ","VILLAGE :",village),
              paste(sep = " ","NOMBRE DE PECHEUR :",pecheur),
              paste(sep = " ","SUIVI :",suivi)
              )
        ))%>%
      addCircles(data= filter(production_geo,!is.na(lng)),opacity = 1, radius = ~production/13, color = 'red',  group = 'Production annuelle par village', popup = ~as.character(
        paste(sep = "<br/>",
              paste(sep = " ","VILLAGE :",village),
              paste(sep = " ","PRODUCTION ANNUELLE :",production_estimee,"t"),
              paste(sep = " ","SUIVI :",suivi)
              )
        ),
        
        label = ~as.character(paste(sep = " ","VILLAGE :",village," / ", "Cliquez pour voir les détails"))
        )%>%
      addLayersControl(
        overlayGroups = c("Zone Corecrabe", "Nombre de pêcheurs par village", "Mangroves", "Carte du monde","Production totale par village"),
        options = layersControlOptions(collapsed = FALSE)
      )
    #%>%
    
    #addMarkers(data = antananarivo, label = ~as.character(nom),
    #labelOptions = labelOptions(noHide = T, textOnly = T, textsize = "16px",offset = c(50, -10))) %>%
    #%>%
    #addLegend("bottomright",colors = projSous_zone$Type, values = projSous_zone$Type,
    #title = "Type",
    #labFormat = labelFormat(prefix = "Km"),
    #opacity = 1
    #) 
    #%>%
    #addCircleMarkers(data= filter(filter(infos_geo, suivi == "OUI"), !is.na(lng)),fill = 'yellow', group = 'fiches', popup = ~as.character(paste(sep = "<br/>",
    #                                                                                                  paste(sep = " ","VILLAGE :",village)
    #))) 
    # Layers control
    #addLayersControl(
    #overlayGroups = c("Suivi enqueteur", "Suivi acheteur"),
    #options = layersControlOptions(collapsed = FALSE)
    #)
  })%>% bindCache(infos_geo)
  
  
  output$plot_taille <- renderPlotly({
    
    donnees_dist_sexe<- donnees_dist_sexe%>%filter(destination %in% c(input$destination))%>%filter(sexe %in% c(input$sexe))%>%filter(taille >= input$rng[1] & taille <= input$rng[2])
    
    if("Tou-te-s" %in% input$engin_dist){
      
    }else{
      donnees_dist_sexe<- donnees_dist_sexe%>%filter(engin %in% c(input$engin_dist))
    }
    if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          donnees_dist_sexe <- donnees_dist_sexe%>%filter(zone%in%input$zone_dist)
       }else{
         donnees_dist_sexe <- donnees_dist_sexe%>%filter(sous_zone%in%input$sous_zone_dist)
       }
    }else{
      donnees_dist_sexe <- donnees_dist_sexe%>%filter(village%in%input$village_dist)
    }
    
    
    p <- ggplot( donnees_dist_sexe, aes(x = taille)) + 
      theme_classic()+ 
      geom_vline(xintercept = 110,col="red", lwd = 3)+
      scale_color_manual(name = "statistics", values =  c(mean = "red"))+
      geom_histogram(aes(y=(..count..)/sum(..count..)),colour="black", fill="#9ECAE1",binwidth = 10, breaks = seq(30, 250, by = 10))+
      labs(x="Classes de taille (mm)",y="Nombre de crabes estimés (%)",font.axis=20)+
      scale_y_continuous(labels = percent)+
      theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=11))+
      theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=12))+
      theme(legend.title = element_text(color="black", size=14))+
      theme(strip.text.x = element_text(size=14,color = "black"))
    ggplotly(p)
  })
  
  
  output$download_taille_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          if(length(input$zone_dist) > 1){
            paste0("corecrabe_distribution_de_taille_par_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_distribution_de_taille_par_zone_", Sys.Date(), "_", input$zone_dist,"_",input$engin_dist, ".csv")
          }
        }else{
          if(length(input$sous_zone_dist) > 1){
            paste0("corecrabe_distribution_de_taille_par_sous_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_distribution_de_taille_par_sous_zone_", Sys.Date(), "_", input$sous_zone_dist,"_",input$engin_dist, ".csv")
          }
        }
      }else{
        if(length(input$village_dist) > 1){
          paste0("corecrabe_distribution_de_taille_par_village_", Sys.Date(),"_",input$engin_dist, ".csv")
        }else{
          paste0("corecrabe_distribution_de_taille_par_village_", Sys.Date(), "_", input$village_dist,"_",input$engin_dist, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        
        donnees_dist_sexe<- donnees_dist_sexe%>%filter(destination %in% c(input$destination))%>%filter(sexe %in% c(input$sexe))%>%filter(taille >= input$rng[1] & taille <= input$rng[2])
        
        if("Tou-te-s" %in% input$engin_dist){
          
        }else{
          donnees_dist_sexe<- donnees_dist_sexe%>%filter(engin %in% c(input$engin_dist))
        }
        if("Tou-te-s" %in% input$village_dist){
          if("Tou-te-s" %in% input$sous_zone_dist){
            donnees_dist_sexe <- donnees_dist_sexe%>%filter(zone%in%input$zone_dist)
          }else{
            donnees_dist_sexe <- donnees_dist_sexe%>%filter(sous_zone%in%input$sous_zone_dist)
          }
        }else{
          donnees_dist_sexe <- donnees_dist_sexe%>%filter(village%in%input$village_dist)
        }
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(donnees_dist_sexe, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )


  output$plot_taille_moyenne <- renderPlotly({
    
    taille_moyenne <- func_taille_moyenne(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)
    if("Tou-te-s" %in% input$village_dist){
      if("Tou-te-s" %in% input$sous_zone_dist){
        p <- ggplot(taille_moyenne, aes(x = Mois, y =mean, group=Zone, color=Zone)) +
          geom_line(aes(color=Zone))+geom_hline(aes(yintercept = 110), color = 'black')+geom_text(aes("Oct 22",112,label = "Taille réglementaire (110mm)"), color='black')+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          scale_y_continuous(name = "", limits = c(0,150), breaks = seq(0,150, by=50))+
          labs(x="",y="Taille moyenne (mm)",font.axis=88,face="bold")
      }else{
        p <- ggplot(taille_moyenne, aes(x = Mois, y =mean, group=Sous_zone, color=Sous_zone)) +
          geom_line(aes(color=Sous_zone))+geom_hline(aes(yintercept = 110), color = 'black')+geom_text(aes("Oct 22",112,label = "Taille réglementaire (110mm)"), color='black')+
          theme_bw(base_size = 8)+
          scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
          scale_y_continuous(name = "", limits = c(0,150), breaks = seq(0,150, by=50))+
          labs(x="",y="Taille moyenne (mm)",font.axis=88,face="bold")
      }
    }else{
      p <- ggplot(taille_moyenne, aes(x = Mois, y =mean, group=Village, color=Village)) +
        geom_line(aes(color=Village))+geom_hline(aes(yintercept = 110), color = 'black')+geom_text(aes("Oct 22",112,label = "Taille réglementaire (110mm)"), color='black')+
        theme_bw(base_size = 8)+
        scale_x_discrete(name ="",limits=filtre_mois[match(format(as.Date("2020/12/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois):match(format(as.Date("2023/08/01",  format="%Y/%m/%d"),"%h %y"),filtre_mois)])+
        scale_y_continuous(name = "", limits = c(0,150), breaks = seq(0,150, by=50))+
        labs(x="",y="Taille moyenne (mm)",font.axis=88,face="bold")
    }
  })
  
  output$download_taille_m_csv <- downloadHandler(
    filename = function() {
      if("Tou-te-s" %in% input$village_dist){
        if("Tou-te-s" %in% input$sous_zone_dist){
          if(length(input$zone_dist) > 1){
            paste0("corecrabe_taille_moyenne_par_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_taille_moyenne_par_zone_", Sys.Date(), "_", input$zone_dist,"_",input$engin_dist, ".csv")
          }
        }else{
          if(length(input$sous_zone_dist) > 1){
            paste0("corecrabe_taille_moyenne_par_sous_zone_", Sys.Date(),"_",input$engin_dist, ".csv")
          }else{
            paste0("corecrabe_taille_moyenne_par_sous_zone_", Sys.Date(), "_", input$sous_zone_dist,"_",input$engin_dist, ".csv")
          }
        }
      }else{
        if(length(input$village_dist) > 1){
          paste0("corecrabe_taille_moyenne_par_village_", Sys.Date(),"_",input$engin_dist, ".csv")
        }else{
          paste0("corecrabe_taille_moyenne_par_village_", Sys.Date(), "_", input$village_dist,"_",input$engin_dist, ".csv")
        }
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        taille_moyenne <- func_taille_moyenne(input$zone_dist,input$village_dist,input$date_dist,input$engin_dist,input$sous_zone_dist)
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(taille_moyenne, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )

  output$download_data_csv <- downloadHandler(
    filename = function() {
      
      if(input$donnee == 1){
        paste0("CORECRABE_Données-avec-taille_", Sys.Date(),"_",input$zone_tel,"_",input$sous_zone_tel, ".csv")
      }else if(input$donnee == 2){
        paste0("CORECRABE_Données-sans-taille_", Sys.Date(),"_",input$zone_tel,"_",input$sous_zone_tel, ".csv")
      }else if(input$donnee == 3){
        paste0("CORECRABE_Données-de-recensement_", Sys.Date(),"_",input$zone_tel,"_",input$sous_zone_tel, ".csv")
      }
      
    },
    content = function(file) {
      withProgress(message = "Chargement du fichier...", {
        setProgress(value = 0.5,message = "Traitement du fichier de sortie...")
        jeu_de_donnee <- func_download(input$donnee,input$zone_tel,input$sous_zone_tel,input$village_tel)
        setProgress(value = 0.9,message = "Téléchargement en cours...")
        write.csv(jeu_de_donnee, file, row.names = FALSE)
        setProgress(value = 1,message = "Téléchargement terminé!")
      })
    }
  )
}
