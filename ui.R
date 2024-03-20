
img_list <- c("mesure.jpg","IMG_7936.jpg","IMG_7986.jpg","IMG_8070.jpg")
ui <- secure_app(bootstrapPage(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("readable"),
    collapsible = TRUE,
    title = div(img(src="brand.png",
                            height = "40px"), style = "padding-left:100px;"),
    id="nav", windowTitle ="Système d'information halieutique collaboratif sur la pêcherie de crabe de mangrove à Madagascar",
    
    #################################### <- ACCUEIL -> #########################################################
    
    tabPanel("Accueil", icon = shiny::icon("home"),
              fluidRow(
                column(width = 12,h3(HTML("<b>Système d'information halieutique collaboratif sur les pêcheries de crabe de mangrove à Madagascar</b>"), style="text-align: center;"), style="background: #2bb8ae; font-weight: 72px"),
                column(width = 6,
                       fluidRow(
                         column(width = 12,style = "margin-top: 10px",
                                slickR(obj = img_list,height = 450, width = "100%") + 
                                  settings(dots = FALSE, autoplay = TRUE, arrows = FALSE)
                                ),
                         column(
                           width = 12,style="text-align: center;margin-top: 5px",
                           div(
                             p("Avec le soutien de :")
                           ),
                           div(img(src="logos.png",
                                   height = "70px")
                               ),
                           div(
                             HTML("<span>Lien projet CORECRABE : </span> <a href=\"http://corecrabe.ird.fr\" target=\"_blank\">corecrabe.ird.fr</a>")
                           )
                         )
                       )
                ),
                column(width = 6, style="margin-top: 5px",
                                            div(
                                              span( h4(HTML("<b>Le SIH-C est une plateforme collaborative en ligne créée en 2022</b>")," pour répondre aux besoins de décrire ", HTML("<b>les petites pêcheries de crabe de mangrove</b>"),span("Scylla serrata", style = "font-style: italic"),"à Madagascar, pour appuyer leur gestion aux échelles locale, régionale et nationale. ")),
                                              
                                              h4(HTML("<b>Il mobilise des informaticiens et des halieutes de l’Institut Halieutique et des Sciences Marines</b>"),span(" (IH.SM) de l’Université de Toliara et ses partenaires"),span(" pour valoriser les données de suivi des pêcheries auprès d’une large communauté d’utilisateurs (pêcheurs, mareyeurs, collecteurs, sociétés d’exportation, administrations, ONGs…).")),
                                              h4(HTML("<b>
                                                  Le SIH-C réalise les tâches suivantes :</b>")),
                                              h4("-	",HTML("<b>Collecter des données standardisées</b>")," selon des protocoles scientifiques validés, mis à disposition des organisations de recherche et hors recherche"),
                                              h4("-	",HTML("<b>Stocker ces données</b>")," après validation sur un serveur en ligne sécurisé, administré par l’IH.SM"),
                                              h4("-	",HTML("<b>Rendre accessibles ces données</b>")," aux utilisateurs selon une procédure sécurisée"),
                                              h4("-	",HTML("<b>Evaluer des indicateurs</b>")," halieutiques et socioéconomiques des pêcheries selon des méthodes validées et transparentes"),
                                              h4("-	",HTML("<b>Visualiser l’évolution de ces indicateurs</b>")," dans l’espace et dans le temps, en proposant une interprétation des tendances observées")
                                            ),
                       )
              )
             ),
    
    
    #################################### <- CARTOGRAPHIE -> #########################################################
    
    tabPanel("Cartographie", icon = shiny::icon("map"),
             div(class="outer",
                 tags$head(includeCSS("./styles.css")),
                 leafletOutput("map", width="100%", height="100%"),
                 absolutePanel(id = "controls", class = "panel panel-primary",
                               top = 80, left = 50, width = 400, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               h5("Suivi de  2020 à 2023 :"),
                               h5(paste(sep = " ", nrow(as.array(unique(infos_geo[infos_geo$zone != "Maintirano",]$zone)))-1," principales zones de production de crabe de mangrove")),
                               p(paste(sep=" ",nrow(as.array(unique(infos_geo[infos_geo$suivi == 'OUI',]$village))),"villages suivis")),
                               p("1484 km² de mangrove"),
                               p(paste(sep=" ",nrow(corecrabe_sans_taille)," enquêtes effectuées")),
                               p(paste(sep=" ",sum(infos_geo$pecheur)," pêcheurs recensés")),
                               p("35 pêcheurs volontaires"),
                              #  p(paste(sep=" ",nb_volontaire$nb_volontaire," volontaires")),
                               p(paste(sep=" ", nb_enq_renafep$nb_renafep," enquêtrices RENAFEP"))
                               #h4(textOutput("nb_pecheur_recense"))
                 ),
                 absolutePanel(id = "controls", class = "panel panel-primary",
                               top = 400, left = 50, width = 520, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               h6(" Production annuelle estimée par zone en 2022"),
                               div(shinycssloaders::withSpinner(tableOutput("table_prod_carte"))),
                               #h4(textOutput("nb_pecheur_recense"))
                 )
                #  ,
                #  absolutePanel(id = "controls", class = "panel panel-primary",
                #                top = 80, left = 460, width = 450, fixed=TRUE,
                #                draggable = TRUE, height = "auto",
                #                h6("> Nombre de pêcheur total par engin par zone en 2021"),
                #                div(shinycssloaders::withSpinner(tableOutput("pecheur_total_carte"))),
                #                #h4(textOutput("nb_pecheur_recense"))
                #  )
             )
    ),
    
    
    #################################### <- EFFORT -> #########################################################
    
    tabPanel("Effort de pêche", icon = shiny::icon("line-chart"),
            sidebarLayout(
              sidebarPanel(width = 2,
                linebreaks(1),
                "Variables de Paramètres globaux",
                linebreaks(2),
                dateRangeInput('date','Période du :',min = min(corecrabe$date_premiere_sortie_de_peche),start = min(corecrabe$date_premiere_sortie_de_peche),max = max(corecrabe$date_premiere_sortie_de_peche),end = max(corecrabe$date_premiere_sortie_de_peche), separator = " au "),
                shinyWidgets::pickerInput("zone","Zone d'étude",choices = unique(corecrabe$zone),selected = unique(corecrabe$zone),multiple = TRUE),
                shinyWidgets::pickerInput("sous_zone","Sous_zones",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$sous_zone)),multiple = TRUE,selected = "Tou-te-s"),
                shinyWidgets::pickerInput("village","Villages",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$village)),multiple = TRUE,selected = "Tou-te-s"),
                shinyWidgets::pickerInput("engin","Engins",choices = c("Balance","Crochet","Ligne","Raquette"),multiple = FALSE,selected = "Crochet"),
              ),
              mainPanel(width = 10,
                tabsetPanel(
                  
                  tabPanel("Pêcheurs actifs (recensement)",
                           linebreaks(1),
                           bsCollapse(id = "nombre", open = "Définition",
                                      bsCollapsePanel("Définition",
                                                      "Nombre de pêcheurs au crabe actifs dans une zone géographique donnée."
                                      )
                           ),
                           bsCollapse(id = "graphe_nombre", open = "Evolution",
                                      bsCollapsePanel("Evolution",
                                                      downloadButton("download_pecheur_csv","Télécharger les données"),
                                                      shinycssloaders::withSpinner(plotlyOutput("pecheur",width = "100%",
                                                                   height = "500px",
                                                                   inline = FALSE))
                                      )
                           )
                  ),
                  tabPanel("Fréquence d'activité",
                           linebreaks(1),
                           bsCollapse(id = "def_taux", open = "Définition",
                                      bsCollapsePanel("Définition",
                                              "Le taux d’activité est le coefficient moyen d’activité qui indique combien de sorties les pêcheurs font en moyenne par jour pour chaque engin."
                                      )
                          ),
                          bsCollapse(id = "graphe_taux", open = "Evolution",
                                     bsCollapsePanel("Evolution",
                                                    downloadButton("download_taux_csv","Télécharger les données"),
                                                     shinycssloaders::withSpinner(plotlyOutput("taux",width = "100%",
                                                         height = "500px",
                                                         inline = FALSE))
                                    )
                          )
                  ),
                  tabPanel("Effort estimé (nombre de sorties)",
                           linebreaks(1),
                           bsCollapse(id = "effort", open = "Définition",
                                      bsCollapsePanel("Définition",
                                                      "L’effort de pêche est l’ensemble des moyens déployés par les pêcheurs pour exploiter le stock. Ici, l’effort de pêche est le nombre de sorties de pêche effectués pendant une période donnée. Il est obtenu par la multiplication du taux d’activité moyen avec le nombre de pêcheurs actifs de la zone et la période considérée."
                                      )
                           ),
                           bsCollapse(id = "graphe_effort", open = "Evolution",
                                      bsCollapsePanel("Evolution",
                                                      downloadButton("download_effort_csv","Télécharger les données"),
                                                      shinycssloaders::withSpinner(plotlyOutput("effort_par_sortie",width = "100%",
                                                                 height = "500px",
                                                                 inline = FALSE))
                                      )
                           )
                  ),
                  
                )         
              )
            )
    ),
    
    #################################### <- CAPTURE -> #########################################################
    
    tabPanel("Capture", icon = shiny::icon("bar-chart"),
        sidebarLayout(
          sidebarPanel(width = 2,
                       linebreaks(1),
                       "Variables de Paramètres globaux",
                       linebreaks(2),
                       dateRangeInput('date_dist','Période du :',min = min(corecrabe$date_premiere_sortie_de_peche),start = min(corecrabe$date_premiere_sortie_de_peche),max = max(corecrabe$date_premiere_sortie_de_peche),end = max(corecrabe$date_premiere_sortie_de_peche), separator = " au "),
                       shinyWidgets::pickerInput("zone_dist","Zone d'étude",choices = unique(corecrabe$zone),selected = unique(corecrabe$zone),multiple = TRUE),
                       shinyWidgets::pickerInput("sous_zone_dist","Sous_zones",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$sous_zone)),multiple = TRUE,selected = "Tou-te-s"),
                       shinyWidgets::pickerInput("village_dist","Villages",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$village)),multiple = TRUE,selected = "Tou-te-s"),
                       shinyWidgets::pickerInput("engin_dist","Engins",choices = c("Balance","Crochet","Ligne","Raquette"),multiple = FALSE,selected = "Crochet"),
                       
          ),
          mainPanel(width = 10,
            tabsetPanel(
              tabPanel(
                "Distribution de taille",
                linebreaks(1),
                bsCollapse(id = "dist", open = "Définition",
                           bsCollapsePanel("Définition",
                                           "La structure en taille est la répartition des individus d'une espèce ou d’une population en fonction de leur taille. Elle décrit la composition et la proportion des différentes classes de tailles présentes dans une population."
                           )
                ),
                bsCollapse(id = "taille", open = "Evolution",
                           bsCollapsePanel("Evolution",
                                           fluidRow(
                                             column(width = 3,
                                                shinyWidgets::pickerInput("destination","Déstinations" , choices=c("Selectionnez tous",unique(donnees_ensenble_destination$destination)),  multiple = TRUE,selected = "Selectionnez tous")
                                            ),
                                            column(width = 3,
                                                   shinyWidgets::pickerInput("sexe","Sexes" , choices=c("Selectionnez tous",list(Mâle='Male',Femelle='Femelle')), multiple = TRUE,selected = "Selectionnez tous" )
                                            ),
                                            column(width = 3,
                                                   sliderInput("rng", "Tailles (mm)", value = c(min(Donnees_dist_taille$taille),max(Donnees_dist_taille$taille)), min = min(Donnees_dist_taille$taille), max = max(Donnees_dist_taille$taille))
                                            ),
                                            column(width = 3,
                                                   downloadButton("download_taille_csv","Télécharger les données"))
                                           ),
                                           linebreaks(1),
                                           shinycssloaders::withSpinner(plotlyOutput("plot_taille",width = "100%",
                                                      height = "500px",
                                                      inline = FALSE))
                           )
                ),
              ),
              tabPanel(
                "Taille moyenne",
                linebreaks(1),
                bsCollapse(id = "taill_mo", open = "Définition",
                           bsCollapsePanel("Définition",
                                           "La taille moyenne est la mesure statistique représentant la taille moyenne (en mm) des individus capturés. Elle est calculée en prenant en compte les tailles individuelles de tous les crabes capturés et en obtenant la moyenne de ces mesures."
                           )
                ),
                bsCollapse(id = "taille_m", open = "Evolution",
                           bsCollapsePanel("Evolution",
                                           downloadButton("download_taille_m_csv","Télécharger les données"),
                                           shinycssloaders::withSpinner(plotlyOutput("plot_taille_moyenne",width = "100%",
                                                                                     height = "500px",
                                                                                     inline = FALSE))
                           )
                ),
              ),
              tabPanel(
                "Rendement",
                linebreaks(1),
                bsCollapse(id = "rend", open = "Définition",
                           bsCollapsePanel("Définition",
                                           "Rendement : CPUE sans modélisation"
                           )
                ),
                bsCollapse(id = "rende", open = "Evolution",
                           bsCollapsePanel("Evolution",
                                           downloadButton("download_rend_csv","Télécharger les données"),
                                           shinycssloaders::withSpinner(plotlyOutput("rendement",width = "100%",
                                                                                     height = "500px",
                                                                                     inline = FALSE))
                           )
                ),
              ),
              tabPanel(
                "Production cumulée estimée",
                linebreaks(1),
                bsCollapse(id = "prod", open = "Définition",
                           bsCollapsePanel("Définition",
                                           "La production estimée cumulée est égale à la somme de la production d’un mois additionnée la production des mois qui lui sont inférieurs."
                           )
                ),
                bsCollapse(id="prod_cum", open = "Evolution",
                           bsCollapsePanel("Evolution",
                                            downloadButton("download_prod_cum_csv","Télécharger les données"),
                                           shinycssloaders::withSpinner(plotlyOutput("prod_cumule",width = "100%",
                                                        height = "500px",
                                                        inline = FALSE
                                           ))
                           )
                )
              ),
              tabPanel(
                "Production totale estimée",
                linebreaks(1),
                bsCollapse(id = "pro", open = "Définition",
                           bsCollapsePanel("Définition",
                                           "La capture totale estimée désigne la quantité (en poids) de crabes pêchés dans une zone géographique bien déterminée et pendant une période de référence. Elle résulte du produit de la CPUE moyenne standardisée (kg par sortie) et l’effort de pêche estimé (nombre de sorties)."
                           )
                ),
                bsCollapse(id="prod_tot", open = "Evolution",
                           bsCollapsePanel("Evolution",
                                            downloadButton("download_prod_csv","Télécharger les données"),
                                            shinycssloaders::withSpinner(plotlyOutput("prod_totale",width = "100%",
                                                        height = "500px",
                                                        inline = FALSE
                                                        )
                                           ))
                           )
              ),
              tabPanel(
                "Production Annuelle estimée",
                linebreaks(1),
                bsCollapse(id = "prod_a", open = "Définition",
                           bsCollapsePanel("Définition",
                                           "La production annuelle estimée désigne une estimation de la quantité totale de produits ou de biens générés au cours d'une année donnée. Elle représente la somme des quantités produites sur une période d'un an de campagne."
                           )
                ),
                bsCollapse(id="prod_ann", open = "Evolution",
                           bsCollapsePanel("Evolution",
                                            downloadButton("download_prod_an_csv","Télécharger les données"),
                                            shinycssloaders::withSpinner(plotlyOutput("prod_ann_plot",width = "100%",
                                                        height = "500px",
                                                        inline = FALSE
                                                        )
                                           )
                                           )
                           )
              )
            )          
          )
        )         
    ),
    
    
    #################################### <- INDICE D'ABONDANCE -> #########################################################
    
    tabPanel("Indice d'abondance", icon = shiny::icon("pie-chart"),
        sidebarLayout(
          sidebarPanel(width = 2,
                       linebreaks(1),
                       "Variables de Paramètres globaux",
                       linebreaks(2),
                       dateRangeInput('date_cpue','Période du :',min = min(corecrabe$date_premiere_sortie_de_peche),start = min(corecrabe$date_premiere_sortie_de_peche),max = max(corecrabe$date_premiere_sortie_de_peche),end = max(corecrabe$date_premiere_sortie_de_peche), separator = " au "),
                       shinyWidgets::pickerInput("zone_cpue","Zone d'étude",choices = unique(corecrabe$zone),selected = unique(corecrabe$zone),multiple = TRUE),
                       shinyWidgets::pickerInput("sous_zone_cpue","Sous_zones",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$sous_zone)),multiple = TRUE,selected = "Tou-te-s"),
                       shinyWidgets::pickerInput("village_cpue","Villages",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$village)),multiple = TRUE,selected = "Tou-te-s"),
                       shinyWidgets::pickerInput("engin_cpue","Engins",choices = c("Balance","Crochet","Ligne","Raquette"),multiple = FALSE,selected = "Crochet"),
                       ),
          mainPanel(width = 10,
                    tabsetPanel(
                      tabPanel(
                        "Biomasse",
                        linebreaks(1),
                        bsCollapse(id = "standardise", open = "Définition",
                                   bsCollapsePanel("Définition",
                                                   "Biomasse : dans le contexte de la pêche, la biomasse fait référence au nombre total d'une ressource de pêche qui se trouve à l’état naturel."
                                   )
                        ),
                        bsCollapse(id = "cpue_standardise", open = "Evolution",
                                   bsCollapsePanel("Evolution",
                                                  downloadButton("download_cpue_csv","Télécharger les données"),
                                                   shinycssloaders::withSpinner(plotlyOutput("cpue",width = "100%",
                                                              height = "500px",
                                                              inline = FALSE))
                                   )
                        ),
                      ),
                      tabPanel(
                        "Abondance",
                        linebreaks(1),
                        bsCollapse(id = "nombre", open = "Définition",
                                   bsCollapsePanel("Définition",
                                                   "L'abondance, dans le contexte des pêcheries, fait référence à la quantité relative d'espèces aquatiques disponibles dans un écosystème donné. Lorsqu'une population de crabes est abondante, cela signifie qu'il y a un grand nombre d'individus de cette espèce dans une zone donnée "
                                   )
                        ),
                        bsCollapse(id = "cpue_nombre", open = "Evolution",
                                   bsCollapsePanel("Evolution",
                                                  downloadButton("download_cpue_nb_csv","Télécharger les données"),
                                                   shinycssloaders::withSpinner(plotlyOutput("cpue_en_nombre",width = "100%",
                                                                                             height = "500px",
                                                                                             inline = FALSE))
                                   )
                        )
                      ),
                      tabPanel(
                        "Abondance par classe de taille",
                        linebreaks(1),
                        bsCollapse(id = "c_taille", open = "Définition",
                                   bsCollapsePanel("Définition",
                                                   "L'abondance par classe de tailles est la répartition des individus en fonction de leur taille. Plutôt que de considérer la population dans son ensemble, l'abondance par classe de tailles permet de déterminer la proportion relative de la ressource appartenant à différentes classes de tailles spécifiques. "
                                   )
                        ),
                        bsCollapse(id = "cpue_taille", open = "Evolution",
                                   bsCollapsePanel("Evolution",
                                                   fluidRow(
                                                     column(width = 4,
                                                            sliderInput("classe_taille", "Classe de taille (mm)", value = c(100,220), min = 10, max = 220, step = 10)
                                                     ),
                                                     column(width = 4,
                                                            downloadButton("download_cpue_taille_csv","Télécharger les données"))
                                                   ),
                                                   linebreaks(1),
                                                   shinycssloaders::withSpinner(plotlyOutput("cpue_en_taille",width = "100%",
                                                                                             height = "500px",
                                                                                             inline = FALSE))
                                   )
                        )
                      ),
                      tabPanel(
                        "Abondance par calibre de taille",
                        linebreaks(1),
                        bsCollapse(id = "cal_taille", open = "Définition",
                                   bsCollapsePanel("Définition",
                                                   " L'abondance par calibre de tailles est la répartition des individus en fonction de leur taille regroupée en catégories. "
                                   )
                        ),
                        bsCollapse(id = "cpue_cal_taille", open = "Evolution",
                                   bsCollapsePanel("Evolution",
                                                   fluidRow(
                                                     #column(width = 4,
                                                            #shinyWidgets::pickerInput("calibre_taille","Calibre de taille",choices = list("]0,70]"=70,"]70,100]"=100,"100 et plus"=110), multiple = TRUE, selected = list("]0,70]"=70,"]70,100]"=100,"100 et plus"=110))
                                                     #),
                                                     column(width = 4,
                                                            downloadButton("download_cpue_cal_taille_csv","Télécharger les données"))
                                                   ),
                                                   linebreaks(1),
                                                   shinycssloaders::withSpinner(plotlyOutput("cpue_en_cal_taille",width = "100%",
                                                                                             height = "500px",
                                                                                             inline = FALSE))
                                   )
                        )
                      )
                    )          
          )
        )
    ),
    
    
    ################################ VALEUR ########################################################"""
    
    tabPanel("Valeur", icon = shiny::icon("chart-area"),
             sidebarLayout(
               sidebarPanel(width = 2,
                            linebreaks(1),
                            "Variables de Paramètres globaux",
                            linebreaks(2),
                            dateRangeInput('date_valeur','Période du :',min = min(corecrabe$date_premiere_sortie_de_peche),start = min(corecrabe$date_premiere_sortie_de_peche),max = max(corecrabe$date_premiere_sortie_de_peche),end = max(corecrabe$date_premiere_sortie_de_peche), separator = " au "),
                            shinyWidgets::pickerInput("zone_valeur","Zone d'étude",choices = unique(corecrabe$zone),selected = unique(corecrabe$zone),multiple = TRUE),
                            shinyWidgets::pickerInput("sous_zone_valeur","Sous_zones",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$sous_zone)),multiple = TRUE,selected = "Tou-te-s"),
                            shinyWidgets::pickerInput("village_valeur","Villages",choices = c("Tou-te-s","Selectionnez tous",unique(corecrabe$village)),multiple = TRUE,selected = "Tou-te-s"),
                            shinyWidgets::pickerInput("engin_valeur","Engins",choices = c("Balance","Crochet","Ligne","Raquette"),multiple = FALSE,selected = "Crochet"),
               ),
               mainPanel(
                 width = 10,
                 tabsetPanel(
                   tabPanel(
                     "Prix aux pecheurs",
                     linebreaks(1),
                     bsCollapse(id = "prix_def", open = "Définition",
                                bsCollapsePanel("Définition",
                                                "Le prix désigne le montant monétaire ou la valeur d'échange fixé pour l'achat ou la vente d'un kilo de crabes. Il représente le coût ou la valeur du kilo de crabe sur un marché donné."
                                )
                     ),
                     bsCollapse(id = "graph_prix", open = "Evolution",
                                bsCollapsePanel("Evolution",
                                                downloadButton("download_prix_csv","Télécharger les données"),
                                                shinycssloaders::withSpinner(plotlyOutput("prix",width = "100%",
                                                                                          height = "500px",
                                                                                          inline = FALSE))
                                )
                     ),
                   ),
                   tabPanel(
                     "Revenu moyen par sortie",
                     linebreaks(1),
                     bsCollapse(id = "rev_def", open = "Définition",
                                bsCollapsePanel("Définition",
                                                "Le revenu moyen par sortie de pêche désigne le montant moyen de revenu généré par un pêcheur lors d'une sortie de pêche donnée. Il représente la somme d'argent totale obtenue en vendant les produits de la pêche capturés lors d'une sortie spécifique, divisée par le nombre de sorties effectuées sur une période donnée."
                                )
                     ),
                     bsCollapse(id = "graph_rev", open = "Evolution",
                                bsCollapsePanel("Evolution",
                                                downloadButton("download_revenu_csv","Télécharger les données"),
                                                shinycssloaders::withSpinner(plotlyOutput("revenu_plot",width = "100%",
                                                                                          height = "500px",
                                                                                          inline = FALSE))
                                )
                     ),
                   ),
                   tabPanel(
                     "Valeur totale",
                     linebreaks(1),
                     bsCollapse(id = "val_def", open = "Définition",
                                bsCollapsePanel("Définition",
                                                "La valeur totale de la pêcherie est la valeur économique globale des activités de pêche dans une région ou pour une espèce spécifique. Elle représente la somme des revenus générés par la vente des produits de la pêche. Elle est calculée en prenant en compte les prix de vente des produits de la pêche multipliée par les quantités capturées."
                                )
                     ),
                     bsCollapse(id = "graph_val", open = "Evolution",
                                bsCollapsePanel("Evolution",
                                                downloadButton("download_valeur_csv","Télécharger les données"),
                                                shinycssloaders::withSpinner(plotlyOutput("valeur_plot",width = "100%",
                                                                                          height = "500px",
                                                                                          inline = FALSE))
                                )
                     ),
                   )
                 )
               )
             ),
             
             ),
             tabPanel("Téléchargement",icon = shiny::icon("download"),
             sidebarLayout(
               sidebarPanel(width = 2,
                            linebreaks(2)),
               mainPanel(width = 10,
                         tabsetPanel(
                           tabPanel("",
                                    linebreaks(2),
                                    bsCollapse(id = "tel_def", open = "Données",
                                               bsCollapsePanel("Données",
                                                            linebreaks(1),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     shinyWidgets::pickerInput("donnee","Jeu de données à télécharger",choices = list("CORECRABE_Données-avec-taille"=1, "CORECRABE_Données-sans-taille"=2,"CORECRABE_Données-de-recensement"=3), multiple = FALSE, selected = 1,options = pickerOptions(liveSearch = TRUE))
                                                                )
                                                            )
                                                            
                                               )
                                    ),
                                    bsCollapse(id = "tel_val", open = "Filtres des données",
                                               bsCollapsePanel("Filtres des données",
                                                     fluidRow(
                                                       column(width = 4,
                                                       shinyWidgets::pickerInput("zone_tel","Zone d'études",choices = "Mahajanga", multiple = TRUE, selected = "Mahajanga")
                                                       ),
                                                       column(width = 4,
                                                              shinyWidgets::pickerInput("sous_zone_tel","Sous_zone",choices = "Sud-Mahajanga", multiple = TRUE, selected = "Sud-Mahajanga")
                                                       ),
                                                       column(width = 4,
                                                              shinyWidgets::pickerInput("village_tel","Villages",choices = "Tou-te-s", multiple = TRUE, selected = "Tou-te-s")
                                                       ),
                                                       column(width = 4,
                                                              downloadButton("download_data_csv","Télécharger les données")
                                                       )
                                                     )         
                                               )
                                               )
                                    )
                         )
               )
             ),
             
             )
  )
    
  )
  ,language = "fr",
  status = "info",
  tags_top = tags$div(
      tags$img(
        src = "brand.png", width = "80%"
      )
    ),
  background  = "linear-gradient(rgba(0, 0, 255, 0.5),
        rgba(255, 255, 0, 0.5));"
)