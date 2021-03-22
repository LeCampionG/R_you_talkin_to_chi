library(shinythemes)
library(shiny)
library(shinydashboard)
library(knitr)

options(shiny.maxRequestSize = 30*1024^2)

ui<-fluidPage(theme = shinytheme("superhero"),
              navbarPage(tags$a("R you talkin' to Chi - Grégoire Le Campion", href = 'https://github.com/LeCampionG/R_you_talkin_to_chi',
                                                                            icon("github"), target="_blank"),
                         tabPanel("A propos",
                                  #code Html
                                  h2(strong("Bienvenue sur l'application R you talkin' to Chi !!!")),
                                  p("Vous en avez marre ! Vous aimeriez tellement faire des stats et de jolies représentations graphiques! Mais vous n'avez que des données qualitatives, le genre qu'on peut juste croisé avec des tableaux lignes/colonnes !"),
                                  p("Haut les coeurs ! ",strong("R you talkin' to Chi "), "est fait pour vous!"),
                                  p("Le chi² est un test statistique bien trop sous-estimé ! C'est un outil très puissant qui permet à la fois d'analyser et représenter des variables qualitatives."),
                                  p("Avant de vous lancer dans vos analyses un petit rappel ou une mise au point sur le chi² peut être nécessaire voire indispensable ! Toutes les infos et les réponses aux questions que vous vous êtes jamais posés sur le chi² se trouve ici sur le tutoriel :", strong(a("C’est chi2 ? Et bien c’est lui !", href="http://ouvrir.passages.cnrs.fr/wp-content/uploads/2019/09/X2.html")),"."),
                                  br(),
                                  p("Avant de commencer il est utile de dire que l'application, ",strong("R you talkin' to Chi "), "permets de réaliser un chi² d'indépendance (ou d'homogénéité ou encore de contingences, c'est la même chose!). C'est à dire que,", strong("R you  talkin' to chi"),"a pour but de vérifier  et représenter l'indépendance (l'absence ou la présence d'un lien) entre deux variables qualitatives. Nous ne réaliserons pas ici un chi² de conformité (ou d'adéquation) qui vise à comparer une distribution observée à une distribution théorique."),
                                  br(),
                                  p("Si vous êtes à la recherche d'autres outils n'hésitez pas à vous rendre sur la plateforme ", strong(a("OUVRIR", href="http://ouvrir.passages.cnrs.fr/")), " Vous y trouverez toutes sortes de tutoriel et d'outils !"),
                                  br(),
                                  p("Toutes remarques est la bienvenue, pour ce faire vous pouvez me contacter à cette adresse : gregoire.lecampion@cnrs.fr"),
                                  h3(strong("Citation")), 
                                  p("Dans l'éventualité où vous utiliseriez cette application dans le cadre d'une publication, vous pouvez citer cet outil comme ceci :"),
                                  p(strong("Le Campion G. ", a("R you talkin' to chi: une application pour réaliser et représenter un chi² d'indépendance.", href="https://analytics.huma-num.fr/Gregoire.LeCampion/chi2/")," Pôle ARD UMR 5319 UMR Passages. 2020."))
                                  
                         ),
                         
                         tabPanel("Import des données",
                                  sidebarLayout(
                                    ############################################################
                                    # 1. Le menu de gauche, présentant les options de l'import #
                                    ############################################################
                                    sidebarPanel(fileInput("file1", "Charger un fichier CSV",
                                                           multiple = FALSE,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain", "text/semicolon-separated-values,text/plain" ,
                                                                      ".csv")),
                                                 h5(helpText("Le poid des fichier est limité à 30Mb")),
                                                 tags$hr(),
                                                 h5(helpText("Ajuster les options suivantes en fonction de votre fichier importé")),
                                                 # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                                 checkboxInput("header", "1ere ligne comme en-tête", TRUE),
                                                 #déterminer le séparateur de champ
                                                 radioButtons("sep", "Séparateur de champ",
                                                              choices = c(Comma = ",",
                                                                          Semicolon = ";",
                                                                          Tab = "\t"),
                                                              selected = ","),
                                                 #déterminer séparateur de texte
                                                 radioButtons("quote", "Séparateur de texte",
                                                              choices = c("Aucun" = "",
                                                                          "Guillemet double" = '"',
                                                                          "Guillemet simple" = "'"),
                                                              selected = '"'),
                                                 #Choix du mode de visualistaion
                                                 radioButtons("disp", "Visualiser",
                                                              choices = c("Uniquement les 1eres lignes" = "head",
                                                                          "Ensemble des données"= "all"),
                                                              selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("1- Mes données",
                                               uiOutput("tb1")
                                      )
                                    )
                                    ))),
                         tabPanel("Chi²",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h4("Sélectionner les variables à croiser"),
                                      selectizeInput("selectl", label="Variable ligne", choices=NULL, multiple=FALSE),
                                      selectizeInput("selectc", label="Variable colonne", choices=NULL, multiple=FALSE),
                                      radioButtons("nulle", "Gestion des données manquantes",
                                                   choices = c("Conserver" = "always",
                                                               "Supprimer" = 'no',
                                                               "Conserver si présent" = "ifany"),
                                                   selected = 'ifany')
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("1- Tableau croisé",
                                                 verbatimTextOutput("tab_result"),
                                                 h5("Remarque : les lignes et les colonnes d’un tableau croisé sont interchangeables. Il n'y a donc pas un sens de lecture bien définis."),
                                                 h5(" Le chi² permets de dire s'il y dépendance entre deux variables pas de dire que la variable A à un effet sur la variable B. C'est votre connaissance du sujet qui vous permettra d'affirmer que c'est l'appartenace à tel modalité qui influence l'appartenance à tel autre.")
                                        ),
                                        tabPanel("2-Chi²",
                                                 h4("Test utilisé"),
                                                 verbatimTextOutput("chi_result"),
                                                 helpText("Si p< 0.05 vous pouvez rejeter l'hypothèse d'indépendance et considérer que vos variables sont donc liées."),
                                                 h4("Effectifs théoriques"),
                                                 verbatimTextOutput("theo"),
                                                 helpText("Les effectifs théoriques sont les effectifs que vous auriez dû avoir en cas d'indépendance parfaite entre vos variables."),
                                                 h4("Tableau des résidus"),
                                                 verbatimTextOutput("residus"),
                                                 helpText("Les résidus sont des informations importantes car donnent un indice sur le sens du lien et surtout le sens de l'écart entre vos efectifs et les effectifs théoriques. Les résidus correspondant à des écarts statistiquement significatifs sont ceux dont la valeur est supérieure à 2 ou inférieure à -2.")
                                        ),
                                        tabPanel("3-Test exact de Fisher",
                                                 h4("Test de Fisher"),
                                                 helpText("Le test exact de Fisher est une alternative au chi². Il vise à tester la même hypothèse et s'interprète de la même manière. Il s'agit d'un test exacte (d'où le nom) et non pas une approximation d'une loi statistique comme le chi²."),
                                                 helpText("Il est très gourmand en resssources, mais très utile quand les conditions du chi² ne sont pas respectées. A savoir pas plus de 20% des effectifs théorique de notre tableau inférieur à 5."),
                                                 verbatimTextOutput("fish_result")
                                        ),
                                        tabPanel("4-V de Cramer",
                                                 h4("V de Cramer"),
                                                 helpText("Permet de quantifier l’importance du lien entre les variables étudiées, toujours sans donner le sens."),
                                                 helpText("Le résultat du V de Cramer sera toujours entre 0 et 1. 0 étant l’indépendance totale et 1 la contingence totale.Donc plus notre V est proche de 1 plus le lien entre nos variables est fort. 0.1 étant faible, 0.3 moyen et au-delà de 0.5 fort."),
                                                 verbatimTextOutput("v_result")
                                        )
                                        
                                      )
                                    )
                                  )
                                  
                         ),
                         tabPanel("Mosaic plot chi²",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      helpText("Ici sont représenté les résidus, vous obtiendrez donc un graphique des ecarts aux effectifs théoriques, pour chaque croisement de modalité. Il s'agit d'une des représentations graphiques classiques du chi², qui aide à emettre une hypothèse sur le sens du lien entre les variables."),
                                      textInput("txttitle", label = "Le titre de votre graphique :", value = ""),
                                      selectizeInput("las", "Style des labels de chaque axe :", choices = c("Parallèle à l'axe"="0", "Toujours horizontal"="1", "Perpendiculaire à l'axe"="2", "Toujours vertical"="3"), multiple=FALSE, selected="1"),
                                      sliderInput("off", label="Pourcentage d'écartement entre chaque niveau du mosaic plot", value=5, min=0, max=50, step=1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats#
                                    ############################################################################
                                    mainPanel(plotOutput("mosaic", height="750px"),
                                              radioButtons(
                                                inputId = "filetype_mosaic",
                                                label = "Quel format d'image :",
                                                inline = TRUE,
                                                choices = list("PDF", "PNG","SVG")),
                                              downloadButton(outputId = "downloadmosaic", label = "Télécharger Mosaic plot")
                                    )
                                  )
                         ),
                         tabPanel("Histogramme chi²",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      helpText("Ici sont représenté les résidus, vous obtiendrez donc un graphique des ecarts aux effectifs théoriques, pour chaque croisement de modalité. Il s'agit d'une des représentations graphiques classiques du chi², qui aide à emettre une hypothèse sur le sens du lien entre les variables."),
                                      textInput("histotitle", label = "Le titre de votre graphique :", value = ""),
                                      sliderInput("taille_label", label="Taille police labels", value=8, min=6, max=14, step=1),
                                      sliderInput("taille_leg", label="Taille police légende", value=10, min=6, max=14, step=1),
                                      sliderInput("tlsrt", label="Degrée d'inclinaison des label", value=0, min=0, max=90, step=1),
                                      sliderInput("xcale", label="Espace entre les barres de l'istogramme", value=0.6, min=0, max=1, step=0.1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats#
                                    ############################################################################
                                    mainPanel(plotOutput("histo", height="750px"),
                                              radioButtons(
                                                inputId = "filetype_histo",
                                                label = "Quel format d'image :",
                                                inline = TRUE,
                                                choices = list("PDF", "PNG","SVG")),
                                              downloadButton(outputId = "downloadhisto", label = "Télécharger Histogramme")
                                    )
                                  )
                         ),
                         
                         tabPanel("Balloon Plot chi²",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      helpText("Ici sont également représentés les résidus de votre chi², cette représentation sous forme balloo plot présente toutefois un biais. La taille des figurés est basé sur la valeur du résidu. Plus il sera faible (négatif) plus le figuré sera petit. Ce qui a tendance à induire une erreur d'interprétation. Un résidu de -15 n'est pas moins important ou plus petit qu'un résidu de 10."),
                                      selectizeInput("themeballoon", "Quel thème pour votre Balloon Plot ?", choices = c("theme_gray","theme_bw","theme_linedraw", "theme_light","theme_minimal", "theme_void","theme_dark", "theme_tufte", "theme_economist","theme_calc","theme_wsj","theme_hc"), multiple=FALSE, selected = 'theme_wsj'),
                                      textInput("balloontitle", label = "Le titre de votre graphique :", value = ""),
                                      selectizeInput("couleur", "Quelle palette de couleur ?", choices = c("viridis", "magma", "plasma", "inferno"), multiple=FALSE, selected="viridis"),
                                      sliderInput("pmin", label="Taille minimale du point", value=1, min=1, max=10, step=1),
                                      sliderInput("pmax", label="Taille maximale du point", value=10, min=10, max=50, step=1),
                                      selectizeInput("forme", "Style des labels de chaque axe :", choices = c("Rond"="21", "Carré"="22", "Losange"="23", "Triangle"="24","Triangle inversé"="25"), multiple=FALSE, selected="21")
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats#
                                    ############################################################################
                                    mainPanel(plotOutput("balloon", height="750px"),
                                              radioButtons(
                                                inputId = "filetype_balloon",
                                                label = "Quel format d'image :",
                                                inline = TRUE,
                                                choices = list("PDF", "PNG","SVG")),
                                              downloadButton(outputId = "downloadballoon", label = "Télécharger Balloon plot")
                                    )
                                  )
                         ),
                         
                         
                         tabPanel("Extra",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      helpText("Ici c'est juste pour le fun ! Vous pourrez représenter votre tableau croisé sous forme de balloon plot. Ce sera moins lisible et moins interprétable mais ça change! Toutefois avec le biais d'induire une hierarchie dans l'interprétation. Les plus petits effectifs sont représentés par les plus petits figurés, ce qui de manière implicite et erroné donne l'impression d'une moindre importance."),
                                      selectizeInput("themetb", "Quel thème pour votre Balloon Plot ?", choices = c("theme_gray","theme_bw","theme_linedraw", "theme_light","theme_minimal", "theme_void","theme_dark", "theme_tufte", "theme_economist","theme_calc","theme_wsj","theme_hc"), multiple=FALSE, selected = 'theme_wsj'),
                                      textInput("tbtitle", label = "Le titre de votre graphique :", value = ""),
                                      selectizeInput("tbcouleur", "Quelle palette de couleur ?", choices = c("viridis", "magma", "plasma", "inferno"), multiple=FALSE, selected="viridis"),
                                      sliderInput("tbpmin", label="Taille minimale du point", value=1, min=1, max=10, step=1),
                                      sliderInput("tbpmax", label="Taille maximale du point", value=10, min=10, max=50, step=1),
                                      selectizeInput("tbshape", "La forme de vos ballons :", choices = c("Rond"="21", "Carré"="22", "Losange"="23", "Triangle"="24","Triangle inversé"="25"), multiple=FALSE, selected="21")
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats#
                                    ############################################################################
                                    mainPanel(plotOutput("bt", height="750px"),
                                              radioButtons(
                                                inputId = "filetype_bt",
                                                label = "Quel format d'image :",
                                                inline = TRUE,
                                                choices = list("PDF", "PNG","SVG")),
                                              downloadButton(outputId = "downloadbt", label = "Télécharger Balloon table")
                                    )
                                  )
                         )
              )
)




server <- function(input, output, session) {
  
  
  library(shiny)
  library(shinythemes)
  library(viridis)
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(wesanderson)
  library(ggthemes)
  library(questionr)
  library(ggpubr)
  library(ggplot2)
  library(vcd)
  
  #####################################################
  # 5. Charger les données importées et les visualiser#
  #####################################################
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
  })  
  output$table <- renderTable({
    req(input$file1)
    
    # Si le séparateur de champs est un point-virgule,
    # Avoir une virgule comme séparateur causera une erreur dans la fonction read.csv
    df <- reactive({ read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
    })
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  
  ################################################################
  # 6. Charger les données et maj des selectizeInput #
  ################################################################ 
  dat <- reactive({
    file1 <- input$file1
    req(file1)
    dataSet <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, dec =input$dec, quote = input$quote)
    vars <- colnames(dataSet)
    #row <- nrow(dataSet)
    updateSelectizeInput(session, "selectl", "Variable ligne", choices=vars, selected = "")
    updateSelectizeInput(session, "selectc", "Variable ligne", choices = vars, selected = "")
    dataSet
  })
  
  #  observe({
  #    varVI <- colnames(dat())
  #    updateSelectizeInput(session, "selectVd", "Choix des variables à corréler", choices = varVI)
  #  })
  
  
  tab <- reactive({
    with(dat(), table(get(input$selectl),get(input$selectc), useNA = input$nulle))
  })
  
  tab1 <- reactive({
    xtabs(as.formula(paste0("~",input$selectl,"+",input$selectc)), dat())
  })
  
  output$tab_result<-renderPrint({
    tab()
  })
  
  chi2 <- reactive({chisq.test(tab())})  
  
  output$chi_result <-renderPrint({
    chi2()
  })
  
  output$theo <-renderPrint({
    chi2()$expected
  })
  
  output$residus <-renderPrint({
    chi2()$residuals
  })
  
  output$fish_result <-renderPrint({
    fisher.test(tab())
  })
  
  output$v_result <-renderPrint({
    cramer.v(tab())
  })
  
  
  residual <- reactive({
    as.data.frame(chi2()$residuals)
  })
  
  
  
  mosaic <- reactive({
    mosaicplot(tab(), shade = TRUE, las = input$las, off=input$off, main = input$txttitle)
  })
  
  
  output$mosaic <- renderPlot({ 
    mosaic()
  })
  
  output$downloadmosaic <- downloadHandler(
    filename = function(){
      paste("Mon_mosaic_plot", tolower(input$filetype_mosaic), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_mosaic == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_mosaic == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      mosaicplot(tab(), shade = TRUE, las = input$las, off=input$off, main = input$txttitle)
      dev.off()
    })
  
  histo <- reactive({
    assoc(tab(), shade = TRUE, las = 3, main =input$histotitle, compress = TRUE, keep_aspect_ratio = TRUE, xscale = input$xcale, rot_labels = input$tlsrt, gp_labels = gpar(fontsize = input$taille_label), legend = legend_resbased(fontsize = input$taille_leg)) # labeling=labeling_residuals)#,
  })
  
  output$histo <- renderPlot({ 
    print(histo())
  })
  
  output$downloadhisto <- downloadHandler(
    filename = function(){
      paste("Mon_histogramme", tolower(input$filetype_histo), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_histo == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_histo == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      assoc(tab(), shade = TRUE, las = 3, main =input$histotitle, compress = TRUE, keep_aspect_ratio = TRUE, xscale = input$xcale, rot_labels = input$tlsrt, gp_labels = gpar(fontsize = input$taille_label), legend = legend_resbased(fontsize = input$taille_leg))
      dev.off()
    })
  
  balloon <- reactive({
    ggballoonplot(residual(), fill = "value", size.range = c(input$pmin, input$pmax), shape = as.numeric(input$forme)) + scale_fill_viridis_c(option = input$couleur) +
      ggtitle(input$balloontitle) + get(input$themeballoon)()
  })
  
  
  output$balloon <- renderPlot({ 
    balloon()
  })
  
  output$downloadballoon <- downloadHandler(
    filename = function(){
      paste("Mon_balloon_plot", tolower(input$filetype_balloon), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_balloon == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_balloon == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      print(balloon())
      dev.off()
    })
  
  tab2 <- reactive({
    as.data.frame(tab())
  })
  
  tb <- reactive({
    ggballoonplot(tab2(), fill = "value", size.range = c(input$tbpmin, input$tbpmax), shape = as.numeric(input$tbshape)) + scale_fill_viridis_c(option = input$tbcouleur) +
      ggtitle(input$tbtitle) + get(input$themetb)()
  })
  
  output$bt <- renderPlot({ 
    tb()
  })
  
  
  output$downloadbt <- downloadHandler(
    filename = function(){
      paste("Mon_tableau_ballon", tolower(input$filetype_bt), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_bt == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_bt == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      print(tb())
      dev.off()
    })
  
}



shinyApp(ui=ui,server=server)


