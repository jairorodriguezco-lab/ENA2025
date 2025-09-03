library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(dplyr)
library(DT)
library(tidyverse)
library(stringr)
library(plotly)
library(shinyjs)
library(shinyBS)
library(readxl)
library(rhandsontable)
library(shinyWidgets)
library(slickR)
library(writexl)

options(scipen = 999)

PopOverT <-
  '<div class="popover popover-lg" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>'
PopOverT2 <-
  '<div class="popover popover2-lg" role="tooltip"><div class="arrow"></div><h3 class="popover-title popover-ttl"></h3><div class="popover-content popover-cnt"></div></div>'


CGS <- as.data.frame(read_excel("CGS.xlsx"))
AGR1 <- as.data.frame(read_excel("AGR1.xlsx"))
AGR2 <- as.data.frame(read_excel("AGR2.xlsx"))
AGR3 <- as.data.frame(read_excel("AGR3.xlsx"))



ui <- dashboardPage(
  title = "Encuesta Nacional Agropecuaria",
  skin = "black",
  dashboardHeader(
    title = tags$img(src = 'logo_INEGI-01.png', style = "height: 30px; width: 130px;display: block; margin-left: auto; margin-top:12px; margin-right: auto;"),
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height:55px}"),
      tags$style(".main-header .logo {height: 55px}")
    ),
    tags$li(
      class = "dropdown",
      tags$img(src = "Logo_ENA2025.png", style = "height: 60px; width: 180px; display: block; margin-left: auto; margin-top:auto; margin-right: 10px;")
      
      
    ),
    dropdownMenu(
      headerText = tags$b("Formas de contacto"),
      type = "messages",
      icon = tags$i(class = "fa-regular fa-comment-dots fa-beat", style = "font-size: 20px;--fa-animation-duration: 4s; color:#003067"),
      
      messageItem(
        "Sitio Principal",
        uiOutput("site"),
        icon = icon("sitemap"),
        href = "https://www.inegi.org.mx/"
      ),
      messageItem(
        "Facebook",
        uiOutput("face"),
        icon = icon("facebook-square"),
        href = "https://www.facebook.com/INEGIInforma/"
      ),
      messageItem(
        "Twitter",
        "",
        icon = icon("square-x-twitter"),
        href = "https://x.com/inegi_informa"
      ),
      
      
      messageItem(
        "Youtube",
        uiOutput("youtube"),
        icon = icon("youtube-square"),
        href = "https://www.youtube.com/user/INEGIInforma"
      ),
      messageItem(
        "Instagram",
        uiOutput("insta"),
        icon = icon("instagram-square"),
        href = "https://www.instagram.com/inegi_informa/"
      ),
      messageItem("RSS", "", icon = icon("square-rss"), href = "https://www.inegi.org.mx/servicios/rss/"),
      
      badgeStatus = NULL
    )
  ),
  
  dashboardSidebar(
    # width = 300,
    tags$head(tags$style(HTML(
      '* {font-family: "arial"};'
    ))),
    tags$head(tags$link(rel = "shortcut icon", href = "enagro2014_ch.png")),
    sidebarMenu(
      id = "ID_Barra",
      menuItem(
        "Tabulados oportunos",
        tabName = "tabuladoOP",
        icon = icon("th")
      )
      ,
      div(menuItem(fluidRow(
        div(
          radioButtons(
            "Grupo_tab",
            label = "Seleccione el apartado",
            c(
              "Características generales" = 1,
              "Agricultura" = 2,
              "Ganadería" = 3
            ),
            selected = 0
          ),
          style = "height: auto; width: auto;display: block;margin-top:0px;"
        )
        
      )) , style = "padding-left:20px;padding-top:0px;margin-top:0px")
    )
  ),
  dashboardBody(useShinyjs(),
                
                # 
                # tags$head(tags$style(HTML('
                #                 /* logo */
                #                 .skin-blue .main-header .logo {
                #                 background-color: #a4ccbe;
                #                 }
                #                /* logo when hovered */
                #                 .skin-blue .main-header .logo:hover {
                #                 background-color: #a4ccbe;
                #                 }
                #                 /* navbar (rest of the header) */
                #                 .skin-blue .main-header .navbar {
                #                 background-color: #a4ccbe;
                #                 }
                # 
                # 
                #                 '))),
                # 
                
                
                tabItems(
    tabItem(useShinyjs(), tabName = "tabuladoOP", fluidPage(
      tabBox(
        width = 13,
        height = "88vh",
        
        id = "TabuladoO",
        tabPanel(
          tags$head(tags$style(
            HTML(
              ".popover.popover-lg {width: 500px; max-width: 500px;}.popover-title{color: #000000;font-size: 17px;background-color: #F7F5F4;font-weight: bold;}"
            )
          )),
          tags$head(tags$style(
            HTML(
              ".popover.popover2-lg {width: auto; max-width: auto;}.popover-title.popover-ttl {color: #000000;font-size: 17px;background: rgba(247, 245, 244, .2);font-weight: bold;text-align:center;font-family: arial}.popover-content.popover-cnt{color: #000000;background-color: #ffffff;font-weight: normal;text-align:center;font-family: arial}"
            )
          )),
          title = "Consulta",
          fluidPage(
            conditionalPanel(
              condition = 'input.Grupo_tab!=null',
              column(
                12,
                align = "center",
                actionButton("generar", "Ver consulta", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                actionButton("reset", "Limpiar", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
              )
            ),
            
              #########################################################CGN###################################################################
             
            conditionalPanel(
              condition = 'input.Grupo_tab==1',
              column(4, wellPanel(
                radioButtons(
                  "CGS_partes",
                  label = "Seleccione",
                  c(
                    "Cantidad de terrenos" = 1,
                    "Categoría juridica" = 2,
                    "Tipo de tenencia" = 3,
                    "Derechos sobre la tierra" = 4,
                    "Uso de suelo" = 5,
                    "Regimen hídrico" = 6
                  ),
                  selected = 0
                )
              )), column(4,  conditionalPanel(condition = 'input.Grupo_tab==1 && input.CGS_partes!=null',wellPanel(
                checkboxGroupInput("UpSup", label = "Unidades o Superficie", 
                  c(
                  "Unidades de producción" = 1, 
                  "Superficie" = 2
                )
                )
              )))),
            conditionalPanel(condition = 'input.Grupo_tab==1 && input.CGS_partes!=null && (input.UpSup.includes("1")||input.UpSup.includes("2"))',
                            column(4, wellPanel(
              conditionalPanel(condition = 'input.CGS_partes!=null && input.UpSup.includes("1")', uiOutput('Agro_Out_UP')),
              conditionalPanel(condition = 'input.CGS_partes!=null && input.UpSup.includes("2")', uiOutput('Agro_Out_SUP')))
            )
),
              #########################################################CGN#
              
              #########################################################AGR
              
            conditionalPanel(
              condition = 'input.Grupo_tab==2',
              column(4, wellPanel(
                radioButtons(
                  "AGR_partes",
                  label = "Seleccione la temática",
                  c(
                    "Producción, cultivos anuales" = 1,
                    "Ciclo primavera verano" = 2,
                    "Ciclo otoño invierno" = 3,
                    "Tipo de semilla" = 4,
                    "Destino de la producción, cultivos anuales" = 5,
                    "Venta de la producción, cultivos anuales" = 6,
                    "Producción, cultivos perennes" = 7,
                    "Edad de la plantación"=8,
                    "Destino de la producción, cultivos perennes"=9,
                    "Venta de la producción, cultivos perennes"=10,
                    "Mermas"=11
                    
                  ),
                  selected = 0
                )
              )),
              
              column(4,wellPanel(
                tags$b("Seleccione la entidad"),
                checkboxInput("todas", tags$b("Todas las entidades"), FALSE),
                selectizeInput(
                  "entidad",
                  label = NULL,
                  unique(AGR1[,1]),
                  multiple = TRUE,
                  options = list(
                    'plugins' = list('remove_button'),
                    'create' = TRUE,
                    'persist'  = FALSE,
                    placeholder = 'Selecciona una opción',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                ),
                    conditionalPanel(condition='input.AGR_partes==11',
                                     checkboxGroupInput("Cultiv_AP", label = "Tipo de cultivo", 
                                                        c(
                                                          "Cultivos anuales"="Anuales",
                                                          "Cultivos perennes"="Perennes"
                                                        )
                                     )
                                     ),
                
               
                     
                     uiOutput('Culti_Out')
                )
              )
            ),
               column(4, 
             uiOutput('Agro_Out')
            )
              #########################################################AGR
          )
          
        )
        
      )
    ))
    
    
  ))
)

server <- function(input, output, session) {
  
  
  
  observeEvent(input$todas, {
    if (input$todas == TRUE) {
      shinyjs::disable("entidad")
      updateSelectizeInput(session =  session,
                           inputId =  "entidad",
                           selected = "")
    } else {
      shinyjs::enable("entidad")
    }
    
  })
  
  observeEvent(input$todos_c, {
    if (input$todos_c == TRUE) {
      shinyjs::disable("cultivo")
      updateSelectizeInput(session =  session,
                           inputId =  "cultivo",
                           selected = "")
    } else {
      shinyjs::enable("cultivo")
    }
    
  })
  
  Cultiv <- eventReactive(req((!is.null(input$entidad) || input$todas==TRUE),input$AGR_partes %in% c(1:10) || (input$AGR_partes %in% c(11:11) && isTruthy(input$Cultiv_AP))),{
    
    #
    if(input$todas==TRUE){Entidades= unique(AGR1[,1])}else{Entidades=input$entidad}
    #
    
    if(input$AGR_partes %in% C(1:6)){
      act<-   unique(na.omit(AGR1[AGR1$'Entidad federativa' %in% Entidades,2]))
    }else{
    if(input$AGR_partes %in% C(7:10))
      {act<-unique(na.omit( AGR2[AGR2$'Entidad federativa' %in% Entidades,2]))}
    else{
      
      act<-unique(na.omit(AGR3[AGR3[AGR3$'Entidad federativa' %in% Entidades,2] %in% input$Cultiv_AP ,3]))
      
    }
    }
  })
  
  output$Culti_Out = renderUI({
    tagList(
    tags$b("Seleccione los cultivos"),
    checkboxInput("todos_c", tags$b("Todos los cultivos"), FALSE),
    selectizeInput(
      "cultivo",
      label = NULL,
      choices = Cultiv(),
      multiple = TRUE,
      options = list(
        'plugins' = list('remove_button'),
        'create' = TRUE,
        'persist'  = FALSE,
        placeholder = 'Selecciona una opción',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  )
  })
  
 
  Var_A <- eventReactive(req((input$AGR_partes %in% c(1:10) && (!is.null(input$entidad) || input$todas==TRUE) && (!is.null(input$cultivo) || input$todos_c==TRUE) ) || (input$AGR_partes %in% c(11:11) && isTruthy(input$Cultiv_AP))),{
    if(input$AGR_partes==1){act<- colnames(AGR1[, 4:16])}else{
      if(input$AGR_partes==2){act<- colnames(AGR1[, c(4,5,17:28)])}else{
        if(input$AGR_partes==3){act<- colnames(AGR1[, c(4,5,29:40)])}else{
          if(input$AGR_partes==4){act<- colnames(AGR1[, c(4,5,41:45)])}else{
            if(input$AGR_partes==5){act<-  c(colnames(AGR1[4]),substr(colnames(AGR1[, 46:51]), 1, nchar(colnames(AGR1[, 46:51])) - 5))}else{
              if(input$AGR_partes==6){act<-  c(colnames(AGR1[4]),"Unidades de producción con cultivos anuales en agricultura a cielo abierto"="Producción en cielo abierto_8","Unidades de producción con venta de la producción"= "Producción vendida" ,substr(colnames(AGR1[, 59:70]), 1, nchar(colnames(AGR1[, 59:70])) - 5))}else{
                if(input$AGR_partes==7){act<- colnames(AGR2[,4:16])}else{
                  if(input$AGR_partes==8){act<-  c(colnames(AGR2[4]),"UP con plantaciones perennes en agricultura a cielo abierto"= "Plantaciones perennes en agricultura a cielo abierto" ,substr(colnames(AGR2[, 18:22]), 1, nchar(colnames(AGR2[, 18:22])) - 5))}else{
                    if(input$AGR_partes==9){act<-  c(colnames(AGR2[4]),"UP con plantaciones perennes en agricultura a cielo abierto"= "Plantaciones perennes en agricultura a cielo abierto" ,substr(colnames(AGR2[, 30:34]), 1, nchar(colnames(AGR2[, 30:34])) - 5))}else{
                      if(input$AGR_partes==10){act<-  c(colnames(AGR2[4]),"Plantaciones perennes en agricultura a cielo abierto"="Producción de cultivos anuales en agricultura a cielo abierto ","Unidades de producción con venta de la producción"= "Producción vendida" ,substr(colnames(AGR2[, 43:53]), 1, nchar(colnames(AGR2[, 43:53])) - 5))}else{
                        if(input$AGR_partes==11)act<- c(colnames(AGR3[,c(5,6)]),substr(colnames(AGR3[, 7:13]), 1, nchar(colnames(AGR3[, 7:13])) - 5))else{}
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
     })
  
  output$Agro_Out = renderUI({
    wellPanel(
    selectizeInput(
      "AGRO_var",
      label = "Selecciones las variables de agricultura",
      Var_A(),
      multiple = TRUE,
      options = list(
        'plugins' = list('remove_button'),
        'create' = TRUE,
        'persist'  = FALSE,
        placeholder = 'Selecciona una opción',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    )
  })
  
  
  Var_CGS_UP <- eventReactive(req(input$CGS_partes,input$Grupo_tab==1 && 1 %in% input$UpSup),{
    if(input$CGS_partes==1){ act<-colnames(CGS[, 2:8])}
    else{if(input$CGS_partes==2){ act<-colnames(CGS[, c(2,16:18)])}
      else{if(input$CGS_partes==3){ act<-colnames(CGS[, c(2,22:26)])}
        else{if(input$CGS_partes==4){ act<-colnames(CGS[, c(2,32:35)])}
          else{if(input$CGS_partes==5){ act<-colnames(CGS[, c(2,40:54)])}
            else{ act<-colnames(CGS[, c(2,70:72)])}}}}}
    
    
  })
  
  output$Agro_Out_UP = renderUI({
    
    selectizeInput(
      "UdeP",
      label = "Características generales de las unidades de producción",
      Var_CGS_UP(),
      multiple = TRUE,
      options = list(
        'plugins' = list('remove_button'),
        'create' = TRUE,
        'persist'  = FALSE,
        placeholder = 'Selecciona una opción',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    
  })
  
  Var_CGS_SUP <- eventReactive(req(input$CGS_partes,input$Grupo_tab==1 && 2 %in% input$UpSup),{
   if(input$CGS_partes==1){ act<-colnames(CGS[, c(9:15)])}
     else{if(input$CGS_partes==2){ act<-colnames(CGS[, c(9,19:21)])}
       else{if(input$CGS_partes==3){ act<-colnames(CGS[, c(9,27:31)])}
         else{if(input$CGS_partes==4){ act<-colnames(CGS[, c(9,36:39)])}
           else{if(input$CGS_partes==5){ act<-colnames(CGS[, c(9,55:69)])}
             else{ act<-colnames(CGS[, c(3,73:75)])}}}}}
    
    
    })
  
  output$Agro_Out_SUP = renderUI({
    
    selectizeInput(
      "SuDeP",
      label = "Características generales de las unidades de producción por superficie",
      Var_CGS_SUP(),
      multiple = TRUE,
      options = list(
        'plugins' = list('remove_button'),
        'create' = TRUE,
        'persist'  = FALSE,
        placeholder = 'Selecciona una opción',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    
  })
  
  
  
  
  shinyBS::addPopover(
    session,
    "elemento",
    "",
    content = paste0(
      tags$em (tags$b("ayuda 1: ")),
      "texto1",
      tags$br(),
      
      tags$em (tags$b("ayuda 2: ")),
      "texto2",
      tags$br()
      
    ),
    "right",
    options = list(template = PopOverT),
    
    trigger = 'hover'
  )
  
  
  
  output$Descargarc <- downloadHandler(
    filename = function() {
      paste(input$mytable,
            "Consulta",
            format(Sys.Date(), "_%d-%m-%y"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(
        Dataframe(),
        file,
        row.names = FALSE,
        fileEncoding = "windows-1252",
        na = ""
      )
    }
    
  )
  output$Descargarx <- downloadHandler(
    filename = function() {
      paste(input$mytable,
            "Consulta",
            format(Sys.Date(), "_%d-%m-%y"),
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      write_xlsx(Dataframe(), file)
    }
    
  )
  
  observeEvent(input$generar, {
    if (
      (!is.null(input$UpSup) && !is.null(input$UdeP)||!is.null(input$SuDeP)) ||
      ((!is.null(input$entidad)||input$todas==TRUE)  && (!is.null(input$cultivo)||input$todos_c==TRUE) && !is.null(input$AGRO_var))
       )
 {
      shinyjs::disable(id = "generar")
      shinyjs::hide(id = "Grupo_tab")
      
      appendTab(inputId = "TabuladoO",
                tab = tabPanel(
                  title = "CGS",
                  wellPanel(
                    fluidRow(
                      column(
                        9,
                        align = "left",
                        dropdownButton(
                          label = "Exportar como:",
                          icon = icon("save"),
                          status = "primary",
                          circle = FALSE,
                          downloadButton("Descargarc", ".csv", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          h6(""),
                          downloadButton("Descargarx", ".xlsx", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        )
                      ),
                      column(
                        3,
                        align = "right",
                        actionButton("Cerrar", "Volver a la selección", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      )
                      
                    ),
                    h6(""),
                    DT::dataTableOutput("mytable"),
                    style = "padding: 10px;padding-top: 10px;margin-top:0px;background-color: #FFFFFF;width:100%"
                  )
                ))
      
      updateTabsetPanel(session, inputId = "TabuladoO", selected = "CGS")
      hideTab(inputId = "TabuladoO",
              target = "Consulta",
              session = getDefaultReactiveDomain())
    }
    else{
      showModal(
        modalDialog(
          align = "center",
          div("ATENCIÓN", align = "center", style = "font-size:150%"),
          "Seleccione todos los campos",
          easyClose = TRUE,
          footer = NULL,
          size = "s"
        )
      )
    }
  })
  
  ################################################################################################
  
  observeEvent(input$reset, {
    updateRadioButtons(
      session,
      "AGR_partes",
      label = "Seleccione el apartado",
      c(
        "Producción, cultivos anuales" = 1,
        "Ciclo primavera verano" = 2,
        "Ciclo otoño invierno" = 3,
        "Tipo de semilla" = 4,
        "Destino de la producción, cultivos anuales" = 5,
        "Venta de la producción, cultivos anuales" = 6,
        "Producción, cultivos perennes" = 7,
        "Edad de la plantación"=8,
        "Destino de la producción, cultivos perennes"=9,
        "Venta de la producción, cultivos perennes"=10,
        "Mermas"=11
      ),
      selected = 0
    )
    
    updateRadioButtons(
      session,
      "CGS_partes",
      label = "Seleccione",
      c(
        "Cantidad de terrenos" = 1,
        "Categoría juridica" = 2,
        "Tipo de tenencia" = 3,
        "Derechos sobre la tierra" = 4,
        "Uso de suelo" = 5,
        "Regimen hídrico" = 6
      ),
      selected = 0
    )
    
    updateCheckboxGroupInput (
      session,
      "UpSup",
      label = "Unidades o Superficie",
      c(
        "Unidades de producción" = 1,
        "Superficie" = 2
      ),
      selected = NULL
    )
    
    updateSelectizeInput(session =  session,
                         inputId =  "UdeP",
                         selected = "")
    updateSelectizeInput(session =  session,
                         inputId =  "SuDeP",
                         selected = "")
    updateSelectizeInput(session =  session,
                         inputId =  "entidad",
                         selected = "")
    updateCheckboxInput(session =  session,
                        inputId =  "todas",
                        value = FALSE)
    
    
  })
  
  
  ######################################################################################################
  
  observeEvent(input$Cerrar, {
    removeTab(inputId = "TabuladoO", "CGS", session = getDefaultReactiveDomain())
    showTab(
      inputId = "TabuladoO",
      "Consulta",
      select = TRUE,
      session = getDefaultReactiveDomain()
    )
    shinyjs::enable(id = "generar")
    shinyjs::show(id = "Grupo_tab")
    
  })
  
  
  
  ###########################################################################################################
  
  ####################################
  
  Dataframe <- reactive({

    if(input$Grupo_tab==1){
    variables <- c(colnames(CGS[1]), input$UdeP, input$SuDeP)
    Data <- CGS[, colnames(CGS) %in% variables]}
    
    else{if(input$Grupo_tab==2&&input$AGR_partes %in% (1:6)){
      if(input$todas==TRUE){Entidades=unique(AGR1[,1])}else{Entidades=input$entidad}
      if(input$todos_c==TRUE){Cultivos=unique(AGR1[,2])}else{Cultivos=input$cultivo}
      
      AGR_var<-colnames(AGR1[grepl(gsub(
        "([()])",
        "\\\\\\1",
        paste(input$AGRO_var, collapse = '|')
      ),
      colnames(AGR1))])
      variables <- c(colnames(AGR1[,1:3]), AGR_var)
      Data <- AGR1[, colnames(AGR1) %in% variables]
      Data <- Data[Data$'Entidad federativa' %in% Entidades,]
      Data<-Data[Data$Cultivo %in% Cultivos | is.na(Data$Cultivo),]
      }
      else{if(input$Grupo_tab==2&&input$AGR_partes %in% (7:10)){
        if(input$todas==TRUE){Entidades=unique(AGR1[,1])}else{Entidades=input$entidad}
        if(input$todos_c==TRUE){Cultivos=unique(AGR1[,2])}else{Cultivos=input$cultivo}
        
        AGR_var<-colnames(AGR2[grepl(gsub(
          "([()])",
          "\\\\\\1",
          paste(input$AGRO_var, collapse = '|')
        ),
        colnames(AGR2))])
        variables <- c(colnames(AGR2[,1:3]), AGR_var)
        Data <- AGR2[, colnames(AGR2) %in% variables]
        Data <- Data[Data$'Entidad federativa' %in% Entidades,]
        Data<-Data[Data$Cultivo %in% Cultivos | is.na(Data$Cultivo),]
      }
        else{if(input$Grupo_tab==2&&input$AGR_partes %in% (11:11)){
          if(input$todas==TRUE){Entidades=unique(AGR1[,1])}else{Entidades=input$entidad}
          if(input$todos_c==TRUE){Cultivos=unique(AGR1[,2])}else{Cultivos=input$cultivo}
          AGR_var<-colnames(AGR3[grepl(gsub(
            "([()])",
            "\\\\\\1",
            paste(input$AGRO_var, collapse = '|')
          ),
          colnames(AGR3))])
          variables <- c(colnames(AGR3[,1:4]), AGR_var)
          Data <- AGR3[, colnames(AGR3) %in% variables]
          Data <- Data[Data$'Entidad federativa' %in% Entidades,]
          Data<-Data[Data$Cultivo %in% Cultivos | is.na(Data$Cultivo),]
        }
          else{}
          
        }
        
      }
      
      }
  })
  
  
  ########################################
  
  
  output$mytable <- DT::renderDataTable(
    datatable(
      Dataframe(),
      
      extensions = list(
        'FixedHeader' = NULL,
        'Buttons' = NULL,
        'ColReorder' = NULL,
        'RowReorder' = NULL
      ),
      selection = 'single',
      rownames = FALSE,
      class = c("display"),
      escape = FALSE,
      options = list(
        search = list(regex = TRUE),
        language = list(
          paginate =
            list('next' = "Siguiente", previous = "Anterior"),
          lengthMenu = 'Resultados por página _MENU_',
          info = "Mostrando _START_ de _END_ filas",
          sSearch = "Buscar: "
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#CDCDCD', 'color': 'black'});",
          "$('th:first-child').css({'border-top-left-radius': '5px'});",
          "$('th:last-child').css({'border-top-right-radius': '5px'});",
          "}"
        ),
        paging = TRUE,
        pageLength = 50,
        fixedHeader = TRUE,
        digits = 3,
        oColReorder = list(colReorder = TRUE),
        oRowReorder = list(rowReorder = TRUE),
        pageLength = 20,
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = FALSE,
        searchHighlight = TRUE,
        server = FALSE,
        dom = 'RBlfrtip',
        # columnDefs = list(list(
        #   className = 'dt-right', targets = c(-1)
        # ),list(
        #   className = 'dt-left', targets = c(1)
        # ),list(
        #   className = 'dt-head-center', targets = "_all"
        # )),
        buttons = list()
      )
    ) %>%
      DT::formatStyle(
        names(Dataframe()),
        backgroundColor = 'white',
        opacity = 1
      ) %>%
      formatRound(purrr::map_lgl(.$x$data, is.numeric), digits = 2)
  )
  
}

shinyApp(ui, server)
