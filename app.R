library(shiny)
library(tidyverse)
library(readxl)
library(purrr)
library(htmltools)
library(spdep)
library(mclust)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(flexmix)
library(plyr)
library(cowplot)
library(ggpubr)
library(leaflet)
library(mixtools)
library(rlang)
library(broom)
library(repr)
library(mclust)
library(cluster)    # clustering algorithms
library(factoextra)
library(flexmix)
library(plyr)
library(cowplot)
library(ggpubr)
library(leaflet)
library(yum)
#install.packages('brio',type="binary")
#install.packages(c("ellipsis","fastmap","fs","glue","magrittr","mclust","processx", "purrr", "rlang"))

################################################################################
### Reading in Data: -----------------------------------------------------------
W_D_data_prep <- dget("W_D_data_prep_V2.R")
W_D_C <- dget("W_D_C_V2.R")

# Shapefile:
QLD_SA2_SHP <- read_sf("sQLD.shp")
df <- W_D_data_prep()
# AUS_SA2_SHP <- read_sf("C:/Users/n10677313/OneDrive - Queensland University of Technology/Desktop/Two_panels","SA2_2016_AUST") %>% filter(!st_is_empty(.)) # SA2.
# QLD_SA2_SHP <- AUS_SA2_SHP %>% filter(STE_CODE16==3) %>% filter(!st_is_empty(.))
QLD_SA2_SHP_noPoly <- st_drop_geometry(QLD_SA2_SHP)

#dfn <- df[(df$opinion_1 != df$opinion_2) | is.na(df$opinion_1) | is.na(df$opinion_2),]
################################################################################
### Read Data:
data <- read.csv("heat_maps.csv")
data[sapply(data[,4:10], is.numeric)] <- lapply(data[sapply(data[,4:10], is.numeric)], as.factor)

# premap <- filter(AUS_SA2_SHP,STE_CODE16 == "1")
premap <- QLD_SA2_SHP
map1 <- spdf <- as(premap, "Spatial")


ui <- navbarPage('My application',id='My application',
                 tabPanel('K-means Cluster Summary',
                          bootstrapPage(
                            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                            absolutePanel(style = "background-color: white;
                         opacity: 1;
                         padding-left: 15px;
                         padding-right: 15px;
                         margin: auto;
                         border-radius: 0pt;
                         box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                         padding-bottom: 2mm;
                         padding-top: 0mm;
                         overflow-y: scroll;",
                                          fixed = TRUE, top = 50, left = 0, right = "auto", bottom = "auto", width = "30%", height = "100%", id="pOPT",
                                          
                                          # Panel Title:
                                          h3("K-means Shiny App", style = "color:#3474A7"),
                                          
                                          # Introductory text:
                                          helpText("Clustering visualiations for SA2 child health vulnerability data.", width = "50%"),
                                          
                                          # Select year:
                                          selectInput(
                                            inputId = "iVULN",
                                            label = "Select Vulnerability:",
                                            choices = c("Physical","Social","Emotional","Language","Communication","Vuln.1","Vuln.2"),
                                            selected = "Physical"
                                          ),
                                          
                                          # Select parameter:
                                          selectInput(
                                            inputId = "iCN",
                                            label = "Cluster Number:",
                                            choices = 1:4,
                                            selected = 1
                                          ),
                                          
                                          h4("Summary", style = "color:#3474A7"),
                                          textOutput("centers1"),textOutput("centers2"),textOutput("centers3"),
                                          textOutput("centers4"),
                            ),
                            
                            absolutePanel(style = "background-color: white;
                         opacity: 1;
                         padding-left: 15px;
                         padding-right: 15px;
                         margin: auto;
                         border-radius: 0pt;
                         box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                         padding-bottom: 2mm;
                         padding-top: 0mm;
                         overflow-y: scroll;",
                                          fixed = TRUE, top = 50, left = "30%", right = "auto", bottom = "auto", width = "70%", height = "100%", id="pPLOT",
                                          
                                          # Panel Title:
                                          h3("Cluster Plots", style = "color:#3474A7"),
                                          
                                          # ggarrange plot
                                          plotOutput("p1", height = 750),
                                          helpText(
                                            "Note: Remoteness (1: inner cities - 5: very remote), IRSD (1:most disadvantaged - 10:least disadvantaged) "
                                          )))),
                 
                 tabPanel(title = 'Interactive Maps', 
                          bootstrapPage(
                            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                            absolutePanel(style = "background-color: white;
                         padding-bottom: 0mm;
                         padding-top: 0mm;",
                                          fixed = TRUE, top = 50, left = 0, right = 0, bottom = 0,
                            leafletOutput(outputId = "map1", height = "100%")),
                            absolutePanel(style = "background-color: white;
                         opacity: 0.85;
                         padding-left: 15px;
                         padding-right: 15px;
                         margin: auto;
                         border-radius: 5pt;
                         box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                         padding-bottom: 2mm;
                         padding-top: 0mm;",
                                          fixed = TRUE, top = 85, left = 10, right = "auto", bottom = "auto", width = 330, height = "auto",
                                          
                                          # Panel Title:
                                          h3("Vulnerability Domain Clusters", style = "color:#3474A7"),
                                          
                                          # Introductory text:
                            
                                          
                                          # # Select method:
                                          # selectInput(
                                          #   inputId = "methodselected1",
                                          #   label = "Select cluster method:",
                                          #   choices = ("Kmeans"),
                                          # bootstrapPage(
                                          #                           tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                          #                           leafletOutput(outputId = "map1", width = "100%", height = "100%"),
                                          #                           absolutePanel(style = "background-color: white;
                                          #                        opacity: 0.85;
                                          #                        padding-left: 15px;
                                          #                        padding-right: 15px;
                                          #                        margin: auto;
                                          #                        border-radius: 5pt;
                                          #                        box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          #                        padding-bottom: 2mm;
                                          #                        padding-top: 0mm;",
                                          #                                         fixed = TRUE, top = 85, left = 10, right = "auto", bottom = "auto", width = 330, height = "auto",
                                          #                                         
                                          #                                         # Panel Title:
                                          #                                         h2("Clusters Inside Health Vulnerability  within Children using K-means", style = "color:#3474A7"),
                                          #                                         
                                          #                                         # Introductory text:
                                          #                                         helpText("Create visualisations of Health vulnerability index parameters.", width = "50%"),
                                          #                                         
                                          #                                         # # Select method:
                                          #                                         # selectInput(
                                          #                                         #   inputId = "methodselected1",
                                          #                                         #   label = "Select cluster method:",
                                          #                                         #   choices = ("Kmeans"),
                                          #                                         #   selected = "Kmeans"
                                          #                                         # ),
                                          #                                         
                                          # Select parameter:
                                          selectInput(
                                            inputId = "variableselected1",
                                            label = "Select Parameter:",
                                            choices = c("Physical","Social","Emotional","Language","Communication","Vuln.1","Vuln.2"),
                                            selected = "Physical"
                                          ),
                                          
                                          # Bottom text:
                                          helpText(
                                            "Note: Visualisations are presented at the statisitcal area level 2 (SA2)."
                                          )
                            ))))




server <- function(input, output,session){
  
  
  output$p1 <- renderPlot({
    OP <- W_D_C(df,input$iVULN,input$iCN)
    OP$PLOT
  })
  
  output$centers1 <- renderText({
    OP <- W_D_C(df,input$iVULN,input$iCN)
    OPtext <- paste0("Cluster size: ", OP$KM)
    OPtext
  })
  
  output$centers2 <- renderText({
    OP <- W_D_C(df,input$iVULN,input$iCN)
    OPtext <- paste0("Cluster mean: ", OP$AVG%>% round (4))
    OPtext
  })
  
  output$centers3 <- renderText({
    OP <- W_D_C(df,input$iVULN,input$iCN)
    OPtext <- paste0("Cluster median: ", OP$MED%>% round (4))
    OPtext
  })
  output$centers4 <- renderText({
    OP <- W_D_C(df,input$iVULN,input$iCN)
    OPtext <- paste0("Cluster range:(", min(OP$RANGE%>% round (4)), ",", max(OP$RANGE%>% round (4)), ")")
    OPtext
  })
  
  #output$table <- renderDT(data)
  
  output$map1 <- renderLeaflet({
    
    
    
    orderSA2 <- match(map1@data$SA2_NAME16, data$SA2_NAME16)
    map1@data <- data[orderSA2, ]
    map1@data <- map1@data[!is.na(map1@data$Physical),]
    
    # Create variableplot
    # ADD this to create variableplot
    a1 <- as.numeric(as.matrix(map1@data[,input$variableselected1]))
    
    map1$variableplot <- a1
    
    # Create leaflet
    # CHANGE map$cases by map$variableplot
    pal <- colorBin("YlOrRd", domain = map1$variableplot, bins = 5)
    factpal <- colorFactor(c('green', 'yellow', 'orange', 'red'), map1$variableplot)
    
    # CHANGE map$cases by map$variableplot
    labels <- sprintf("%s: %g", map1$SA2_NAME16, (map1$variableplot)) %>%
      lapply(htmltools::HTML)
    
    # CHANGE cases by variableplot
    l <- leaflet(map1) %>% addTiles()%>% addPolygons(
      color = "grey",
      weight = 1,
      fillColor = ~factpal(variableplot),
      fillOpacity = 1,
      highlightOptions = highlightOptions(weight = 4),
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px", direction = "auto"
      )
    ) %>% addLegend(
      pal = factpal, values = ~variableplot, opacity = 0.7,
      title = paste0(input$variableselected1), position = "bottomright"
    )
  })
  
}



shinyApp(ui, server)

