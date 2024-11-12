pacman::p_load(shiny, sf, tmap, tidyverse, sfdep,shinycssloaders, shinydashboard, shinythemes, bslib,
             st, tidyverse, raster, tmap, tmaptools, ggplot2, spatstat,knitr)

msia <- read_rds("data/rds/msia.rds")
msia_sf <- read_sf(dsn = "data/geospatial/mys_adm_unhcr_20210211_shp", 
                   layer = "mys_admbnda_adm2_unhcr_20210211") %>%
  st_as_sf(coords =c(
    "longitude", "latitude"),
    crs = 4326) %>%
  st_transform(crs = 3168)
set.seed(2321)

combined_data <- read_rds("data/rds/combined_data.rds")

#========================#
###### Shiny UI ######
#========================#  

ui <- navbarPage(
  title = "Abang Smith",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("EDA",
           sidebarLayout(
             sidebarPanel(
               titlePanel("EDA"),
               selectInput(inputId = "EDAVariable", "Select variable for EDA",
                           choices = c("Participation rate" = "p_rate",
                                       "Unemployment rate" = "u_rate",
                                       "Crimes" = "crimes")),
               radioButtons(inputId = "EDAyear",
                            label = "Year",
                            choices = c("2019", 
                                        "2020",
                                        "2021",
                                        "2022"),
                            selected = "2019"),
               actionButton("EDAUpdate", "Plot"),
             ),
             mainPanel(
               fluidRow(
                 column(6,
                        plotOutput("edaHistogram") %>% withSpinner(color = "#3498db")
                        ),
                 column(6,
                        plotOutput("edaBoxplot") %>% withSpinner(color = "#3498db")
                        )
               ),
               plotOutput("qtmPlot") %>% withSpinner(color = "#3498db")
             )
           )
           ),
  tabPanel("Spatial Correlation",
           sidebarLayout(
             sidebarPanel(
                 titlePanel("Global and Local Spatial"),
                 fluidRow(column(7,
                                 
                                selectInput(inputId = "categoryVariable",
                                 label = "Select Category",
                                 choices = NULL),
                                 
                                selectInput(inputId = "typeVariable",
                                            label = "Select Type",
                                            choices = NULL),
                 ),
                 column(3, offset = 1,
                        radioButtons(inputId = "year",
                                     label = "Year",
                                     choices = c("2019", 
                                                 "2020",
                                                 "2021",
                                                 "2022"),
                                     selected = "2019"),
                 )),
                 radioButtons(inputId = "Contiguity1",
                              label = "Contiguity Method",
                              choices = c("Queen" = TRUE, 
                                          "Rook" = FALSE),
                              selected = "TRUE",
                              inline = TRUE),
                 selectInput("MoranWeights", "Spatial Weights Style",
                             choices = c("W: Row standardised" = "W",
                                         "B: Binary" = "B",
                                         "C: Globally standardised" = "C",
                                         "U: C / no of neighbours" = "U",
                                         "minmax" = "minmax",
                                         "S: Variance" = "S"),
                             selected = "W"),
                 sliderInput(inputId = "MoranSims", 
                             label = "Number of Simulations:", 
                             min = 99, max = 499,
                             value = 99, step = 100),
                 selectInput("LisaClass", "Select Lisa Classification",
                             choices = c("mean" = "mean",
                                         "median" = "median",
                                         "pysal" = "pysal"),
                             selected = "mean"),
                 selectInput("localmoranstats", "Select Local Moran's Stat:",
                             choices = c("local moran(ii)" = "local moran(ii)",
                                         "expectation(eii)" = "expectation(eii)",
                                         "variance(var_ii)" = "variance(var_ii)",
                                         "std deviation(z_ii)" = "std deviation(z_ii)",
                                         "P-value" = "p_value"),
                             selected = "local moran(ii)"),
                 sliderInput(inputId = "MoranConf", 
                             label = "Select Confidence level", 
                             min = 0.75, max = 0.99,
                             value = 0.75, step = 0.05),
                 titlePanel("Emerging Hot Spot Analysis"),
                 selectInput(inputId = "categoryVariable2",
                             label = "Select Category",
                             choices = NULL),
                 selectInput(inputId = "typeVariable2",
                             label = "Select Type",
                             choices = NULL),
                 sliderInput(inputId = "EHSASims", 
                             label = "Number of Simulations:", 
                             min = 99, max = 499,
                             value = 99, step = 100),
                 sliderInput(inputId = "MoranConf2", 
                             label = "Select Confidence level", 
                             min = 0.75, max = 0.99,
                             value = 0.75, step = 0.05),
               actionButton("MoranUpdate", "Plot Map"),
             ),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Global Spatial Correlation", plotOutput("GlobalHistogram") %>% withSpinner(color = "#3498db")),
                           tabPanel("Local Spatial Correlation", 
                                    plotOutput("LocalMoranMap") %>% withSpinner(color = "#3498db"),
                                    tmapOutput("LISA") %>% withSpinner(color = "#3498db")
                                  ),
                           tabPanel("Emerging Hot Spot Analysis", 
                                    plotOutput("EHSA") %>% withSpinner(color = "#3498db"),
                                    plotOutput("EHSABar") %>% withSpinner(color = "#3498db")),
               )
             )
           )
  ),
  tabPanel("Geographically Weighted Regression",
           sidebarLayout(
             sidebarPanel(
               titlePanel("Data Selection"),
               selectInput("IndependentVar", "Select Independent Variable (Backspace to remove selection)",
                           choices = c("Poverty relative" = "pr",
                                       "Poverty absolute" = "pa",
                                       "Income inequality" = "ii",
                                       "Mean household income" = "mehi",
                                       "Median household income" = "mdhi"),
                           selected = c("pr", "pa", "ii", "mehi", "mdhi"),
                           multiple = TRUE),
               fluidRow(column(7,
                               
                               selectInput(inputId = "categoryVariable3",
                                           label = "Select Category",
                                           choices = NULL,
                                           multiple = TRUE),
                               
                               selectInput(inputId = "typeVariable3",
                                           label = "Select Type",
                                           choices = NULL,
                                           multiple = TRUE),
               ),
               column(3, offset = 1,
                      radioButtons(inputId = "year",
                                   label = "Year",
                                   choices = c("2019",
                                               "2022"),
                                   selected = "2019"),
               )),
               sliderInput(inputId = "SigLvl", 
                           label = "Significance Level:", 
                           min = 0, max = 1,
                           value = 0.05, step = 0.01),
               titlePanel("Bandwidth computation parameters"),
               selectInput("Bandwidth", "Select Bandwidth",
                           choices = c("Adaptive" = TRUE,
                                       "Fixed" = FALSE),
                           selected = FALSE),
               selectInput("Bandwidth", "Computation Function",
                           choices = c("Gaussian" = "gaussian",
                                       "Exponential" = "exponential",
                                       "Bisquare" = "bisquare",
                                       "Tricube" = "tricube",
                                       "Boxcar" = "boxcar"),
                           selected = "gaussian"),
               selectInput("Bandwidth", "Select Approach",
                           choices = c("Cross VAlidation (CV)" = "CV",
                                       "Akaike Information Criterion (AIC)" = "AIC"),
                           selected = "CV"),
              
               
               actionButton("GwrUpdate", "Plot"),
             ),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Global Spatial Correlation", plotOutput("GlobalHistogram") %>% withSpinner(color = "#3498db")),
                           tabPanel("Local Spatial Correlation", 
                                    plotOutput("LocalMoranMap") %>% withSpinner(color = "#3498db"),
                                    tmapOutput("LISA") %>% withSpinner(color = "#3498db")
                           ),
                           tabPanel("Emerging Hot Spot Analysis", 
                                    plotOutput("EHSA") %>% withSpinner(color = "#3498db"),
                                    plotOutput("EHSABar") %>% withSpinner(color = "#3498db")),
               )
             )
           )
           
           )
)

#========================#
###### Shiny Server ######
#========================# 

server <- function(input, output, session){
  
  #Load choices for category
  unique_category <- c("all", unique(msia$category))
  updateSelectInput(session, "categoryVariable", choices = unique_category)
  updateSelectInput(session, "categoryVariable2", choices = unique(msia$category))
  updateSelectInput(session, "categoryVariable3", choices = unique(combined_data$category))
  
  #Load choices for type
  unique_type <- c("all", unique(msia$type))
  updateSelectInput(session, "typeVariable", choices = unique_type)
  updateSelectInput(session, "typeVariable2", choices = unique(msia$type))
  updateSelectInput(session, "typeVariable3", choices = unique(combined_data$type))
  
  #==========================================================
  # EDA
  #==========================================================
  EDAResults <- eventReactive(input$EDAUpdate,{
    
    if(nrow(msia) == 0) return(NULL)  # Exit if no data
    
    withProgress(message = "Filtering data...", value = 0, {
      incProgress(0.5)
      msia_filtered <- msia %>% filter(year %in% input$EDAyear)
      
      incProgress(0.5)
      
    })
    
    return(msia_filtered)       
  })
  
  
  #==========================================================
  # Filtering of data for local and global spaital
  #==========================================================   
  
  MsiaFiltered <- eventReactive(input$MoranUpdate,{
    
    if(nrow(msia) == 0) return(NULL)  # Exit if no data
    
    withProgress(message = "Filtering data...", value = 0, {
      #For Global and Local Filter
      msia_filtered <- msia %>% filter(year %in% input$year, )
      
      if(input$categoryVariable != "all") {
        msia_filtered <- msia_filtered %>% filter(category %in% input$categoryVariable)
      }
      else{
        showModal(modalDialog(
          title = "Message",
          "Longer render time is expected as you have selected all categories",
          easyClose = TRUE,)
        )
      }
      
      if(input$typeVariable != "all") {
        msia_filtered <- msia_filtered %>% filter(type %in% input$typeVariable)
      }
      incProgress(0.5)
      
      # Computing Contiguity Spatial Weights
      msia_filtered_nb_q <- st_contiguity(msia_filtered, queen = input$Contiguity1)
      msia_filtered_wm_rs <- st_weights(msia_filtered_nb_q, style=input$MoranWeights)
      wm_q_filtered <- msia_filtered %>%
        mutate(nb = msia_filtered_nb_q,
               wt = msia_filtered_wm_rs,
               .before = 1)
      incProgress(0.25)
      
      lisa <- wm_q_filtered %>%
        mutate(local_moran = local_moran(
          crimes, nb, wt, nsim = as.numeric(input$MoranSims), zero.policy=TRUE),
          .before = 1) %>%
        unnest(local_moran)
      
      lisa <- lisa %>%
        rename("local moran(ii)" = "ii", "expectation(eii)" = "eii",
               "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
               "p_value" = "p_ii")
      
      incProgress(0.25)
      
    })
    results <- list(
      global = wm_q_filtered,
      lisa = lisa
    )
    return (results)
  })
  
  #==========================================================
  # EHSA
  #==========================================================   
  
  EHSAResults <- eventReactive(input$MoranUpdate,{
    
    if(nrow(msia) == 0) return(NULL)  # Exit if no data
    
    msia_filtered_EHSA <- msia %>% filter(category %in% input$categoryVariable2, type %in% input$typeVariable2)
    # Computing EHSA
    msia_df <- msia_filtered_EHSA %>%
      dplyr::select(year, crimes, ADM2_EN) %>%
      st_drop_geometry()
    
    msia_sf_filtered <- msia_sf %>%
      semi_join(msia_df, by = "ADM2_EN")
    
    msia_df <- msia_df %>%
      group_by(year, ADM2_EN) %>%
      summarise(crimes = mean(crimes, na.rm = TRUE)) %>%
      ungroup()
    
    msia_spt <- spacetime(msia_df, msia_sf_filtered,
                          .loc_col = "ADM2_EN",
                          .time_col = "year")
    
    ehsa <- emerging_hotspot_analysis(
      x = msia_spt, 
      .var = "crimes", 
      k = 1, 
      nsim = as.numeric(input$EHSASims)
    )
    
    ehsa_sf <- left_join(msia_sf, ehsa, by = c("ADM2_EN" = "location"))
    
    
    return(ehsa_sf)       
  })
  
  #==========================================================
  # Render output maps
  #==========================================================
  
  #Render Histogram for EDA
  output$edaHistogram <- renderPlot({
    df <- EDAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    histogram <- {
      ggplot(data = df, aes_string(x = input$EDAVariable)) +
        geom_histogram(bins = 20, color = "black", fill = "light blue")
    }
    histogram
  })
  
  #Render Boxpot for EDA
  output$edaBoxplot <- renderPlot({
    df <- EDAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    boxplot <- {
      ggplot(data = df, aes_string(x = input$EDAVariable)) +
        geom_boxplot(color = "black", fill = "light blue")
    }
    boxplot
  })
  
  #Render Boxpot for EDA
  output$qtmPlot <- renderPlot({
    df <- EDAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    return (qtm(df,input$EDAVariable))
  })
  
  #Render Histogram for global spatial
  output$GlobalHistogram <- renderPlot({
    df <- MsiaFiltered()$global
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    gmres <-global_moran_perm(df$crimes,
                              df$nb,
                              df$wt,
                              zero.policy = TRUE,
                              nsim = as.numeric(input$MoranSims),
                              na.action=na.omit)
    
    histogram <- {
      hist(gmres$res, main="Histogram of Global Moran's I Monte-Carlo Simulation", xlab="Monte-Carlo Results", ylab="Frequency")
      abline(v = gmres$statistic, col = "red")
    }
    histogram
  })
  
  
  #Render local Moran I statistics
  output$LocalMoranMap <- renderPlot({
    df <- MsiaFiltered()$lisa
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    tmap_mode("plot")
    # Map creation using tmap
    localMI_map <- tm_shape(df) +
      tm_fill(col = input$localmoranstats, 
              style = "pretty", 
              palette = "RdBu", 
              title = input$localmoranstats) +
      tm_borders(alpha = 0.5) +
      tm_text("u_rate", size = 0.7, col = "black")
    
    localMI_map 
  })
  
  #Render LISA map 
  output$LISA <- renderTmap({
    df <- MsiaFiltered()$lisa
    if(is.null(df)) return()
    
    lisa_sig <- df  %>%
      filter(p_value < (1 - as.numeric(input$MoranConf)))  
    
    lisamap <- tm_shape(df) +
      tm_polygons() + 
      tm_borders(alpha = 0.5) + 
      tm_shape(lisa_sig) + 
      tm_fill(col = input$LisaClass, title = (paste("Significance:", input$LisaClass))) +
      tm_text("u_rate", size = 0.6, col = "black") +
      tm_borders(alpha = 0.4) +
      tm_layout(main.title = "LISA map of crimes", main.title.size = 1)
    
    lisamap 
  })
  
  #Render EHSA map 
  output$EHSA <- renderPlot({
    df <- EHSAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    EHSA_sig <- df %>%
      filter(p_value < (1 - as.numeric(input$MoranConf2)))
    
    ehsamap <- tm_shape(df) +
      tm_polygons() +
      tm_borders(alpha = 0.5) +
      tm_shape(EHSA_sig) +
      tm_fill(col = "classification", title = "Classification") + 
      tm_borders(alpha = 0.4) +
      tm_layout(main.title = "EHSA", main.title.size = 1)
    
    ehsamap 
  })
  
  #Render EHSA bar graph 
  output$EHSABar <- renderPlot({
    df <- EHSAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    EHSA_sig <- df %>%
      filter(p_value < (1 - as.numeric(input$MoranConf2)))
    
    ehsabar <- ggplot(data = EHSA_sig,
                      aes(y = classification,fill = classification)) +
      geom_bar(show.legend = FALSE)
    
    ehsabar 
  })
}

shinyApp (ui=ui, server=server)

