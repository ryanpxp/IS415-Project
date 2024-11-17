pacman::p_load(shiny, sf, tmap, tidyverse, sfdep,shinycssloaders, shinydashboard, shinythemes, bslib,
             st, tidyverse, raster, tmap, tmaptools, ggplot2, spatstat,knitr,performance, see, sfdep, GWmodel,olsrr, ggstatsplot)

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

ui <-
  tagList(
  shinythemes::themeSelector(),
  navbarPage(
  title = "Abang Smith",
  theme=shinytheme("cosmo"),
  fluid = TRUE,
  id = "navbarID",
  tabPanel("EDA",
           tabsetPanel(type = "tabs",
                       tabPanel("Data Set",
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
                       tabPanel("LISA",
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("LISA"),
                                    selectInput(inputId = "EDAVariable2", "Select EDA variable for LISA",
                                                choices = c("Participation rate" = "p_rate",
                                                            "Unemployment rate" = "u_rate",
                                                            "Crimes" = "crimes")),
                                    selectInput("LisaClass2", "Select Lisa Classification",
                                                choices = c("mean" = "mean",
                                                            "median" = "median",
                                                            "pysal" = "pysal"),
                                                selected = "mean"),
                                    radioButtons(inputId = "EDAyear2",
                                                 label = "Year",
                                                 choices = c("2019", 
                                                             "2020",
                                                             "2021",
                                                             "2022"),
                                                 selected = "2019"),
                                    actionButton("EDAUpdate2", "Plot"),
                                  ),
                                  mainPanel(
                                    tmapOutput("LISA2") %>% withSpinner(color = "#3498db")
                                  )
                                )
                       ),
                     ),
           ),
  tabPanel("Geographically Weighted Regression",
           tabsetPanel(type = "tabs",
                       tabPanel("Assumptions checks",
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Data Selection"),
                                    selectInput("IndependentVar", "Select Independent Variable (Backspace to remove selection)",
                                                choices = c("Poverty relative" = "poverty_relative",
                                                            "Poverty absolute" = "poverty_absolute",
                                                            "Income inequality" = "inequality",
                                                            "Mean household income" = "income_mean",
                                                            "Median household income" = "income_median"),
                                                selected = c("poverty_relative", "poverty_absolute", "inequality", "income_mean", "income_median"),
                                                multiple = TRUE),
                                    fluidRow(column(7,
                                                    
                                                    selectInput(inputId = "typeVariable3",
                                                                label = "Select Crime Type",
                                                                choices = NULL,
                                                                multiple = TRUE),
                                    ),
                                    column(3, offset = 1,
                                           radioButtons(inputId = "GWRyear",
                                                        label = "Year",
                                                        choices = c("2019",
                                                                    "2022"),
                                                        selected = "2019"),
                                    )),
                                    sliderInput(inputId = "SigLvl", 
                                                label = "Significance Level:", 
                                                min = 0, max = 1,
                                                value = 0.05, step = 0.01),
                                    sliderInput(inputId = "PVal", 
                                                label = "P-Value:", 
                                                min = 0, max = 1,
                                                value = 0.05, step = 0.01),
                                    
                                    
                                    actionButton("AssumptionUpdate", "Plot"),
                                  ),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Correlation analysis", 
                                                         plotOutput("CorrelationMatrix") %>% withSpinner(color = "#3498db")
                                                ),
                                                tabPanel("Model Performance", 
                                                         plotOutput("ModelPerformance") %>% withSpinner(color = "#3498db"),
                                                         verbatimTextOutput("ModelPerformance2") %>% withSpinner(color = "#3498db")
                                                ),
                                                tabPanel("Checks", 
                                                         
                                                         fluidRow(column(5,
                                                                plotOutput("Linearity") %>% withSpinner(color = "#3498db"), 
                                                                plotOutput("Outliers") %>% withSpinner(color = "#3498db"),          
                                                              
                                                         ),
                                                         column(5, offset = 1,
                                                                plotOutput("Normality") %>% withSpinner(color = "#3498db"),
                                                                plotOutput("Multicollinearity") %>% withSpinner(color = "#3498db"),
                                                                
                                                         )),
                                                )
                                    )
                                  )
                                )
                                ),
                       tabPanel("GWR model",
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Data Selection"),
                                    selectInput("IndependentVar2", "Select Independent Variable (Backspace to remove selection)",
                                                choices = c("Poverty relative" = "poverty_relative",
                                                            "Poverty absolute" = "poverty_absolute",
                                                            "Income inequality" = "inequality",
                                                            "Mean household income" = "income_mean",
                                                            "Median household income" = "income_median"),
                                                selected = c("poverty_relative", "poverty_absolute", "inequality", "income_mean", "income_median"),
                                                multiple = TRUE),
                                    fluidRow(column(7,
                                                    # TODO: error for non input
                                                    selectInput(inputId = "typeVariable4",
                                                                label = "Select Crime Type",
                                                                choices = NULL,
                                                                multiple = TRUE),
                                    ),
                                    column(3, offset = 1,
                                           radioButtons(inputId = "GWRyear2",
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
                                    selectInput("ComputationFunc", "Computation Function",
                                                choices = c("Gaussian" = "gaussian",
                                                            "Exponential" = "exponential",
                                                            "Bisquare" = "bisquare",
                                                            "Tricube" = "tricube",
                                                            "Boxcar" = "boxcar"),
                                                selected = "gaussian"),
                                    selectInput("Approach", "Select Approach",
                                                choices = c("Cross Validation (CV)" = "CV",
                                                            "Akaike Information Criterion (AIC)" = "AIC"),
                                                selected = "CV"),
                                    
                                    
                                    actionButton("GwrUpdate", "Plot"),
                                  ),
                                  mainPanel(
                                    fluidRow(column(6,
                                          tmapOutput("GWR") %>% withSpinner(color = "#3498db"),
                                          tmapOutput("GWR2") %>% withSpinner(color = "#3498db"),
                                    ),
                                    column(6, 
                                           
                                           verbatimTextOutput("LM") %>% withSpinner(color = "#3498db")
                                    ))
                                             ),
                                    
                                )
                                
                            )
                       )
           ),
  tabPanel("Spatial Correlation",
           tabsetPanel(type = "tabs",
                       tabPanel("Global and Local Spatial Correlation",
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Global and Local Spatial"),
                                    fluidRow(column(7,
                                                    selectInput(inputId = "categoryVariable",
                                                                label = "Select Category",
                                                                choices = c("all","assault","property")),
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
                                    actionButton("MoranUpdate", "Plot Map"),
                                  ),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Global Spatial Correlation",
                                                         fluidRow(
                                                           column(6,
                                                                  verbatimTextOutput("GlobalTest") %>% withSpinner(color = "#3498db"),
                                                           ),
                                                           column(6,
                                                                  verbatimTextOutput("GlobalPerm") %>% withSpinner(color = "#3498db"),
                                                           )
                                                         ),
                                                         
                                                         plotOutput("GlobalHistogram") %>% withSpinner(color = "#3498db")),
                                                tabPanel("Local Spatial Correlation", 
                                                         plotOutput("LocalMoranMap") %>% withSpinner(color = "#3498db"),
                                                         tmapOutput("LISA") %>% withSpinner(color = "#3498db")
                                                ),
                                    )
                                  )
                                )
                       ),
                       tabPanel("Emerging Hot Spot Analysis", 
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Emerging Hot Spot Analysis"),
                                    selectInput(inputId = "categoryVariable2",
                                                label = "Select Category",
                                                choices = c("assault","property")),
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
                                    actionButton("MoranUpdate2", "Plot Map"),
                                  ),
                                  mainPanel(
                                    plotOutput("EHSA") %>% withSpinner(color = "#3498db"),
                                    plotOutput("EHSABar") %>% withSpinner(color = "#3498db")),
                                  
                                )
                       )
           ),
           
  ),
  )
)

#========================#
###### GWR Function ######
#========================# 
run_regression <- function(data, response, predictors) {
  # Create formula from response and predictors
  formula <- as.formula(
    paste(response, "~", paste(predictors, collapse = " + "))
  )
  
  # Run the linear model
  model <- lm(formula = formula, data = data)
  
  return(model)
}

run_stepwise_selection <- function(model, direction = "forward", p_val = 0.05, details = FALSE) {
  if (!direction %in% c("forward", "backward", "both")) {
    stop("Invalid direction. Choose from 'forward', 'backward', or 'both'.")
  }
  
  stepwise_model <- switch(
    direction,
    "forward" = ols_step_forward_p(model, p_val = p_val, details = details),
    "backward" = ols_step_backward_p(model, p_val = p_val, details = details),
    "both" = ols_step_both_p(model, p_val = p_val, details = details)
  )
  
  return(stepwise_model)
}

#========================#
###### Shiny Server ######
#========================# 

server <- function(input, output, session){
  
  #Load typeVariable2 based on categoryVariable2
  #Load choices for type
  filtered_data2 <- reactive({
    req(input$categoryVariable2)
    
    if (input$categoryVariable2 == "all") {
      msia
    } else {
      subset(msia, category == input$categoryVariable2)
    }
  })
  
  filtered_types2 <- reactive({
    if (input$categoryVariable2 == "all") {
      unique_type <- c("all", unique(filtered_data2()$type))
    } else {
      unique(filtered_data2()$type)
    }
    
  })
  
  # Update district input based on selected province
  observe({
    updateSelectInput(session, "typeVariable2", choices = filtered_types2())
  })
  
  updateSelectInput(session, "typeVariable3", choices = unique(combined_data$type))
  updateSelectInput(session, "typeVariable4", choices = unique(combined_data$type))
  
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
  # EDA - LISA
  #==========================================================
  LISAResults <- eventReactive(input$EDAUpdate2,{
    
    if(nrow(msia) == 0) return(NULL)  # Exit if no data
    
    withProgress(message = "Filtering data...", value = 0, {
      incProgress(0.25)
      msia_filtered <- msia %>% filter(year %in% input$EDAyear2)
      
      incProgress(0.25)
      # Computing Contiguity Spatial Weights
      msia_filtered_nb_q <- st_contiguity(msia_filtered, queen = TRUE)
      msia_filtered_wm_rs <- st_weights(msia_filtered_nb_q, style="W")
      wm_q_filtered <- msia_filtered %>%
        mutate(nb = msia_filtered_nb_q,
               wt = msia_filtered_wm_rs,
               .before = 1)
      incProgress(0.25)
      
      lisa <- wm_q_filtered %>%
        mutate(local_moran = local_moran(
          .data[[input$EDAVariable2]], nb, wt, nsim = 99, zero.policy=TRUE),
          .before = 1) %>%
        unnest(local_moran)
      
      lisa <- lisa %>%
        rename("local moran(ii)" = "ii", "expectation(eii)" = "eii",
               "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
               "p_value" = "p_ii")
      incProgress(0.25)
      
    })
    
    return(lisa)       
  })
  
  
  #==========================================================
  # Filtering of data for local and global spatial
  #==========================================================   
  
  MsiaFiltered <- eventReactive(input$MoranUpdate,{
    
    if(nrow(msia) == 0) return(NULL)  # Exit if no data
    
    withProgress(message = "Filtering data...", value = 0, {
      #For Global and Local Filter
      msia_filtered <- msia %>% filter(year %in% input$year, )
      
      if(input$categoryVariable != "all") {
        msia_filtered <- msia_filtered %>% filter(category %in% input$categoryVariable)
        incProgress(0.5)
      }
      else{
        showModal(modalDialog(
          title = "Message",
          "Longer render time is expected as you have selected all categories",
          easyClose = TRUE,)
        )
        incProgress(0.5)
      }
      
      
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
  
  EHSAResults <- eventReactive(input$MoranUpdate2,{
    
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
  
  #Render Boxplot for EDA
  output$edaBoxplot <- renderPlot({
    df <- EDAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    boxplot <- {
      ggplot(data = df, aes_string(x = input$EDAVariable)) +
        geom_boxplot(color = "black", fill = "light blue")
    }
    boxplot
  })
  
  #Render QTMPlot for EDA
  output$qtmPlot <- renderPlot({
    df <- EDAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    return (qtm(df,input$EDAVariable))
  })
  
  #Render global moran test
  output$GlobalTest <- renderPrint({
    df <- MsiaFiltered()$global
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    gmtest <- global_moran_test(df$crimes,
                               df$nb,
                               df$wt,
                               zero.policy = TRUE,
                               na.action=na.omit)
    gmtest
  })
  
  #Render global moran perm test
  output$GlobalPerm <- renderPrint({
    df <- MsiaFiltered()$global
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    gmres <-global_moran_perm(df$crimes,
                              df$nb,
                              df$wt,
                              zero.policy = TRUE,
                              nsim = as.numeric(input$MoranSims),
                              na.action=na.omit)
    
    gmres
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
  
  #Render LISA map for EDA
  output$LISA2 <- renderTmap({
    df <- LISAResults()
    if(is.null(df)) return()
    
    lisamap <- tm_shape(df) +
      tm_polygons() + 
      tm_borders(alpha = 0.5) + 
      tm_shape(df) + 
      tm_fill(col = input$LisaClass2, title = (paste("Significance:", input$LisaClass2))) +
      tm_borders(alpha = 0.4) +
      tm_layout(main.title = "LISA Map", main.title.size = 1)
    
    lisamap 
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
  
  
  #==========================================================
  # GWR
  #==========================================================
  AssumptionResults <- eventReactive(input$AssumptionUpdate,{
    
    if(nrow(combined_data) == 0) return(NULL)  # Exit if no data
    
    combined_data_filtered <- combined_data %>% filter(
      type %in% input$typeVariable3, year %in% input$GWRyear)
    
    return (combined_data_filtered)
      
  })
  
  output$CorrelationMatrix <- renderPlot({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    ggcorrmat(df[, 6:11])
  })
  
  output$ModelPerformance <- renderPlot({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    predictors <- input$IndependentVar
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = df,
      response = "crimes",
      predictors = predictors
    )
    
    forward_selection <- run_stepwise_selection(
      model = base_model,
      direction = "forward",
      p_val = 0.05, #todo
      details = FALSE
    )
    backward_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "backward",
      p_val = 0.05,
      details = FALSE
    )
    bidirectional_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "both",
      p_val = 0.05,
      details = FALSE
    )
    metric <- compare_performance(base_model, 
                                  forward_selection$model,
                                  backward_elimination$model,
                                  bidirectional_elimination$model)
    metric$Name <- gsub(".*\\\\([a-zA-Z0-9_]+)\\\\, \\\\model\\\\.*", "\\1", metric$Name)
    plot(metric)
    
  })
  
  output$ModelPerformance2 <- renderPrint({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    predictors <- input$IndependentVar
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = df,
      response = "crimes",
      predictors = predictors
    )
    
    
    ols_regress(base_model)
    
  })
  
  output$Linearity <- renderPlot({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    predictors <- input$IndependentVar
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = df,
      response = "crimes",
      predictors = predictors
    )
    
    forward_selection <- run_stepwise_selection(
      model = base_model,
      direction = "forward",
      p_val = 0.05, #todo
      details = FALSE
    )
    backward_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "backward",
      p_val = 0.05,
      details = FALSE
    )
    bidirectional_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "both",
      p_val = 0.05,
      details = FALSE
    )
    out <- plot(check_model(bidirectional_elimination$model, 
                            panel = FALSE))
    
    
    out[[2]]
  })
  
  output$Normality <- renderPlot({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    predictors <- input$IndependentVar
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = df,
      response = "crimes",
      predictors = predictors
    )
    
    forward_selection <- run_stepwise_selection(
      model = base_model,
      direction = "forward",
      p_val = 0.05, #todo
      details = FALSE
    )
    backward_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "backward",
      p_val = 0.05,
      details = FALSE
    )
    bidirectional_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "both",
      p_val = 0.05,
      details = FALSE
    )
    
    ols_plot_resid_hist(bidirectional_elimination$model) +
      labs(title = "Normality")
  })

  output$Outliers <- renderPlot({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    predictors <- input$IndependentVar
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = df,
      response = "crimes",
      predictors = predictors
    )
    
    forward_selection <- run_stepwise_selection(
      model = base_model,
      direction = "forward",
      p_val = 0.05, #todo
      details = FALSE
    )
    backward_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "backward",
      p_val = 0.05,
      details = FALSE
    )
    bidirectional_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "both",
      p_val = 0.05,
      details = FALSE
    )
    
    plot(check_outliers(bidirectional_elimination$model,
                        method = "cook")) + labs(title = "Outliers")
  })
  
  output$Multicollinearity <- renderPlot({
    df <- AssumptionResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    predictors <- input$IndependentVar
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = df,
      response = "crimes",
      predictors = predictors
    )
    
    forward_selection <- run_stepwise_selection(
      model = base_model,
      direction = "forward",
      p_val = 0.05, #todo
      details = FALSE
    )
    backward_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "backward",
      p_val = 0.05,
      details = FALSE
    )
    bidirectional_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "both",
      p_val = 0.05,
      details = FALSE
    )
    
    plot(check_collinearity(bidirectional_elimination$model)) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  GwrResults <- eventReactive(input$GwrUpdate,{
    
    if(nrow(combined_data) == 0) return(NULL)  # Exit if no data
    
    combined_data_filtered2 <- combined_data %>% filter(
      type %in% input$typeVariable4, year %in% input$GWRyear2)
    
    predictors <- input$IndependentVar2
    
    # Run the function with the specified data and predictors
    base_model <- run_regression(
      data = combined_data_filtered2,
      response = "crimes",
      predictors = predictors
    )
    
    forward_selection <- run_stepwise_selection(
      model = base_model,
      direction = "forward",
      p_val = 0.05, #todo
      details = FALSE
    )
    backward_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "backward",
      p_val = 0.05,
      details = FALSE
    )
    bidirectional_elimination <- run_stepwise_selection(
      model = base_model,
      direction = "both",
      p_val = 0.05,
      details = FALSE
    )
    
    mlr_output <- as.data.frame(bidirectional_elimination$model$residuals) %>%
      rename(`SB_MLR_RES` = `bidirectional_elimination$model$residuals`)
    
    residual <- data.frame(MLR_RES = rep(NA, nrow(combined_data_filtered2)))
    residual[rownames(mlr_output), "MLR_RES"] <- mlr_output$SB_MLR_RES
    combined_data_filtered2 <- cbind(combined_data_filtered2, 
                                  residual)
    combined_data_filtered2_st <- st_as_sf(combined_data_filtered2)
    
    combined_data_filtered2_st <- combined_data_filtered2_st %>%
      filter(
        !is.na(crimes) & !is.na(poverty_relative) & !is.na(poverty_absolute) &
          !is.na(inequality) & !is.na(income_mean) & !is.na(income_median) &
          is.finite(crimes) & is.finite(poverty_relative) & is.finite(poverty_absolute) &
          is.finite(inequality) & is.finite(income_mean) & is.finite(income_median)
      )
    
    bw <- bw.gwr(formula = crimes ~ poverty_relative + poverty_absolute + inequality +
                         income_mean + income_median, 
                       data=combined_data_filtered2_st, 
                       approach=input$Approach, 
                       kernel=input$ComputationFunc, 
                       adaptive=as.logical(input$Bandwidth), #TODO: Remove?
                       longlat=FALSE)
    
    gwr.result <- gwr.basic(formula = crimes ~ poverty_relative + poverty_absolute + inequality +
                             income_mean + income_median, 
                           data=combined_data_filtered2_st, 
                           bw=bw, 
                           kernel = input$ComputationFunc, 
                           longlat = FALSE)
    
    return(gwr.result)
  })
  
  output$GWR <- renderTmap({
    df <- GwrResults()
    if(is.null(df)) return()
    
    df <- st_as_sf(df$SDF) %>%
      st_transform(crs=3168)
    
    tmap_mode("view")
    Local_R2 <- tm_shape(df) +
      tm_polygons(col = "Local_R2", alpha = 0.6) +
      tm_view(set.zoom.limits = c(5, 9))
  })
  
  output$GWR2 <- renderTmap({
    df <- GwrResults()
    if(is.null(df)) return()
    
    df <- st_as_sf(df$SDF) %>%
      st_transform(crs=3168)
    
    tmap_mode("view")
    Inequality_TV <- tm_shape(df) +
      tm_polygons(col = "inequality_TV", alpha = 0.6) +
      tm_view(set.zoom.limits = c(5, 9))
  })
  
  output$LM <- renderPrint({
    df <- GwrResults()
    
    if (is.null(df) || is.null(df$SDF)) return("No results found.")
    output_text <- capture.output(print(df))
    cat(output_text, sep = "\n")
  })
}

shinyApp (ui=ui, server=server)

