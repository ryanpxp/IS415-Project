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
#========================#
###### Shiny UI ######
#========================#  

ui <- navbarPage(
  title = "Abang Smith",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("EDA"),
  tabPanel("Spatial Correlation",
           sidebarLayout(
             sidebarPanel(
               box(
                 title = "Global and Local Spatial", width = 20,
                     selectInput(inputId = "categoryVariable",
                                 label = "Select Category",
                                 choices = NULL),
                     selectInput(inputId = "typeVariable",
                                 label = "Select Type",
                                 choices = NULL),
                     radioButtons(inputId = "year",
                                  label = "Year",
                                  choices = c("2019", 
                                              "2020",
                                              "2021",
                                              "2022"),
                                  selected = "2019"),
                     radioButtons(inputId = "MoranConf",
                                  label = "Select Confidence level",
                                  choices = c("0.85" = 0.15, 
                                              "0.95" = 0.05),
                                  selected = 0.05,
                                  inline = TRUE),
               ),
               br(),
               box(
                 title = "Emerging Hot Spot Analysis", width = 20,
                 selectInput(inputId = "categoryVariable2",
                             label = "Select Category",
                             choices = NULL),
                 selectInput(inputId = "typeVariable2",
                             label = "Select Type",
                             choices = NULL),
                 radioButtons(inputId = "MoranConf2",
                              label = "Select Confidence level",
                              choices = c("0.85" = 0.15, 
                                          "0.95" = 0.05),
                              selected = 0.05,
                              inline = TRUE),
               ),
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
  tabPanel("Geographically Weighted Regression")
)

#========================#
###### Shiny Server ######
#========================# 

server <- function(input, output, session){
  
  #Load choices for category
  unique_category <- c("all", unique(msia$category))
  updateSelectInput(session, "categoryVariable", choices = unique_category)
  updateSelectInput(session, "categoryVariable2", choices = unique(msia$category))
  
  #Load choices for type
  unique_type <- c("all", unique(msia$type))
  updateSelectInput(session, "typeVariable", choices = unique_type)
  updateSelectInput(session, "typeVariable2", choices = unique(msia$type))
  
  #==========================================================
  # Filtering of data
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
      msia_filtered_nb_q <- st_contiguity(msia_filtered, queen=TRUE)
      msia_filtered_wm_rs <- st_weights(msia_filtered_nb_q, style="W")
      wm_q_filtered <- msia_filtered %>%
        mutate(nb = msia_filtered_nb_q,
               wt = msia_filtered_wm_rs,
               .before = 1)
      incProgress(0.25)
      
      lisa <- wm_q_filtered %>%
        mutate(local_moran = local_moran(
          crimes, nb, wt, nsim = 99, zero.policy=TRUE),
          .before = 1) %>%
        unnest(local_moran)
      
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
      nsim = 99
    )
    
    ehsa_sf <- left_join(msia_sf, ehsa, by = c("ADM2_EN" = "location"))
    
    
    return(ehsa_sf)       
  })
  
  #==========================================================
  # Render output maps
  #==========================================================
  
  #Render Histogram
  output$GlobalHistogram <- renderPlot({
    df <- MsiaFiltered()$global
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    gmres <-global_moran_perm(df$crimes,
                              df$nb,
                              df$wt,
                              zero.policy = TRUE,
                              nsim = 999,
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
      tm_fill("p_ii",
              breaks = c(0, 0.001, 0.01, 0.05, 1),
              labels = c("< 0.001", "< 0.01", "< 0.05", "Not sig")) + 
      tm_borders(alpha = 0.5) +
      tm_text("u_rate", size = 0.7, col = "black") +  # Adding `p_value_2` as a text label
      tm_layout(main.title = "p-values of Two Variables",
                main.title.size = 0.8)
    
    localMI_map 
  })
  
  #Render LISA map 
  output$LISA <- renderTmap({
    df <- MsiaFiltered()$lisa
    if(is.null(df)) return()
    
    lisa_sig <- df  %>%
      filter(p_ii_sim < 0.05)  
    
    lisamap <- tm_shape(df) +
      tm_polygons() + 
      tm_borders(alpha = 0.5) + 
      tm_shape(lisa_sig) + 
      tm_fill("mean", title = "LISA class") +
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
      filter(p_value < 0.15)
    
    ehsamap <- tm_shape(df) +
      tm_polygons() +
      tm_borders(alpha = 0.5) +
      tm_shape(EHSA_sig) +
      tm_fill(col = "classification", title = "Classification") + 
      tm_borders(alpha = 0.4) +
      tm_layout(main.title = "EHSA (>85%)", main.title.size = 1)
    
    ehsamap 
  })
  
  #Render EHSA bar graph 
  output$EHSABar <- renderPlot({
    df <- EHSAResults()
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    
    ehsabar <- ggplot(data = df,
                      aes(y = classification,fill = classification)) +
      geom_bar(show.legend = FALSE)
    
    ehsabar 
  })
}

shinyApp (ui=ui, server=server)

