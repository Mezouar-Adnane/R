library(tidyverse)        
library(shiny)
library(shinydashboard)
library(DT)
library(wordcloud)
library(treemapify)
#library(ggrepel)
library(lubridate)
library(stringr)
library(plotly)
#library(colourpicker)
library(wordcloud2)
library(leaflet)
library(sf)
#library(maps)
library(tmap)
library(shinydashboardPlus)

#source("moduleChangeTheme.R")
load("data_final.RData")
#mycss <- "
#.irs-bar,
#.irs-bar-edge,
#.irs-single,
#.irs-grid-pol {
#background: green;
#border-color: green;
#}
#"
shinyApp(
  ui <- dashboardPagePlus(
    
    skin = "green",
    
    header = dashboardHeaderPlus(
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "sliders",
      title = "WTA data analysis and dashboard",
      tags$li(class = "dropdown",
              tags$a(href="https://www.linkedin.com/in/adnane-mezouar/", 
                     target="_blank", 
                     icon("linkedin", "fa-1.5x", lib = "font-awesome"))),
      
      tags$li(class = "dropdown",
              tags$a(href="https://github.com/Mezouar-Adnane", 
                     target="_blank", 
                     icon("github", "fa-1.5x", lib = "font-awesome"))
      ),
      
      tags$li(class = "dropdown", column(12))
      
    ),
    
    sidebar = dashboardSidebar(
      
      sidebarMenu(
        
        menuItem("Explore the data set", 
                 tabName = "explore", 
                 icon= icon("wpexplorer")),
        
        menuItem("Career Benchmarks/Analysis", 
                 tabName = "performance", 
                 icon = icon("chart-bar"))
      ),
      
     # tags$style(mycss),
      
      sliderInput("date", h4("Select your time frame"),              
                  min = min(data_final$year), max = max(data_final$year),                      
                  value = c(1984,2017) , step = 1, sep = ""), # TIME FRAME
      
      selectInput("country", "Country", 
                  choices = c("All", unique(data_final$country_code)), 
                  selected = "All", 
                  multiple = T ),
      
      
      numericInput("obs", h4("How many observations?"), 
                   min = 1, max = 50, step = 1, value = 100), # OBSERVATIONS SELECTION
      
      fluidRow(column(12, radioButtons("show", "Show full data tables ?",
                                       c("yes" = "yes",
                                         "no" = "no")))),
      sidebarMenu(
        
        menuItem("Data and downloads", 
                 tabName = "Dataset", 
                 icon = icon("database"))
      )
    ),
    
    body = dashboardBody( 
     
           tabItems(
        
        tabItem(
          tabName = "explore",
          tabBox( 
            title = "Exploratory Visualizations", height = "920px", width = 12,
            
            tabPanel('Where are our players from?', 
                     fluidRow(leafletOutput("my_map")),
                     column(6, boxPlus(
                       title = "Missing values", 
                       closable = TRUE,
                       width = NULL,
                       enable_label = TRUE,
                       label_text = "Note",
                       label_status = "danger",
                       status = "danger", 
                       solidHeader = FALSE, 
                       collapsible = TRUE,
                       collapsed = T,
                       p("Algeria, Germany, Netherlands and other european country values missing due to country code ISO 3 mismatch.
                         For data, check table or next tab")
                       ))),
            
            
            tabPanel("Proportion by country", 
                     fluidRow( plotOutput("country_tree")),
                     fluidRow(column(6, boxPlus(
                       title = "Insight", 
                       closable = TRUE, 
                       width = NULL,
                       enable_label = TRUE,
                       label_text = "Question",
                       label_status = "success",
                       status = "success", 
                       solidHeader = FALSE, 
                       collapsible = TRUE,
                       p("Play with the time frame slider, can you see the USA share shrinking 
                         due to increased competition from young foreign players? 
                         who are the countries eating America's share of player number?")
                       )),
                       column(6, conditionalPanel(condition = "input.show == 'yes'",
                                                  DT::dataTableOutput("tree_data_table"))))
                     
                     ),
            
            tabPanel("Who are our players?", 
                     wordcloud2Output("word_cloud",width = "100%", height = "400px"),
                     fluidRow(
                       column(6, boxPlus(
                         title = "Insight", 
                         closable = TRUE, 
                         width = NULL,
                         enable_label = TRUE,
                         label_text = "Exercise",
                         label_status = "success",
                         status = "success", 
                         solidHeader = FALSE, 
                         collapsible = TRUE,
                         p("What was the most common Japanese (JPN) name in the WTA between 2005 and 2017 ?")
                       ) ),
                       column(6, conditionalPanel(condition = "input.show == 'yes'",
                                                  DT::dataTableOutput("cloud_data_table")))
                     )
                     
            )#,
            
         #   tabPanel('Handedness',
                     
        #             plotlyOutput("dominant_hand_plotly"),
            #         fluidRow(column(6, boxPlus(
           #            title = "Insight", 
            #           closable = TRUE, 
            #           width = NULL,
              #         enable_label = TRUE,
              #         label_text = "Exercise",
               #        label_status = "success",
              #         status = "success", 
             #          solidHeader = FALSE, 
            #           collapsible = TRUE,
           #            p("How many registered WTA players belong to China's oldest age group by birth year between 2012-2017?
          #               Are they left or right handed? Hint: CHN")
         #              )),
        #               column(6, conditionalPanel(condition = "input.show == 'yes'",
         #                                         DT::dataTableOutput("dominant_hand_plotly_table")))
                       
        #             )
        #             ) 
                     )),
        
        tabItem(tabName="performance",
                tabBox(
                  title = "Rank and career analysis", height = "920px", width = 12,
                  
                  tabPanel("WTA rank dominations",
                           fluidRow(
                             boxPlus(
                               title = "Info Boxes", 
                               closable = F, 
                               width = NULL,
                               enable_label = TRUE,
                               label_text = 3,
                               label_status = "success",
                               status = "success", 
                               solidHeader = FALSE, 
                               collapsible = TRUE,
                               fluidRow(
                                 column(5,infoBoxOutput("box3", width = 12)),
                                 column(3, infoBoxOutput("box1", width = 12)),
                                 column(4, infoBoxOutput("box2", width = 12)))
                             )
                           ),
                           
                           fluidRow(
                             plotlyOutput("ranking_scat",width = "98%", height = "400px")
                           ),
                           fluidRow(),
                           
                           fluidRow(  
                             column(6,
                                    boxPlus(
                                      title = "Insight", 
                                      closable = TRUE, 
                                      width = NULL,
                                      enable_label = TRUE,
                                      label_text = "Exercise",
                                      label_status = "success",
                                      status = "success", 
                                      solidHeader = FALSE, 
                                      collapsible = TRUE,
                                      p("Among French (FRA), Russian(RUS) and Belgian (BEL) Players... Who dominated 3rd ranking between 1995 and 2005?
                                        hint: Go to second tab in the right sidebar to change ranking settings. ")
                                      )
                                    )
                             ,
                             column(6,
                                    conditionalPanel(condition = "input.show == 'yes'",
                                                     DT::dataTableOutput("rec_freq_table"))))) ,
                  tabPanel("Career Overviews", 
                           
                           fluidRow(
                             plotlyOutput("career")
                           ),
                           fluidRow(),
                           fluidRow(
                             
                             column(6,
                                    boxPlus(
                                      title = "Static Trend line ", 
                                      closable = F, 
                                      width = NULL,
                                      enable_label = TRUE,
                                      label_text = 1,
                                      label_status = "success",
                                      status = "success", 
                                      solidHeader = FALSE, 
                                      collapsible = TRUE,
                                      plotOutput("analysis", height = "300px", width = "100%"))
                             ),
                             
                             column(6, boxPlus(
                               title = "Insights", 
                               closable = F, 
                               width = NULL,
                               status = "success", 
                               enable_label = TRUE,
                               label_text = "Exercise",
                               label_status = "success",
                               solidHeader = FALSE, 
                               collapsible = TRUE,
                               enable_dropdown = TRUE,
                               dropdown_icon = "lightbulb",
                               dropdown_menu = dropdownItemList(
                                 dropdownItem(url = "https://www.theguardian.com/sport/2006/apr/15/tennis.gdnsport3", name = "Serena Williams 2006"),
                                 dropdownItem(url = "https://www.theguardian.com/sport/2010/oct/19/serena-williams-foot-injury-season", name = "Serena Williams 2010"),
                                 dropdownItem(url = "https://bleacherreport.com/articles/2610653-why-serena-williams-vs-maria-sharapova-is-still-the-wtas-best-rivalry", name = "Rivalry"),
                                 dropdownDivider(),
                                 dropdownItem(url = "https://www.youtube.com/watch?v=Uiitj-NHC7k", name = "Bonus video")
                               ),
                               p(paste("Adjust player names in the career overview tab on the right sidebar to compare Serena williams to Maria Sharapova.
                                       How many major setbacks did Serena have?
                                       Did Maria Benefit from them?
                                       What happened to Serena in 2006 and 2010?
                                       CLICK ON THE LIGHT BULB FOR ANSWERS "))
                               ))
                               )
                               )
                  
                               )),
        
        tabItem(tabName = "Dataset",
                tabBox(
                  title = "Data sources and downloads",
                  tabPanel("table",
                           downloadButton(outputId = "download_data", label = "Download"),
                           DT::dataTableOutput("data_years_table"))
                ))
        
                           )
                  ),
    
    rightsidebar = rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Dominant Hand",
        icon = "hands",
        active = TRUE,
        sliderInput("birth", h4("Select players' birth year"),            
                    min = min(data_final$birth_year[!is.na(data_final$birth_year)]), 
                    max = max(data_final$birth_year[!is.na(data_final$birth_year)]),          
                    value = c(1943,2002) ,
                    step = 1, 
                    sep = "")
      ),
      rightSidebarTabContent(
        id = 2,
        icon = "award",
        title = "Rank dominations",
        numericInput("rank", "Ranking to analyse", 
                     min = 1, max = 999, step = 1, value = 1),
        numericInput("obs_scat", "How many observations?", 
                     min = 1, max = 50, step = 1, value = 15),
        numericInput("size", "Point size", 10, 5,20,2)
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "balance-scale",
        title = "Career Overview",
        numericInput("size2", "Point size", 10, 5,20,2),
        selectInput("player", "Player name", 
                    choices = unique(data_final$full_name), 
                    selected = c(data_final$full_name[1], 
                                 data_final$full_name[2]), 
                    multiple = T)
      )
    )),
  
  
  
  
  #*** SERVER 
  server <- function(input, output, session){
    
    ##### 1st MENU ITEM EXPLORE DATASET SECTION ##### 
    
    ## Two most important reactive data sets ## 
    
    # DATA_YEARS CONTROLS OUR TIME VIEW # 
    
    data_years <- reactive( 
      
      if(input$country != "All"){data_final %>%
          filter (year >= input$date[1],
                  year <= input$date[2],
                  country_code == input$country)} else {data_final %>%
                      filter (year >= input$date[1],
                              year <= input$date[2])}
      
    )
    
    #Data for dominating hand 
    
    players_by_year <- reactive({
      
      data_years() %>%
        filter( 
          birth_year >= input$birth[1],
          birth_year <= input$birth[2]
        ) %>%
        group_by(birth_year, hand) %>%
        #filter(birth_year>=1950) %>%
        filter(!is.na(birth_year)) %>%
        filter(nchar(hand)>0) %>%
        summarise(Players = n())
      
    })
    
    # Dominant hand plotly visualization
    
    output$dominant_hand_plotly <- renderPlotly({
      
      temp_df <- players_by_year() %>% spread(hand, Players)
      
      hands <- plot_ly( temp_df,
                        x = temp_df$birth_year, 
                        y = temp_df$R,
                        type = 'bar',
                        name = 'Right handed',
                        marker = list(color = '#31a354')
      ) %>%
        
        add_trace(y = temp_df$L,
                  name = 'Left handed', 
                  marker = list(color = '#e5f5e0')) %>%
        
        add_trace(y = temp_df$U, 
                  name = 'Undetermined',
                  marker = list(color = '#a1d99b')) %>%
        
        layout(title = "Dominant hand by birth year" ,
               yaxis = list(title = "Number of Players"),
               xaxis = list(title = "Birth Years" ),
               barmode = 'stack')
    })
    
    # showing the table # 
    
    output$dominant_hand_plotly_table <- DT::renderDataTable({
      data <- players_by_year()
      data
    })
    
    ### WORD CLOUD SECTION ## 
    
    # CLOUD DATA # 
    cloud_data <- reactive(
      
      data_years() %>% 
        group_by(first_name) %>%
        summarise(name_freq = n()) %>% 
        arrange(desc(name_freq)) %>%
        top_n(input$obs, wt = name_freq)
    )
    
    
    # WordCloud output #
    
    output$word_cloud <- renderWordcloud2({
      
      wordcloud2(cloud_data()) #wordcloud2
      
    })
    
    
    # Wordcloud's table displayed # 
    
    output$cloud_data_table <- DT::renderDataTable({
      
      data <- cloud_data()
      data
      
    })
    ### TREE MAP SECTION  ### 
    
    # TREE MAP DATA 
    
    tree_data <- reactive( 
      data_years() %>%
        group_by(country_code) %>%
        summarise(num_players = n()) %>%
        top_n(input$obs, wt = num_players) ) #obs
    
    #Tree map visualization # 
    
    output$country_tree <- renderPlot({
      
      
      ggplot(tree_data(), aes(area = num_players, fill = num_players, label = country_code)) +
        geom_treemap(aes(alpha = num_players),fill = "#31a354") +
        geom_treemap_text(colour = "white", 
                          place = "centre",
                          grow = TRUE) +
        ggtitle(paste("Number of WTA players for the first", input$obs, "countries"))
      
    }) 
    
    
    # Tree map table visually shown # 
    output$tree_data_table <- DT::renderDataTable({
      
      data <- tree_data()
      data
      
    })
    ### MAP SECTION ### 
    
    output$my_map <- renderLeaflet({
      
      data("World")
      names(World)[1] <- "country_code"
      map_data <- tree_data() %>%
        right_join(World, by = "country_code") # RIGHT JOIN SO AS NOT TO HAVE EMPY COUNTRIES (BIG OCEAN) 
      
      palet <- colorQuantile(palette = "Greens",                                        
                             domain = map_data %>% pull(num_players), 
                             n=9) # BY 9 
      
      labels <- sprintf(                                                  
        "<strong>%s</strong><br/>%g Players",                         
        map_data$country_code,                                                 
        map_data$num_players                                                  
      ) %>% lapply(htmltools::HTML)                                      
      
      map_data %>% 
        #filter(year == 2011) %>%                        # Keep the one year of data
        data.frame() %>%                                # Turn into dataframe (technical)
        sf::st_sf() %>%                                 # Format in sf
        st_transform("+init=epsg:4326") %>%             # Convert in particular coordinate reference 
        leaflet() %>%                                   # Call leaflet
        addPolygons(fillColor = ~palet(num_players),        # Create the map (colored polygons)
                    weight = 2,                         # Width of separation line
                    opacity = 1,                        # Opacity of separation line
                    color = "white",                    # Color of separation line
                    dashArray = "3",                    # Dash size of separation line
                    fillOpacity = 1,                    # Opacity of polygon colors
                    highlight = highlightOptions(       # 5 lines below control the cursor impact
                      weight = 2,                       # Width of line
                      color = "#CBCBCB",                # Color of line
                      dashArray = "",                   # No dash
                      fillOpacity = 0.7,                # Opacity
                      bringToFront = TRUE),
                    label = labels,                    
                    labelOptions = labelOptions(       
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        addLegend(pal = palet,                    # Legend: comes from palet colors defined above
                  values = ~num_players,              # Values come from lifeExp variable
                  opacity = 0.7,                  # Opacity of legend
                  title = "Quantiles | Number of players",           # Title of legend
                  position = "bottomright")       # Position of legend
    })
    
    
    
    ##### 2nd MENU ITEM CAREER AND PERFORMANCE SECTION ##### 
    
    # RANKINGS SCATTER PLOT SECTION # 
    
    #SCATTERPLOT REACTIVE DATA 
    
    scat_data <- reactive( 
      
      data_years() %>%
        filter(ranking == input$rank) %>%        #only keep wished ranks
        mutate(full_name = paste(first_name, last_name)) %>% #glue the name to avoid confusion 
        group_by(full_name) %>%                   #pivot table
        summarise(
          ranking_times = n(),                    #repetitions in name
          country = min(country_code),            #keep one country code
          latest_ranking = max(ranking_date)
        ) %>%                                     #Most recent date rank was achieved
        mutate(latest_ranking_year = substr(latest_ranking, 1, 4)) %>%#only keep year
        top_n(input$obs_scat, wt = ranking_times) 
    )
    
    # RANKING VISUALIZATION # 
    
    output$ranking_scat <- renderPlotly(
      
      tmp<- plot_ly(scat_data(),
                    x = scat_data()$ranking_times , 
                    y = scat_data()$latest_ranking_year, 
                    text = scat_data()$full_name,
                    # color = scat_data()$latest_ranking_year,
                    # colors = "green",
                    mode = "markers",
                    marker = list(size = input$size,
                                  color = 'green',
                                  line = list(color = 'black',
                                              width = 2))) %>%
        add_text(textposition = "top right") %>% 
        layout(title = "Consistency and recency analysis",
               xaxis = list(title = paste("# of times ranked #", input$rank) ),
               yaxis= list(title = paste("last time ranking #", input$rank) ) )
    )
    # INFO BOXES SECTION # 
    
    output$box1 <- renderInfoBox({
      #  temp_dat <- scat_data()
      infoBox(
        value = max(scat_data()$ranking_times),
        title = "A rate of", 
        subtitle = "times", 
        icon = icon("list-ol"),
        color = "green"
      )
    })
    
    output$box2 <- renderInfoBox({
      
      temp_dat <- scat_data() %>%
        filter(ranking_times == max(scat_data()$ranking_times)) %>% 
        select(latest_ranking_year)
      
      infoBox(
        
        value = 2019 - max(as.numeric(temp_dat)),
        title = "last time achieved the rank", 
        subtitle = "years ago", 
        icon = icon("calendar-check"),
        color = "green"
        
      )
    })
    
    output$box3 <- renderInfoBox({
      
      temp_dat <- scat_data() %>%
        filter(ranking_times == max(scat_data()$ranking_times)) %>% 
        select(full_name)
      
      infoBox(
        value = temp_dat,
        title = paste("Who dominated the number", input$rank ,"position?"), 
        subtitle = "WITH", 
        icon = icon("address-card"),
        color = "green"
      )
      
    })
    
    # SHOWING THE DATA TABLE 
    
    output$rec_freq_table <- DT::renderDataTable({
      data <- scat_data()
      data
    })
    
    ## CAREER BENCHMARKING SECTION  ## 
    
    #Career trend reactive data
    
    data_rank_analysis <- reactive({
      data_final %>% 
        mutate(date = ymd(ranking_date)) %>%
        filter(full_name == input$player)
    })
    
    
    # Career analysis interactive graph
    output$career <- renderPlotly({
      career <- plot_ly(data_rank_analysis(),
                        x = data_rank_analysis()$date, 
                        y = data_rank_analysis()$ranking,
                        split = data_rank_analysis()$full_name,
                        marker = list(size = input$size2,
                                      #color = data_rank_analysis()$full_name, #'Set1'
                                      #color = "Greens",
                                      #alpha = 0.5,
                                      line = list(color = 'black',
                                                  width = 0.75))) %>%
        layout(title = paste("interactive |",input$player[1],"'s career overview versus", input$player[2] ),
               yaxis = list(title = "Ranking wiht Log10 scale", type = "log", autorange="reversed"),
               xaxis = list(title = "Years" ))
    })
    
    # Career analysis Trend line 
    
    output$analysis <- renderPlot({
      
      ggplot(data_rank_analysis(), 
             aes(x = date, y = ranking, color = full_name)) +
        geom_line(size = 1.5) +
        theme_bw()+
        scale_y_reverse() + 
        ggtitle("Trend lines | Players' careers")
      
    })
    # SHOWING THE DATA TABLE 
    output$career_table <- DT::renderDataTable({
      data <- data_rank_analysis()
      data
    })
    
    ### 3rd MENU ITEM DOWNLOAD THE SLICED DATA ###
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = "WTA_data.csv",
      content = function(file) {
        data <- data_years()
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # data to be downloaded shown visually 
    output$data_years_table <- DT::renderDataTable({
      data <- data_years()
      data
    })
  })

shinyApp(ui, server)
