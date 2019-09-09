#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#install.packages("pacman")

pacman::p_load("shiny", "pool", "DBI", "dplyr", "tidyr", "stringr", "ggplot2",
               "data.table", "devtools", "reshape2", "D3partitionR", "janitor", "tidyverse", "purrr", "lmerTest", "emmeans", "reshape2", "plotly",
               "ggrepel", "pander", "stargazer", "lattice", "RColorBrewer")

#ZeaGG1: gringlobal; Pollination data 2015-present
channel1 <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "arsiaamp3zeagg1", timeout = Inf, Database = "gringlobal", Trusted_Connection = "TRUE")

# Query for Nursery Processing Status
actions <- dbSendQuery(channel1, "SELECT 
                       i.inventory_number_part1
                       + COALESCE(' '+ CONVERT(NVARCHAR, i.inventory_number_part2), '')
                       + COALESCE(' '+ i.inventory_number_part3,'')
                       + COALESCE(' '+ i.form_type_code,'') AS inventory
                       ,ia2.[action_name_code]
                       ,ia2.[completed_date]
                       ,a.accession_number_part1
                       + COALESCE(' '+ CONVERT(NVARCHAR, a.accession_number_part2), '')
                       + COALESCE(' '+ a.accession_number_part3, '') AS accession
                       ,[m].[name] AS method
                       ,[m2].[name] AS image_method
                       FROM inventory_action ia 
                       INNER JOIN inventory i ON ia.inventory_id = i.inventory_id AND (i.inventory_number_part3 LIKE '1_nc_i%' 
                       OR i.inventory_number_part3 LIKE '1_nc_x%' 
                       OR i.inventory_number_part3 LIKE '2_nc_i%' 
                       OR i.inventory_number_part3 LIKE '2_nc_x%')
                       INNER JOIN inventory_action ia2 on i.inventory_id = ia2.inventory_id AND ia2.action_name_code IN ('PLANTED','HARVESTED','BULKSAMPLE', 'SCANNED', 'STORE')
                       INNER JOIN accession a ON i.accession_id = a.accession_id AND a.taxonomy_species_id IN (311987)
                       INNER JOIN   method m ON ia.method_id = m.method_id AND m.[name] LIKE 'MAIZE.INC.%.20%'
                       LEFT JOIN method m2 ON ia2.method_id = m2.method_id AND ia2.action_name_code LIKE 'SCANNED'
                       WHERE ia.action_name_code = ('PLANTED')
                       AND ia.completed_date > '2010-01-01 00:00:00'")

action_table <- dbFetch(actions)

df <- mutate(action_table, method = str_replace_all(method, "MAIZE.INC.", "")) %>%
  ## clean up group method names
  mutate(completed_date = as.Date(completed_date)) %>% mutate_if(is.character,as.factor) %>% arrange(completed_date) %>%
  ## Remove small nurseries, duplicated nurseries (e.g., multiple plantings)
  filter(!method == "ND.NPSAS.2012", 
         !method == "PRICO.3MG.2015.A", 
         !method == "PRICO.3MG.2015.B", 
         !method == "NC.NCSTATE-ARS.2017B") %>%
  
  ## Create new field using grepl to pull out Inc site.
  mutate(site =ifelse(grepl("GH", method), "GH",
                      ifelse(grepl("AMES", method),"AMES",
                      ifelse(grepl("STCROIX", method), "STCROIX",
                             ifelse(grepl("3MG", method),"3MG",
                                    ifelse(grepl("ICIA", method), "ICIA",
                                           ifelse(grepl("PRICO.PIONEER", method), "PRICO.PIONEER",
                                                  ifelse(grepl("TOLUCA", method), "TOLUCA",
                                                         ifelse(grepl("PVALLARTA", method), "PVALLARTA",
                                                                ifelse(grepl("OAHU.MONSANTO", method), "OAHU.MONSANTO",
                                                                        NA)))))))))) %>%
  
  mutate(year =ifelse(grepl("19nc", inventory),"2019",
                      ifelse(grepl("18nc", inventory), "2018",
                             ifelse(grepl("17nc", inventory),"2017",
                                    ifelse(grepl("16nc", inventory), "2016",
                                           ifelse(grepl("15nc", inventory), "2015",
                                                  ifelse(grepl("14nc", inventory), "2014",
                                                         ifelse(grepl("13nc", inventory), "2013",
                                                                ifelse(grepl("12nc", inventory), "2012",
                                                                       ifelse(grepl("11nc", inventory), "2011",
                                                                              ifelse(grepl("10nc", inventory), "2010",NA))))))))))) %>%
  
  
  ## Filter out SCANNED actions not required
  filter(!image_method %like% 'OFFTYPE') %>% 
  filter(!image_method %like% '17TH') %>%
  filter(!image_method %like% '1-BAL') %>%
  ## Mutate SCANNED actions to identify type
  mutate(action_name_code = str_replace_all(action_name_code, "SCANNED", 
                                            ifelse(grepl("ISOL",image_method), "ISOSCAN",
                                                   ifelse(grepl("KERNELS",image_method),"KERNELSCAN",
                         ifelse(grepl("EARS",image_method), "EARSCAN", 
                                ifelse(grepl("EAR-SECTIONS",image_method), "COBSCAN",
                                        NA)))))) %>%
  ## filter duplicates
  distinct(inventory, action_name_code, .keep_all = T)

actions <- c("PLANTED", "HARVESTED", "EARSCAN","COBSCAN","BULKSAMPLE", "KERNELSCAN","ISOSCAN","STORE")
action_table$completed_date <- as.Date(action_table$completed_date)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navbarPage("Maize Germplasm Management", 
             position = "fixed-top", 
             header = tags$style(type="text/css", "body {padding-top: 70px;}"),
             tabPanel("Processing",
   
   # Sidebar with a dropdown menu for nursery methods
   sidebarLayout(
      sidebarPanel(
         checkboxGroupInput("NurseryLoc",
                     "Choose nursery location", levels(as.factor(df$site)), selected = "AMES"),
         checkboxGroupInput("NurseryYear",
                    "Choose nursery year", levels(as.factor(df$year)), selected = "2019")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("status_bar"),
         plotOutput("gantt_plot", brush = brushOpts(id = "plot_brush", fill = "#ccc", direction = "x")),
         verbatimTextOutput("info")
      )
   )
),
            tabPanel("Increase Efficiency"
                     ),
            tabPanel("Adaptation Zones",
  sidebarLayout(
    sidebarPanel(
        textInput("accession-id", label = h3("Accession"), value = "Enter accession ID"),
                   hr(),
                   fluidRow(column(3, verbatimTextOutput("value")))
              ),
    mainPanel(
          plotOutput("scatter-plot"),
          verbatimTextOutput("group")
              )
            )),
            tabPanel("Collection Status",
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel("Tree Map")
  ))
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
# Nursery Processing Server

  ## Dataset
   nurseryInput<-reactive({
    dataset <- filter(df, site %in% input$NurseryLoc)%>% 
      filter(year %in% input$NurseryYear)
    dataset
  })

   output$status_bar <- renderPlot({
     ggplot(nurseryInput(), aes(x = action_name_code, fill = action_name_code)) + 
       geom_bar() +
       geom_text(stat = 'count', aes(label = ..count..)) +
       scale_x_discrete(limits = actions) +
       coord_flip() + 
       theme(legend.position = "none") +
       xlab("Action") + ylab("Count") + 
       facet_wrap(~year, scales = "fixed", dir = "v", strip.position = "left")
   })
   
   output$gantt_plot <- renderPlot({
     ggplot(nurseryInput(), aes(y = action_name_code, x = completed_date, color = action_name_code, shape = year)) + 
       geom_jitter(size = 2, width = 0) + 
       scale_y_discrete(limits = actions) + 
       theme(legend.position = "none") + 
       ylab("Action") + xlab("Completed Date") + facet_wrap(~year, dir = "v", scales = "free_x")
   })
   
   output$info <- renderPrint({
     brushedPoints(nurseryInput(), input$plot_brush)
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

