#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pool)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(sp)
library(rgdal)
library(raster)
library(dismo)
library(latticeExtra)
library(maptools)
library(plyr)
library(dplyr)
library(PBSmapping) 
library(splancs)
data(wrld_simpl)

setwd("S:/vmbernau/R_Files/OSU_Chiles/Chile-Distribution-Shiny")
df <- read.csv("Collections_by_year_for_figure_6_28_19_with_clim.csv") #dataset with lat/long and climate values
map <- raster("pep_suit_greater_4.tif") #species distribution model cropped at .4
mex <- shapefile("MEX_adm0.shp") #shape file to map Mexico

## Extract values from a raster file
# cas<-read.csv("read in file with lat and lon of points you want to extract data from")
# points<-(cas$Lon, cas$Lat)
# files <- list.files(path=("F:/wc2.0_30s_bio"), pattern='tif', full.names=TRUE)
# predictors <- stack(files)
# values <- extract(predictors, points)
# final<-cbind(points,values)
# write.csv(final, "path to where you want the data to go")


projection(mex) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
collection <- levels(as.factor(df$Collection))

variables <- colnames(df)[5:length(colnames(df))]

labels <- c("Available soil watercapacity volumetric faction FC=pF2.0",	
            "Available soil water capacity volumetric faction with_FC=pF2.3",
            "Available soil water capacity volumetric faction with_FC=pF2.5",
            "Total available soil water capacity",
            "BIO1 = Annual Mean Temperature",
            "BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))",
            "BIO3 = Isothermality (BIO2/BIO7) (* 100)", 
            "BIO4 = Temperature Seasonality (standard deviation *100)",
            "BIO5 = Max Temperature of Warmest Month",
            "BIO6 = Min Temperature of Coldest Month",
            "BIO7 = Temperature Annual Range (BIO5-BIO6)",
            "BIO8 = Mean Temperature of Wettest Quarter",
            "BIO9 = Mean Temperature of Driest Quarter",
            "BIO10 = Mean Temperature of Warmest Quarter",
            "BIO11 = Mean Temperature of Coldest Quarter",
            "BIO12 = Annual Precipitation",
            "BIO13 = Precipitation of Wettest Month",
            "BIO14 = Precipitation of Driest Month",
            "BIO15 = Precipitation Seasonality (Coefficient of Variation)",
            "BIO16 = Precipitation of Wettest Quarter",
            "BIO17 = Precipitation of Driest Quarter",
            "BIO18 = Precipitation of Warmest Quarter",
            "BIO19 = Precipitation of Coldest Quarter",
            "Bulk Density",
            "Cation Exchange Capacity",
            "Percent Clay",
            "Volumetric Percentage of Coarse Fragments",
            "Soil Organic Carbon Density",
            "Soil Organic Carbon Stock",	
            "Soil Organic Carbon content",
            "pH index measured in water",
            "pH index measured in KCl",	
            "Silt Content Percent",
            "Sand Content Percent",
            "Texture Class",
            "Available soil water capacity until wilting point"
            )

colnames(df)[5:length(colnames(df))] <- labels

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title ----
              titlePanel("Mexican Chiles: Environmental and Geographic Space"),
              sidebarLayout(
                  sidebarPanel(
                          checkboxGroupInput("Collection",
                                             "Choose collection(s)", collection, selected = "2013_2015"
                                             ),
                          selectInput("x", "Choose x-axis variable", labels, selected = labels[16]),
                          selectInput("y", "Choose y-axis variable", labels, selected = labels[5])
                  ),

                        # Show a plot of the generated distribution
                mainPanel(#tableOutput("table"),
                          plotOutput("distr_map", brush = brushOpts(id = "map_brush", fill = "#ccc")
                                      ),
                          #verbatimTextOutput("map_info"),
                          plotOutput("distr_box", brush = brushOpts(id = "plot_brush", fill = "#ccc")
                                     ),
                          verbatimTextOutput("distr_info")
                        )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # filter dataset based on selected input nursery
  dataInput<-reactive({
   data <- na.omit(df) %>% filter(Collection %in% input$Collection) %>% 
     dplyr::select(Accession.Number, Collection, Lon, Lat, input$x, input$y)
    data
    })
  # filter distribution from mapping dataset so it doesn't obscure the map
  dataMap <- reactive({
    data <- na.omit(df) %>% filter(Collection %in% input$Collection) %>% 
      filter(!Collection %in% 'Distribution') %>%
      dplyr::select(Accession.Number, Collection, Lon, Lat, input$x, input$y)
    data
  })
  
  # Map of collection points
  output$distr_map <- renderPlot({

      plot(mex)
      plot(map, add=T)
      points(dataMap()$Lon, dataMap()$Lat, col=dataMap()$Collection, pch=20, cex=1)
  })
  
  output$distr_box <- renderPlot({
    data <- dataInput()
    head(data)
    
    
    find_hull <- function(data) data[chull(data[,5], data[,6]), ]
    hulls2 <- ddply(data, "Collection", find_hull)
    
      ggplot(data, aes(data[,5], data[,6], colour=dataInput()$Collection, shape=dataInput()$Collection)) +
        theme_bw()+
        xlab(input$x) + 
        ylab(input$y) +
        geom_point(alpha=0.2) +
        theme(legend.position="bottom")
  })
  
  output$distr_info <- renderPrint({
    data <- dataInput()
    brushedPoints(data, input$plot_brush, input$x, input$y)
  })
}

# Run the application 
shinyApp(ui,server)
