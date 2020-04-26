############################################################################
# ---- ------ ------ ----- LIS 4761 - Final Project ------ ------ ----- ----
############################################################################
# ---
# title:   "LIS 4761 - Final Project"
# author:  "Kevin Hitt"
# date:    "April 2020"
# ---
# 

# Analyzing Airbnb Listings
# Interactive Association Rules Mining with Shiny
# "vacation rental space in the sharing economy"
#
# The objective of this project is to utilize association rules mining to find 
# relationships between the dataset variables and price. 
#
# A Shiny app will be implemented to allow the viewer to interactively set the 
# confidence level, support level, and minimum number of rules of the aRules 
# visualization. This is of particular interest to me because I found it difficult 
# during the Titanic association rules assignment to find the “sweet spot” of the 
# support and confidence levels. I believe the interactive sliders in a Shiny app 
# would be the perfect way to solve this. 
#
# The ultimate question to be answered is, “Which factor most strongly correlates 
# to differences in price?” Do good marketing techniques (i.e. catchy description, 
# frequent reviews, multiple listings) correspond with a higher price? Or is it 
# based strongly on a single factor like location or room privacy?
#
# Download source: https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
# Data originally from: http://insideairbnb.com/ 
# 48,895 records of 16 variables

# load libraries
library(arules)
library(arulesViz)
library(devtools)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)

# read data
airbnb <- read.csv("NYC_2019.csv", header=T)

# explore data via dplyr
glimpse(airbnb)

###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                              SHINY APP                               ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

# create user interface
ui <- fluidPage(
  titlePanel("Association Rules of Airbnb NYC Data"),
  sidebarLayout(
    # set input options
    sidebarPanel(
      sliderInput("supp", "Support Level:", min = 0.01, max = 1, value = 0.5, step = 0.005),
      sliderInput("conf", "Confidence Level:", min = 0.1, max = 1, value = 0.5, step = 0.005),
      sliderInput("len", "Minimum Rule Length:", min = 1, max =10, value = 1, step = 1),
      sliderInput("mlen", "Maximum Rule Length:", min = 1, max =10, value = 4, step = 1)
    ),
    # set navigation tabs
    mainPanel(
      tabsetPanel(id = "shiny_tabs",
                  tabPanel("Summary",
                           fluidRow( column(12,DT::dataTableOutput("price_breaks")) ),
                           fluidRow( column(12,DT::dataTableOutput("borough_breaks")) ),
                           fluidRow( column(12,DT::dataTableOutput("neighb_breaks")) ),
                           fluidRow( column(12,DT::dataTableOutput("minn_breaks")) ),
                           fluidRow( column(12,DT::dataTableOutput("host_breaks")) ),
                           fluidRow( column(12,DT::dataTableOutput("avail_breaks")) )
                  ),
                  tabPanel("Rules", value = "datatable", DT::dataTableOutput("rules")),
                  tabPanel("Scatter Plot", value = "graph", plotlyOutput("plot1")),
                  tabPanel("Circle Layout Plot", value = "graph", plotOutput("plot2")),
                  tabPanel("Paracoord Plot", value = "graph", plotOutput("plot3")))
    )
  ) 
)

# create shiny server
server <- function(input, output) {
  
  ###-------------------------------------------------------------------------
  ###                        DESCRIPTIVE STATISTICS                        ---
  ###-------------------------------------------------------------------------
  
  # read data
  airbnb <- read.csv("NYC_2019.csv", header=T)
  
  # rename columns for clarity during rule analysis
  names(airbnb) <- c("id",           #listing id
                     "title",        #listing description
                     "id_host",      #host id
                     "host",         #host first name
                     "borough",      #nyc borough
                     "neighborhood", #nyc borough neighborhood
                     "lat",          #latitude
                     "lon",          #longitude
                     "room_type",    #privacy level (i.e. single room, whole apartment)
                     "price",        #listing price
                     "min_nights",   #minimum # of nights required to book listing
                     "num_rev",      #total number of listing reviews
                     "last_rev",     #date of last review
                     "monthly_rev",  #calculated average # of reviews per month
                     "host_count",   #count of all a single host's listings
                     "avail365")     #number of days listing is available annually
  
  # convert date column to date format from factor
  airbnb$last_rev <- as.Date(airbnb$last_rev, format = "%Y-%m-%d")
  
  # find columns with NA
  names(airbnb)[sapply(airbnb, anyNA)]
  
  # substite NA values in columns where NA found
  airbnb$monthly_rev[is.na(airbnb$monthly_rev)] <- 0
  airbnb$last_rev[is.na(airbnb$last_rev)] <- "1970-01-01" #cannot replace with 0
  
  # replace 'last_rev' with number of days since data collection date
  # this allows for discretization and therefore inclusion in the ARM
  airbnb$last_rev <- max(airbnb$last_rev) - airbnb$last_rev
  
  # convert time difference format to numeric (i.e. '24 days difference' to '24')
  airbnb$last_rev <- as.numeric(airbnb$last_rev)
  
  # find percentages of price groups with custom break points
  # dplyr not used because of custom break points (still could be possible)
  price_breaks <- data.frame(percentage=integer())
  price_breaks[1,] <- airbnb %>% tally(price <= 100)
  price_breaks[2,] <- airbnb %>% tally(price > 100 & price <= 200)
  price_breaks[3,] <- airbnb %>% tally(price > 200 & price <= 300)
  price_breaks[4,] <- airbnb %>% tally(price > 300 & price <= 400)
  price_breaks[5,] <- airbnb %>% tally(price > 400 & price <= 500)
  price_breaks[6,] <- airbnb %>% tally(price > 500 & price <= 1000)
  price_breaks[7,] <- airbnb %>% tally(price > 1000)
  price_breaks <- format(round(price_breaks / 48895 * 100, 2), nsmall = 2)
  price_breaks <- cbind(price_breaks, price_group = c("0-100","101-200","201-300","301-400",
                                                      "401-500", "501-1000", ">1000"))
  price_breaks     #48% of listings are $100 or less
  
  # find percentages of borough groups
  # since column is categorical I can more easily use dplyr
  borough_breaks <- airbnb %>%
    group_by(borough) %>%
    tally()
  borough_breaks$percentage <- format(round(borough_breaks$n / 48895 *100, 2), nsmall = 2)
  borough_breaks   #Over 80% of listings are in Brooklyn & Manhattan
  
  # find percentages of neighborhood groups
  neighb_breaks <- airbnb %>%
    group_by(neighborhood) %>%
    tally()
  neighb_breaks$p <- format(round(neighb_breaks$n / 48895 *100, 2), nsmall = 2)
  neighb_breaks <- neighb_breaks %>%
    arrange(desc(p))
  head(neighb_breaks) #With over 200 different neighborhoods, not much stands out
  
  # find percentages of minimum nights
  minn_breaks <- airbnb %>%
    group_by(min_nights) %>%
    tally()
  minn_breaks$p <- format(round(minn_breaks$n / 48895 *100, 2), nsmall = 2)
  minn_breaks <- minn_breaks %>%
    arrange(desc(p))
  
  head(minn_breaks) #26% of listings are for 1 night, 7% for 30 nights
  
  # find percentages of total host listing counts
  host_breaks <- data.frame(percentage=integer())
  host_breaks[1,] <- airbnb %>% tally(host_count == 1)
  host_breaks[2,] <- airbnb %>% tally(host_count == 2)
  host_breaks[3,] <- airbnb %>% tally(host_count >= 3 & host_count <= 10) 
  host_breaks[4,] <- airbnb %>% tally(host_count > 10)
  host_breaks <- format(round(host_breaks / 48895 * 100, 2), nsmall = 2)
  host_breaks <- cbind(host_breaks, host_group = c("1","2","3-10",">10"))
  
  host_breaks   #66% of hosts only have 1 listing total
  
  # find percentages of annual availability
  avail_breaks <- data.frame(percentage=integer())
  avail_breaks[1,] <- airbnb %>% tally(avail365 == 0)
  avail_breaks[2,] <- airbnb %>% tally(avail365 >= 1 & avail365 <= 20)
  avail_breaks[3,] <- airbnb %>% tally(avail365 >= 21 & avail365 <= 100) 
  avail_breaks[4,] <- airbnb %>% tally(avail365 >= 101 & avail365 <= 200) 
  avail_breaks[5,] <- airbnb %>% tally(avail365 >= 201 & avail365 <= 300) 
  avail_breaks[6,] <- airbnb %>% tally(avail365 >= 301) 
  avail_breaks <- format(round(avail_breaks / 48895 * 100, 2), nsmall = 2)
  avail_breaks <- cbind(avail_breaks, avail_group = c("0","1-20","21-100","101-200","201-300",">301"))
  
  avail_breaks   #35% of listings are assumedly unavailable
  
  
  ###------------------------------------------------------------------------
  ###                    ASSOCIATION RULE MINING (ARM)                    ---
  ###------------------------------------------------------------------------
  
  # remove specific variables to limit confusion in ARM
  airbnb_slim <- subset(airbnb, select = -c(id, id_host, host, title,
                                            lat, lon, neighborhood))
  
  # discretize integer values into k-means clustered categories
  # - create labels for discretization settings
  dlabelsd <- c("most recent","recent","old")
  dlabels3 <- c("low","medium","high")
  dlabels5 <- c("lowest","low", "medium", "high", "highest")
  dlabels7 <- c("lowest","lower", "low", "medium", "high", "higher", "highest")
  
  # - discretize 
  airbnb_slim <- discretizeDF(airbnb_slim,
                              methods = list(
                                price = list(method = "cluster", breaks = 7, labels=dlabels7),
                                host_count = list(method = "cluster", breaks = 5, labels=dlabels5),
                                avail365 = list(method = "cluster", breaks = 7, labels=dlabels7),
                                min_nights = list(method = "cluster", breaks = 3, labels=dlabels3),
                                num_rev = list(method = "cluster", breaks = 3, labels=dlabels3),
                                last_rev = list(method = "cluster", breaks = 3, labels=dlabelsd),
                                monthly_rev = list(method = "cluster", breaks = 3, labels=dlabels3)
                              ),
                              default = list(method = "interval", 
                                             breaks = 7,
                                             labels = dlabels7)
  )
  
  
  # Convert dataframe to transaction data set
  rent_trans <- as(airbnb_slim, "transactions")
  
  # Use aRules to calculate rule clusters in transactions
  rent_rules <- reactive({ apriori(data=rent_trans, 
                                   parameter=list(support=as.numeric(input$supp), 
                                                  confidence=as.numeric(input$conf),
                                                  minlen=as.numeric(input$len) +1,
                                                  maxlen=as.numeric(input$mlen),
                                                  target="rules")
  )})

  
  ###-------------------------------------------------------------------------
  ###                         SHINY  OUTPUT CALLS                          ---
  ###-------------------------------------------------------------------------
  
  # descriptive stats tables
  output$price_breaks <- DT::renderDataTable({ price_breaks })
  output$borough_breaks <- DT::renderDataTable({ borough_breaks })
  output$neighb_breaks <- DT::renderDataTable({ neighb_breaks })
  output$minn_breaks <- DT::renderDataTable({ minn_breaks })
  output$host_breaks <- DT::renderDataTable({ host_breaks })
  output$avail_breaks <- DT::renderDataTable({ avail_breaks })
  
  # rules table
  output$rules <-  DT::renderDataTable  ( {
    rent_rules_table <- rent_rules()
    rent_rules_table <- DATAFRAME(rent_rules_table, separate =T)
    rent_rules_table[,3] <- format(round(rent_rules_table[,3], 2), nsmall = 2)
    rent_rules_table[,4] <- format(round(rent_rules_table[,4], 2), nsmall = 2)
    rent_rules_table[,5] <- format(round(rent_rules_table[,5], 2), nsmall = 2)
    rent_rules_table
  })
  
  # plots
  output$plot1 <- renderPlotly( plot(rent_rules(), method="scatter", 
                                     measure = "support", shading = "lift", engine="htmlwidget") )
  
  output$plot2 <- renderPlot( plot(rent_rules(), method="graph", 
                                   igraphLayout = "layout_in_circle", measure = "support", 
                                   shading = "lift") )
  
  output$plot3 <- renderPlot( plot(rent_rules(), method="paracoord", control=list(reorder=TRUE), 
                                   measure = "support", shading = "lift") )
  
}

# run application
hotel_app <- shinyApp(ui = ui, server = server)
#runApp(hotel_app, display.mode = "showcase")


# save RData file

# must manually run by removing "reactive" brackets for rent_rules
# otherwise, is cannot be saved as it is dynamically loaded by Shiny
#save(airbnb, airbnb_slim, rent_rules,rent_trans, 
#     ui, server, dlabels5, dlabels7, dlabels3, dlabelsd,
#     price_breaks,borough_breaks, neighb_breaks, 
#     minn_breaks, host_breaks, avail_breaks, hotel_app,
#     file = "hitt-final-data.RData")
