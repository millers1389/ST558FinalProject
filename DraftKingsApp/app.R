#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)

library(tidyverse)
library(haven)
library(knitr)
library(dplyr)
library(rgl)
library(tree)
library(ggplot2)
library(ggiraphExtra)
library(gbm)
library(DT)


##READING IN and MANIPULATING THE DATA
draftKingsData <- read_csv("draftKingsData.csv")


draftKingsData$PlayerName <- as.factor(draftKingsData$PlayerName)

draftKingsData$`Inj` <- ifelse(is.na(draftKingsData$Inj), "None", draftKingsData$Inj)

draftKingsData$Inj <- as.factor(draftKingsData$Inj)

draftKingsData$Pos <- as.factor(draftKingsData$Pos)

draftKingsData$Team <- as.factor(draftKingsData$Team)

draftKingsData$Opp <- as.factor(draftKingsData$Opp)

draftKingsData$Rest <- as.numeric(draftKingsData$Rest)

draftKingsData$DvP <- substr(draftKingsData$DvP, 1, nchar(draftKingsData$DvP, type="chars")-1)

draftKingsData$DvP <- as.numeric(draftKingsData$DvP)

draftKingsData

# USER INTERFACE

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Draft Kings NBA Application", titleWidth = 1000),
                    
                    dashboardSidebar(sidebarMenu(
                        ##TABS
                        #TAB1 INFO
                        menuItem("Data and App Information", 
                                 tabName = "info", icon = icon("archive")),
                        
                        #TAB 2 GRAPHS
                        menuItem("Draft Kings Data Exploration", 
                                 tabName = "graphs", icon = icon("basketball-ball")),
                        
                        #TAB3 USL
                        menuItem("Unsupervised Learning", 
                                 tabName = "usl", icon = icon("basketball-ball")),
                        
                        #TAB4 MODEL
                        menuItem("Linear/Boost Modeling", 
                                 tabName = "model", icon = icon("basketball-ball")),
                        
                        #TAB5 DATA
                        menuItem("Draft Kings NBA Data", 
                                 tabName = "data", icon = icon("basketball-ball"))
                    ) #Closes Sidebar Menu
                    ), #Closes Dashboard Sidebar
                    
                    
                    dashboardBody(
                        tabItems(
                            
                            
                            #FIRST TAB BODY
                            tabItem(tabName = "info",
                                    # fluidRow(
                                    #   withMathJax(),
                                    column(6,
                                           h1("About the Data"),
                                           box(width = 12,
                                               tabsetPanel(
                                                   
                                                   #FIRST TAB IN FIRST BOX
                                                   tabPanel("Data Set Description",
                                                            h5("This application uses data found in a Draft Kings NBA data set compiled by Alan Du on Kaggle called Daily Fantasy Basketball - DraftKings NBA. The data can be found at the following website:"),
                                                            a("Draft Kings Data", href="https://www.kaggle.com/alandu20/daily-fantasy-basketball-draftkings"),
                                                            
                                                            h5("The data set includes about a month of Draft Kings NBA game data from November 27, 2017 to December 28, 2017. This data includes individual player statistics, opposing team statistics, projected fantasy points on a given night, and the actual fantasy points scored by that player that night, along with other statistics to predict daily fantasy basketball success such as player value and salary."),
                                                            h5("The data set also included Draft Kings contest information, player salary information, and Draft King payout structures. However, this appliction only uses a compilation of the month of Draft Kings NBA data ")
                                                   ),#Closes First Tab
                                                   
                                                   #SECOND TAB IN FIRST BOX
                                                   tabPanel("Data Manipulation",
                                                            h5("In order to get the data in a usable form, I had to compile each day's data into one data frame for the entire month."),
                                                            h5("I read in each individual CSV file (one for each day) and joined them into a data frame that consisted of the entire month of data."),
                                                            h5("I coerced certain variables to the proper class type so that they can be used properly in the modeling and predictions of the application. I also removed all players with an 'out' injury designation so they would not impact the predictions."),
                                                            h5("Lastly, I removed some of the variables that did not aid in the modeling of the data.")
                                                   ), #Closes Second Tab
                                                   
                                                   #THIRD TAB IN FIRST BOX
                                                   tabPanel("Variables Used",
                                                            h2("Variables"),
                                                            h5("Player Name"),
                                                            h5("Inj - Injury Status"),
                                                            h5("Pos - Position"),
                                                            h5("Salary - Draft Kings Salary"),
                                                            h5("Team - Player's Team"),
                                                            h5("Opp - Opposing Team"),
                                                            h5("Rest - Number of Days off since Last Game"),
                                                            h5("USG - Usage Rate"),
                                                            h5("PER - Player Efficiency Rating"),
                                                            h5("Pace - Opponenet Pace of Play"),
                                                            h5("DEff - Opponent Defensive Efficiency"),
                                                            h5("DvP - Opponent Defensive Rating vs Position"),
                                                            h5("L2FGA - Average Field Goals Attempted over Last 2 Games"),
                                                            h5("L5FGA - Average Field Goals Attempted over Last 5 Games"),
                                                            h5("SFGA - Season Average Field Goals Attempted"),
                                                            h5("L2Min - Average Minutes Played over Last 2 Games"),
                                                            h5("L5Min - Average Minutes Played over Last 5 Games"),
                                                            h5("SMin - Season Average Minutes Played"),
                                                            h5("L2FP - Average Fantasy Points over Last 2 Games"),
                                                            h5("L5FP - Average Fantasy Points over Last 5 Games"),
                                                            h5("SFP - Season Average Fantasy Points"),
                                                            h5("Floor - Season Low Fantasy Points"),
                                                            h5("CeilFP - Season High Fantasy Points"),
                                                            h5("ProjMin - Projected Minutes"),
                                                            h5("FP - Actual Fantasy Points Scored that Night (Response Variable)"),
                                                            h5("Value - Actual Draft Kings Value")
                                                   )#Closes Third Tab - Tab Panel
                                               )#Closes TabSet Panel
                                           ) #Closes Box
                                    ), #Closes Column
                                    
                                    #Second Box in First Tab
                                    column(6,
                                           h1("Application Functionality"),
                                           box(width = 12,
                                               
                                               h5("This application uses the Draft Kings NBA data to further analyze trends in the data. The application has 4 additional tabs: Data Exploration, Unsupervised Learning (K-Means Clustering), Modeling (Linear Regression and Boosted), and Data, which allow the user to select particular parameters and further explore trends in this data. The ultimate goal is for the user to select predictor variables for the model and input values for these predictors to predict the fantasy points scored by a player."),
                                               icon("basketball-ball"),
                                               h5("The application contains a number of different interactive plots and tables. It allows the user to select a player and see the statistics of choice. It also allows the user to select variables to see how some predictor variables affect the actual fantasy points. The app can also show the k-means clustering of the data for variables. In addition, this app allows the user to select predictor variables for linear regression and shows how good the fit of the model is by presenting adjust r square and RMSE in addition to other statistics. The boosted model tab creates a boosted model from the predictors selected by the user and shows the relative importance of these variables. Lastly, the app shows the linear model and boosted model predicted fantasy points of a specific NBA player.")
                                           ) #Closes Box
                                    ) #Closes Column
                                    # ) #Closes Fluid Row
                            ), #Closes TabItem
                            
                            
                            #SECOND TAB BODY
                            tabItem(tabName = "graphs",
                                    h1("Visualizing the Data"),
                                    tabsetPanel(
                                        #FIRST SIDETAB OF SECOND TAB
                                        tabPanel("Player Data Table",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         
                                                         h3("Choose a Player:"),
                                                         selectizeInput("player", "Player",
                                                                        selected = "LeBron James",
                                                                        choices = levels(as.factor(draftKingsData$PlayerName))),
                                                         
                                                         h3("Choose a Stat:"),
                                                         selectizeInput("stat", "Statistic",
                                                                        selected = "Average Fantasy Points",
                                                                        choices = c("Average Fantasy Points", 
                                                                                    "Average Value", 
                                                                                    "Average Minutes", 
                                                                                    "Average Usage", 
                                                                                    "Average Player Efficiency Rating",
                                                                                    "Average Field Goals Attempted")),
                                                         
                                                         downloadButton("saveData1", "Download Data")
                                                         
                                                         
                                                         
                                                     ),#Closes Sidebar Panel
                                                     
                                                     
                                                     #FIRST MAIN TAB
                                                     mainPanel(
                                                         tableOutput("playerTable")
                                                     ) #Closes mainPanel
                                                 )#closes sidebar Layout
                                        ),#Closes tabPanel
                                        
                                        #####SECOND TAB GRAPHS
                                        tabPanel("Variable Graphs",
                                                 sidebarLayout(
                                                     #SECOND SIDETAB
                                                     sidebarPanel(
                                                         
                                                         h3("Choose a variable to see trends in fantasy points:"),
                                                         selectizeInput("variables", "Variables", 
                                                                        selected = "Actual Fantasy Points", 
                                                                        choices = c("Actual Fantasy Points", "Usage", 
                                                                                    "Player Efficiency Rating", 
                                                                                    "Salary", "Rest", "Position", 
                                                                                    "Opponent Defensive Efficiency", 
                                                                                    "Opponent Defense vs Position", 
                                                                                    "Opponent Pace", 
                                                                                    "Average Field Goals Attempted", 
                                                                                    "Average Minutes", "
                                                        Season Average Fantasy Points", 
                                                                                    "Value", "Salary Vs. Value")),
                                                         
                                                         conditionalPanel("input.variables", 
                                                                          checkboxInput("zero", 
                                                                                        h5("Remove observations with 0 fantasy points"))),
                                                         
                                                         downloadButton("savePlot1", "Save Plot"),
                                                         downloadButton("saveData2", "Download Data")
                                                         
                                                     ), #closes sidebarPanel
                                                     
                                                     #SECOND MAIN TAB
                                                     mainPanel(
                                                         plotlyOutput("variablePlot")
                                                     ) #Closes mainpanel
                                                 )#closes sidebar layout
                                        ) #Closes tabPanel
                                    ) #Closes tabset Panel
                                    
                            ), #Closes TabItem
                            ###CLOSES SECOND TAB
                            
                            #TAB3 USL
                            tabItem(tabName = "usl",
                                    h1("Unsupervised Learning: Clustering"),
                                    sidebarLayout(
                                        sidebarPanel(
                                            h3("Select a variable:"),
                                            
                                            selectizeInput("xvar", "Variables", 
                                                           choices = c("Actual Fantasy Points" = "FP", "Value" = "Value", 
                                                                       "Usage" = "USG", "Player Efficiency Rating" = "PER", 
                                                                       "Salary" = "Salary", "Rest" = "Rest", 
                                                                       "Opponent Defensive Efficiency" = "DEff", 
                                                                       "Opponent Defense vs Position"= "DvP", 
                                                                       "Opponent Pace" = "Pace", 
                                                                       "Average Field Goals Attempted" = "SFGA", 
                                                                       "Average Minutes"= "SMin", 
                                                                       "Season Average Fantasy Points" = "SFP")),#Closes selectizeInput
                                            
                                            br(),
                                            
                                            h3("Select a DIFFERENT variable:"),
                                            selectizeInput("yvar", "Variables", 
                                                           choices = c("Season Average Fantasy Points" = "SFP", 
                                                                       "Actual Fantasy Points" = "FP", "Value" = "Value",
                                                                       "Usage" = "USG", "Player Efficiency Rating" = "PER",
                                                                       "Salary" = "Salary", "Rest" = "Rest", 
                                                                       "Opponent Defensive Efficiency" = "DEff", 
                                                                       "Opponent Defense vs Position"= "DvP", 
                                                                       "Opponent Pace" = "Pace", 
                                                                       "Average Field Goals Attempted" = "SFGA", 
                                                                       "Average Minutes"= "SMin")),#Closes selectizeInput
                                            
                                            numericInput('cluster', 'Cluster Count', 3,
                                                         min = 1, max = 9),
                                            
                                            downloadButton("savePlot2", "Save Plot"),
                                            downloadButton("saveData3", "Download Data")
                                        ),#Closes sidebarPanel
                                        mainPanel(
                                            plotOutput("clusterPlot")
                                        )#Closes mainPanel
                                    )#Closes sidebarLayout
                            ),#Closes tabItem
                            
                            
                            
                            #TAB4 MODEL
                            tabItem(tabName = "model",
                                    h1("Predicting the Data"),
                                    tabsetPanel(
                                        #FIRST SIDETAB OF SECOND TAB
                                        tabPanel("Linear Regression",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         h3("Choose predictor variables for linear regression model:"),
                                                         checkboxGroupInput("lr", "Linear Regression Predictors",
                                                                            choices = c("Usage" = "USG", "Player Efficiency Rating" =
                                                                                            "PER", "Rest" = "Rest",
                                                                                        "Opponent Defensive Efficiency" = "DEff",
                                                                                        "Opponent Defense vs Position"= "DvP", 
                                                                                        "Opponent Pace" = "Pace", 
                                                                                        "Last 2 Games: Average Field Goals Attempted" = "L2FGA",
                                                                                        "Last 5 Games: Average Field Goals Attempted" = "L5FGA",
                                                                                        "Average Field Goals Attempted" = "SFGA", 
                                                                                        "Projected Minutes" = "ProjMin", 
                                                                                        "Last 5 Games: Average Fantasy Points" = "L5FP", 
                                                                                        "Season Average Fantasy Points" = "SFP" )), #Closes CheckboxGroup
                                                         
                                                         downloadButton("savePlot3", "Save Plot"),
                                                         downloadButton("saveData4", "Download Data")
                                                         
                                                     ),#Closes sidebarPanel
                                                     mainPanel(
                                                         
                                                         textOutput("Formula"),
                                                         plotOutput("lrPlot"),
                                                         tableOutput("lrTable")
                                                     )
                                                 )#Closes Sidebar Layout
                                        ),#Closes TabPanel
                                        
                                        tabPanel("Boosted Model",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         h3("Choose predictor variables for boosted regression model:"),
                                                         
                                                         checkboxGroupInput("boosted", "Boosted Model Predictors", 
                                                                            choices = c("Usage" = "USG", "Player Efficiency Rating" =
                                                                                            "PER", "Rest" = "Rest", 
                                                                                        "Opponent Defensive Efficiency" = "DEff",
                                                                                        "Opponent Defense vs Position"= "DvP", 
                                                                                        "Opponent Pace" = "Pace", 
                                                                                        "Last 2 Games: Average Field Goals Attempted" = "L2FGA",
                                                                                        "Last 5 Games: Average Field Goals Attempted" = "L5FGA",
                                                                                        "Average Field Goals Attempted" = "SFGA", 
                                                                                        "Projected Minutes" = "ProjMin", 
                                                                                        "Last 5 Games: Average Fantasy Points" = "L5FP", 
                                                                                        "Season Average Fantasy Points" = "SFP" )),
                                                         
                                                         numericInput("trees", 
                                                                      "Select Number of Trees (between 100 and 5000):",
                                                                      1000, min=100, max = 5000),
                                                         
                                                         numericInput("shrinkage", 
                                                                      "Select Shrinkage:", .01, min=0, max = 1),
                                                         
                                                         numericInput("interaction", 
                                                                      "Select Interaction Depth:", 1, min=1, max = 20),
                                                         
                                                         downloadButton("savePlot4", "Save Plot"),
                                                         downloadButton("saveData5", "Download Data")
                                                         
                                                         
                                                     ),#Closes sidebarPanel
                                                     mainPanel(
                                                         textOutput("boostFormula"),
                                                         plotOutput("boostPlot"),
                                                         tableOutput("boostTable")
                                                     )
                                                 )#Closes sidebarLayout
                                        ),# Closes tabPanel
                                        
                                        
                                        tabPanel("Making Predictions",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         h1("Select Predictor Parameters"),
                                                         h3("Choose a Player:"),
                                                         selectizeInput("player2", "Player",
                                                                        selected = "LeBron James",
                                                                        choices = levels(as.factor(draftKingsData$PlayerName))),
                                                         numericInput("Usage", "Fill in Average Usage",
                                                                      value = 20,  min=0, max=40),
                                                         
                                                         numericInput("PER", "Fill in PER",
                                                                      value = 20,  min=0, max=40),
                                                         
                                                         numericInput("SFGA", "Fill in Season Average Field Goal Attempts",
                                                                      value = 10,  min=0, max=40),
                                                         
                                                         numericInput("Mins", "Fill in Projected Minutes",
                                                                      value = 24,  min=0, max=48),
                                                         numericInput("DEff", "Fill in Defensive Efficiency",
                                                                      value = 103,  min=97, max=109),
                                                         numericInput("Pace", "Fill in Opponent Pace",
                                                                      value = 103, min=97, max=109),
                                                         numericInput("DvP",
                                                                      "Fill in Opponent Defense Vs Position",
                                                                      value = 0, min=-18, max=13),
                                                         numericInput("SFP", "Fill in Season Average Fantasy Points",
                                                                      value = 10,  min=0, max=100),
                                                         br(),
                                                         h3("Select tuning parameters for boosted model prediction"),
                                                         
                                                         
                                                         numericInput("trees2", "Select Number of Trees (between 100 and 5000):",
                                                                      1000, min=100, max = 5000),
                                                         numericInput("shrinkage2", "Select Shrinkage:",
                                                                      value = .01, min=.00001, max = 1),
                                                         numericInput("interaction2",
                                                                      "Select Interaction Depth:",
                                                                      1, min=1, max = 20),
                                                         
                                                         downloadButton("savePlot5", "Save Plot"),
                                                         downloadButton("saveData6", "Download Data")
                                                     ),#Closes sidebarPanel
                                                     
                                                     mainPanel(
                                                         h1("Predicted Fantasy Points"),
                                                         tableOutput("Predictions")
                                                         
                                                     )
                                                 )#Closes sidebarLayout
                                        )# Closes tabPanel
                                        
                                    )#Closes TabsetPanel
                            ),#Closes Tab Item,
                            
                            #TAB5 DATA
                            tabItem(tabName = "data",
                                    fluidRow(
                                        column(12,
                                               h1("The Data"),
                                               DT::dataTableOutput("draftKingsDataset"))))
                        ) #Closes TabItems
                    ) #Closes Dashboard Body
) #Closes Dashboard Page

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
    
    
    getData <- reactive({
        playerName <- input$player
        
        playerData <- draftKingsData %>% filter(PlayerName == playerName) %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        playerData
    })
    
    #getData2 <- reactive({
    #variableName <- input$variables
    
    variableData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min )
    variableData
    #})
    
    getData3 <- reactive({
        nonZero <- input$zero
        
        nonZeroData <- variableData %>% filter(FP != 0)
        nonZeroData
        
    })
    
    getData4 <- reactive({
        clusterData <- input$cluster
        
        clusterData <- draftKingsData %>% select(FP, Value, USG, PER, SFGA, DEff, DvP, SFP, Pace, SMin, Salary, Rest)
        clusterData
    })
    
    getData5 <- reactive({
        modelData <- input$lr
        
        modelData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        modelData
    })
    
    getData6 <- reactive({
        
        modelData2 <- na.omit(draftKingsData) %>% filter(PlayerName == input$player2) %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min)
        train <- sample(1:nrow(na.omit(modelData2)), size = nrow(na.omit(modelData2))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData2), train)
        predDataTrain <- modelData2[train, ]
        predDataTest <- modelData2[test, ]
    })
    
    
    output$variablePlot <- renderPlotly({
        #get data
        variableData <- draftKingsData %>% select(-Likes, -PS, -ProjFP, -ProjVal, -Min )
        
        nonZeroData <- getData3()
        
        #base plotting object
        if(input$variables == "Actual Fantasy Points" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = FP))
            
            g + geom_histogram(fill = "navy", color = "gold")
        } else if(input$variables=="Actual Fantasy Points"){
            g <- ggplot(variableData, aes(x = FP))
            
            g + geom_histogram(fill = "navy", color = "gold")
        } else if(input$variables == "Player Efficiency Rating" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = PER, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Player Efficiency Rating"){
            g <- ggplot(variableData, aes(x = PER, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Usage" & input$zero){
            g <- ggplot(nonZeroData, aes(x = USG, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Usage"){
            g <- ggplot(variableData, aes(x = USG, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Salary" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = Salary, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Salary"){
            g <- ggplot(variableData, aes(x = Salary, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Rest" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = Rest, y = FP, fill=Rest))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Rest"){
            g <- ggplot(variableData, aes(x = Rest, y = FP, fill = Rest))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Position" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = Pos, y = FP, fill = Pos))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Position"){
            g <- ggplot(variableData, aes(x = Pos, y = FP, fill = Pos))
            
            g + geom_bar(stat = "summary", fun.y = "mean")
        } else if(input$variables=="Opponent Defensive Efficiency" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = DEff, y = FP))
            
            g + geom_bar(stat = "summary", fun.y = "mean", fill="navy")
        } else if(input$variables=="Opponent Defensive Efficiency"){
            g <- ggplot(variableData, aes(x = DEff, y = FP))
            
            g + geom_bar(stat = "summary", fun.y = "mean", fill="navy")
        } else if(input$variables=="Opponent Defense vs Position" & input$zero ==TRUE){
            g <- ggplot(nonZeroData, aes(x = DvP, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        }else if(input$variables=="Opponent Defense vs Position"){
            g <- ggplot(variableData, aes(x = DvP, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        }else if(input$variables=="Opponent Pace" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = Pace, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        }else if(input$variables=="Opponent Pace"){
            g <- ggplot(variableData, aes(x = Pace, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        }else if(input$variables=="Average Field Goals Attempted" & input$zero == TRUE){
            g <- ggplot(nonZeroData, aes(x = SFGA, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        }else if(input$variables=="Average Field Goals Attempted"){
            g <- ggplot(variableData, aes(x = SFGA, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Average Minutes" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = SMin, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Average Minutes"){
            g <- ggplot(variableData, aes(x = SMin, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Season Average Fantasy Points" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = SFP, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Season Average Fantasy Points"){
            g <- ggplot(variableData, aes(x = SFP, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Value" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = Value, y = FP))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Value"){
            g <- ggplot(variableData, aes(x = Value, y = FP))
            
            g + geom_point(color="seagreen") + geom_smooth()
        } else if(input$variables=="Salary Vs. Value" & input$zero==TRUE){
            g <- ggplot(nonZeroData, aes(x = Salary, y = Value))
            
            g + geom_point(color="dodgerblue") + geom_smooth()
        } else if(input$variables=="Salary Vs. Value"){
            g <- ggplot(variableData, aes(x = Salary, y = Value))
            
            g + geom_point(color="seagreen") + geom_smooth()
        }
        
    })
    #     
    
    output$playerTable <- renderTable({   
        
        playerData <- getData()
        
        if(input$stat=="Average Fantasy Points"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Fantasy Points" = mean(FP))
        } else if(input$stat== "Average Value"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Value" = mean(Value))
        } else if(input$stat=="Average Minutes"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Minutes" = mean(SMin))
        } else if(input$stat == "Average Usage"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Usage" = mean(USG))
        } else if(input$stat == "Average Player Efficiency Rating"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Player Efficiency Rating" = mean(PER))
        } else if(input$stat == "Average Field Goals Attempted"){
            playerData %>% group_by(PlayerName) %>% summarise("Average Field Goals Attempted" = mean(SFGA))
        }
        
    })
    
    #    ###CLUSTER GRAPH 
    selectedData <- reactive({
        clusterData <- getData4()
        na.omit(clusterData)[, c(input$xvar, input$yvar)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$cluster)
    })
    
    output$clusterPlot <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    
    ####MODELING
    
    output$Formula <- renderPrint({
        if(is.null(input$lr)){
            "Please Select Predictor Variables"
        } else{
            
            paste("Regression Formula: FP", "~", paste(input$lr, collapse= " + "))
        }
    })
    
    output$lrPlot <- renderPlot({
        
        if(is.null(input$lr)){
            print("Please select a predictor")
        } else{
            
            modelData <-getData5()
            
            #Create training set and testing set
            set.seed(31)
            train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
            test <- dplyr::setdiff(1:nrow(modelData), train)
            predDataTrain <- modelData[train, ]
            predDataTest <- modelData[test, ]
            
            formula <- as.formula(paste("FP", "~", paste(input$lr, collapse= " + ")))
            
            lrFit <- lm(formula = formula, data = predDataTrain)
            
            lrPredict <- predict(lrFit, predDataTest)
            
            par(mfrow=c(2,2))
            # if(length(input$lr) > 0 ){
            #     ggPredict(lrFit)
            # } else "Select Predictor Variables"
            plot(lrFit)
            
        }
        
        
        
    })
    
    
    output$lrTable <- renderTable({
        
        if(is.null(input$lr)){
            "Please select a predictor"
        } else {
            modelData <-getData5()
            
            set.seed(31)
            train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
            test <- dplyr::setdiff(1:nrow(modelData), train)
            predDataTrain <- modelData[train, ]
            predDataTest <- modelData[test, ]
            
            formula <- as.formula(paste("FP", "~", paste(input$lr, collapse= " + ")))
            
            lrFit <- lm(formula = formula, data = predDataTrain)
            
            lrPredict <- predict(lrFit, predDataTest)
            #mean(as.vector(lrPredict)-as.vector(predDataTest$FP))^2
            
            RMSE<-sqrt(mean((lrPredict-predDataTest$FP)^2, na.rm = TRUE))
            require(MuMIn)
            fitStats <- data.frame(fitStat = c("RMSE", "Adj R Square", "AIC", "AICc", "BIC"),
                                   "Linear Regression Fit" = round(c(RMSE,summary(lrFit)$adj.r.squared, AIC(lrFit), MuMIn::AICc(lrFit), BIC(lrFit)), 3))
            
            
            
            
            
        }
    })
    
    output$boostFormula <- renderPrint({
        if(is.null(input$boosted)){
            "Please Select Predictor Variables"
        } else{
            
            paste("Boosted Model Formula: FP", "~", paste(input$boosted, collapse= " + "))
        }
    })
    
    output$boostPlot <- renderPlot({
        
        if(is.null(input$boosted)){
            "Please Select Predictor Variables"
        } else{
            
            modelData <-getData5()
            
            #Create training set and testing set
            set.seed(31)
            train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
            test <- dplyr::setdiff(1:nrow(modelData), train)
            predDataTrain <- modelData[train, ]
            predDataTest <- modelData[test, ]
            
            formula <- as.formula(paste("FP", "~", paste(input$boosted, collapse= " + ")))
            
            boostFit <- gbm(formula, data = predDataTrain, distribution = "gaussian", 
                            n.trees = input$trees, shrinkage = input$shrinkage, interaction.depth = input$interaction)
            
            boostPred <- predict(boostFit, 
                                 newdata = dplyr::select(predDataTest, -FP), 
                                 n.trees = input$trees)
            
            
            # if(length(input$lr) > 0 ){
            #     ggPredict(lrFit)
            # } else "Select Predictor Variables"
            
            
            # plot(boostPred, type="l")
            
            summary(boostFit)
            
        }
    })
    
    output$boostTable <- renderTable({
        
        if(is.null(input$boosted)){
            "Please Select Predictor Variables"
        } else{
            
            modelData <-getData5()
            
            #Create training set and testing set
            set.seed(31)
            train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
            test <- dplyr::setdiff(1:nrow(modelData), train)
            predDataTrain <- modelData[train, ]
            predDataTest <- modelData[test, ]
            
            formula <- as.formula(paste("FP", "~", paste(input$boosted, collapse= " + ")))
            
            boostFit <- gbm(formula, data = predDataTrain, distribution = "gaussian", 
                            n.trees = input$trees, shrinkage = input$shrinkage, 
                            interaction.depth = input$interaction)
            
            boostPred <- predict(boostFit, 
                                 newdata = dplyr::select(predDataTest, -FP), 
                                 n.trees = input$trees)
            
            
            summary(boostFit)
            
        }
    })
    
    output$Predictions <- renderTable({
        
        modelData <-getData5()
        
        #Create training set and testing set
        set.seed(31)
        train <- sample(1:nrow(na.omit(modelData)), size = nrow(na.omit(modelData))*0.8)
        test <- dplyr::setdiff(1:nrow(modelData), train)
        predDataTrain <- modelData[train, ]
        predDataTest <- modelData[test, ]
        
        formula1 <- as.formula(paste("FP", "~", "USG", "+", "PER", "+", "SFGA", "+", "ProjMin", "+", 
                                     "DEff", "+", "Pace", "+", "DvP", "+", "SFP"))
        # 
        lrFit <- lm(formula = formula1, data = predDataTrain)
        # 
        lrPredict <- predict(lrFit, newdata= data.frame(PlayerName = input$player2, 
                                                        USG = input$Usage, PER = input$PER, 
                                                        SFGA = input$SFGA, ProjMin = input$Mins, 
                                                        DEff = input$DEff, Pace = input$Pace, 
                                                        DvP = input$DvP, SFP = input$SFP))
        # 
        # formula2 <- as.formula(paste("FP", "~", paste(input$boosted, collapse= " + ")))
        # 
        boostFit <- gbm(formula1, data = predDataTrain, distribution = "gaussian",
                        n.trees = input$trees, 
                        shrinkage = input$shrinkage, 
                        interaction.depth = input$interaction)
        # 
        boostPred <- predict(boostFit,
                             newdata= data.frame(PlayerName = input$player2, USG = input$Usage, PER = input$PER, SFGA = input$SFGA, ProjMin = input$Mins, DEff = input$DEff, Pace = input$Pace, DvP = input$DvP, SFP = input$SFP),
                             n.trees = input$trees2,
                             shrinkage = input$shrinkage2,
                             interaction.depth= input$interaction2)
        
        
        
        predictionTable <- data.frame("Player Name" = input$player2, "Predicted Fantasy Points: Linear Regression Model" = lrPredict, "Predicted Fantasy Points: Boosted Tree Model" = boostPred)
        colnames(predictionTable) = c("Player Name", "Linear Regression Model", "Boosted Tree Model")
        predictionTable
    })
    
    ###OUTPUT DATATABLE
    output$draftKingsDataset <- DT::renderDataTable({
        DT::datatable(draftKingsData, options = list(scrollX=TRUE))
    })
    
    observe({
        usageVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(USG) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "Usage", "Fill in Average Usage",
                           value = round(usageVal,2),  min=0, max=40)
        
        PerVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(PER) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "PER", "Fill in Player Efficiency Rating",
                           value = round(PerVal,2),  min=0, max=40)
        
        FgaVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(SFGA) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "SFGA", "Fill in Average Field Goal Attempts",
                           value = round(FgaVal,2),  min=0, max=40)
        
        MinVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(ProjMin) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "Mins", "Fill in Projected Minutes",
                           value = round(MinVal,2),  min=0, max=40)
        
        SFPVal <- draftKingsData %>% filter(PlayerName == input$player2) %>% select(SFP) %>% `[[`(1) %>% mean()
        updateNumericInput(session, "SFP", "Fill in Average Fantasy Points",
                           value = round(SFPVal,2),  min=0, max=100)
    })
    
    
    output$saveData1 <- downloadHandler(
        filename = function(){
            "playerData.csv"
        },
        content = function(file){
            
            playerData <- getData()
            
            write_csv(playerData, file)
        }
    )
    
    
    
    output$saveData2 <- downloadHandler(
        filename = function(){
            "variableData.csv"
        },
        content = function(file){
            
            if(input$zero != TRUE){
                variableData 
                
                write_csv(variableData, file)
            } else {
                nonZeroData <- getData3() 
                
                write_csv(nonZeroData, file)
                
            }
        }
    )
    
    
    output$saveData3 <- downloadHandler(
        filename = function(){
            "clusterData.csv"
        },
        content = function(file){
            
            clusterData <- getData4()
            
            write_csv(clusterData, file)
        }
    )
    
    output$saveData4 <- downloadHandler(
        filename = function(){
            "modelData.csv"
        },
        content = function(file){
            
            modelData <- getData5() 
            
            write_csv(modelData, file)
        }
    )
    
    output$saveData5 <- downloadHandler(
        filename = function(){
            "modelData.csv"
        },
        content = function(file){
            
            modelData <- getData5() 
            
            write_csv(modelData, file)
        }
    )
    
    output$saveData6 <- downloadHandler(
        filename = function(){
            "modelData2.csv"
        },
        content = function(file){
            
            modelData2 <- getData6() 
            
            write_csv(modelData2, file)
        }
    )
    
    
    # output$savePlot2 <- downloadHandler(
    #     filename = function() { 
    #         "variablePlot.png" 
    #                 },
    #     content = function(file) {
    #         
    #         
    #         ggsave(file, plot = clusterPlot)
    #     }
    # )
    
})
#             




# Run the application 
shinyApp(ui = ui, server = server)

