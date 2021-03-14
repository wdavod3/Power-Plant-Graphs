#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Data sortening instructions:
# After egrid2018_data_v2 is downloaded right click on the tab PLNT18. When the menu pops up click on move or copy. 
# Make sure PLNT18 is highlighted and then check the box the says "Create a copy". 
# Under the section that says "To book:" choose the option "(new book)".
# Once the data is opened in a new tab go to File and Save As. 
# Choose the name for your file (ex: "PLNT18") and save it as a "CSV (Comma delimited) (*.csv)" file. 
# After this is saved remove any of the columns not of interest for the project. Do this by selecting a column, right clicking on it, and choosing "Delete". 
# Once you choose delete select the option that says "Entire column" then click the button that says "ok". 
# After that do the same for the second row of the data. 
# The final thing to do is go through the data and remove any instance of the '#' character. 
# Do ctrl F and choose the tab called "Replace". In the "Find What" section enter # and in the "Replace With" section just enter a space (" " no quotations). 


library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(DT)
library(stringr)

# --------------------------------------------------------------------- 2018 Data ------------------------------------------------------------------------

# Read in the data
plantData <- read.csv(file = "PLNT18.csv", sep = ",", header = TRUE)


# Simplify the data names
colnames(plantData)[5] <- "Coal"
colnames(plantData)[6] <- "Oil"
colnames(plantData)[7] <- "Gas"
colnames(plantData)[8] <- "Nuclear"
colnames(plantData)[9] <- "Hydro"
colnames(plantData)[10] <- "Bio"
colnames(plantData)[11] <- "Wind"
colnames(plantData)[12] <- "Solar"
colnames(plantData)[13] <- "Geo"
colnames(plantData)[14] <- "Fossil"
colnames(plantData)[15] <- "Unknown"


# Make the data columns into numbers not characters
plantData$Coal <- as.numeric(gsub(",","",plantData$Coal))
plantData$Oil <- as.numeric(gsub(",","",plantData$Oil))
plantData$Gas <- as.numeric(gsub(",","",plantData$Gas))
plantData$Nuclear <- as.numeric(gsub(",","",plantData$Nuclear))
plantData$Hydro <- as.numeric(gsub(",","",plantData$Hydro))
plantData$Bio <- as.numeric(gsub(",","",plantData$Bio))
plantData$Wind <- as.numeric(gsub(",","",plantData$Wind))
plantData$Solar <- as.numeric(gsub(",","",plantData$Solar))
plantData$Geo <- as.numeric(gsub(",","",plantData$Geo))
plantData$Fossil <- as.numeric(gsub(",","",plantData$Fossil))
plantData$Unknown <- as.numeric(gsub(",","",plantData$Unknown))

# Combine the data for the other columns
Other <- plantData$Fossil + plantData$Unknown
plantData <- cbind(plantData, Other)
plantData$Fossil <- NULL
plantData$Unknown <- NULL

#Convert State Names
plantData$Plant.state.abbreviation <- state.name[match(plantData$Plant.state.abbreviation,state.abb)]

# Other Columns
TotalGen <- plantData$Coal + plantData$Oil + plantData$Gas + plantData$Nuclear + plantData$Hydro + plantData$Bio + plantData$Wind + plantData$Solar + plantData$Geo + plantData$Other
plantData <- cbind(plantData, TotalGen)
CoalPer <- plantData$Coal / plantData$TotalGen
plantData <- cbind(plantData, CoalPer)
OilPer <- plantData$Oil / plantData$TotalGen
plantData <- cbind(plantData, OilPer)
GasPer <- plantData$Gas / plantData$TotalGen
plantData <- cbind(plantData, GasPer)
NuclearPer <- plantData$Nuclear / plantData$TotalGen
plantData <- cbind(plantData, NuclearPer)
HydroPer <- plantData$Hydro / plantData$TotalGen
plantData <- cbind(plantData, HydroPer)
BioPer <- plantData$Bio / plantData$TotalGen
plantData <- cbind(plantData, BioPer)
WindPer <- plantData$Wind / plantData$TotalGen
plantData <- cbind(plantData, WindPer)
SolarPer <- plantData$Solar / plantData$TotalGen
plantData <- cbind(plantData, SolarPer)
GeoPer <- plantData$Geo / plantData$TotalGen
plantData <- cbind(plantData, GeoPer)
OtherPer <- plantData$Other / plantData$TotalGen
plantData <- cbind(plantData, OtherPer)


TotalRenew <- plantData$Hydro + plantData$Bio + plantData$Wind + plantData$Solar + plantData$Geo
plantData <- cbind(plantData, TotalRenew)
RenewPer <- plantData$TotalRenew / plantData$TotalGen
plantData <- cbind(plantData, RenewPer)


TotalNonRenew <- plantData$Coal + plantData$Oil + plantData$Gas + plantData$Nuclear + plantData$Other
plantData <- cbind(plantData, TotalNonRenew)
NonPer <- plantData$TotalNonRenew / plantData$TotalGen
plantData <- cbind(plantData, NonPer)

coal18 <- subset(plantData, plantData$Coal > 0)
coal18 <- coal18[, -c(6:14)]
oil18 <- subset(plantData, plantData$Oil > 0)
oil18 <- oil18[, -5]
oil18 <- oil18[, -c(6:13)]        
gas18 <- subset(plantData, plantData$Gas > 0)
gas18 <- gas18[, -c(5:6)]
gas18 <- gas18[, -c(6:12)]
nuc18 <- subset(plantData, plantData$Nuclear > 0)
nuc18 <- nuc18[, -c(5:7)]
nuc18 <- nuc18[, -c(6:11)]
hyd18 <- subset(plantData, plantData$Hydro > 0)
hyd18 <- hyd18[, -c(5:8)]
hyd18 <- hyd18[, -c(6:10)]
bio18 <- subset(plantData, plantData$Bio > 0)
bio18 <- bio18[, -c(5:9)]
bio18 <- bio18[, -c(6:9)]
wind18 <- subset(plantData, plantData$Wind > 0)
wind18 <- wind18[, -c(5:10)]
wind18 <- wind18[, -c(6:8)]
sol18 <- subset(plantData, plantData$Solar > 0)
sol18 <- sol18[, -c(5:11)]
sol18 <- sol18[, -c(6:7)]
geo18 <- subset(plantData, plantData$Geo > 0)
geo18 <- geo18[, -c(5:12)]
geo18 <- geo18[, -6]
oth18 <- subset(plantData, plantData$Other > 0)
oth18 <- oth18[, -c(5:13)]

#---------------------------------------------------------------------------------------Data for part 1 map
illinois <- subset(plantData, plantData$Plant.state.abbreviation == "Illinois")

illCoal <- subset(illinois, illinois$Coal > 0)
illCoal <- illCoal[, -c(6:14)]
illOil <- subset(illinois, illinois$Oil > 0)
illOil <- illOil[, -5]
illOil <- illOil[, -c(6:13)]
illGas <- subset(illinois, illinois$Gas > 0)
illGas <- illGas[, -c(5:6)]
illGas <- illGas[, -c(6:12)]
illNuc <- subset(illinois, illinois$Nuclear > 0)
illNuc <- illNuc[, -c(5:7)]
illNuc <- illNuc[, -c(6:11)]
illHyd <- subset(illinois, illinois$Hydro > 0)
illHyd <- illHyd[, -c(5:8)]
illHyd <- illHyd[, -c(6:10)]
illBio <- subset(illinois, illinois$Bio > 0)
illBio <- illBio[, -c(5:9)]
illBio <- illBio[, -c(6:9)]
illWind <- subset(illinois, illinois$Wind > 0)
illWind <- illWind[, -c(5:10)]
illWind <- illWind[, -c(6:8)]
illSol <- subset(illinois, illinois$Solar > 0)
illSol <- illSol[, -c(5:11)]
illSol <- illSol[, -c(6:7)]
illGeo <- subset(illinois, illinois$Geo > 0)
illGeo <- illGeo[, -c(5:12)]
illGeo <- illGeo[, -6]
illOth <- subset(illinois, illinois$Other > 0)
illOth <- illOth[, -c(5:13)]

#Lists for the checkBoxes and dropdown menus
plantList = c("All", "Coal", "Oil",  "Gas", "Nuclear", "Hydro", "Bio", "Wind", "Solar", "Geo", "Other")


# --------------------------------------------------------------------- 2000 Data ------------------------------------------------------------------------
# Read in the data
plantData2000 <- read.csv(file = "PLNT2000.csv", sep = ",", header = TRUE)
plantData2000$OPRNAME.Plant.operator.name <- NULL

# Simplify the data names
colnames(plantData2000)[5] <- "Coal"
colnames(plantData2000)[6] <- "Oil"
colnames(plantData2000)[7] <- "Gas"
colnames(plantData2000)[8] <- "Nuclear"
colnames(plantData2000)[9] <- "Hydro"
colnames(plantData2000)[10] <- "Bio"
colnames(plantData2000)[11] <- "Wind"
colnames(plantData2000)[12] <- "Solar"
colnames(plantData2000)[13] <- "Geo"
colnames(plantData2000)[14] <- "Other"

# Make the data columns into numbers not characters
plantData2000$Coal <- as.numeric(plantData2000$Coal)
plantData2000$Oil <- as.numeric(plantData2000$Oil)
plantData2000$Gas <- as.numeric(plantData2000$Gas)
plantData2000$Nuclear <- as.numeric(plantData2000$Nuclear)
plantData2000$Hydro <- as.numeric(plantData2000$Hydro)
plantData2000$Bio <- as.numeric(plantData2000$Bio)
plantData2000$Wind <- as.numeric(plantData2000$Wind)
plantData2000$Solar <- as.numeric(plantData2000$Solar)
plantData2000$Geo <- as.numeric(plantData2000$Geo)
plantData2000$Other <- as.numeric(plantData2000$Other)
plantData2000$LAT.Plant.latitude <- as.numeric(gsub(",","",plantData2000$LAT.Plant.latitude))
plantData2000$LON.Plant.longitude <- as.numeric(gsub(",","",plantData2000$LON.Plant.longitude))
plantData2000$LON.Plant.longitude <- (plantData2000$LON.Plant.longitude * -1)
plantData2000<-plantData2000[-which(is.na(plantData2000$LAT.Plant.latitude)),]
plantData2000<-plantData2000[-which(is.na(plantData2000$Coal)),]


#Convert State Names
plantData2000$PSTATABB.State.abbreviation <- state.name[match(plantData2000$PSTATABB.State.abbreviation,state.abb)]

# Other Columns
TotalGen2000 <- plantData2000$Coal + plantData2000$Oil + plantData2000$Gas + plantData2000$Nuclear + plantData2000$Hydro + plantData2000$Bio + plantData2000$Wind + plantData2000$Solar + plantData2000$Geo + plantData2000$Other
plantData2000 <- cbind(plantData2000, TotalGen2000)
CoalPer2000 <- plantData2000$Coal / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, CoalPer2000)
OilPer2000 <- plantData2000$Oil / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, OilPer2000)
GasPer2000 <- plantData2000$Gas / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, GasPer2000)
NuclearPer2000 <- plantData2000$Nuclear / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, NuclearPer2000)
HydroPer2000 <- plantData2000$Hydro / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, HydroPer2000)
BioPer2000 <- plantData2000$Bio / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, BioPer2000)
WindPer2000 <- plantData2000$Wind / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, WindPer2000)
SolarPer2000 <- plantData2000$Solar / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, SolarPer2000)
GeoPer2000 <- plantData2000$Geo / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, GeoPer2000)
OtherPer2000 <- plantData2000$Other / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, OtherPer2000)


TotalRenew2000 <- plantData2000$Hydro + plantData2000$Bio + plantData2000$Wind + plantData2000$Solar + plantData2000$Geo
plantData2000 <- cbind(plantData2000, TotalRenew2000)
RenewPer2000 <- plantData2000$TotalRenew2000 / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, RenewPer2000)


TotalNonRenew2000 <- plantData2000$Coal + plantData2000$Oil + plantData2000$Gas + plantData2000$Nuclear + plantData2000$Other
plantData2000 <- cbind(plantData2000, TotalNonRenew2000)
NonPer2000 <- plantData2000$TotalNonRenew2000 / plantData2000$TotalGen2000
plantData2000 <- cbind(plantData2000, NonPer2000)


coal0 <- subset(plantData2000, plantData2000$Coal > 0.0 )
coal0 <- coal0[, -c(6:14)]
oil0 <- subset(plantData2000, plantData2000$Oil > 0.0)
oil0 <- oil0[, -5]
oil0 <- oil0[, -c(6:13)] 
gas0 <- subset(plantData2000, plantData2000$Gas > 0.0)
gas0 <- gas0[, -c(5:6)]
gas0 <- gas0[, -c(6:12)]
nuc0 <- subset(plantData2000, plantData2000$Nuclear > 0.0)
nuc0 <- nuc0[, -c(5:7)]
nuc0 <- nuc0[, -c(6:11)]
hyd0 <- subset(plantData2000, plantData2000$Hydro > 0.0)
hyd0 <- hyd0[, -c(5:8)]
hyd0 <- hyd0[, -c(6:10)]
bio0 <- subset(plantData2000, plantData2000$Bio > 0.0)
bio0 <- bio0[, -c(5:9)]
bio0 <- bio0[, -c(6:9)]
wind0 <- subset(plantData2000, plantData2000$Wind > 0.0)
wind0 <- wind0[, -c(5:10)]
wind0 <- wind0[, -c(6:8)]
wind0<-wind0[-which(is.na(wind0$LAT.Plant.latitude))]
sol0 <- subset(plantData2000, plantData2000$Solar > 0.0)
sol0 <- sol0[, -c(5:11)]
sol0 <- sol0[, -c(6:7)]
geo0 <- subset(plantData2000, plantData2000$Geo > 0.0)
geo0 <- geo0[, -c(5:12)]
geo0 <- geo0[, -6]
oth0 <- subset(plantData2000, plantData2000$Other > 0.0)
oth0 <- oth0[, -c(5:13)]


# --------------------------------------------------------------------- 2010 Data ------------------------------------------------------------------------
# Read in the data
plantData10 <- read.csv(file = "PLNT10.csv", sep = ",", header = TRUE)

# Simplify the data names
colnames(plantData10)[5] <- "Coal"
colnames(plantData10)[6] <- "Oil"
colnames(plantData10)[7] <- "Gas"
colnames(plantData10)[8] <- "Nuclear"
colnames(plantData10)[9] <- "Hydro"
colnames(plantData10)[10] <- "Bio"
colnames(plantData10)[11] <- "Wind"
colnames(plantData10)[12] <- "Solar"
colnames(plantData10)[13] <- "Geo"
colnames(plantData10)[14] <- "Fossil"
colnames(plantData10)[15] <- "Unknown"

# Make the data columns into numbers not characters
plantData10$Coal <- as.numeric(gsub(",","",plantData10$Coal))
plantData10$Oil <- as.numeric(gsub(",","",plantData10$Oil))
plantData10$Gas <- as.numeric(gsub(",","",plantData10$Gas))
plantData10$Nuclear <- as.numeric(gsub(",","",plantData10$Nuclear))
plantData10$Hydro <- as.numeric(gsub(",","",plantData10$Hydro))
plantData10$Bio <- as.numeric(gsub(",","",plantData10$Bio))
plantData10$Wind <- as.numeric(gsub(",","",plantData10$Wind))
plantData10$Solar <- as.numeric(gsub(",","",plantData10$Solar))
plantData10$Geo <- as.numeric(gsub(",","",plantData10$Geo))
plantData10$Fossil <- as.numeric(gsub(",","",plantData10$Fossil))
plantData10$Unknown <- as.numeric(gsub(",","",plantData10$Unknown))

# Combine the data for the other columns
Other <- plantData10$Fossil + plantData10$Unknown
plantData10 <- cbind(plantData10, Other)
plantData10$Fossil <- NULL
plantData10$Unknown <- NULL

#Convert State Names
plantData10$Plant.state.abbreviation <- state.name[match(plantData10$Plant.state.abbreviation,state.abb)]


# Other Columns
TotalGen10 <- plantData10$Coal + plantData10$Oil + plantData10$Gas + plantData10$Nuclear + plantData10$Hydro + plantData10$Bio + plantData10$Wind + plantData10$Solar + plantData10$Geo + plantData10$Other
plantData10 <- cbind(plantData10, TotalGen10)
CoalPer10 <- plantData10$Coal / plantData10$TotalGen10
plantData10 <- cbind(plantData10, CoalPer10)
OilPer10 <- plantData10$Oil / plantData10$TotalGen10
plantData10 <- cbind(plantData10, OilPer10)
GasPer10 <- plantData10$Gas / plantData10$TotalGen10
plantData10 <- cbind(plantData10, GasPer10)
NuclearPer10 <- plantData10$Nuclear / plantData10$TotalGen10
plantData10 <- cbind(plantData10, NuclearPer10)
HydroPer10 <- plantData10$Hydro / plantData10$TotalGen10
plantData10 <- cbind(plantData10, HydroPer10)
BioPer10 <- plantData10$Bio / plantData10$TotalGen10
plantData10 <- cbind(plantData10, BioPer10)
WindPer10 <- plantData10$Wind / plantData10$TotalGen10
plantData10 <- cbind(plantData10, WindPer10)
SolarPer10 <- plantData10$Solar / plantData10$TotalGen10
plantData10 <- cbind(plantData10, SolarPer10)
GeoPer10 <- plantData10$Geo / plantData10$TotalGen10
plantData10 <- cbind(plantData10, GeoPer10)
OtherPer10 <- plantData10$Other / plantData10$TotalGen10
plantData10 <- cbind(plantData10, OtherPer10)


TotalRenew10 <- plantData10$Hydro + plantData10$Bio + plantData10$Wind + plantData10$Solar + plantData10$Geo
plantData10 <- cbind(plantData10, TotalRenew10)
RenewPer10 <- plantData10$TotalRenew10 / plantData10$TotalGen10
plantData10 <- cbind(plantData10, RenewPer10)


TotalNonRenew10 <- plantData10$Coal + plantData10$Oil + plantData10$Gas + plantData10$Nuclear + plantData10$Other
plantData10 <- cbind(plantData10, TotalNonRenew10)
NonPer10 <- plantData10$TotalNonRenew10 / plantData10$TotalGen10
plantData10 <- cbind(plantData10, NonPer10)


coal10 <- subset(plantData10, plantData10$Coal > 0)
coal10 <- coal10[, -c(6:14)]
oil10 <- subset(plantData10, plantData10$Oil > 0)
oil10 <- oil10[, -5]
oil10 <- oil10[, -c(6:13)]        
gas10 <- subset(plantData10, plantData10$Gas > 0)
gas10 <- gas10[, -c(5:6)]
gas10 <- gas10[, -c(6:12)]
nuc10 <- subset(plantData10, plantData10$Nuclear > 0)
nuc10 <- nuc10[, -c(5:7)]
nuc10 <- nuc10[, -c(6:11)]
hyd10 <- subset(plantData10, plantData10$Hydro > 0)
hyd10 <- hyd10[, -c(5:8)]
hyd10 <- hyd10[, -c(6:10)]
bio10 <- subset(plantData10, plantData10$Bio > 0)
bio10 <- bio10[, -c(5:9)]
bio10 <- bio10[, -c(6:9)]
wind10 <- subset(plantData10, plantData10$Wind > 0)
wind10 <- wind10[, -c(5:10)]
wind10 <- wind10[, -c(6:8)]
sol10 <- subset(plantData10, plantData10$Solar > 0)
sol10 <- sol10[, -c(5:11)]
sol10 <- sol10[, -c(6:7)]
geo10 <- subset(plantData10, plantData10$Geo > 0)
geo10 <- geo10[, -c(5:12)]
geo10 <- geo10[, -6]
oth10 <- subset(plantData10, plantData10$Other > 0)
oth10 <- oth10[, -c(5:13)]




# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Will Davidson CS 424 Project 2",
    
    tabPanel(title = "Illinois 2018",
               fluidRow(
                   column(4, 
                      box(title = "Illinois", solidHeader = TRUE, status = "primary", width = 8,
                          leafletOutput("illinois")
                      )
                          ), 
                   column(2, 
                          box(title = "Legend", solidHeader = TRUE, status = "primary", width = 8,
                              span(textOutput("coal"), style="color:#CB1B45"), 
                              span(textOutput("oil"), style="color:#A5A051"),
                              span(textOutput("gas"), style="color:#F05E1C"),
                              span(textOutput("nuclear"), style="color:#FFB11B"),
                              span(textOutput("hydro"), style="color:#66BAB7"),
                              span(textOutput("bio"), style="color:#1E88A8"),
                              span(textOutput("wind"), style="color:#4A225D"),
                              span(textOutput("solar"), style="color:#434343"),
                              span(textOutput("geo"), style="color:#E03C8A"),
                              span(textOutput("other"), style="color:#78552B")
                              )), 
                   
                   column(2, checkboxGroupInput("checkIll", label = "Types:", choices = plantList, selected = plantList)), 
                   fluidRow(actionButton("resetIll", label = "Reset"))
                   
               ), 
    ), 
    
    navbarMenu(title = "State Comparison", 
               tabPanel(title = "Individual", 
                        column(4, fluidRow(
                          selectInput("state1", "Year", choices = c(2000, 2010, 2018), selected = 2018)
                        ), 
                        fluidRow(
                          selectInput("stateName1", "State", choices = state.name, selected = "Illinois")
                        ),
                              fluidRow(box(title = "Map 1", solidHeader = TRUE, status = "primary", width = 8,
                                           leafletOutput("map1")
                                           ))), 
                        column(2, checkboxGroupInput("checkMap1", label = " Map 1 Types:", choices = plantList, selected = plantList), 
                               fluidRow(actionButton("reset1", label = "Reset Map 1"))),
                        
                        
                        column(4, fluidRow(
                          selectInput("state2", "Year", choices = c(2000, 2010, 2018), selected = 2000)
                        ), 
                        fluidRow(
                          selectInput("stateName2", "State", choices = state.name, selected = "Illinois")
                        ),
                        fluidRow( box(title = "Map 2", solidHeader = TRUE, status = "primary", width = 8,
                                      leafletOutput("map2")))), 
                        
                        column(2, checkboxGroupInput("checkMap2", label = " Map 2 Types:", choices = plantList, selected = plantList), 
                               fluidRow(actionButton("reset2", label = "Reset Map 2")))
                        ), 
               
               
               tabPanel(title = "Linked", 
                        column(4, fluidRow(
                          selectInput("state3", "Year", choices = c(2000, 2010, 2018), selected = 2018)
                        ), 
                        fluidRow(
                          selectInput("stateName3", "State", choices = state.name, selected = "Illinois")
                        ),
                        fluidRow(box(title = "Map 1", solidHeader = TRUE, status = "primary", width = 8,
                                     leafletOutput("map3")
                        )), 
                        fluidRow(actionButton("reset3", label = "Reset Map 1"))), 
                        column(2, checkboxGroupInput("checkMap", label = " Map Types:", choices = plantList, selected = plantList)),
                        
                        
                        column(4, selectInput("state4", "Year", choices = c(2000, 2010, 2018), selected = 2000), 
                               fluidRow(
                                 selectInput("stateName4", "State", choices = state.name, selected = "Illinois")
                               ),
                               fluidRow(box(title = "Map 2", solidHeader = TRUE, status = "primary", width = 8,
                                            leafletOutput("map4"))), 
                               fluidRow(actionButton("reset4", label = "Reset Map 2"))
                        ), 
                        )
               ), 
    
    tabPanel(title = "US Map", 
             column(8, fluidRow(
               selectInput("year", "Year", choices = c(2000, 2010, 2018), selected = 2018)
             ), 
             fluidRow(
               selectInput("state", "State", choices = c(state.name, "US Map"), selected = "US Map")
             ),    
             fluidRow(
               column(2, sliderInput("max", label = "Max MWh", min = 0, max = 31199935, value = 31199935)), 
               
               column(2, sliderInput("min", label = "Min MWh", min = 0, max = 31199935, value = 0))
             ),
                    
                  fluidRow(box(title = "Map", solidHeader = TRUE, status = "primary", width = 8,
                               leafletOutput("map"))
             )), 
             column(2, checkboxGroupInput("checkMap0", label = " Map Types:", choices = plantList), 
                    fluidRow(actionButton("resetMap", label = "Reset Map")))
             
             ), 
    
    tabPanel(title = "About", 
             p("This data presented in the app comes from https://www.epa.gov/egrid/download-data "),
             p("This data covers the different electric power plants and the energy sources for those plants. The Data comes from the years 2000, 2010, and 2018"),
             p("This app was written by Will Davidson on 3/13/2021.")
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  # --------------------------------------------------------------------------- Illinois Map -----------------------------------------------------------------------------------------------   
  
  # Use for the legend
  output$coal <- renderText({"Coal Color"})
  
  output$oil <- renderText({"Oil Color"})
  
  output$gas <- renderText({"Gas Color"})
  
  output$nuclear <- renderText({"Nuclear Color"})
  
  output$hydro <- renderText({"Hydro Color"})
  
  output$bio <- renderText({"Bio Color"})
  
  output$wind <- renderText({"Wind Color"})
  
  output$solar <- renderText({"Solar Color"})
  
  output$geo <- renderText({"Geo Color"})

  output$other <- renderText({"Other Color"})  
  
  # The Illinois map
   output$illinois <- renderLeaflet({
      
     illMap <- leaflet(data = illinois) %>% 
       addTiles()
     
    if (input$resetIll || !input$resetIll) {
      
     if(nrow(illCoal) > 0 && ("Coal" %in% input$checkIll || "All" %in% input$checkIll)){
       illMap <- illMap %>% addCircleMarkers(lng = illCoal$Plant.longitude, lat = illCoal$Plant.latitude, label = illCoal$Plant.name, radius = 2, color = "#CB1B45") 
     }
     if(nrow(illOil) > 0 && ("Oil" %in% input$checkIll || "All" %in% input$checkIll)) {
       illMap <- illMap %>% addCircleMarkers(lng = illOil$Plant.longitude, lat = illOil$Plant.latitude, label = illOil$Plant.name, radius = 2, color = "#A5A051") 
     }
     if(nrow(illGas) > 0 && ("Gas" %in% input$checkIll || "All" %in% input$checkIll)) {
       illMap <- illMap %>% addCircleMarkers(lng = illGas$Plant.longitude, lat = illGas$Plant.latitude, label = illGas$Plant.name, radius = 2, color = "#F05E1C") 
     }
     if(nrow(illNuc) > 0 && ("Nuclear" %in% input$checkIll || "All" %in% input$checkIll)) {   
       illMap <- illMap %>% addCircleMarkers(lng = illNuc$Plant.longitude, lat = illNuc$Plant.latitude, label = illNuc$Plant.name, radius = 2, color = "#FFB11B")
     }
     if(nrow(illHyd) > 0 && ("Hydro" %in% input$checkIll || "All" %in% input$checkIll)) {    
       illMap <- illMap %>% addCircleMarkers(lng = illHyd$Plant.longitude, lat = illHyd$Plant.latitude, label = illHyd$Plant.name, radius = 2, color = "#66BAB7") 
     }
     if(nrow(illBio) > 0 && ("Bio" %in% input$checkIll || "All" %in% input$checkIll)){
       illMap <- illMap %>% addCircleMarkers(lng = illBio$Plant.longitude, lat = illBio$Plant.latitude, label = illBio$Plant.name, radius = 2, color = "#1E88A8") 
     }
     if(nrow(illWind) > 0 && ("Wind" %in% input$checkIll || "All" %in% input$checkIll)) {
       illMap <- illMap %>% addCircleMarkers(lng = illWind$Plant.longitude, lat = illWind$Plant.latitude, label = illWind$Plant.name, radius = 2, color = "#4A225D")
     }
     if(nrow(illSol) > 0 && ("Solar" %in% input$checkIll || "All" %in% input$checkIll)) {
       illMap <- illMap %>% addCircleMarkers(lng = illSol$Plant.longitude, lat = illSol$Plant.latitude, label = illSol$Plant.name, radius = 2, color = "#434343") 
     }
     if(nrow(illGeo) > 0) {
       illMap <- illMap %>% addCircleMarkers(lng = illGeo$Plant.longitude, lat = illGeo$Plant.latitude, label = illGeo$Plant.name, radius = 2, color = "#E03C8A") 
     }
     if(nrow(illOth) > 0 && ("Other" %in% input$checkIll || "All" %in% input$checkIll)) {
       illMap <- illMap %>% addCircleMarkers(lng = illOth$Plant.longitude, lat = illOth$Plant.latitude, label = illOth$Plant.name, radius = 2, color = "#78552B")
     }
     illMap}
   })
   
  
   # --------------------------------------------------------------------------- Non linked Map 1 -----------------------------------------------------------------------------------------------   
   
   
   #Map 1
   output$map1 <- renderLeaflet({
     
     # Use to filter for each state
     stateName <- input$stateName1
     
     
     coal18 <- subset(coal18, coal18$Plant.state.abbreviation == stateName)
     oil18 <- subset(oil18, oil18$Plant.state.abbreviation == stateName)
     gas18 <- subset(gas18, gas18$Plant.state.abbreviation == stateName)
     nuc18 <- subset(nuc18, nuc18$Plant.state.abbreviation == stateName)
     hyd18 <- subset(hyd18, hyd18$Plant.state.abbreviation == stateName)
     bio18 <- subset(bio18, bio18$Plant.state.abbreviation == stateName)
     wind18 <- subset(wind18, wind18$Plant.state.abbreviation == stateName)
     sol18 <- subset(sol18, sol18$Plant.state.abbreviation == stateName)
     geo18 <- subset(geo18, geo18$Plant.state.abbreviation == stateName)
     oth18 <- subset(oth18, oth18$Plant.state.abbreviation == stateName)
     
     coal10 <- subset(coal10, coal10$Plant.state.abbreviation == stateName)
     oil10 <- subset(oil10, oil10$Plant.state.abbreviation == stateName)
     gas10 <- subset(gas10, gas10$Plant.state.abbreviation == stateName)
     nuc10 <- subset(nuc10, nuc10$Plant.state.abbreviation == stateName)
     hyd10 <- subset(hyd10, hyd10$Plant.state.abbreviation == stateName)
     bio10 <- subset(bio10, bio10$Plant.state.abbreviation == stateName)
     wind10 <- subset(wind10, wind10$Plant.state.abbreviation == stateName)
     sol10 <- subset(sol10, sol10$Plant.state.abbreviation == stateName)
     geo10 <- subset(geo10, geo10$Plant.state.abbreviation == stateName)
     oth10 <- subset(oth10, oth10$Plant.state.abbreviation == stateName)
     
     coal0 <- subset(coal0, coal0$Plant.state.abbreviation == stateName)
     oil0 <- subset(oil0, oil0$Plant.state.abbreviation == stateName)
     gas0 <- subset(gas0, gas0$Plant.state.abbreviation == stateName)
     nuc0 <- subset(nuc0, nuc0$Plant.state.abbreviation == stateName)
     hyd0 <- subset(hyd0, hyd0$Plant.state.abbreviation == stateName)
     bio0 <- subset(bio0, bio0$Plant.state.abbreviation == stateName)
     wind0 <- subset(wind0, wind0$Plant.state.abbreviation == stateName)
     sol0 <- subset(sol0, sol0$Plant.state.abbreviation == stateName)
     geo0 <- subset(geo0, geo0$Plant.state.abbreviation == stateName)
     oth0 <- subset(oth0, oth0$Plant.state.abbreviation == stateName)
     
     # 2018 Data
     if (input$state1 == 2018) {
       
       if (input$reset1 || ! input$reset1) {
       map1 <- leaflet(data = (plantData$Plant.state.abbreviation == input$stateName1)) %>% 
         addTiles()
       
       if(nrow(coal18) > 0 && ("Coal" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = coal18$Plant.longitude, lat = coal18$Plant.latitude, label = coal18$Plant.name, radius = (coal18$Coal / 1000000), color = "#CB1B45")
         
       }
       
       if(nrow(oil18) > 0 && ("Oil" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = oil18$Plant.longitude, lat = oil18$Plant.latitude, label = oil18$Plant.name, radius = (oil18$Oil / 1000000), color = "#A5A051")
       }
       
       if(nrow(gas18) > 0 && ("Gas" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = gas18$Plant.longitude, lat = gas18$Plant.latitude, label = gas18$Plant.name, radius = (gas18$Gas / 1000000), color = "#F05E1C")
       }
       
       if(nrow(nuc18) > 0 && ("Nuclear" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = nuc18$Plant.longitude, lat = nuc18$Plant.latitude, label = nuc18$Plant.name, radius = (nuc18$Nuclear / 1000000), color = "#FFB11B")
       
       }
       
       if(nrow(hyd18) > 0 && ("Hydro" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = hyd18$Plant.longitude, lat = hyd18$Plant.latitude, label = hyd18$Plant.name, radius = (hyd18$Hydro / 1000000), color = "#66BAB7")
       }
       
       if(nrow(bio18) > 0 && ("Bio" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = bio18$Plant.longitude, lat = bio18$Plant.latitude, label = bio18$Plant.name, radius = (bio18$Bio / 1000000), color = "#1E88A8")
       }
       
       if(nrow(wind18) > 0 && ("Wind" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = wind18$Plant.longitude, lat = wind18$Plant.latitude, label = wind18$Plant.name, radius = (wind18$Wind / 1000000), color = "#4A225D")
       }
       
       if(nrow(sol18) > 0 && ("Solar" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = sol18$Plant.longitude, lat = sol18$Plant.latitude, label = sol18$Plant.name, radius = (sol18$Solar / 1000000), color = "#434343")
       }
       if(nrow(geo18) > 0 && ("Geo" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = geo18$Plant.longitude, lat = geo18$Plant.latitude, label = geo18$Plant.name, radius = (geo18$Geo / 1000000), color = "#E03C8A")
       }
       if(nrow(oth18) > 0 && ("Other" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = oth18$Plant.longitude, lat = oth18$Plant.latitude, label = oth18$Plant.name, radius = (oth18$Other / 1000000), color = "#78552B")
       }
       }
     }
     
     # 2010 Data
     else if (input$state1 == 2010) {
       
       if (input$reset1 || ! input$reset1) {
       map1 <- leaflet(data = plantData10 ) %>% 
         addTiles()
       
       if(nrow(coal10) > 0 && ("Coal" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = coal10$Plant.longitude, lat = coal10$Plant.latitude, label = coal10$Plant.name, radius = (coal10$Coal / 1000000), color = "#CB1B45")
       }
       
       if(nrow(oil10) > 0 && ("Oil" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = oil10$Plant.longitude, lat = oil10$Plant.latitude, label = oil10$Plant.name, radius = (oil10$Oil / 1000000), color = "#A5A051")
       }
       
       if(nrow(gas10) > 0 && ("Gas" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = gas10$Plant.longitude, lat = gas10$Plant.latitude, label = gas10$Plant.name, radius = (gas10$Gas / 1000000), color = "#F05E1C")
       }
       
       if(nrow(nuc10) > 0 && ("Nuclear" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = nuc10$Plant.longitude, lat = nuc10$Plant.latitude, label = nuc10$Plant.name, radius = (nuc10$Nuclear / 1000000), color = "#FFB11B")
       }
       
       if(nrow(hyd10) > 0 && ("Hydro" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = hyd10$Plant.longitude, lat = hyd10$Plant.latitude, label = hyd10$Plant.name, radius = (hyd10$Hydro / 1000000), color = "#66BAB7")
       }
       
       if(nrow(bio10) > 0 && ("Bio" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = bio10$Plant.longitude, lat = bio10$Plant.latitude, label = bio10$Plant.name, radius = (bio10$Bio / 1000000), color = "#1E88A8")
       }
       
       if(nrow(wind10) > 0 && ("Wind" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = wind10$Plant.longitude, lat = wind10$Plant.latitude, label = wind10$Plant.name, radius = (wind10$Wind / 1000000), color = "#4A225D")
       }
       
       if(nrow(sol10) > 0 && ("Solar" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = sol10$Plant.longitude, lat = sol10$Plant.latitude, label = sol10$Plant.name, radius = (sol10$Solar / 1000000), color = "#434343")
       }
       
       if(nrow(geo10) > 0 && ("Geo" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = geo10$Plant.longitude, lat = geo10$Plant.latitude, label = geo10$Plant.name, radius = (geo10$Geo / 1000000), color = "#E03C8A")
       }
       
       if(nrow(oth10) > 0 && ("Other" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = oth10$Plant.longitude, lat = oth10$Plant.latitude, label = oth10$Plant.name, radius = (oth10$Other / 1000000), color = "#78552B")
       }
       }
      }
     
     # 2000 Data
     else if (input$state1 == 2000) {
       
       if (input$reset1 || ! input$reset1) {
       map1 <- leaflet(data = plantData2000 ) %>% 
         addTiles()
       
       if(nrow(coal0) > 0 && ("Coal" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = coal0$Plant.longitude, lat = coal0$Plant.latitude, label = coal0$Plant.name, radius = (coal0$Coal / 1000000), color = "#CB1B45")
       }
       
       if(nrow(oil0) > 0 && ("Oil" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = oil0$Plant.longitude, lat = oil0$Plant.latitude, label = oil0$Plant.name, radius = (oil0$Oil / 1000000), color = "#A5A051")
       }
       
       if(nrow(gas0) > 0 && ("Gas" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = gas0$Plant.longitude, lat = gas0$Plant.latitude, label = gas0$Plant.name, radius = (gas0$Gas / 1000000), color = "#F05E1C")
       }
       
       if(nrow(nuc0) > 0 && ("Nuclear" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = nuc0$Plant.longitude, lat = nuc0$Plant.latitude, label = nuc0$Plant.name, radius = (nuc0$Nuclear / 1000000), color = "#FFB11B")
       }
       
       if(nrow(hyd0) > 0 && ("Hydro" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = hyd0$Plant.longitude, lat = hyd0$Plant.latitude, label = hyd0$Plant.name, radius = (hyd0$Hydro / 1000000), color = "#66BAB7")
       }
       
       if(nrow(bio0) > 0 && ("Bio" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = bio0$Plant.longitude, lat = bio0$Plant.latitude, label = bio0$Plant.name, radius = (bio0$Bio / 1000000), color = "#1E88A8")
       }
       
       if(nrow(wind0) > 0 && ("Wind" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = wind0$Plant.longitude, lat = wind0$Plant.latitude, label = wind0$Plant.name, radius = (wind0$Wind / 1000000), color = "#4A225D")
       }
       
       if(nrow(sol0) > 0 && ("Solar" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = sol0$Plant.longitude, lat = sol0$Plant.latitude, label = sol0$Plant.name, radius = (sol0$Solar / 1000000), color = "#434343")
       }
       
       if(nrow(geo0) > 0 && ("Geo" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = geo0$Plant.longitude, lat = geo0$Plant.latitude, label = geo0$Plant.name, radius = (geo0$Geo / 1000000), color = "#E03C8A")
       }
       
       if(nrow(oth0) > 0 && ("Other" %in% input$checkMap1 || "All" %in% input$checkMap1)) {
         map1 <- map1 %>% addCircleMarkers(lng = oth0$Plant.longitude, lat = oth0$Plant.latitude, label = oth0$Plant.name, radius = (oth0$Other / 1000000), color = "#78552B")
       }     
       }
    }
     
     
   })
   # --------------------------------------------------------------------------- Non linked Map 2 -----------------------------------------------------------------------------------------------   
   # Map 2
   
   output$map2 <- renderLeaflet({
     
     # Use to filter for states
     
     stateName <- input$stateName2
     
     
     coal18 <- subset(coal18, coal18$Plant.state.abbreviation == stateName)
     oil18 <- subset(oil18, oil18$Plant.state.abbreviation == stateName)
     gas18 <- subset(gas18, gas18$Plant.state.abbreviation == stateName)
     nuc18 <- subset(nuc18, nuc18$Plant.state.abbreviation == stateName)
     hyd18 <- subset(hyd18, hyd18$Plant.state.abbreviation == stateName)
     bio18 <- subset(bio18, bio18$Plant.state.abbreviation == stateName)
     wind18 <- subset(wind18, wind18$Plant.state.abbreviation == stateName)
     sol18 <- subset(sol18, sol18$Plant.state.abbreviation == stateName)
     geo18 <- subset(geo18, geo18$Plant.state.abbreviation == stateName)
     oth18 <- subset(oth18, oth18$Plant.state.abbreviation == stateName)
     
     coal10 <- subset(coal10, coal10$Plant.state.abbreviation == stateName)
     oil10 <- subset(oil10, oil10$Plant.state.abbreviation == stateName)
     gas10 <- subset(gas10, gas10$Plant.state.abbreviation == stateName)
     nuc10 <- subset(nuc10, nuc10$Plant.state.abbreviation == stateName)
     hyd10 <- subset(hyd10, hyd10$Plant.state.abbreviation == stateName)
     bio10 <- subset(bio10, bio10$Plant.state.abbreviation == stateName)
     wind10 <- subset(wind10, wind10$Plant.state.abbreviation == stateName)
     sol10 <- subset(sol10, sol10$Plant.state.abbreviation == stateName)
     geo10 <- subset(geo10, geo10$Plant.state.abbreviation == stateName)
     oth10 <- subset(oth10, oth10$Plant.state.abbreviation == stateName)
     
     coal0 <- subset(coal0, coal0$Plant.state.abbreviation == stateName)
     oil0 <- subset(oil0, oil0$Plant.state.abbreviation == stateName)
     gas0 <- subset(gas0, gas0$Plant.state.abbreviation == stateName)
     nuc0 <- subset(nuc0, nuc0$Plant.state.abbreviation == stateName)
     hyd0 <- subset(hyd0, hyd0$Plant.state.abbreviation == stateName)
     bio0 <- subset(bio0, bio0$Plant.state.abbreviation == stateName)
     wind0 <- subset(wind0, wind0$Plant.state.abbreviation == stateName)
     sol0 <- subset(sol0, sol0$Plant.state.abbreviation == stateName)
     geo0 <- subset(geo0, geo0$Plant.state.abbreviation == stateName)
     oth0 <- subset(oth0, oth0$Plant.state.abbreviation == stateName)
     
     # 2018 Data
     if (input$state2 == 2018) {
       
       if (input$reset2 || ! input$reset2) {
       map2 <- leaflet(data = (plantData$Plant.state.abbreviation == input$stateName2)) %>% 
         addTiles()
       
       if(nrow(coal18) > 0 && ("Coal" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = coal18$Plant.longitude, lat = coal18$Plant.latitude, label = coal18$Plant.name, radius = (coal18$Coal / 1000000), color = "#CB1B45")
         
       }
       
       if(nrow(oil18) > 0 && ("Oil" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = oil18$Plant.longitude, lat = oil18$Plant.latitude, label = oil18$Plant.name, radius = (oil18$Oil / 1000000), color = "#A5A051")
       }
       
       if(nrow(gas18) > 0 && ("Gas" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = gas18$Plant.longitude, lat = gas18$Plant.latitude, label = gas18$Plant.name, radius = (gas18$Gas / 1000000), color = "#F05E1C")
       }
       
       if(nrow(nuc18) > 0 && ("Nuclear" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = nuc18$Plant.longitude, lat = nuc18$Plant.latitude, label = nuc18$Plant.name, radius = (nuc18$Nuclear / 1000000), color = "#FFB11B")
         
       }
       
       if(nrow(hyd18) > 0 && ("Hydro" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = hyd18$Plant.longitude, lat = hyd18$Plant.latitude, label = hyd18$Plant.name, radius = (hyd18$Hydro / 1000000), color = "#66BAB7")
       }
       
       if(nrow(bio18) > 0 && ("Bio" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = bio18$Plant.longitude, lat = bio18$Plant.latitude, label = bio18$Plant.name, radius = (bio18$Bio / 1000000), color = "#1E88A8")
       }
       
       if(nrow(wind18) > 0 && ("Wind" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = wind18$Plant.longitude, lat = wind18$Plant.latitude, label = wind18$Plant.name, radius = (wind18$Wind / 1000000), color = "#4A225D")
       }
       
       if(nrow(sol18) > 0 && ("Solar" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = sol18$Plant.longitude, lat = sol18$Plant.latitude, label = sol18$Plant.name, radius = (sol18$Solar / 1000000), color = "#434343")
       }
       if(nrow(geo18) > 0 && ("Geo" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = geo18$Plant.longitude, lat = geo18$Plant.latitude, label = geo18$Plant.name, radius = (geo18$Geo / 1000000), color = "#E03C8A")
       }
       if(nrow(oth18) > 0 && ("Other" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = oth18$Plant.longitude, lat = oth18$Plant.latitude, label = oth18$Plant.name, radius = (oth18$Other / 1000000), color = "#78552B")
       }
       }
     }
     
     # 2010 Data
     else if (input$state2 == 2010) {
       
       if (input$reset2 || ! input$reset2) {
       map2 <- leaflet(data = plantData10 ) %>% 
         addTiles()
       
       if(nrow(coal10) > 0 && ("Coal" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = coal10$Plant.longitude, lat = coal10$Plant.latitude, label = coal10$Plant.name, radius = (coal10$Coal / 1000000), color = "#CB1B45")
       }
       
       if(nrow(oil10) > 0 && ("Oil" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = oil10$Plant.longitude, lat = oil10$Plant.latitude, label = oil10$Plant.name, radius = (oil10$Oil / 1000000), color = "#A5A051")
       }
       
       if(nrow(gas10) > 0 && ("Gas" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = gas10$Plant.longitude, lat = gas10$Plant.latitude, label = gas10$Plant.name, radius = (gas10$Gas / 1000000), color = "#F05E1C")
       }
       
       if(nrow(nuc10) > 0 && ("Nuclear" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = nuc10$Plant.longitude, lat = nuc10$Plant.latitude, label = nuc10$Plant.name, radius = (nuc10$Nuclear / 1000000), color = "#FFB11B")
       }
       
       if(nrow(hyd10) > 0 && ("Hydro" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = hyd10$Plant.longitude, lat = hyd10$Plant.latitude, label = hyd10$Plant.name, radius = (hyd10$Hydro / 1000000), color = "#66BAB7")
       }
       
       if(nrow(bio10) > 0 && ("Bio" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = bio10$Plant.longitude, lat = bio10$Plant.latitude, label = bio10$Plant.name, radius = (bio10$Bio / 1000000), color = "#1E88A8")
       }
       
       if(nrow(wind10) > 0 && ("Wind" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = wind10$Plant.longitude, lat = wind10$Plant.latitude, label = wind10$Plant.name, radius = (wind10$Wind / 1000000), color = "#4A225D")
       }
       
       if(nrow(sol10) > 0 && ("Solar" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = sol10$Plant.longitude, lat = sol10$Plant.latitude, label = sol10$Plant.name, radius = (sol10$Solar / 1000000), color = "#434343")
       }
       
       if(nrow(geo10) > 0 && ("Geo" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = geo10$Plant.longitude, lat = geo10$Plant.latitude, label = geo10$Plant.name, radius = (geo10$Geo / 1000000), color = "#E03C8A")
       }
       
       if(nrow(oth10) > 0 && ("Other" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = oth10$Plant.longitude, lat = oth10$Plant.latitude, label = oth10$Plant.name, radius = (oth10$Other / 1000000), color = "#78552B")
       }
       }
     }
     
     # 2000 Data
     else if (input$state2 == 2000) {
       
       if (input$reset2 || ! input$reset2) {
       map2 <- leaflet(data = plantData2000 ) %>% 
         addTiles()
       
       if(nrow(coal0) > 0 && ("Coal" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = coal0$Plant.longitude, lat = coal0$Plant.latitude, label = coal0$Plant.name, radius = (coal0$Coal / 1000000), color = "#CB1B45")
       }
       
       if(nrow(oil0) > 0 && ("Oil" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = oil0$Plant.longitude, lat = oil0$Plant.latitude, label = oil0$Plant.name, radius = (oil0$Oil / 1000000), color = "#A5A051")
       }
       
       if(nrow(gas0) > 0 && ("Gas" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = gas0$Plant.longitude, lat = gas0$Plant.latitude, label = gas0$Plant.name, radius = (gas0$Gas / 1000000), color = "#F05E1C")
       }
       
       if(nrow(nuc0) > 0 && ("Nuclear" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = nuc0$Plant.longitude, lat = nuc0$Plant.latitude, label = nuc0$Plant.name, radius = (nuc0$Nuclear / 1000000), color = "#FFB11B")
       }
       
       if(nrow(hyd0) > 0 && ("Hydro" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = hyd0$Plant.longitude, lat = hyd0$Plant.latitude, label = hyd0$Plant.name, radius = (hyd0$Hydro / 1000000), color = "#66BAB7")
       }
       
       if(nrow(bio0) > 0 && ("Bio" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = bio0$Plant.longitude, lat = bio0$Plant.latitude, label = bio0$Plant.name, radius = (bio0$Bio / 1000000), color = "#1E88A8")
       }
       
       if(nrow(wind0) > 0 && ("Wind" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = wind0$Plant.longitude, lat = wind0$Plant.latitude, label = wind0$Plant.name, radius = (wind0$Wind / 1000000), color = "#4A225D")
       }
       
       if(nrow(sol0) > 0 && ("Solar" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = sol0$Plant.longitude, lat = sol0$Plant.latitude, label = sol0$Plant.name, radius = (sol0$Solar / 1000000), color = "#434343")
       }
       
       if(nrow(geo0) > 0 && ("Geo" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = geo0$Plant.longitude, lat = geo0$Plant.latitude, label = geo0$Plant.name, radius = (geo0$Geo / 1000000), color = "#E03C8A")
       }
       
       if(nrow(oth0) > 0 && ("Other" %in% input$checkMap2 || "All" %in% input$checkMap2)) {
         map2 <- map2 %>% addCircleMarkers(lng = oth0$Plant.longitude, lat = oth0$Plant.latitude, label = oth0$Plant.name, radius = (oth0$Other / 1000000), color = "#78552B")
       }     
       }
     }
     
     
   })

   
   # --------------------------------------------------------------------------- Linked Map 1 -----------------------------------------------------------------------------------------------   
   
   
   # Map 1
   output$map3 <- renderLeaflet({
     
     #Use to filter states
     stateName <- input$stateName3
     
     
     coal18 <- subset(coal18, coal18$Plant.state.abbreviation == stateName)
     oil18 <- subset(oil18, oil18$Plant.state.abbreviation == stateName)
     gas18 <- subset(gas18, gas18$Plant.state.abbreviation == stateName)
     nuc18 <- subset(nuc18, nuc18$Plant.state.abbreviation == stateName)
     hyd18 <- subset(hyd18, hyd18$Plant.state.abbreviation == stateName)
     bio18 <- subset(bio18, bio18$Plant.state.abbreviation == stateName)
     wind18 <- subset(wind18, wind18$Plant.state.abbreviation == stateName)
     sol18 <- subset(sol18, sol18$Plant.state.abbreviation == stateName)
     geo18 <- subset(geo18, geo18$Plant.state.abbreviation == stateName)
     oth18 <- subset(oth18, oth18$Plant.state.abbreviation == stateName)
     
     coal10 <- subset(coal10, coal10$Plant.state.abbreviation == stateName)
     oil10 <- subset(oil10, oil10$Plant.state.abbreviation == stateName)
     gas10 <- subset(gas10, gas10$Plant.state.abbreviation == stateName)
     nuc10 <- subset(nuc10, nuc10$Plant.state.abbreviation == stateName)
     hyd10 <- subset(hyd10, hyd10$Plant.state.abbreviation == stateName)
     bio10 <- subset(bio10, bio10$Plant.state.abbreviation == stateName)
     wind10 <- subset(wind10, wind10$Plant.state.abbreviation == stateName)
     sol10 <- subset(sol10, sol10$Plant.state.abbreviation == stateName)
     geo10 <- subset(geo10, geo10$Plant.state.abbreviation == stateName)
     oth10 <- subset(oth10, oth10$Plant.state.abbreviation == stateName)
     
     coal0 <- subset(coal0, coal0$Plant.state.abbreviation == stateName)
     oil0 <- subset(oil0, oil0$Plant.state.abbreviation == stateName)
     gas0 <- subset(gas0, gas0$Plant.state.abbreviation == stateName)
     nuc0 <- subset(nuc0, nuc0$Plant.state.abbreviation == stateName)
     hyd0 <- subset(hyd0, hyd0$Plant.state.abbreviation == stateName)
     bio0 <- subset(bio0, bio0$Plant.state.abbreviation == stateName)
     wind0 <- subset(wind0, wind0$Plant.state.abbreviation == stateName)
     sol0 <- subset(sol0, sol0$Plant.state.abbreviation == stateName)
     geo0 <- subset(geo0, geo0$Plant.state.abbreviation == stateName)
     oth0 <- subset(oth0, oth0$Plant.state.abbreviation == stateName)
     
     # 2018 Data
     if (input$state3 == 2018) {
       
       if (input$reset3 || ! input$reset3) {
         map3 <- leaflet(data = (plantData$Plant.state.abbreviation == input$stateName3)) %>% 
           addTiles()
         
         if(nrow(coal18) > 0 && ("Coal" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = coal18$Plant.longitude, lat = coal18$Plant.latitude, label = coal18$Plant.name, radius = (coal18$Coal / 1000000), color = "#CB1B45")
           
         }
         
         if(nrow(oil18) > 0 && ("Oil" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = oil18$Plant.longitude, lat = oil18$Plant.latitude, label = oil18$Plant.name, radius = (oil18$Oil / 1000000), color = "#A5A051")
         }
         
         if(nrow(gas18) > 0 && ("Gas" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = gas18$Plant.longitude, lat = gas18$Plant.latitude, label = gas18$Plant.name, radius = (gas18$Gas / 1000000), color = "#F05E1C")
         }
         
         if(nrow(nuc18) > 0 && ("Nuclear" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = nuc18$Plant.longitude, lat = nuc18$Plant.latitude, label = nuc18$Plant.name, radius = (nuc18$Nuclear / 1000000), color = "#FFB11B")
           
         }
         
         if(nrow(hyd18) > 0 && ("Hydro" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = hyd18$Plant.longitude, lat = hyd18$Plant.latitude, label = hyd18$Plant.name, radius = (hyd18$Hydro / 1000000), color = "#66BAB7")
         }
         
         if(nrow(bio18) > 0 && ("Bio" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = bio18$Plant.longitude, lat = bio18$Plant.latitude, label = bio18$Plant.name, radius = (bio18$Bio / 1000000), color = "#1E88A8")
         }
         
         if(nrow(wind18) > 0 && ("Wind" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = wind18$Plant.longitude, lat = wind18$Plant.latitude, label = wind18$Plant.name, radius = (wind18$Wind / 1000000), color = "#4A225D")
         }
         
         if(nrow(sol18) > 0 && ("Solar" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = sol18$Plant.longitude, lat = sol18$Plant.latitude, label = sol18$Plant.name, radius = (sol18$Solar / 1000000), color = "#434343")
         }
         if(nrow(geo18) > 0 && ("Geo" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = geo18$Plant.longitude, lat = geo18$Plant.latitude, label = geo18$Plant.name, radius = (geo18$Geo / 1000000), color = "#E03C8A")
         }
         if(nrow(oth18) > 0 && ("Other" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = oth18$Plant.longitude, lat = oth18$Plant.latitude, label = oth18$Plant.name, radius = (oth18$Other / 1000000), color = "#78552B")
         }
       }
     }
     
     # 2010 Data
     else if (input$state3 == 2010) {
       
       if (input$reset3 || ! input$reset3) {
         map3 <- leaflet(data = plantData10 ) %>% 
           addTiles()
         
         if(nrow(coal10) > 0 && ("Coal" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = coal10$Plant.longitude, lat = coal10$Plant.latitude, label = coal10$Plant.name, radius = (coal10$Coal / 1000000), color = "#CB1B45")
         }
         
         if(nrow(oil10) > 0 && ("Oil" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = oil10$Plant.longitude, lat = oil10$Plant.latitude, label = oil10$Plant.name, radius = (oil10$Oil / 1000000), color = "#A5A051")
         }
         
         if(nrow(gas10) > 0 && ("Gas" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = gas10$Plant.longitude, lat = gas10$Plant.latitude, label = gas10$Plant.name, radius = (gas10$Gas / 1000000), color = "#F05E1C")
         }
         
         if(nrow(nuc10) > 0 && ("Nuclear" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = nuc10$Plant.longitude, lat = nuc10$Plant.latitude, label = nuc10$Plant.name, radius = (nuc10$Nuclear / 1000000), color = "#FFB11B")
         }
         
         if(nrow(hyd10) > 0 && ("Hydro" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = hyd10$Plant.longitude, lat = hyd10$Plant.latitude, label = hyd10$Plant.name, radius = (hyd10$Hydro / 1000000), color = "#66BAB7")
         }
         
         if(nrow(bio10) > 0 && ("Bio" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = bio10$Plant.longitude, lat = bio10$Plant.latitude, label = bio10$Plant.name, radius = (bio10$Bio / 1000000), color = "#1E88A8")
         }
         
         if(nrow(wind10) > 0 && ("Wind" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = wind10$Plant.longitude, lat = wind10$Plant.latitude, label = wind10$Plant.name, radius = (wind10$Wind / 1000000), color = "#4A225D")
         }
         
         if(nrow(sol10) > 0 && ("Solar" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = sol10$Plant.longitude, lat = sol10$Plant.latitude, label = sol10$Plant.name, radius = (sol10$Solar / 1000000), color = "#434343")
         }
         
         if(nrow(geo10) > 0 && ("Geo" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = geo10$Plant.longitude, lat = geo10$Plant.latitude, label = geo10$Plant.name, radius = (geo10$Geo / 1000000), color = "#E03C8A")
         }
         
         if(nrow(oth10) > 0 && ("Other" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = oth10$Plant.longitude, lat = oth10$Plant.latitude, label = oth10$Plant.name, radius = (oth10$Other / 1000000), color = "#78552B")
         }
       }
     }
     
     # 2000 Data
     else if (input$state3 == 2000) {
       
       if (input$reset3 || ! input$reset3) {
         map3 <- leaflet(data = plantData2000 ) %>% 
           addTiles()
         
         if(nrow(coal0) > 0 && ("Coal" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = coal0$Plant.longitude, lat = coal0$Plant.latitude, label = coal0$Plant.name, radius = (coal0$Coal / 1000000), color = "#CB1B45")
         }
         
         if(nrow(oil0) > 0 && ("Oil" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = oil0$Plant.longitude, lat = oil0$Plant.latitude, label = oil0$Plant.name, radius = (oil0$Oil / 1000000), color = "#A5A051")
         }
         
         if(nrow(gas0) > 0 && ("Gas" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = gas0$Plant.longitude, lat = gas0$Plant.latitude, label = gas0$Plant.name, radius = (gas0$Gas / 1000000), color = "#F05E1C")
         }
         
         if(nrow(nuc0) > 0 && ("Nuclear" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = nuc0$Plant.longitude, lat = nuc0$Plant.latitude, label = nuc0$Plant.name, radius = (nuc0$Nuclear / 1000000), color = "#FFB11B")
         }
         
         if(nrow(hyd0) > 0 && ("Hydro" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = hyd0$Plant.longitude, lat = hyd0$Plant.latitude, label = hyd0$Plant.name, radius = (hyd0$Hydro / 1000000), color = "#66BAB7")
         }
         
         if(nrow(bio0) > 0 && ("Bio" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = bio0$Plant.longitude, lat = bio0$Plant.latitude, label = bio0$Plant.name, radius = (bio0$Bio / 1000000), color = "#1E88A8")
         }
         
         if(nrow(wind0) > 0 && ("Wind" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = wind0$Plant.longitude, lat = wind0$Plant.latitude, label = wind0$Plant.name, radius = (wind0$Wind / 1000000), color = "#4A225D")
         }
         
         if(nrow(sol0) > 0 && ("Solar" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = sol0$Plant.longitude, lat = sol0$Plant.latitude, label = sol0$Plant.name, radius = (sol0$Solar / 1000000), color = "#434343")
         }
         
         if(nrow(geo0) > 0 && ("Geo" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = geo0$Plant.longitude, lat = geo0$Plant.latitude, label = geo0$Plant.name, radius = (geo0$Geo / 1000000), color = "#E03C8A")
         }
         
         if(nrow(oth0) > 0 && ("Other" %in% input$checkMap || "All" %in% input$checkMap)) {
           map3 <- map3 %>% addCircleMarkers(lng = oth0$Plant.longitude, lat = oth0$Plant.latitude, label = oth0$Plant.name, radius = (oth0$Other / 1000000), color = "#78552B")
         }     
       }
     }
     
     
   })
   
   
   # --------------------------------------------------------------------------- Linked Map 2 -----------------------------------------------------------------------------------------------   
   
   
   #Map 2
   output$map4 <- renderLeaflet({
     
     #Use to filter states
     stateName <- input$stateName4
     
     
     coal18 <- subset(coal18, coal18$Plant.state.abbreviation == stateName)
     oil18 <- subset(oil18, oil18$Plant.state.abbreviation == stateName)
     gas18 <- subset(gas18, gas18$Plant.state.abbreviation == stateName)
     nuc18 <- subset(nuc18, nuc18$Plant.state.abbreviation == stateName)
     hyd18 <- subset(hyd18, hyd18$Plant.state.abbreviation == stateName)
     bio18 <- subset(bio18, bio18$Plant.state.abbreviation == stateName)
     wind18 <- subset(wind18, wind18$Plant.state.abbreviation == stateName)
     sol18 <- subset(sol18, sol18$Plant.state.abbreviation == stateName)
     geo18 <- subset(geo18, geo18$Plant.state.abbreviation == stateName)
     oth18 <- subset(oth18, oth18$Plant.state.abbreviation == stateName)
     
     coal10 <- subset(coal10, coal10$Plant.state.abbreviation == stateName)
     oil10 <- subset(oil10, oil10$Plant.state.abbreviation == stateName)
     gas10 <- subset(gas10, gas10$Plant.state.abbreviation == stateName)
     nuc10 <- subset(nuc10, nuc10$Plant.state.abbreviation == stateName)
     hyd10 <- subset(hyd10, hyd10$Plant.state.abbreviation == stateName)
     bio10 <- subset(bio10, bio10$Plant.state.abbreviation == stateName)
     wind10 <- subset(wind10, wind10$Plant.state.abbreviation == stateName)
     sol10 <- subset(sol10, sol10$Plant.state.abbreviation == stateName)
     geo10 <- subset(geo10, geo10$Plant.state.abbreviation == stateName)
     oth10 <- subset(oth10, oth10$Plant.state.abbreviation == stateName)
     
     coal0 <- subset(coal0, coal0$Plant.state.abbreviation == stateName)
     oil0 <- subset(oil0, oil0$Plant.state.abbreviation == stateName)
     gas0 <- subset(gas0, gas0$Plant.state.abbreviation == stateName)
     nuc0 <- subset(nuc0, nuc0$Plant.state.abbreviation == stateName)
     hyd0 <- subset(hyd0, hyd0$Plant.state.abbreviation == stateName)
     bio0 <- subset(bio0, bio0$Plant.state.abbreviation == stateName)
     wind0 <- subset(wind0, wind0$Plant.state.abbreviation == stateName)
     sol0 <- subset(sol0, sol0$Plant.state.abbreviation == stateName)
     geo0 <- subset(geo0, geo0$Plant.state.abbreviation == stateName)
     oth0 <- subset(oth0, oth0$Plant.state.abbreviation == stateName)
     
     # 2018 Data
     if (input$state == 2018) {
       
       if (input$resetMap || ! input$resetMap) {
         map <- leaflet(data = (plantData$Plant.state.abbreviation == input$stateName)) %>% 
           addTiles()
         
         if(nrow(coal18) > 0 && ("Coal" %in% input$checkMap || "All" %in% input$checkMap)) {
           map <- map %>% addCircleMarkers(lng = coal18$Plant.longitude, lat = coal18$Plant.latitude, label = coal18$Plant.name, radius = (coal18$Coal / 1000000), color = "#CB1B45")
           
         }
         
         if(nrow(oil18) > 0 && ("Oil" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = oil18$Plant.longitude, lat = oil18$Plant.latitude, label = oil18$Plant.name, radius = (oil18$Oil / 1000000), color = "#A5A051")
         }
         
         if(nrow(gas18) > 0 && ("Gas" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = gas18$Plant.longitude, lat = gas18$Plant.latitude, label = gas18$Plant.name, radius = (gas18$Gas / 1000000), color = "#F05E1C")
         }
         
         if(nrow(nuc18) > 0 && ("Nuclear" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = nuc18$Plant.longitude, lat = nuc18$Plant.latitude, label = nuc18$Plant.name, radius = (nuc18$Nuclear / 1000000), color = "#FFB11B")
           
         }
         
         if(nrow(hyd18) > 0 && ("Hydro" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = hyd18$Plant.longitude, lat = hyd18$Plant.latitude, label = hyd18$Plant.name, radius = (hyd18$Hydro / 1000000), color = "#66BAB7")
         }
         
         if(nrow(bio18) > 0 && ("Bio" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = bio18$Plant.longitude, lat = bio18$Plant.latitude, label = bio18$Plant.name, radius = (bio18$Bio / 1000000), color = "#1E88A8")
         }
         
         if(nrow(wind18) > 0 && ("Wind" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = wind18$Plant.longitude, lat = wind18$Plant.latitude, label = wind18$Plant.name, radius = (wind18$Wind / 1000000), color = "#4A225D")
         }
         
         if(nrow(sol18) > 0 && ("Solar" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = sol18$Plant.longitude, lat = sol18$Plant.latitude, label = sol18$Plant.name, radius = (sol18$Solar / 1000000), color = "#434343")
         }
         if(nrow(geo18) > 0 && ("Geo" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = geo18$Plant.longitude, lat = geo18$Plant.latitude, label = geo18$Plant.name, radius = (geo18$Geo / 1000000), color = "#E03C8A")
         }
         if(nrow(oth18) > 0 && ("Other" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = oth18$Plant.longitude, lat = oth18$Plant.latitude, label = oth18$Plant.name, radius = (oth18$Other / 1000000), color = "#78552B")
         }
       }
     }
     
     # 2010 Data
     else if (input$state4 == 2010) {
       
       if (input$reset4 || ! input$reset4) {
         map4 <- leaflet(data = plantData10 ) %>% 
           addTiles()
         
         if(nrow(coal10) > 0 && ("Coal" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = coal10$Plant.longitude, lat = coal10$Plant.latitude, label = coal10$Plant.name, radius = (coal10$Coal / 1000000), color = "#CB1B45")
         }
         
         if(nrow(oil10) > 0 && ("Oil" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = oil10$Plant.longitude, lat = oil10$Plant.latitude, label = oil10$Plant.name, radius = (oil10$Oil / 1000000), color = "#A5A051")
         }
         
         if(nrow(gas10) > 0 && ("Gas" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = gas10$Plant.longitude, lat = gas10$Plant.latitude, label = gas10$Plant.name, radius = (gas10$Gas / 1000000), color = "#F05E1C")
         }
         
         if(nrow(nuc10) > 0 && ("Nuclear" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = nuc10$Plant.longitude, lat = nuc10$Plant.latitude, label = nuc10$Plant.name, radius = (nuc10$Nuclear / 1000000), color = "#FFB11B")
         }
         
         if(nrow(hyd10) > 0 && ("Hydro" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = hyd10$Plant.longitude, lat = hyd10$Plant.latitude, label = hyd10$Plant.name, radius = (hyd10$Hydro / 1000000), color = "#66BAB7")
         }
         
         if(nrow(bio10) > 0 && ("Bio" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = bio10$Plant.longitude, lat = bio10$Plant.latitude, label = bio10$Plant.name, radius = (bio10$Bio / 1000000), color = "#1E88A8")
         }
         
         if(nrow(wind10) > 0 && ("Wind" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = wind10$Plant.longitude, lat = wind10$Plant.latitude, label = wind10$Plant.name, radius = (wind10$Wind / 1000000), color = "#4A225D")
         }
         
         if(nrow(sol10) > 0 && ("Solar" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = sol10$Plant.longitude, lat = sol10$Plant.latitude, label = sol10$Plant.name, radius = (sol10$Solar / 1000000), color = "#434343")
         }
         
         if(nrow(geo10) > 0 && ("Geo" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = geo10$Plant.longitude, lat = geo10$Plant.latitude, label = geo10$Plant.name, radius = (geo10$Geo / 1000000), color = "#E03C8A")
         }
         
         if(nrow(oth10) > 0 && ("Other" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = oth10$Plant.longitude, lat = oth10$Plant.latitude, label = oth10$Plant.name, radius = (oth10$Other / 1000000), color = "#78552B")
         }
       }
     }
     
     # 2000 Data
     else if (input$state4 == 2000) {
       
       if (input$reset4 || ! input$reset4) {
         map4 <- leaflet(data = plantData2000 ) %>% 
           addTiles()
         
         if(nrow(coal0) > 0 && ("Coal" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = coal0$Plant.longitude, lat = coal0$Plant.latitude, label = coal0$Plant.name, radius = (coal0$Coal / 1000000), color = "#CB1B45")
         }
         
         if(nrow(oil0) > 0 && ("Oil" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = oil0$Plant.longitude, lat = oil0$Plant.latitude, label = oil0$Plant.name, radius = (oil0$Oil / 1000000), color = "#A5A051")
         }
         
         if(nrow(gas0) > 0 && ("Gas" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = gas0$Plant.longitude, lat = gas0$Plant.latitude, label = gas0$Plant.name, radius = (gas0$Gas / 1000000), color = "#F05E1C")
         }
         
         if(nrow(nuc0) > 0 && ("Nuclear" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = nuc0$Plant.longitude, lat = nuc0$Plant.latitude, label = nuc0$Plant.name, radius = (nuc0$Nuclear / 1000000), color = "#FFB11B")
         }
         
         if(nrow(hyd0) > 0 && ("Hydro" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = hyd0$Plant.longitude, lat = hyd0$Plant.latitude, label = hyd0$Plant.name, radius = (hyd0$Hydro / 1000000), color = "#66BAB7")
         }
         
         if(nrow(bio0) > 0 && ("Bio" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = bio0$Plant.longitude, lat = bio0$Plant.latitude, label = bio0$Plant.name, radius = (bio0$Bio / 1000000), color = "#1E88A8")
         }
         
         if(nrow(wind0) > 0 && ("Wind" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = wind0$Plant.longitude, lat = wind0$Plant.latitude, label = wind0$Plant.name, radius = (wind0$Wind / 1000000), color = "#4A225D")
         }
         
         if(nrow(sol0) > 0 && ("Solar" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = sol0$Plant.longitude, lat = sol0$Plant.latitude, label = sol0$Plant.name, radius = (sol0$Solar / 1000000), color = "#434343")
         }
         
         if(nrow(geo0) > 0 && ("Geo" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = geo0$Plant.longitude, lat = geo0$Plant.latitude, label = geo0$Plant.name, radius = (geo0$Geo / 1000000), color = "#E03C8A")
         }
         
         if(nrow(oth0) > 0 && ("Other" %in% input$checkMap || "All" %in% input$checkMap)) {
           map4 <- map4 %>% addCircleMarkers(lng = oth0$Plant.longitude, lat = oth0$Plant.latitude, label = oth0$Plant.name, radius = (oth0$Other / 1000000), color = "#78552B")
         }     
       }
     }
     
     
   })

   
# --------------------------------------------------------------------------- US Total Map -----------------------------------------------------------------------------------------------   
   # If time change name to filter map
   
   
   
   # Map of the whole us
   output$map <- renderLeaflet({
     
     
     # Use to filter states or show map of whole US
     stateName <- input$state
     
     if (stateName != "US Map") {
       coal18 <- subset(coal18, coal18$Plant.state.abbreviation == stateName)
       oil18 <- subset(oil18, oil18$Plant.state.abbreviation == stateName)
       gas18 <- subset(gas18, gas18$Plant.state.abbreviation == stateName)
       nuc18 <- subset(nuc18, nuc18$Plant.state.abbreviation == stateName)
       hyd18 <- subset(hyd18, hyd18$Plant.state.abbreviation == stateName)
       bio18 <- subset(bio18, bio18$Plant.state.abbreviation == stateName)
       wind18 <- subset(wind18, wind18$Plant.state.abbreviation == stateName)
       sol18 <- subset(sol18, sol18$Plant.state.abbreviation == stateName)
       geo18 <- subset(geo18, geo18$Plant.state.abbreviation == stateName)
       oth18 <- subset(oth18, oth18$Plant.state.abbreviation == stateName)
       
       coal10 <- subset(coal10, coal10$Plant.state.abbreviation == stateName)
       oil10 <- subset(oil10, oil10$Plant.state.abbreviation == stateName)
       gas10 <- subset(gas10, gas10$Plant.state.abbreviation == stateName)
       nuc10 <- subset(nuc10, nuc10$Plant.state.abbreviation == stateName)
       hyd10 <- subset(hyd10, hyd10$Plant.state.abbreviation == stateName)
       bio10 <- subset(bio10, bio10$Plant.state.abbreviation == stateName)
       wind10 <- subset(wind10, wind10$Plant.state.abbreviation == stateName)
       sol10 <- subset(sol10, sol10$Plant.state.abbreviation == stateName)
       geo10 <- subset(geo10, geo10$Plant.state.abbreviation == stateName)
       oth10 <- subset(oth10, oth10$Plant.state.abbreviation == stateName)
       
       coal0 <- subset(coal0, coal0$Plant.state.abbreviation == stateName)
       oil0 <- subset(oil0, oil0$Plant.state.abbreviation == stateName)
       gas0 <- subset(gas0, gas0$Plant.state.abbreviation == stateName)
       nuc0 <- subset(nuc0, nuc0$Plant.state.abbreviation == stateName)
       hyd0 <- subset(hyd0, hyd0$Plant.state.abbreviation == stateName)
       bio0 <- subset(bio0, bio0$Plant.state.abbreviation == stateName)
       wind0 <- subset(wind0, wind0$Plant.state.abbreviation == stateName)
       sol0 <- subset(sol0, sol0$Plant.state.abbreviation == stateName)
       geo0 <- subset(geo0, geo0$Plant.state.abbreviation == stateName)
       oth0 <- subset(oth0, oth0$Plant.state.abbreviation == stateName)
       }
     
     #Use to set the max for the data
     mapMax <- input$max
     
     coal18 <- subset(coal18, coal18$Coal <= mapMax)
     oil18 <- subset(oil18, oil18$Oil <= mapMax)
     gas18 <- subset(gas18, gas18$Gas <= mapMax)
     nuc18 <- subset(nuc18, nuc18$Nuclear <= mapMax)
     hyd18 <- subset(hyd18, hyd18$Hydro <= mapMax)
     bio18 <- subset(bio18, bio18$Bio <= mapMax)
     wind18 <- subset(wind18, wind18$Wind <= mapMax)
     sol18 <- subset(sol18, sol18$Solar <= mapMax)
     geo18 <- subset(geo18, geo18$Geo <= mapMax)
     oth18 <- subset(oth18, oth18$Other <= mapMax)
     
     coal10 <- subset(coal10, coal10$Coal <= mapMax)
     oil10 <- subset(oil10, oil10$Oil <= mapMax)
     gas10 <- subset(gas10, gas10$Gas <= mapMax)
     nuc10 <- subset(nuc10, nuc10$Nuclear <= mapMax)
     hyd10 <- subset(hyd10, hyd10$Hydro <= mapMax)
     bio10 <- subset(bio10, bio10$Bio <= mapMax)
     wind10 <- subset(wind10, wind10$Wind <= mapMax)
     sol10 <- subset(sol10, sol10$Solar <= mapMax)
     geo10 <- subset(geo10, geo10$Geo <= mapMax)
     oth10 <- subset(oth10, oth10$Other <= mapMax)
     
     coal0 <- subset(coal0, coal0$Coal <= mapMax)
     oil0 <- subset(oil0, oil0$Oil <= mapMax)
     gas0 <- subset(gas0, gas0$Gas <= mapMax)
     nuc0 <- subset(nuc0, nuc0$Nuclear <= mapMax)
     hyd0 <- subset(hyd0, hyd0$Hydro <= mapMax)
     bio0 <- subset(bio0, bio0$Bio <= mapMax)
     wind0 <- subset(wind0, wind0$Wind <= mapMax)
     sol0 <- subset(sol0, sol0$Solar <= mapMax)
     geo0 <- subset(geo0, geo0$Geo <= mapMax)
     oth0 <- subset(oth0, oth0$Other <= mapMax)
     
     #Use to set the min for the data
     mapMin <- input$min
     
     coal18 <- subset(coal18, coal18$Coal >= mapMin)
     oil18 <- subset(oil18, oil18$Oil >= mapMin)
     gas18 <- subset(gas18, gas18$Gas >= mapMin)
     nuc18 <- subset(nuc18, nuc18$Nuclear >= mapMin)
     hyd18 <- subset(hyd18, hyd18$Hydro >= mapMin)
     bio18 <- subset(bio18, bio18$Bio >= mapMin)
     wind18 <- subset(wind18, wind18$Wind >= mapMin)
     sol18 <- subset(sol18, sol18$Solar >= mapMin)
     geo18 <- subset(geo18, geo18$Geo >= mapMin)
     oth18 <- subset(oth18, oth18$Other >= mapMin)
     
     coal10 <- subset(coal10, coal10$Coal >= mapMin)
     oil10 <- subset(oil10, oil10$Oil >= mapMin)
     gas10 <- subset(gas10, gas10$Gas >= mapMin)
     nuc10 <- subset(nuc10, nuc10$Nuclear >= mapMin)
     hyd10 <- subset(hyd10, hyd10$Hydro >= mapMin)
     bio10 <- subset(bio10, bio10$Bio >= mapMin)
     wind10 <- subset(wind10, wind10$Wind >= mapMin)
     sol10 <- subset(sol10, sol10$Solar >= mapMin)
     geo10 <- subset(geo10, geo10$Geo >= mapMin)
     oth10 <- subset(oth10, oth10$Other >= mapMin)
     
     coal0 <- subset(coal0, coal0$Coal >= mapMin)
     oil0 <- subset(oil0, oil0$Oil >= mapMin)
     gas0 <- subset(gas0, gas0$Gas >= mapMin)
     nuc0 <- subset(nuc0, nuc0$Nuclear >= mapMin)
     hyd0 <- subset(hyd0, hyd0$Hydro >= mapMin)
     bio0 <- subset(bio0, bio0$Bio >= mapMin)
     wind0 <- subset(wind0, wind0$Wind >= mapMin)
     sol0 <- subset(sol0, sol0$Solar >= mapMin)
     geo0 <- subset(geo0, geo0$Geo >= mapMin)
     oth0 <- subset(oth0, oth0$Other >= mapMin)
     
     
     # 2018 Data
     if (input$year == 2018) {
       
       if (input$resetMap || ! input$resetMap) {
         map <- leaflet(data = plantData) %>% 
           addTiles()
         
         if(nrow(coal18) > 0 && ("Coal" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = coal18$Plant.longitude, lat = coal18$Plant.latitude, label = coal18$Plant.name, radius = 2, color = "#CB1B45")
           
         }
         
         if(nrow(oil18) > 0 && ("Oil" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = oil18$Plant.longitude, lat = oil18$Plant.latitude, label = oil18$Plant.name, radius = 2, color = "#A5A051")
         }
         
         if(nrow(gas18) > 0 && ("Gas" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = gas18$Plant.longitude, lat = gas18$Plant.latitude, label = gas18$Plant.name, radius = 2, color = "#F05E1C")
         }
         
         if(nrow(nuc18) > 0 && ("Nuclear" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = nuc18$Plant.longitude, lat = nuc18$Plant.latitude, label = nuc18$Plant.name, radius = 2, color = "#FFB11B")
           
         }
         
         if(nrow(hyd18) > 0 && ("Hydro" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = hyd18$Plant.longitude, lat = hyd18$Plant.latitude, label = hyd18$Plant.name, radius = 2, color = "#66BAB7")
         }
         
         if(nrow(bio18) > 0 && ("Bio" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = bio18$Plant.longitude, lat = bio18$Plant.latitude, label = bio18$Plant.name, radius = 2, color = "#1E88A8")
         }
         
         if(nrow(wind18) > 0 && ("Wind" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = wind18$Plant.longitude, lat = wind18$Plant.latitude, label = wind18$Plant.name, radius = 2, color = "#4A225D")
         }
         
         if(nrow(sol18) > 0 && ("Solar" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = sol18$Plant.longitude, lat = sol18$Plant.latitude, label = sol18$Plant.name, radius = 2, color = "#434343")
         }
         if(nrow(geo18) > 0 && ("Geo" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = geo18$Plant.longitude, lat = geo18$Plant.latitude, label = geo18$Plant.name, radius = 2, color = "#E03C8A")
         }
         if(nrow(oth18) > 0 && ("Other" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = oth18$Plant.longitude, lat = oth18$Plant.latitude, label = oth18$Plant.name, radius = 2, color = "#78552B")
         }
       }
     }
     
     # 2010 Data
     else if (input$year == 2010) {
       
       if (input$resetMap || ! input$resetMap) {
         map <- leaflet(data = plantData10 ) %>% 
           addTiles()
         
         if(nrow(coal10) > 0 && ("Coal" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = coal10$Plant.longitude, lat = coal10$Plant.latitude, label = coal10$Plant.name, radius = (coal10$Coal / 1000000), color = "#CB1B45")
         }
         
         if(nrow(oil10) > 0 && ("Oil" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = oil10$Plant.longitude, lat = oil10$Plant.latitude, label = oil10$Plant.name, radius = 2, color = "#A5A051")
         }
         
         if(nrow(gas10) > 0 && ("Gas" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = gas10$Plant.longitude, lat = gas10$Plant.latitude, label = gas10$Plant.name, radius = 2, color = "#F05E1C")
         }
         
         if(nrow(nuc10) > 0 && ("Nuclear" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = nuc10$Plant.longitude, lat = nuc10$Plant.latitude, label = nuc10$Plant.name, radius = 2, color = "#FFB11B")
         }
         
         if(nrow(hyd10) > 0 && ("Hydro" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = hyd10$Plant.longitude, lat = hyd10$Plant.latitude, label = hyd10$Plant.name, radius = 2, color = "#66BAB7")
         }
         
         if(nrow(bio10) > 0 && ("Bio" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = bio10$Plant.longitude, lat = bio10$Plant.latitude, label = bio10$Plant.name, radius = 2, color = "#1E88A8")
         }
         
         if(nrow(wind10) > 0 && ("Wind" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = wind10$Plant.longitude, lat = wind10$Plant.latitude, label = wind10$Plant.name, radius = 2, color = "#4A225D")
         }
         
         if(nrow(sol10) > 0 && ("Solar" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = sol10$Plant.longitude, lat = sol10$Plant.latitude, label = sol10$Plant.name, radius = 2, color = "#434343")
         }
         
         if(nrow(geo10) > 0 && ("Geo" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = geo10$Plant.longitude, lat = geo10$Plant.latitude, label = geo10$Plant.name, radius = 2, color = "#E03C8A")
         }
         
         if(nrow(oth10) > 0 && ("Other" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = oth10$Plant.longitude, lat = oth10$Plant.latitude, label = oth10$Plant.name, radius = 2, color = "#78552B")
         }
       }
     }
     
     # 2000 Data
     else if (input$year == 2000) {
       
       if (input$resetMap || ! input$resetMap) {
         map <- leaflet(data = plantData2000 ) %>% 
           addTiles()
         
         if(nrow(coal0) > 0 && ("Coal" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = coal0$Plant.longitude, lat = coal0$Plant.latitude, label = coal0$Plant.name, radius = 2, color = "#CB1B45")
         }
         
         if(nrow(oil0) > 0 && ("Oil" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = oil0$Plant.longitude, lat = oil0$Plant.latitude, label = oil0$Plant.name, radius = 2, color = "#A5A051")
         }
         
         if(nrow(gas0) > 0 && ("Gas" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = gas0$Plant.longitude, lat = gas0$Plant.latitude, label = gas0$Plant.name, radius = 2, color = "#F05E1C")
         }
         
         if(nrow(nuc0) > 0 && ("Nuclear" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = nuc0$Plant.longitude, lat = nuc0$Plant.latitude, label = nuc0$Plant.name, radius = 2, color = "#FFB11B")
         }
         
         if(nrow(hyd0) > 0 && ("Hydro" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = hyd0$Plant.longitude, lat = hyd0$Plant.latitude, label = hyd0$Plant.name, radius = 2, color = "#66BAB7")
         }
         
         if(nrow(bio0) > 0 && ("Bio" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = bio0$Plant.longitude, lat = bio0$Plant.latitude, label = bio0$Plant.name, radius = 2, color = "#1E88A8")
         }
         
         if(nrow(wind0) > 0 && ("Wind" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = wind0$Plant.longitude, lat = wind0$Plant.latitude, label = wind0$Plant.name, radius = 2, color = "#4A225D")
         }
         
         if(nrow(sol0) > 0 && ("Solar" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = sol0$Plant.longitude, lat = sol0$Plant.latitude, label = sol0$Plant.name, radius = 2, color = "#434343")
         }
         
         if(nrow(geo0) > 0 && ("Geo" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = geo0$Plant.longitude, lat = geo0$Plant.latitude, label = geo0$Plant.name, radius = 2, color = "#E03C8A")
         }
         
         if(nrow(oth0) > 0 && ("Other" %in% input$checkMap0 || "All" %in% input$checkMap0)) {
           map <- map %>% addCircleMarkers(lng = oth0$Plant.longitude, lat = oth0$Plant.latitude, label = oth0$Plant.name, radius = 2, color = "#78552B")
         }     
       }
     }
     
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
