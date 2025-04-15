# Loading the required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(readr)
library(shinyjs)  # For JavaScript interactions
library(sp)       # For spatial data handling

# Loading the datasets
receiving <- read_csv("Data/receiving.csv", show_col_types = FALSE)
problems(receiving)
sending <- read_csv("Data/sending.csv", show_col_types = FALSE)
problems(sending)

# Data preprocessing
country_data <- receiving %>%
  group_by(receiving_country) %>%
  summarise(TotalStudents = sum(count)) %>%
  ungroup()

# Mapping country names in country_data to align with the natural earth data
country_data$receiving_country <- case_when(
  country_data$receiving_country == "AUSTRIA" ~ "Austria",
  country_data$receiving_country == "BELGIUM" ~ "Belgium",
  country_data$receiving_country == "BULGARIA" ~ "Bulgaria",
  country_data$receiving_country == "CROATIA" ~ "Croatia",
  country_data$receiving_country == "CYPRUS" ~ "Cyprus",
  country_data$receiving_country == "CZECH REPUBLIC" ~ "Czech Republic",
  country_data$receiving_country == "DENMARK" ~ "Denmark",
  country_data$receiving_country == "ESTONIA" ~ "Estonia",
  country_data$receiving_country == "FINLAND" ~ "Finland",
  country_data$receiving_country == "FRANCE" ~ "France",
  country_data$receiving_country == "GERMANY" ~ "Germany",
  country_data$receiving_country == "GREECE" ~ "Greece",
  country_data$receiving_country == "HUNGARY" ~ "Hungary",
  country_data$receiving_country == "ICELAND" ~ "Iceland",
  country_data$receiving_country == "IRELAND" ~ "Ireland",
  country_data$receiving_country == "ITALY" ~ "Italy",
  country_data$receiving_country == "LATVIA" ~ "Latvia",
  country_data$receiving_country == "LIECHTENSTEIN" ~ "Liechtenstein",
  country_data$receiving_country == "LITHUANIA" ~ "Lithuania",
  country_data$receiving_country == "LUXEMBOURG" ~ "Luxembourg",
  country_data$receiving_country == "MALTA" ~ "Malta",
  country_data$receiving_country == "NETHERLANDS" ~ "Netherlands",
  country_data$receiving_country == "NORWAY" ~ "Norway",
  country_data$receiving_country == "POLAND" ~ "Poland",
  country_data$receiving_country == "PORTUGAL" ~ "Portugal",
  country_data$receiving_country == "ROMANIA" ~ "Romania",
  country_data$receiving_country == "SLOVAKIA" ~ "Slovakia",
  country_data$receiving_country == "SLOVENIA" ~ "Slovenia",
  country_data$receiving_country == "SPAIN" ~ "Spain",
  country_data$receiving_country == "SWEDEN" ~ "Sweden",
  country_data$receiving_country == "SWITZERLAND" ~ "Switzerland",
  country_data$receiving_country == "TURKEY" ~ "Turkey",
  country_data$receiving_country == "UNITED KINGDOM" ~ "United Kingdom",
  TRUE ~ country_data$receiving_country  # default case
)

# Loading Europe map data using rnaturalearth package
world <- ne_countries(scale = "medium", returnclass = "sf")
europe_map <- subset(world, continent == "Europe")

# Performing the join
europe_map <- europe_map %>%
  left_join(country_data, by = c("name" = "receiving_country"))

# Defining UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("ERASMUS Student Exchange Visualization"),
  leafletOutput("europeMap"),
  leafletOutput("regionalMap")  # Output for the regional map
)

# Defining Server
server <- function(input, output, session) {
  # Render the Europe map
  output$europeMap <- renderLeaflet({
     leaflet(data = europe_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", TotalStudents)(TotalStudents),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(name, ": ", TotalStudents, "students"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal"),
          textsize = "13px",
          direction = "auto"
        )
      )
  })
  
  # Initializing a reactive value to store the selected country
  selected_country <- reactiveVal()
  
  # Observing clicks on the Europe map and update the selected country
  observe({
    click <- input$europeMap_click
    if(!is.null(click)) {
      selected_country(click$ID)  
    }
  })
  
  # Placeholder for logic to render a regional map upon clicking a country
 
  output$regionalMap <- renderLeaflet({
   
  })

}
# Run the application
shinyApp(ui = ui, server = server)
