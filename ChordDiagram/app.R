if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("chorddiag", quietly = TRUE)) {
  remotes::install_github("mattflor/chorddiag")
}

library(shiny)
library(chorddiag)


data <- read.csv("Data/chordData_final.csv", row.names = 1)
m <- as.matrix(data)
print(m)
countries <- rownames(m)
dimnames(m) <- list(from = countries, to = countries)

val = 10
default_countries <- rownames(m)[1:val]
updated_matrix <- m[default_countries, default_countries]

# New colors
groupColors <- RColorBrewer::brewer.pal(12, "Set3")
additional_colors <- rainbow(22)
groupColors <- c(groupColors, additional_colors)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Chord Diagram"),
  sidebarLayout(
    sidebarPanel(
      # Input: Slider to select the number of countries to display
      sliderInput("num_countries", "Select Number of Countries:", 
                  min = 1, max = length(countries), value = val, step = 1),
      # Input: Select specific countries
      selectizeInput("selected_countries", "Select Specific Countries:",
                     choices = countries, multiple = TRUE),
      # Button to update chord diagram
      actionButton("update_button", "Update Diagram")
    ),
    mainPanel(
      # Output: Chord diagram
      chorddiagOutput("chordDiagram", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value to store the current matrix
  data_matrix <- reactiveVal(updated_matrix)
  
  # Update matrix when the button is clicked
  observeEvent(input$update_button, {
    selected_countries <- input$selected_countries
    if (!is.null(selected_countries) && length(selected_countries) > 0) {
      # Update matrix based on selected countries
      updated_matrix <- m[selected_countries, selected_countries]
    } else {
      # Update matrix based on the number of countries to display
      updated_matrix <- m[1:input$num_countries, 1:input$num_countries]
    }
    data_matrix(updated_matrix)
  })
  
  # Render chord diagram
  output$chordDiagram <- renderChorddiag({
    chorddiag(data_matrix(), groupColors = groupColors, groupnamePadding = 40, groupnameFontsize = 10) #type = "bipartite"
  })
}

# Run the Shiny app
shinyApp(ui, server)
