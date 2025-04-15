library(shiny)
library(tidyverse)
library(plotly)

source("R/utils.R")

#sending
sending <- read.csv("Data/sending.csv")

sending_country <- 
  aggregate(sending$count, 
            by=list(Country=sending$sending_country), 
            FUN=sum)
sending_country$Parent <- rep("Countries", nrow(sending_country))
sending_country <- sending_country[order(sending_country$x, decreasing=TRUE), ]
sum_countries_sending <- sum(sending_country$x)

sending_city <- 
  aggregate(sending$count, 
            by=list(Country=sending$sending_country, City=sending$sending_city), 
            FUN=sum)
sending_city$Parent <- sending_city$Country

sending_institution <- 
  aggregate(sending$count,
            by=list(Country=sending$sending_country, City=sending$sending_city, 
                    Institution=sending$sending_institution),
            FUN=sum)
sending_institution$Parent <- sending_institution$City

#receiving
receiving <- read.csv("Data/receiving.csv")

receiving_country <- 
  aggregate(receiving$count, 
            by=list(Country=receiving$receiving_country), 
            FUN=sum)
receiving_country$Parent <- rep("Countries", nrow(receiving_country))
receiving_country <- receiving_country[order(receiving_country$x, decreasing=TRUE), ]
sum_countries_receiving <- sum(receiving_country$x)

receiving_city <- 
  aggregate(receiving$count, 
            by=list(Country=receiving$receiving_country, City=receiving$receiving_city), 
            FUN=sum)
receiving_city$Parent <- receiving_city$Country

receiving_institution <- 
  aggregate(receiving$count,
            by=list(Country=receiving$receiving_country, City=receiving$receiving_city, 
                    Institution=receiving$receiving_institution),
            FUN=sum)
receiving_institution$Parent <- receiving_institution$City

#combined
combined <- 
  bind_rows(
    sending %>% rename(combined_country = sending_country, combined_city=sending_city,
                       combined_institution=sending_institution), 
    receiving %>% rename(combined_country = receiving_country, combined_city=receiving_city,
                         combined_institution=receiving_institution)
  ) 

combined <- 
  aggregate(combined$count, 
            by=list(combined_country=combined$combined_country, 
                    combined_city=combined$combined_city, 
                    combined_institution=combined$combined_institution),
            FUN=sum)

combined_country <- 
  aggregate(combined$x, 
            by=list(Country=combined$combined_country), 
            FUN=sum)
combined_country$Parent <- rep("Countries", nrow(combined_country))
combined_country <- combined_country[order(combined_country$x, decreasing=TRUE), ]
sum_countries_combined <- sum(combined_country$x)

combined_city <- 
  aggregate(combined$x, 
            by=list(Country=combined$combined_country, City=combined$combined_city), 
            FUN=sum)
combined_city$Parent <- combined_city$Country

combined_institution <- 
  aggregate(combined$x,
            by=list(Country=combined$combined_country, City=combined$combined_city, 
                    Institution=combined$combined_institution),
            FUN=sum)
combined_institution$Parent <- combined_institution$City


# Define UI for application
ui <- fluidPage(

    titlePanel("Treemap App"),
    
    HTML("<br><br>"),

    sidebarLayout(
      sidebarPanel(
        
        sliderInput("items_layer", 
                    label = "How many items do you want to display per layer",
                    min = 5, max = 40, value = 15),
        
        HTML("<br><br><br>"),
        
        checkboxInput("other_items", label = 'Don\'t show "other" items', value = FALSE),
        
        HTML("</br></br><br>"),
        
        selectInput("datatype", label = "Show the number of students that are sent,
                    received or both combined by each entity", 
                    choices = c("Sent students"="Sending", "Received students"="Receiving",
                                "Combined sent and received" ="Combined"), selected = "Sending")
      ),
      
      mainPanel(
        textOutput("title"),
        tags$head(tags$style(HTML("#title {font-size: 20px; text-align: center; text-bold:true; font-weight:bold}"))),
        #h3("<pre> Displaying how many student each \"entity\" SENDS</pre>"),
        plotlyOutput("treemap", width="100%", height = "100%")
      )
    )
    
)

# Define server logic
server <- function(input, output) {

  output$title <- renderText({
    switch(input$datatype, 
           "Sending" = return("Number of sent students by entity"),
           "Receiving" = return("Number of received studentes by entity"),
           "Combined" = return("Combined number of send and received students by entity")
    )
  })
  
  n_items <- reactive({
    input$items_layer
  })

  country_nested <- reactive({
    switch(input$datatype, 
           "Sending" = countriesFunc(n_items(), sending_country),
           "Receiving" = countriesFunc(n_items(), receiving_country),
           "Combined" = countriesFunc(n_items(), combined_country)
    )
  })
  
  city_nested <- reactive({
    switch(input$datatype, 
           "Sending" = citiesFunc(n_items(), sending_city),
           "Receiving" = citiesFunc(n_items(), receiving_city),
           "Combined" = citiesFunc(n_items(), combined_city)
    )
  })
  
  institution_nested <- reactive({
    switch(input$datatype, 
           "Sending" = institutionsFunc(n_items(), sending_institution, sending_city),
           "Receiving" = institutionsFunc(n_items(), receiving_institution, receiving_city),
           "Combined" = institutionsFunc(n_items(), combined_institution, combined_city)
    )
  })
  
  country_simple_temp <- reactive({
    switch(input$datatype, 
           "Sending" = countriesFuncSimple(n_items(), sending_country),
           "Receiving" = countriesFuncSimple(n_items(), receiving_country),
           "Combined" = countriesFuncSimple(n_items(), combined_country)
    )
    
  })
  
  city_simple_temp <- reactive({
    switch(input$datatype, 
           "Sending" = citiesFuncSimple(n_items(), cities = sending_city, countries = country_simple_temp()),
           "Receiving" = citiesFuncSimple(n_items(), cities = receiving_city, countries = country_simple_temp()),
           "Combined" = citiesFuncSimple(n_items(), cities = combined_city, countries = country_simple_temp())

    )
      })
  
  simple <- reactive({
    switch(input$datatype, 
           "Sending" = institutionFuncSimple(n_items(), institutions =  sending_institution, 
                                             cities =  city_simple_temp(), 
                                             countries =  country_simple_temp()),
           "Receiving" = institutionFuncSimple(n_items(), institutions =  receiving_institution, 
                                               cities =  city_simple_temp(), 
                                               countries =  country_simple_temp()),
           "Combined" = institutionFuncSimple(n_items(), institutions =  combined_institution, 
                                              cities =  city_simple_temp(), 
                                              countries =  country_simple_temp())
    )
  })
  
  country_simple <- reactive({ simple()[[1]] })
  city_simple <- reactive({ simple()[[2]] })
  institution_simple <- reactive({ simple()[[3]] })
  
  
  decider <- reactive({
    if (!input$other_items) {
      list(country_nested(), city_nested(), institution_nested())
    } else {
      list(country_simple(), city_simple(), institution_simple())
    }
  })
  
  
  # Prepare all values for Plotly
  my_ids <- reactive({
    c("Countries",
      decider()[[1]]$Country,
      decider()[[2]]$City,
      decider()[[3]]$Institution)
  }) 
  
  my_labels <- reactive({
    temp_labels <- my_ids()
    for (i in 1:length(temp_labels)) {
      if (grepl( "Other Countries", temp_labels[i], fixed = TRUE)) {
        temp_labels[i] <- "Other Countries"
      }
      if (grepl( "Other City", temp_labels[i], fixed = TRUE)) {
        temp_labels[i] <- "Other Cities"
      }
      if (grepl( "Other Institution", temp_labels[i], fixed = TRUE)) {
        temp_labels[i] <- "Other Institutions"
      }
    }
    
    my_labels <- str_wrap(temp_labels, width=15, whitespace_only = FALSE)
    my_labels <- str_to_title(my_labels)
    return(my_labels)
  }) 
  
  my_parents <- reactive({
    c("", 
      decider()[[1]]$Parent, 
      decider()[[2]]$Parent, 
      decider()[[3]]$Parent)
  }) 
  
  sum_countries <- reactive({
    if (input$other_items) { sum(decider()[[1]]$x) }
    else {
      switch(input$datatype, 
             "Sending" = sum_countries_sending,
             "Receiving" = sum_countries_receiving,
             "Combined" = sum_countries_combined
      )
    }
  })
  
  values_CoCiIn <- reactive({
      c(sum_countries(),
        decider()[[1]]$x,
        decider()[[2]]$x,
        decider()[[3]]$x)
  })
  
  hover_value <- reactive({
    if (input$other_items) { 
      c(sum_countries(),
        decider()[[1]]$x,
        decider()[[2]]$hover_value,
        decider()[[3]]$hover_value)
    }
    else { values_CoCiIn() }
  })
  
  output$treemap <- renderPlotly({
    plot_ly(
      ids = my_ids(),
      labels = my_labels(),
      parents = my_parents(),
      values = values_CoCiIn(),
      hovertext = hover_value(),
      type = "treemap",
      branchvalues = "total",
      maxdepth = 2,
      hoverinfo = "label+text" 
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
