library(shiny)
library(plotly)
library(tidyverse)

# Define the UI
ui <- fluidPage(
    titlePanel("IV Fluid Contamination Simulator"),
    sidebarLayout(
        sidebarPanel(
            numericInput("sodium", "Sodium (mEq/L):", value = 140),
            numericInput("potassium", "Potassium (mEq/L):", value = 4.0),
            numericInput("chloride", "Chloride (mEq/L):", value = 100),
            numericInput("calcium", "Calcium (mg/dL):", value = 8.0),
            numericInput("glucose", "Glucose (mEq/L):", value = 100),
            sliderInput("mixture_ratio", "Mixture Ratio:", min = 0, max = 1, value = 0.5, step = 0.01),
            radioButtons("iv_fluid", "IV Fluid:",
                         choices = c("0.9% Sodium Chloride",
                                     "0.9% Sodium Chloride with 5% Dextrose",
                                     "Lactated Ringer's",
                                     "Lactated Ringer's with 5% Dextrose",
                                     "5% Dextrose in Water",
                                     "Custom Fluid"))
        ),
        mainPanel(
            h4("Output Results:"),
            verbatimTextOutput("output_results")
        )
    )
)

# Define the server
server <- function(input, output) {
    
    # Define the IV fluid concentrations
    iv_fluids <- list(
        "0.9% Sodium Chloride" = list(sodium = 154, potassium = 0, chloride = 154, calcium = 0, glucose = 0),
        "0.9% Sodium Chloride with 5% Dextrose" = list(sodium = 154, potassium = 4, chloride = 154, calcium = 0,  glucose = 5000),
        "Lactated Ringer's" = list(sodium = 130, potassium = 4, chloride = 109, calcium = 5.4, glucose = 0),
        "Lactated Ringer's with 5% Dextrose" = list(sodium = 130, potassium = 4, chloride = 109, calcium = 5.4, glucose = 5000),
        "5% Dextrose in Water" = list(sodium = 0, potassium = 0, chloride = 0, calcium = 0, glucose = 5000)
    )
    
    # Calculate the output results based on the inputs, mixture ratio, and selected IV fluid
    output_results <- reactive({
        mixture_ratio <- input$mixture_ratio
        input_results <- list(
            sodium = input$sodium,
            chloride = input$chloride,
            potassium = input$potassium,
            calcium = input$calcium,
            glucose = input$glucose
        )
        iv_fluid <- iv_fluids[[input$iv_fluid]]
        mixture_results <- lapply(names(input_results), function(x) {
            c(analyte = x,
              true_result = input_results[[x]],
              contaminated_result = (1 - mixture_ratio) * input_results[[x]] + mixture_ratio * iv_fluid[[x]])
        }) %>% bind_rows() %>% as.data.frame()
    })
    

    # Output the output results as a list
    output$output_results <- renderPrint({
        output_results <- output_results()
        fluid_name <- gsub("'", "\\'", input$iv_fluid)
        mixture_ratio <- input$mixture_ratio * 100
        message <- paste0("Contamination by ", fluid_name, 
                          " at ", mixture_ratio, 
                          "% mixture will give the results below.\n\n")
        cat(message)
        print(output_results)
    })
    
}
# Run the app
shinyApp(ui = ui, server = server)