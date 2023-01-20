library(shiny)
library(tidyverse)
library(palmerpenguins)

# Create the user interface:
ui <- fluidPage(
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("put my widgets here",
                 radioButtons(inputId = "penguin_species",
                              label = "Choose penguin species",
                              choices = c("Adelie","Gentoo","Cool Chinstrap" = "Chinstrap"),
                 ), ### endsidebarpanel
    selectInput(inputId = "pt_color",
                label = "Select point color",
                choices = c("Awesome red!" = "red",
                            "Pretty purple" = "purple",
                            "ORAAANGE" = "orange"))
                              ),
    mainPanel("put my graph here",
    plotOutput(outputId = "penguin_plot"),
    tableOutput(outputId = "penguin_table")
  ) ### end mainPanel
  ) ### end sidebarLayer
  ) ### end fluidPage

server <- function(input, output) {
  penguin_select <- reactive({
    penguins %>%
      filter(species == input$penguin_species)
  })

  penguin_table <- reactive({
    penguins %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(
        mean_flip = mean(flipper_length_mm),
        mean_mass = mean(body_mass_g)
      )
  })


  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select(),
           aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(color= input$pt_color)
  })

  output$penguin_table <- renderTable({

    penguin_table()

  })

}

# Combine them into an app:
shinyApp(ui = ui, server = server)



