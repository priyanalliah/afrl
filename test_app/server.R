#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    output$dateDistribution <- renderPlot({
        data <- read_csv(here("data", "DLI_data_export_for_Priya.csv"))
        
        # From the whole data structure, keep only the data for a single user:
        selected_user <- "Zahra"
        
        user_data <- data %>%
            filter(user_name == selected_user)
        
        user_data %>% 
            mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>% 
            ggplot(aes(date, fill = month)) +
            geom_histogram(bins = 30) +
            labs(title = paste(selected_user, "| Distribution of data points over time"),
                 subtitle = paste("Range of dates:", min(date(data$date)), "to", max(date(data$date)))) +
            guides(fill = "none")
    })

}
