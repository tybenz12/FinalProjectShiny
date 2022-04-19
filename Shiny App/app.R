library(shiny)
library(tidyverse)
library(lubridate)
library(rsconnect)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

new_covid_data <- covid19 %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(census_pop_est_2018,
            by = c("state")) %>% 
  group_by(state) %>% 
  mutate(one_day_lag = replace_na(lag(cases, 1), 0),
         new_cases = cases - one_day_lag, 
         covid_per_100000 = (new_cases/est_pop_2018)*100000)

ui <- fluidPage(
  sliderInput(inputId = "date", 
              label = "Date Range",
              min = as.Date(min(new_covid_data$date)),
              max = as.Date(max(new_covid_data$date)),
              value=c(as.Date(min(new_covid_data$date)), as.Date(max(new_covid_data$date)))),
  selectInput(inputId = "state", 
              label = "Select States To Compare", 
              choices = new_covid_data %>% 
                arrange(state) %>% 
                distinct(state) %>% 
                pull(state), 
              multiple = TRUE),
  submitButton(text = "Compare States"), 
  plotOutput(outputId = "timeplot")
)

server <- function(input, output){
  output$timeplot <- renderPlot({
    new_covid_data %>% 
      filter(state %in% input$state) %>% 
      ggplot() +
      geom_line(aes(x = date, y = covid_per_100000, color = state)) +
      scale_x_date(limits = input$date) +
      theme_minimal() +
      labs(title = "Comparing Daily COVID Cases per 100,000 Between States", 
           x = "", 
           y = "", 
           color = "State")
  })
}

shinyApp(ui = ui, server = server)