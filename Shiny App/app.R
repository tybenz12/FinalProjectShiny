library(shiny)
library(tidyverse)
library(lubridate)
library(rsconnect)
library(zoo)

Data2016 <- read_csv("2016FullPitchData.csv", 
                     col_types = cols(player_id = col_number(), 
                                      year = col_number(), player_age = col_number(), 
                                      p_game = col_number(), p_formatted_ip = col_number(), 
                                      p_strikeout = col_number(), p_k_percent = col_number(), 
                                      p_era = col_number(), n = col_number(), 
                                      n_ff_formatted = col_number(), ff_avg_speed = col_number(), 
                                      ff_avg_spin = col_number(), ff_avg_break_x = col_number(), 
                                      ff_avg_break_z = col_number(), ff_avg_break = col_number(), 
                                      ff_range_speed = col_number(), n_sl_formatted = col_number(), 
                                      sl_avg_speed = col_number(), sl_avg_spin = col_number(), 
                                      sl_avg_break_x = col_number(), sl_avg_break_z = col_number(), 
                                      sl_avg_break = col_number(), sl_range_speed = col_number(), 
                                      n_ch_formatted = col_number(), ch_avg_speed = col_number(), 
                                      ch_avg_spin = col_number(), ch_avg_break_x = col_number(), 
                                      ch_avg_break_z = col_number(), ch_avg_break = col_number(), 
                                      ch_range_speed = col_number(), n_cukc_formatted = col_number(), 
                                      cu_avg_speed = col_number(), cu_avg_spin = col_number(), 
                                      cu_avg_break_x = col_number(), cu_avg_break_z = col_number(), 
                                      cu_avg_break = col_number(), cu_range_speed = col_number(), 
                                      n_sift_formatted = col_number(), 
                                      si_avg_speed = col_number(), si_avg_spin = col_number(), 
                                      si_avg_break_x = col_number(), si_avg_break_z = col_number(), 
                                      si_avg_break = col_number(), si_range_speed = col_number(), 
                                      n_fc_formatted = col_number(), fc_avg_speed = col_number(), 
                                      fc_avg_spin = col_number(), fc_avg_break_x = col_number(),
                                      fc_avg_break_z = col_number(), fc_avg_break = col_number(),
                                      fc_range_speed = col_number(), n_fs_formatted = col_number(),
                                      fs_avg_speed = col_number(), fs_avg_break_x = col_number(),
                                      fs_avg_break_z = col_number(), fs_avg_break = col_number(),
                                      fs_range_speed = col_number(), n_kn_formatted = col_number(),
                                      kn_avg_speed = col_number(), kn_avg_break_x = col_number(),
                                      kn_avg_break_z = col_number(), kn_avg_break = col_number(),
                                      kn_range_speed = col_number(), `...69` = col_skip()), skip = 1, na = "0")

Data2017 <- read_csv("2017FullPitchData.csv", 
                     col_types = cols(player_id = col_number(), 
                                      year = col_number(), player_age = col_number(), 
                                      p_game = col_number(), p_formatted_ip = col_number(), 
                                      p_strikeout = col_number(), p_k_percent = col_number(), 
                                      p_era = col_number(), n = col_number(), 
                                      n_ff_formatted = col_number(), ff_avg_speed = col_number(), 
                                      ff_avg_spin = col_number(), ff_avg_break_x = col_number(), 
                                      ff_avg_break_z = col_number(), ff_avg_break = col_number(), 
                                      ff_range_speed = col_number(), n_sl_formatted = col_number(), 
                                      sl_avg_speed = col_number(), sl_avg_spin = col_number(), 
                                      sl_avg_break_x = col_number(), sl_avg_break_z = col_number(), 
                                      sl_avg_break = col_number(), sl_range_speed = col_number(), 
                                      n_ch_formatted = col_number(), ch_avg_speed = col_number(), 
                                      ch_avg_spin = col_number(), ch_avg_break_x = col_number(), 
                                      ch_avg_break_z = col_number(), ch_avg_break = col_number(), 
                                      ch_range_speed = col_number(), n_cukc_formatted = col_number(), 
                                      cu_avg_speed = col_number(), cu_avg_spin = col_number(), 
                                      cu_avg_break_x = col_number(), cu_avg_break_z = col_number(), 
                                      cu_avg_break = col_number(), cu_range_speed = col_number(), 
                                      n_sift_formatted = col_number(), 
                                      si_avg_speed = col_number(), si_avg_spin = col_number(), 
                                      si_avg_break_x = col_number(), si_avg_break_z = col_number(), 
                                      si_avg_break = col_number(), si_range_speed = col_number(), 
                                      n_fc_formatted = col_number(), fc_avg_speed = col_number(), 
                                      fc_avg_spin = col_number(), fc_avg_break_x = col_number(),
                                      fc_avg_break_z = col_number(), fc_avg_break = col_number(),
                                      fc_range_speed = col_number(), n_fs_formatted = col_number(),
                                      fs_avg_speed = col_number(), fs_avg_break_x = col_number(),
                                      fs_avg_break_z = col_number(), fs_avg_break = col_number(),
                                      fs_range_speed = col_number(), n_kn_formatted = col_number(),
                                      kn_avg_speed = col_number(), kn_avg_break_x = col_number(),
                                      kn_avg_break_z = col_number(), kn_avg_break = col_number(),
                                      kn_range_speed = col_number(), `...69` = col_skip()), skip = 1, na = "0")

Data2018 <- read_csv("2018FullPitchData.csv", 
                     col_types = cols(player_id = col_number(), 
                                      year = col_number(), player_age = col_number(), 
                                      p_game = col_number(), p_formatted_ip = col_number(), 
                                      p_strikeout = col_number(), p_k_percent = col_number(), 
                                      p_era = col_number(), n = col_number(), 
                                      n_ff_formatted = col_number(), ff_avg_speed = col_number(), 
                                      ff_avg_spin = col_number(), ff_avg_break_x = col_number(), 
                                      ff_avg_break_z = col_number(), ff_avg_break = col_number(), 
                                      ff_range_speed = col_number(), n_sl_formatted = col_number(), 
                                      sl_avg_speed = col_number(), sl_avg_spin = col_number(), 
                                      sl_avg_break_x = col_number(), sl_avg_break_z = col_number(), 
                                      sl_avg_break = col_number(), sl_range_speed = col_number(), 
                                      n_ch_formatted = col_number(), ch_avg_speed = col_number(), 
                                      ch_avg_spin = col_number(), ch_avg_break_x = col_number(), 
                                      ch_avg_break_z = col_number(), ch_avg_break = col_number(), 
                                      ch_range_speed = col_number(), n_cukc_formatted = col_number(), 
                                      cu_avg_speed = col_number(), cu_avg_spin = col_number(), 
                                      cu_avg_break_x = col_number(), cu_avg_break_z = col_number(), 
                                      cu_avg_break = col_number(), cu_range_speed = col_number(), 
                                      n_sift_formatted = col_number(), 
                                      si_avg_speed = col_number(), si_avg_spin = col_number(), 
                                      si_avg_break_x = col_number(), si_avg_break_z = col_number(), 
                                      si_avg_break = col_number(), si_range_speed = col_number(), 
                                      n_fc_formatted = col_number(), fc_avg_speed = col_number(), 
                                      fc_avg_spin = col_number(), fc_avg_break_x = col_number(),
                                      fc_avg_break_z = col_number(), fc_avg_break = col_number(),
                                      fc_range_speed = col_number(), n_fs_formatted = col_number(),
                                      fs_avg_speed = col_number(), fs_avg_break_x = col_number(),
                                      fs_avg_break_z = col_number(), fs_avg_break = col_number(),
                                      fs_range_speed = col_number(), n_kn_formatted = col_number(),
                                      kn_avg_speed = col_number(), kn_avg_break_x = col_number(),
                                      kn_avg_break_z = col_number(), kn_avg_break = col_number(),
                                      kn_range_speed = col_number(), `...69` = col_skip()), skip = 1, na = "0")

Data2019 <- read_csv("2019FullPitchData.csv", 
                     col_types = cols(player_id = col_number(), 
                                      year = col_number(), player_age = col_number(), 
                                      p_game = col_number(), p_formatted_ip = col_number(), 
                                      p_strikeout = col_number(), p_k_percent = col_number(), 
                                      p_era = col_number(), n = col_number(), 
                                      n_ff_formatted = col_number(), ff_avg_speed = col_number(), 
                                      ff_avg_spin = col_number(), ff_avg_break_x = col_number(), 
                                      ff_avg_break_z = col_number(), ff_avg_break = col_number(), 
                                      ff_range_speed = col_number(), n_sl_formatted = col_number(), 
                                      sl_avg_speed = col_number(), sl_avg_spin = col_number(), 
                                      sl_avg_break_x = col_number(), sl_avg_break_z = col_number(), 
                                      sl_avg_break = col_number(), sl_range_speed = col_number(), 
                                      n_ch_formatted = col_number(), ch_avg_speed = col_number(), 
                                      ch_avg_spin = col_number(), ch_avg_break_x = col_number(), 
                                      ch_avg_break_z = col_number(), ch_avg_break = col_number(), 
                                      ch_range_speed = col_number(), n_cukc_formatted = col_number(), 
                                      cu_avg_speed = col_number(), cu_avg_spin = col_number(), 
                                      cu_avg_break_x = col_number(), cu_avg_break_z = col_number(), 
                                      cu_avg_break = col_number(), cu_range_speed = col_number(), 
                                      n_sift_formatted = col_number(), 
                                      si_avg_speed = col_number(), si_avg_spin = col_number(), 
                                      si_avg_break_x = col_number(), si_avg_break_z = col_number(), 
                                      si_avg_break = col_number(), si_range_speed = col_number(), 
                                      n_fc_formatted = col_number(), fc_avg_speed = col_number(), 
                                      fc_avg_spin = col_number(), fc_avg_break_x = col_number(),
                                      fc_avg_break_z = col_number(), fc_avg_break = col_number(),
                                      fc_range_speed = col_number(), n_fs_formatted = col_number(),
                                      fs_avg_speed = col_number(), fs_avg_break_x = col_number(),
                                      fs_avg_break_z = col_number(), fs_avg_break = col_number(),
                                      fs_range_speed = col_number(), n_kn_formatted = col_number(),
                                      kn_avg_speed = col_number(), kn_avg_break_x = col_number(),
                                      kn_avg_break_z = col_number(), kn_avg_break = col_number(),
                                      kn_range_speed = col_number(), `...69` = col_skip()), skip = 1, na = "0")

Data2020 <- read_csv("2020FullPitchData.csv", 
                     col_types = cols(player_id = col_number(), 
                                      year = col_number(), player_age = col_number(), 
                                      p_game = col_number(), p_formatted_ip = col_number(), 
                                      p_strikeout = col_number(), p_k_percent = col_number(), 
                                      p_era = col_number(), n = col_number(), 
                                      n_ff_formatted = col_number(), ff_avg_speed = col_number(), 
                                      ff_avg_spin = col_number(), ff_avg_break_x = col_number(), 
                                      ff_avg_break_z = col_number(), ff_avg_break = col_number(), 
                                      ff_range_speed = col_number(), n_sl_formatted = col_number(), 
                                      sl_avg_speed = col_number(), sl_avg_spin = col_number(), 
                                      sl_avg_break_x = col_number(), sl_avg_break_z = col_number(), 
                                      sl_avg_break = col_number(), sl_range_speed = col_number(), 
                                      n_ch_formatted = col_number(), ch_avg_speed = col_number(), 
                                      ch_avg_spin = col_number(), ch_avg_break_x = col_number(), 
                                      ch_avg_break_z = col_number(), ch_avg_break = col_number(), 
                                      ch_range_speed = col_number(), n_cukc_formatted = col_number(), 
                                      cu_avg_speed = col_number(), cu_avg_spin = col_number(), 
                                      cu_avg_break_x = col_number(), cu_avg_break_z = col_number(), 
                                      cu_avg_break = col_number(), cu_range_speed = col_number(), 
                                      n_sift_formatted = col_number(), 
                                      si_avg_speed = col_number(), si_avg_spin = col_number(), 
                                      si_avg_break_x = col_number(), si_avg_break_z = col_number(), 
                                      si_avg_break = col_number(), si_range_speed = col_number(), 
                                      n_fc_formatted = col_number(), fc_avg_speed = col_number(), 
                                      fc_avg_spin = col_number(), fc_avg_break_x = col_number(),
                                      fc_avg_break_z = col_number(), fc_avg_break = col_number(),
                                      fc_range_speed = col_number(), n_fs_formatted = col_number(),
                                      fs_avg_speed = col_number(), fs_avg_break_x = col_number(),
                                      fs_avg_break_z = col_number(), fs_avg_break = col_number(),
                                      fs_range_speed = col_number(), n_kn_formatted = col_number(),
                                      kn_avg_speed = col_number(), kn_avg_break_x = col_number(),
                                      kn_avg_break_z = col_number(), kn_avg_break = col_number(),
                                      kn_range_speed = col_number(), `...69` = col_skip()), skip = 1, na = "0")

Data2021 <- read_csv("2021FullPitchData.csv", 
                     col_types = cols(player_id = col_number(), 
                                      year = col_number(), player_age = col_number(), 
                                      p_game = col_number(), p_formatted_ip = col_number(), 
                                      p_strikeout = col_number(), p_k_percent = col_number(), 
                                      p_era = col_number(), n = col_number(), 
                                      n_ff_formatted = col_number(), ff_avg_speed = col_number(), 
                                      ff_avg_spin = col_number(), ff_avg_break_x = col_number(), 
                                      ff_avg_break_z = col_number(), ff_avg_break = col_number(), 
                                      ff_range_speed = col_number(), n_sl_formatted = col_number(), 
                                      sl_avg_speed = col_number(), sl_avg_spin = col_number(), 
                                      sl_avg_break_x = col_number(), sl_avg_break_z = col_number(), 
                                      sl_avg_break = col_number(), sl_range_speed = col_number(), 
                                      n_ch_formatted = col_number(), ch_avg_speed = col_number(), 
                                      ch_avg_spin = col_number(), ch_avg_break_x = col_number(), 
                                      ch_avg_break_z = col_number(), ch_avg_break = col_number(), 
                                      ch_range_speed = col_number(), n_cukc_formatted = col_number(), 
                                      cu_avg_speed = col_number(), cu_avg_spin = col_number(), 
                                      cu_avg_break_x = col_number(), cu_avg_break_z = col_number(), 
                                      cu_avg_break = col_number(), cu_range_speed = col_number(), 
                                      n_sift_formatted = col_number(), 
                                      si_avg_speed = col_number(), si_avg_spin = col_number(), 
                                      si_avg_break_x = col_number(), si_avg_break_z = col_number(), 
                                      si_avg_break = col_number(), si_range_speed = col_number(), 
                                      n_fc_formatted = col_number(), fc_avg_speed = col_number(), 
                                      fc_avg_spin = col_number(), fc_avg_break_x = col_number(),
                                      fc_avg_break_z = col_number(), fc_avg_break = col_number(),
                                      fc_range_speed = col_number(), n_fs_formatted = col_number(),
                                      fs_avg_speed = col_number(), fs_avg_break_x = col_number(),
                                      fs_avg_break_z = col_number(), fs_avg_break = col_number(),
                                      fs_range_speed = col_number(), n_kn_formatted = col_number(),
                                      kn_avg_speed = col_number(), kn_avg_break_x = col_number(),
                                      kn_avg_break_z = col_number(), kn_avg_break = col_number(),
                                      kn_range_speed = col_number(), `...69` = col_skip()), skip = 1, na = "0")

FinalData <- rbind(Data2016, Data2017, Data2018, Data2019, Data2020, Data2021) %>% 
  group_by(player_id) %>% 
  mutate(full_player_name_id = paste(last_name, first_name, player_id, sep = ", "))

ui <- fluidPage(
  selectInput(inputId = "Player", 
              label = "Select a Player to View Stats:", 
              choices = FinalData %>% 
                arrange(full_player_name_id) %>% 
                distinct(full_player_name_id) %>% 
                pull(full_player_name_id), 
              multiple = FALSE), 
  sliderInput(inputId = "Years", 
              label = "Year Range",
              min = 2016,
              max = 2021,
              value=c(2016, 2021), 
              sep = ""),
  selectInput(inputId = "Charts", 
              label = "Select the Pitch Chart:", 
              choices = c("", "Average Speed by Pitch Type", 
                          "Pitch Frequency by Pitch Type", 
                          "Pitch Movement by Pitch Type"), 
              multiple = FALSE), 
  plotOutput(outputId = "pitchChart")
)


server <- function(input, output) {
  player_name <- reactive({
    FinalData %>% 
      filter(full_player_name_id == input$Player)
  })
  
  observeEvent(player_name(), {
    year <- player_name() %>% 
      summarise(yearMin = min(year), 
                yearMax = max(year))
    updateSliderInput(inputId = "Years", 
                      min = year$yearMin,
                      max = year$yearMax,
                      value=c(year$yearMin, year$yearMax)) 
  })
  
  player_year <- reactive({
    player_name() %>% 
      filter(min(input$Years) <= year & year <= max(input$Years))
  })
  
  
  observeEvent(player_year(), {
    if(input$Charts == "Average Speed by Pitch Type") {
      output$pitchChart <- renderPlot({
        #put pitch speed plot here
      })
    }
    
    else if(input$Charts == "Pitch Frequency by Pitch Type") {
      output$pitchChart <- renderPlot({
        #put pitch frequency chart here
      })
    }
    
    else if(input$Charts == "Pitch Movement by Pitch Type") {
      output$pitchChart <- renderPlot({
        #put pitch movement chart here
      })
    }
  })
}

shinyApp(ui = ui, server = server)