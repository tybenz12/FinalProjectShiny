library(shiny)
library(tidyverse)
library(lubridate)
library(rsconnect)
library(zoo)
library(dplyr)
library(ggthemes)

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
  choices <- 
  selectInput(inputId = "Player", 
              label = "Select a Player to View Stats:", 
              choices = FinalData %>% 
                arrange(full_player_name_id) %>% 
                distinct(full_player_name_id) %>% 
                pull(full_player_name_id), 
              multiple = FALSE), 
  selectInput(inputId = "Years", 
              label = "Select the Year",
              choices = c(2016, 2017, 2018, 2019, 2020, 2021), 
              multiple = FALSE),
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
    years <- player_name() %>% 
      arrange(year) %>% 
      distinct(year) %>% 
      pull(year)
    updateSelectInput(inputId = "Years", choices = years) 
  })
  
  player_year <- reactive({
    player_name() %>% 
      filter(input$Years == year)
  })
  
  toListen <- reactive({
    c(input$Years, input$Charts)
  })
  
  observeEvent(toListen(), {
    FFData <- player_year() %>%
      select(c(1:12), contains("ff")) %>%
      add_column(Pitch_Type = "Four-Seam Fastball") %>%
      mutate(pitch_frequency = n_ff_formatted, 
             avg_speed = ff_avg_speed, 
             avg_spin = ff_avg_spin, 
             avg_break_x = ff_avg_break_x, 
             avg_break_z = ff_avg_break_z, 
             avg_break = ff_avg_break, 
             range_speed = ff_range_speed) %>%
      select(-contains("ff"))
    
    SliderData <- player_year() %>%
      select(c(1:12), contains("sl")) %>%
      add_column(Pitch_Type = "Slider") %>%
      mutate(pitch_frequency = n_sl_formatted, 
             avg_speed = sl_avg_speed, 
             avg_spin = sl_avg_spin, 
             avg_break_x = sl_avg_break_x, 
             avg_break_z = sl_avg_break_z, 
             avg_break = sl_avg_break, 
             range_speed = sl_range_speed) %>%
      select(-contains("sl"))
    
    ChangeData <- player_year() %>%
      select(c(1:12), contains("ch")) %>%
      add_column(Pitch_Type = "Changeup") %>%
      mutate(pitch_frequency = n_ch_formatted, 
             avg_speed = ch_avg_speed, 
             avg_spin = ch_avg_spin, 
             avg_break_x = ch_avg_break_x, 
             avg_break_z = ch_avg_break_z, 
             avg_break = ch_avg_break, 
             range_speed = ch_range_speed) %>%
      select(-c(13:19))
    
    CurveData <- player_year() %>%
      select(c(1:12), contains("cu")) %>%
      add_column(Pitch_Type = "Curveball") %>%
      mutate(pitch_frequency = n_cukc_formatted, 
             avg_speed = cu_avg_speed, 
             avg_spin = cu_avg_spin, 
             avg_break_x = cu_avg_break_x, 
             avg_break_z = cu_avg_break_z, 
             avg_break = cu_avg_break, 
             range_speed = cu_range_speed) %>%
      select(-contains("cu"))
    
    SinkerData <- player_year() %>%
      select(c(1:12), contains("si")) %>%
      add_column(Pitch_Type = "Sinker") %>%
      mutate(pitch_frequency = n_sift_formatted, 
             avg_speed = si_avg_speed, 
             avg_spin = si_avg_spin, 
             avg_break_x = si_avg_break_x, 
             avg_break_z = si_avg_break_z, 
             avg_break = si_avg_break, 
             range_speed = si_range_speed) %>%
      select(-contains("si"))
    
    CutterData <- player_year() %>%
      select(c(1:12), contains("fc")) %>%
      add_column(Pitch_Type = "Cutter") %>%
      mutate(pitch_frequency = n_fc_formatted, 
             avg_speed = fc_avg_speed, 
             avg_spin = fc_avg_spin, 
             avg_break_x = fc_avg_break_x, 
             avg_break_z = fc_avg_break_z, 
             avg_break = fc_avg_break, 
             range_speed = fc_range_speed) %>%
      select(-contains("fc"))
    
    SplitterData <- player_year() %>%
      select(c(1:12), contains("fs")) %>%
      add_column(Pitch_Type = "Splitter") %>%
      mutate(pitch_frequency = n_fs_formatted, 
             avg_speed = fs_avg_speed, 
             avg_spin = fs_avg_spin, 
             avg_break_x = fs_avg_break_x, 
             avg_break_z = fs_avg_break_z, 
             avg_break = fs_avg_break, 
             range_speed = fs_range_speed) %>%
      select(-contains("fs"))
    
    KnuckleData <- player_year() %>%
      select(c(1:12), contains("kn")) %>%
      add_column(Pitch_Type = "Knuckleball") %>%
      mutate(pitch_frequency = n_kn_formatted, 
             avg_speed = kn_avg_speed, 
             avg_spin = kn_avg_spin, 
             avg_break_x = kn_avg_break_x, 
             avg_break_z = kn_avg_break_z, 
             avg_break = kn_avg_break, 
             range_speed = kn_range_speed) %>%
      select(-contains("kn"))
    
    TidyPitchData <- bind_rows(FFData, SliderData, ChangeData, CurveData, SinkerData, CutterData, SplitterData, KnuckleData)
    if(input$Charts == "Average Speed by Pitch Type") {
      output$pitchChart <- renderPlot({
        TidyPitchData %>%
          na.omit() %>%
          mutate(max_speed = avg_speed + 0.5*(range_speed),
                 min_speed = avg_speed - 0.5*(range_speed)) %>%
          mutate(mph_speed = str_c(avg_speed, " mph")) %>%
          ggplot() +
          geom_linerange(aes(y = Pitch_Type, xmin = min_speed, xmax = max_speed, color = avg_speed), size=5) + 
          geom_linerange(aes(y = 0, xmin = 59, xmax = 65), color = "#FFFF00", size=5) +
          geom_linerange(aes(y = 0, xmin = 65, xmax = 75), color = "#FFCC00", size=5) +
          geom_linerange(aes(y = 0, xmin = 75, xmax = 85), color = "#FF9933", size=5) +
          geom_linerange(aes(y = 0, xmin = 85, xmax = 95), color = "#FF3333", size=5) +
          geom_linerange(aes(y = 0, xmin = 95, xmax = 105), color = "#990000", size=5) +
          geom_point(aes(x = avg_speed, y = Pitch_Type), size=2, shape=21, fill="white") + 
          binned_scale(aesthetics = "color",
                       scale_name = "stepsn", 
                       palette = function(x) c("#FFFF00", "#FFCC00", "#FF9933", "#FF3333", "#990000"),
                       breaks = c(59, 65, 75, 85, 95, 105),
                       limits = c(59, 105),
                       show.limits = TRUE, 
                       guide = "colorsteps") +
          geom_text(aes(x = max_speed, y = Pitch_Type, label = mph_speed), 
                    hjust = -0.5, 
                    size = 3, 
                    color = "black",
                    fontface = "bold",
                    nudge_x = .5) +
          labs(title = "Average Pitch Speed", 
               x = "Pitch Speed (mph)",
               y = NULL) + 
          scale_x_continuous(expand = expansion(mult = c(0, .1))) +
          theme_classic() + 
          theme(legend.position = "none", 
                panel.grid.major.y = element_line(color = "lightgray", size = 0.5, linetype = "dotted"), 
                axis.text = element_text(colour = "black"))
      })
    }
    
    else if(input$Charts == "Pitch Frequency by Pitch Type") {
      output$pitchChart <- renderPlot({
        TidyPitchData %>%
          na.omit() %>%
          mutate(pitch_percent = str_c(pitch_frequency, "%")) %>%
          ggplot(aes(x = pitch_frequency, y = fct_reorder(Pitch_Type, pitch_frequency, mean, .desc=FALSE), fill = Pitch_Type)) +
          geom_col() + 
          geom_text(aes(label = str_c(pitch_percent, Pitch_Type, sep = "\n")), 
                    hjust = 0, 
                    size = 3, 
                    color = "black",
                    fontface = "bold",
                    nudge_x = .5) + 
          labs(title = "Pitch Type Frequency", 
               x = "Frequency (%)", 
               y = "") + 
          scale_x_continuous(expand = expansion(mult = c(0, .65))) + 
          theme_classic() + 
          theme(axis.text.y = element_blank(), legend.position = "none") 
      })
    }
    
    else if(input$Charts == "Pitch Movement by Pitch Type") {
      output$pitchChart <- renderPlot({
        TidyPitchData %>% 
          na.omit() %>%
          ggplot() +
          geom_point(aes(x = avg_break_x, 
                         y = avg_break_z,
                         color = avg_speed,
                         shape = Pitch_Type,
                         size = 8)) +
          geom_text(aes(x = avg_break_x, 
                        y = avg_break_z, 
                        label = Pitch_Type), 
                    hjust = -0.5) + 
          scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 8, 10)) +
          scale_color_gradient(low = "orange", high = "red") +
          scale_size(guide = "none") +
          labs(shape = "Pitch Type", 
               color = "Pitch Speed (mph)",
               title = "Average Pitch Movement",
               x = "Horizontal Break (inches)\nView from Catcher POV",
               y = "Vertical Break (inches)") + 
          scale_x_continuous(expand = expansion(mult = c(.15, .25))) +
          theme_classic() + 
          theme(legend.position="bottom", 
                panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = "dotted"), 
                panel.grid.major.y = element_line(color = "lightgray", size = 0.5, linetype = "dotted")) + 
          guides(shape = "none")
      })
    }
  })
}

shinyApp(ui = ui, server = server)