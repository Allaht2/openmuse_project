# Shiny application for census dashboard

# Loading the packages needed for the app
library(shiny)
library(ggplot2)
library(haven)
library(dplyr)
library(bslib)
library(leaflet)
library(tidyr)
library(patchwork)
# library(thematic)
# library(ragg)

# Loading the data
census_data_all <- read_sav("combined_census_data.sav") %>%
  filter(exclude == 0)
pre_snapshot <- read_sav("pre_snapshot.sav")
post_snapshot <- read_sav("post_snapshot.sav")

# Adding duplicate of the data for all cities plots
all_cities_data_census <- census_data_all %>%
  mutate(City = "All Cities")
census_data_all <- bind_rows(census_data_all, all_cities_data_census)
all_cities_data_pre <- pre_snapshot %>%
  mutate(City = "All Cities")
pre_snapshot <- bind_rows(pre_snapshot, all_cities_data_pre)

# The color used by the barplots
global_colors <- ("#007CBB")

# options(shiny.useragg = TRUE)
#
# thematic_shiny(font = "auto")

# Defining the ui for the application
ui <- page_fillable(

  tags$head(HTML("<title>OpenMusE Dashboard</title>"),
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

  layout_columns(

    layout_columns(

      card(

        card_header("Input"),

        fluidRow(

          column(5,

            shinyWidgets::pickerInput("city", "City:",
                          c("Helsinki", "Lviv", "Heidelberg", "Mannheim", "Vilnius", "All Cities"),
                          multiple = TRUE, selected = "Helsinki",
                          options = list(container = "body"))
          ),
          column(5,

            shinyWidgets::pickerInput("gender", "Gender:", c("Female" , "Male", "Another" = "Another gender identity"),
                        multiple = TRUE, selected = "Female",
                        options = list(container = "body"))
          )
        )
    ),

      card(

        full_screen = TRUE,

        card_header("Audience Data Plots"),

        mainPanel(

          titlePanel("Gender distribution"),

          plotOutput("Gender", width = "150%", height = "600px")

        ),

        mainPanel(

          titlePanel("Age distribution"),

          plotOutput("Age", width = "150%")

        ),

        mainPanel(

          titlePanel("Do you live in the city area?"),

          plotOutput("CityArea", width = "150%", height = "600px")

        ),

        mainPanel(

          titlePanel("Total spending"),

          tableOutput("Spending_table"),

          plotOutput("Spending", width = "150%")

        ),

        titlePanel("Spending on: "),

        shinyWidgets::pickerInput(inputId = "spend_category", label = "", choices = c(
          "Ticket/entry" = "Q6_1", "Transportation" = "Q6_2",
          "Food/drink" = "Q6_3", "Merchandise" = "Q6_4",
          "Accommondation" = "Q6_5"), multiple = TRUE,
          selected = c("Ticket/entry" = "Q6_1")),

        mainPanel(

          tableOutput("Spending_on_table"),

          plotOutput("Spending_on", width = "150%")

        ),

        mainPanel(

          titlePanel("Will you / did you eat at restaurants?"),

          plotOutput("Restaurant", width = "150%", height = "650px")

        ),

        mainPanel(

          titlePanel("How will you / did you get home?"),

          plotOutput("Transport", width = "150%")

        ),

        mainPanel(

          titlePanel("How was your night?"),

          plotOutput("Night", width = "150%", height = "650px")

        ),

        mainPanel(

          titlePanel("How would you rate the live music scene in your city?"),

          plotOutput("Scene", width = "150%", height = "650px")

        ),

        mainPanel(

          titlePanel("What kinds of music do you most often go to see live?"),

          plotOutput("MusicType", width = "150%")

        ),

        mainPanel(

          titlePanel("How many live music events do you attend per month, on average"),

          plotOutput("Attend", width = "150%", height = "650px")

        ),

        mainPanel(

          titlePanel("What size of venue you most enjoy seeing live music?"),

          plotOutput("VenueSize", width = "150%", height = "650px")

        ),

        mainPanel(

          titlePanel("What has prevented from seeing live music events more often"),

          plotOutput("Prevent", width = "150%")

        ),

        mainPanel(

          titlePanel("Which of the following would encourage you to see more live music?"),

          plotOutput("Encourage", width = "150%")

        ),

        mainPanel(

          titlePanel("Do you agree with the following statement?"),

          plotOutput("Statement", width = "150%", height = "800px")

        ),

      ),

      col_widths = c(12,24),
      row_heights = c(1,3)
    ),

    layout_columns(

        # nav_panel(title = "Venue Map",
        #
        #           shinyWidgets::pickerInput("city_coord", "City:",
        #               c("Helsinki" = "60.192,24.946",
        #                 "Lviv" = "49.843,24.031",
        #                 "Heidelberg" = "49.408,8.691",
        #                 "Mannheim" = "49.489,8.469",
        #                 "Vilnius" = "54.687,25.280"), selected = "Helsinki"),
        #
        #   card_header("Map"),
        #
        #   leafletOutput("venuemap")
        #
        # ),

      card(

        full_screen = TRUE,

        card_header("Venue Data Plots"),

      # Start venue plots here

      # Pre snapshot plots

      mainPanel(

        titlePanel("On which days is the venue open?"),

        plotOutput("Operation_days", width = "150%")

      ),

      mainPanel(

        titlePanel("Is your venue negatively affected by?"),

        plotOutput("Negative_effects", width = "150%", height = "500px")

      ),

      mainPanel(

        titlePanel("Music venues and festivals have a responsibility \n to become environmentally friendly"),

        plotOutput("Envi_friendly", width = "150%", height = "600px")

      ),

      mainPanel(

        titlePanel("The live music scene in my city has discrimination problems"),

        plotOutput("Discri_prob", width = "150%", height = "600px")

      ),

      mainPanel(

        titlePanel("The live music scene in my city has corruption problems"),

        plotOutput("Corrupt_prob", width = "150%", height = "600px")

      ),

      mainPanel(

        titlePanel("Are you optimistic about the future of your venue?"),

        plotOutput("Optimistic", width = "150%", height = "600px")

      ),

      # Post snapshot plots

      # End here
      )
    )
    ),

  # theme = bs_theme()

  )

# Define server logic
server <- function(input, output) {

  # # custom
  # bs_themer()

  # Number of answers per data
  # census_n <- reactive({
  #   census_n <- census_data_all %>%
  #   haven::as_factor() %>%
  #   filter(City %in% input$city, Q2 %in% input$gender) %>%
  #   group_by(City) %>%
  #   summarise(N = n())
  # })

  census_n2 <- reactive({
    census_n2 <- census_data_all %>%
      haven::as_factor() %>%
      filter(Q2 %in% input$gender) %>%
      mutate(Q6 = if_any(starts_with("Q6"), ~!is.na(.)),
             Q8 = if_any(starts_with("Q8"), ~!is.na(.)),
             Q12 = if_any(starts_with("Q12"), ~!is.na(.)),
             Q15 = if_any(starts_with("Q15"), ~!is.na(.)),
             Q17 = if_any(starts_with("Q17"), ~!is.na(.))) %>%
      mutate(across(where(is.logical), ~ifelse(. == TRUE, 1, NA))) %>%
      group_by(City) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      select(City, num_range("Q", range = 1:30)) %>%
      select(-Q21, -Q18, -Q10, -Q19) %>%
      setNames(c("City", paste0(names(.)[-1], "_n")))
  })


  # pre_snapshot_n <- reactive({
  #   pre_snapshot_n <- pre_snapshot %>%
  #   haven::as_factor() %>%
  #   filter(City %in% input$city) %>%
  #   group_by(City) %>%
  #   summarise(N = n())
  # })

  pre_snapshot_n2 <- reactive({
    pre_snapshot_n2 <- pre_snapshot %>%
      select(-Q14_15_TEXT) %>%
      mutate(Q8 = if_any(starts_with("Q8"), ~!is.na(.)),
             Q9 = if_any(starts_with("Q9"), ~!is.na(.)),
             Q12 = if_any(starts_with("Q12"), ~!is.na(.)),
             Q14 = if_any(starts_with("Q14"), ~!is.na(.))) %>%
      mutate(across(where(is.logical), ~ifelse(. == TRUE, 1, NA))) %>%
      group_by(City) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      select(City, num_range("Q", range = 1:30), Q15_1, Q15_2, Q15_3) %>%
      select(City, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15_1, Q15_2, Q15_3, Q18) %>%
      setNames(c("City", paste0(names(.)[-1], "_n")))
  })

  # Census data
  gender_data <- reactive({
    gender_data <- census_data_all %>%
      haven::as_factor() %>%
      mutate(Q2 = case_when(Q2 == "Another gender identity" ~ "Another",
                            TRUE ~ Q2)) %>%
      filter(City %in% input$city) %>%
      count(Q2, City) %>%
      na.omit() %>%
      arrange(Q2)
  })

  age_data <- reactive({
    age_data <- census_data_all %>%
      select(Age, City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      mutate(
        Age_group = case_when(
          Age > 17 & Age < 30 ~ "18-29",
          Age > 29 & Age < 40 ~ "30-39",
          Age > 39 & Age < 50 ~ "40-49",
          Age > 49 & Age < 60 ~ "50-59",
          Age > 59 & Age < 70 ~ "60-69",
          Age > 69 ~ "70+"
        )) %>%
      na.omit() %>%
      count(Age_group, City)
  })

  city_area <- reactive({
    city_area <- census_data_all %>%
      select(City, Q2, Q4) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q4, City) %>%
      na.omit() %>%
      # mutate(Q4 = case_when(is.na(Q4) ~ "No Answer",
      #                       TRUE ~ Q4),
      #        Q4 = factor(Q4, levels = c("No", "Yes", "No Answer"))) %>%
      arrange(Q4)
  })

  music_type <- reactive({
    music_type <- census_data_all %>%
      select(starts_with("Q12"), City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      select(-Q2) %>%
      pivot_longer(!City, names_to = "question", values_to = "value") %>%
      na.omit %>%
      count(value, City)
    # na_count <- census_data_all %>%
    #   select(starts_with("Q12"), City, Q2) %>%
    #   haven::as_factor() %>%
    #   filter(City %in% input$city, Q2 %in% input$gender) %>%
    #   group_by(City) %>%
    #   mutate(Q12 = if_all(starts_with("Q12"), ~is.na(.))) %>%
    #   summarise('No Answer' = sum(Q12)) %>%
    #   pivot_longer(!City, names_to = "value", values_to = "n")
    # music_type <- bind_rows(music_type, na_count)
  })

  statement_data <- reactive({
    statement_data <- census_data_all %>%
      select(starts_with("Q17"), City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      select(-Q2) %>%
      pivot_longer(!City, names_to = "question", values_to = "value") %>%
      na.omit %>%
      count(value, City)
    # na_count <- census_data_all %>%
    #   select(starts_with("Q17"), City, Q2) %>%
    #   haven::as_factor() %>%
    #   filter(City %in% input$city, Q2 %in% input$gender) %>%
    #   group_by(City) %>%
    #   mutate(Q17 = if_all(starts_with("Q17"), ~is.na(.))) %>%
    #   summarise('No Answer' = sum(Q17)) %>%
    #   pivot_longer(!City, names_to = "value", values_to = "n")
    # statement_data <- bind_rows(statement_data, na_count)
  })

  spending_data <- reactive({
    spending_data <- census_data_all %>%
      select(starts_with("Q6"), City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("Q6")))) %>%
      select(sums, City) %>%
      na.omit()
  })

  spending_on_data <- reactive({
    spending_on_data <- census_data_all %>%
      select(input$spend_category, City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("Q6")))) %>%
      select(sums, City) %>%
      na.omit()
  })

  restaurant_data <- reactive({
    restaurant_data <- census_data_all %>%
      select(City, Q2, Q7) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q7, City) %>%
      na.omit() %>%
      # mutate(Q7 = case_when(is.na(Q7) ~ "No Answer",
      #                       TRUE ~ Q7),
      #        Q7 = factor(Q7, levels = c("No", "Yes", "No Answer"))) %>%
      arrange(Q7)
  })

  transport_data <- reactive({
    transport_data <- census_data_all %>%
      select(starts_with("Q8"), City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      select(-Q2) %>%
      pivot_longer(!City, names_to = "transport", values_to = "value") %>%
      na.omit() %>%
      count(value, City)
    # na_count <- census_data_all %>%
    #   select(starts_with("Q8"), City, Q2) %>%
    #   haven::as_factor() %>%
    #   filter(City %in% input$city, Q2 %in% input$gender) %>%
    #   group_by(City) %>%
    #   mutate(Q8 = if_all(starts_with("Q8"), ~is.na(.))) %>%
    #   summarise('No Answer' = sum(Q8)) %>%
    #   pivot_longer(!City, names_to = "value", values_to = "n")
    # transport_data <- bind_rows(transport_data, na_count)
  })

  night_data <- reactive({
    night_data <- census_data_all %>%
      select(City, Q2, Q9) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q9, City) %>%
      na.omit() %>%
      # mutate(Q9 = case_when(is.na(Q9) ~ "No Answer",
      #                       TRUE ~ Q9),
      #        Q9 = factor(Q9, levels = c("Very bad", "Bad", "Average", "Good", "Very good", "No Answer"))) %>%
      arrange(Q9)
  })

  scene_data <- reactive({
    scene_data <- census_data_all %>%
      select(City, Q2, Q11) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q11, City) %>%
      na.omit() %>%
      # mutate(Q11 = case_when(is.na(Q11) ~ "No Answer",
      #                       TRUE ~ Q11),
      #        Q11 = factor(Q11, levels = c("Very bad", "Bad", "Average", "Good", "Very good", "No Answer"))) %>%
      arrange(Q11)
  })

  attend_data <- reactive({
    attend_data <- census_data_all %>%
      select(City, Q2, Q13) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q13, City) %>%
      na.omit() %>%
      # mutate(Q13 = case_when(is.na(Q13) ~ "No Answer",
      #                       TRUE ~ Q13),
      #        Q13 = factor(Q13, levels = c("0 - 1", "2 - 3", "4 - 5", "More than 6", "No Answer"))) %>%
      arrange(Q13)
  })

  venue_size_data <- reactive({
    venue_size_data <- census_data_all %>%
      haven::as_factor() %>%
      mutate(Q14 = case_when(Q14 == "Very small (less than 200)" ~ "Very small (< 200)",
                             TRUE ~ Q14)) %>%
      select(City, Q2, Q14) %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q14, City) %>%
      na.omit() %>%
      # mutate(Q14 = case_when(is.na(Q14) ~ "No Answer",
      #                       TRUE ~ Q14),
      #        Q14 = factor(Q14, levels = c("Very small (< 200)", "Small (200 - 699)",
      #                                     "Medium (700 - 1999)", "Large (2000 or more)", "No Answer"))) %>%
      arrange(Q14)
  })

  prevent_data <- reactive({
    prevent_data <- census_data_all %>%
      select(starts_with("Q15"), City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      select(-Q2) %>%
      pivot_longer(!City, names_to = "transport", values_to = "value") %>%
      na.omit() %>%
      count(value, City)
    # na_count <- census_data_all %>%
    #   select(starts_with("Q15"), City, Q2) %>%
    #   haven::as_factor() %>%
    #   filter(City %in% input$city, Q2 %in% input$gender) %>%
    #   group_by(City) %>%
    #   mutate(Q15 = if_all(starts_with("Q15"), ~is.na(.))) %>%
    #   summarise('No Answer' = sum(Q15)) %>%
    #   pivot_longer(!City, names_to = "value", values_to = "n")
    # prevent_data <- bind_rows(prevent_data, na_count)
  })

  encourage_data <- reactive({
    encourage_data <- census_data_all %>%
      select(Q16, City, Q2) %>%
      haven::as_factor() %>%
      filter(City %in% input$city, Q2 %in% input$gender) %>%
      count(Q16, City) %>%
      # mutate(Q16 = case_when(is.na(Q16) ~ "No Answer",
      #                        TRUE ~ Q16))
      na.omit()
  })

  # Pre snapshot data
  operation_days_data <- reactive({
    operation_days <- pre_snapshot %>%
      select(starts_with("Q8"), City) %>%
      haven::as_factor() %>%
      pivot_longer(!City, names_to = "question", values_to = "value") %>%
      na.omit %>%
      filter(City %in% input$city) %>%
      count(value, City)
    # na_count <- pre_snapshot %>%
    #   select(starts_with("Q8"), City) %>%
    #   haven::as_factor() %>%
    #   filter(City %in% input$city) %>%
    #   group_by(City) %>%
    #   mutate(Q8 = if_all(starts_with("Q8"), ~is.na(.))) %>%
    #   summarise('No Answer' = sum(Q8)) %>%
    #   pivot_longer(!City, names_to = "value", values_to = "n")
    # operation_days <- bind_rows(operation_days, na_count) %>%
    #   mutate(value = factor(value, levels = c("Everyday", "Monday", "Tuesday", "Wednesday",
    #                                           "Thursday", "Friday", "Saturday", "Sunday", "No Answer"))) %>%
    #   arrange(value)
  })

  negative_effect_data <- reactive({
    negative_effect <- pre_snapshot %>%
      select(starts_with("Q14"), City) %>%
      select(!Q14_15_TEXT) %>%
      haven::as_factor() %>%
      pivot_longer(!City, names_to = "effect", values_to = "value") %>%
      na.omit %>%
      filter(City %in% input$city) %>%
      count(value, City)
    # na_count <- census_data_all %>%
    #   select(starts_with("Q14"), City) %>%
    #   haven::as_factor() %>%
    #   filter(City %in% input$city) %>%
    #   group_by(City) %>%
    #   mutate(Q14 = if_all(starts_with("Q14"), ~is.na(.))) %>%
    #   summarise('No Answer' = sum(Q14)) %>%
    #   pivot_longer(!City, names_to = "value", values_to = "n")
    # negative_effect <- bind_rows(negative_effect, na_count)
  })

  envi_friendly_data <- reactive({
    envi_friendly <- pre_snapshot %>%
      select(Q15_1, City) %>%
      haven::as_factor() %>%
      filter(City %in% input$city) %>%
      count(Q15_1, City) %>%
      na.omit %>%
      # mutate(Q15_1 = case_when(is.na(Q15_1) ~ "No Answer",
      #                        TRUE ~ Q15_1),
      #        Q15_1 = factor(Q15_1, levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree",
      #                                     "Strongly disagree", "I don't know", "No Answer"))) %>%
      arrange(Q15_1)
  })

  discri_prob_data <- reactive({
    discri_prob <- pre_snapshot %>%
      select(Q15_2, City) %>%
      haven::as_factor() %>%
      filter(City %in% input$city) %>%
      count(Q15_2, City) %>%
      na.omit %>%
      # mutate(Q15_2 = case_when(is.na(Q15_2) ~ "No Answer",
      #                          TRUE ~ Q15_2),
      #        Q15_2 = factor(Q15_2, levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree",
      #                                         "Strongly disagree", "I don't know", "No Answer"))) %>%
      arrange(Q15_2)
  })

  corrupt_prob_data <- reactive({
    corrupt_prob <- pre_snapshot %>%
      select(Q15_3, City) %>%
      haven::as_factor() %>%
      filter(City %in% input$city) %>%
      count(Q15_3, City) %>%
      na.omit %>%
      # mutate(Q15_3 = case_when(is.na(Q15_3) ~ "No Answer",
      #                          TRUE ~ Q15_3),
      #        Q15_3 = factor(Q15_3, levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree",
      #                                         "Strongly disagree", "I don't know", "No Answer"))) %>%
      arrange(Q15_3)
  })

  optimistic_data <- reactive({
    optimistic <- pre_snapshot %>%
      select(Q18, City) %>%
      haven::as_factor() %>%
      filter(City %in% input$city) %>%
      count(Q18, City) %>%
      na.omit %>%
      # mutate(Q18 = case_when(is.na(Q18) ~ "No Answer",
      #                          TRUE ~ Q18),
      #        Q18 = factor(Q18, levels = c("Yes", "No", "I don't know", "No Answer"))) %>%
      arrange(Q18)
  })

  # Post snapshot data

  # Census plots
  output$Gender <- renderPlot({
    gender_plot_data <- gender_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q2, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q2 <- unique(gender_plot_data$Q2)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q2)), unique_q2)
    # Plots
    p <- ggplot(gender_plot_data, aes(x = "", y = n, fill = Q2)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(gender_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q2) * 0.05, color = Q2), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q2) * 0.05, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      facet_wrap(~City, ncol = 1) + scale_color_manual(values = color_palette)
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Age <- renderPlot({
    age_plot_data <- age_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # lim <- max(age_plot_data$n)
    ggplot(age_plot_data, aes(x = Age_group, y = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(y = n - n/5, label = paste0(n, "\n", " (", round(age_plot_data$percentage, 1), "%)")),
                vjust = -0.1, size = 2.8) +
      labs(x = "Age group", y = "Count") + facet_wrap(~City2, ncol = 3, scales = "free_y") + # ylim(0, lim + 10) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) + coord_cartesian(clip = "off")
  })

  output$CityArea <- renderPlot({
    city_area_plot_data <- city_area() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q4, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q4 <- unique(city_area_plot_data$Q4)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q4)), unique_q4)
    # Plots
    p <- ggplot(city_area_plot_data, aes(x = "", y = n, fill = Q4)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(city_area_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q4) * 0.05, color = Q4), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q4) * 0.05, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      facet_wrap(~City, ncol = 1) + scale_color_manual(values = color_palette)
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })


  output$MusicType <- renderPlot({
    music_type_plot_data <- music_type() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q12_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q12_n))
    # lim <- max(music_type_plot_data$n)
    ggplot(music_type_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 3.1) +
      labs(x = "Count", y = "Category") + # xlim(0, lim + 20) +
      facet_wrap(~City2, ncol = 3, scales = "free_x") +
      coord_cartesian(clip = "off")
  })

  output$Statement <- renderPlot({
    statement_plot_data <- statement_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q17_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q17_n))
    # lim <- max(statement_plot_data$n)
    ggplot(statement_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 3.1) +
      labs(x = "Count", y = "Statement") + # xlim(0, lim + 30) +
      facet_wrap(~City2, ncol = 2, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(60)) + coord_cartesian(clip = "off")
  })

  output$Spending <- renderPlot({
    spending_plot_data <- spending_data() %>%
      left_join(census_n2(), by = "City") %>%
      mutate(City2 = paste0(City, "\nN = ", Q6_n))
    ggplot(spending_plot_data, aes(x = sums, y = after_stat(density))) +
      geom_histogram(bins = 20) + geom_density(size = 1.5, color = "red") +
      labs(x = "Spending") + facet_wrap(~City2, ncol = 3)

  })

  output$Spending_table <- renderTable({
    spending_data() %>%
      group_by(City) %>%
      summarise(mean = mean(sums), median = median(sums))
  })

  output$Spending_on <- renderPlot({
    spending_on_plot_data <- spending_on_data() %>%
      left_join(census_n2(), by = "City") %>%
      mutate(City2 = paste0(City, "\nN = ", Q6_n))
    ggplot(spending_on_plot_data, aes(x = sums, y = after_stat(density))) +
      geom_histogram(bins = 20) + geom_density(size = 1.5, color = "red") +
      labs(x = "Spending") + facet_wrap(~City2, ncol = 3)
  })

  output$Spending_on_table <- renderTable({
    spending_on_data() %>%
      group_by(City) %>%
      summarise(mean = mean(sums), median = median(sums))
  })

  output$Restaurant <- renderPlot({
    restaurant_plot_data <- restaurant_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q7, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q7 <- unique(restaurant_plot_data$Q7)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q7)), unique_q7)
    # Plots
    p <- ggplot(restaurant_plot_data, aes(x = "", y = n, fill = Q7)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(restaurant_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q7) * 0.05, color = Q7), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q7) * 0.05, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      facet_wrap(~City, ncol = 1) + scale_color_manual(values = color_palette)
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Transport <- renderPlot({
    transport_plot_data <- transport_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q8_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q8_n))
    # lim <- max(transport_plot_data$n)
    ggplot(transport_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                size = 3.1, hjust = 0.1) +
      labs(x = "Count", y = "Mode of transport") + #  xlim(0, lim + 10) +
      coord_cartesian(clip = "off") +
      scale_y_discrete(labels = scales::label_wrap(50)) +
      facet_wrap(~City2, ncol = 3, scales = "free_x")
  })

  output$Night <- renderPlot({
    night_plot_data <- night_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q9, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q9 <- unique(night_plot_data$Q9)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q9)), unique_q9)
    # Plots
    p <- ggplot(night_plot_data, aes(x = "", y = n, fill = Q9)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(night_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q9) * 0.03, color = Q9), size = 3.7, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q9) * 0.03, label = label), hjust = 0, size = 3.2) +
      theme_void() + xlim(0, 4.5) + ylim(-1, 0) + theme(legend.position = "none") +
      scale_color_manual(values = color_palette) + facet_wrap(~City, ncol = 1)
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Scene <- renderPlot({
    scene_plot_data <- scene_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q11, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q11 <- unique(scene_plot_data$Q11)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q11)), unique_q11)
    # Plots
    p <- ggplot(scene_plot_data, aes(x = "", y = n, fill = Q11)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(scene_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q11) * 0.03, color = Q11), size = 3.7, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q11) * 0.03, label = label), hjust = 0, size = 3.2) +
      theme_void() + xlim(0, 4.5) + theme(legend.position = "none") +
      ylim(-1,0) + scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1) + coord_cartesian(clip = "off")
    p + p_text + plot_layout(ncol = 2, widths = c(2,1))
  })

  output$Attend <- renderPlot({
    attend_plot_data <- attend_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q13, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q13 <- unique(attend_plot_data$Q13)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q13)), unique_q13)
    # Plots
    p <- ggplot(attend_plot_data, aes(x = "", y = n, fill = Q13)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(attend_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q13) * 0.03, color = Q13), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q13) * 0.03, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      scale_color_manual(values = color_palette) + facet_wrap(~City, ncol = 1)
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$VenueSize <- renderPlot({
    venue_size_plot_data <- venue_size_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q14, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q14 <- unique(venue_size_plot_data$Q14)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q14)), unique_q14)
    # Plots
    p <- ggplot(venue_size_plot_data, aes(x = "", y = n, fill = Q14)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(venue_size_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q14) * 0.03, color = Q14), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q14) * 0.03, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      scale_color_manual(values = color_palette) + facet_wrap(~City, ncol = 1) +
      coord_cartesian(clip = "off")
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Prevent <- renderPlot({
    prevent_plot_data <- prevent_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q15_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q15_n))
    # lim <- max(prevent_plot_data$n)
    ggplot(prevent_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 3.1) +
      labs(x = "Count", y = "Reason") + # xlim(0, lim + 20) +
      facet_wrap(~City2, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off")
  })

  output$Encourage <- renderPlot({
    encourage_plot_data <- encourage_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q16_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q16_n))
    # lim <- max(encourage_plot_data$n)
    ggplot(encourage_plot_data, aes(y = Q16, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 3.1) +
      labs(x = "Count", y = "Reason") + # xlim(0, lim + 20) +
      facet_wrap(~City2, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off")
  })

  # Pre snapshot plots
  output$Operation_days <- renderPlot({
    operation_days <- operation_days_data() %>%
      group_by(City) %>%
      left_join(pre_snapshot_n2(), by = "City") %>%
      mutate(percentage = n / Q8_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q8_n))
    # lim <- max(operation_days$n)
    ggplot(operation_days, aes(x = value, y = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(y = n - n/5, label = paste0(n, "\n", "(", round(percentage, 1), "%)")),
                vjust = -0.1, size = 2.8) +
      labs(x = "Weekday", y = "Count") +
      facet_wrap(~City2, ncol = 3, scales = "free_y") + coord_cartesian(clip = "off") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) # +  ylim(0, lim + 5)
  })

  output$Negative_effects <- renderPlot({
    negative_effects_plot_data <- negative_effect_data() %>%
      group_by(City) %>%
      left_join(pre_snapshot_n2(), by = "City") %>%
      mutate(percentage = n / Q14_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q14_n))
    # lim <- max(negative_effects_plot_data$n)
    ggplot(negative_effects_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 3.1) +
      labs(x = "Count", y = "Effect") + # xlim(0, lim + 10) +
      facet_wrap(~City2, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(50)) + coord_cartesian(clip = "off")
  })

  output$Envi_friendly <- renderPlot({
    envi_friendly_plot_data <- envi_friendly_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q15_1, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color map
    unique_q15_1 <- unique(envi_friendly_plot_data$Q15_1)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q15_1)), unique_q15_1)
    # Plots
    p <- ggplot(envi_friendly_plot_data, aes(x = "", y = n, fill = Q15_1)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(envi_friendly_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q15_1) * 0.06, color = Q15_1), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q15_1) * 0.06, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      facet_wrap(~City, ncol = 1) + scale_color_manual(values = color_palette) #+
      #coord_cartesian(clip = "off")
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Discri_prob <- renderPlot({
    discri_prob_plot_data <- discri_prob_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q15_2, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q15_2 <- unique(discri_prob_plot_data$Q15_2)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q15_2)), unique_q15_2)
    # Plots
    p <- ggplot(discri_prob_plot_data, aes(x = "", y = n, fill = Q15_2)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(discri_prob_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q15_2) * 0.045, color = Q15_2), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q15_2) * 0.045, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      facet_wrap(~City, ncol = 1) + scale_color_manual(values = color_palette) # +
      # coord_cartesian(clip = "off")
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Corrupt_prob <- renderPlot({
    corrupt_prob_plot_data <- corrupt_prob_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q15_3, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q15_3 <- unique(corrupt_prob_plot_data$Q15_3)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q15_3)), unique_q15_3)
    # Plots
    p <- ggplot(corrupt_prob_plot_data, aes(x = "", y = n, fill = Q15_3)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(corrupt_prob_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q15_3) * 0.05, color = Q15_3), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q15_3) * 0.05, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      scale_color_manual(values = color_palette) + facet_wrap(~City, ncol = 1) #+
      # coord_cartesian(clip = "off")
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  output$Optimistic <- renderPlot({
    optimistic_plot_data <- optimistic_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q18, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q18 <- unique(optimistic_plot_data$Q18)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q18)), unique_q18)
    # Plots
    p <- ggplot(optimistic_plot_data, aes(x = "", y = n, fill = Q18)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) + theme(legend.position = "none")
    p_text <- ggplot(optimistic_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q18) * 0.05, color = Q18), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q18) * 0.05, label = label), hjust = 0) +
      theme_void() + xlim(0, 2.5) + ylim(-1, 0) + theme(legend.position = "none") +
      scale_color_manual(values = color_palette) + facet_wrap(~City, ncol = 1) #+
      # coord_cartesian(clip = "off")
    p + p_text + plot_layout(ncol = 2, widths = c(2, 1))
  })

  # Post snapshot plots

  # Venue map output
  # output$venuemap <- renderLeaflet({
  #
  #   coord <- input$city_coord %>%
  #     strsplit(split = ",") %>%
  #     unlist() %>%
  #     as.numeric()
  #
  #   leaflet() %>%
  #     addProviderTiles(providers$CartoDB.Positron,
  #                      options = providerTileOptions(noWrap = TRUE)) %>%
  #     setView(lng = coord[2], lat = coord[1], zoom = 10)
  # })

}

# Run the application
shinyApp(ui = ui, server = server)
