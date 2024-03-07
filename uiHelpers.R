seasonInput <- selectInput(
  inputId = "season_year",
  label = "Choose season:",
  selected = transfers_df$season %>% unique() %>% sample(size = 1),
  multiple = T,
  choice = transfers_df$season %>% unique()
)

sidebar_season <- card(
  min_height = "300px",
  #full_screen = TRUE,
  card_header("Select Season"),
  seasonInput
)

arr_dep <- checkboxGroupInput(
  inputId = "arrdep",
  label = "Transfers Type",
  choices = c("Arrivals", "Departures"),
  selected = c("Arrivals", "Departures")
)

sidebar_club <- card(
  min_height = "300px",
  card_header("Compare Up to Six Clubs"),
  uiOutput(outputId = "teamInput"), # sourced from server from renderUI
  arr_dep
)



vbs <- list(
  value_box(
    title = "Arrivals Count",
    value = textOutput(outputId = "arrivals_count"),
    theme = bslib::value_box_theme(bg = "#e6f2fd", fg = "#9B9B4D")
  ),
  value_box(
    title = "Departures Count",
    value = textOutput(outputId = "departures_count"),
    theme = bslib::value_box_theme(bg = "#5F4B8B", fg = "#e6f2fd")
  ),
  value_box(
    title = "Sum Arrivals Fee",
    value = textOutput(outputId = "arrivals_sumfee"),
    theme = bslib::value_box_theme(bg = "#e6f2fd", fg = "#9B9B4d")
  ),
  value_box(
    title = "Sum Departures Fee",
    value = textOutput(outputId = "departures_sumfee"),
    theme = bslib::value_box_theme(bg = "#5F4B8B", fg = "#e6f2fd")
  )
)


vbs_club <- list(
  value_box(
    title = "Arrivals Count",
    value = textOutput(outputId = "club_arrivals_count"),
    theme = bslib::value_box_theme(bg = "#e6f2fd", fg = "#9B9B4D")
  ),
  value_box(
    title = "Departures Count",
    value = textOutput(outputId = "club_departures_count"),
    theme = bslib::value_box_theme(bg = "#5F4B8B", fg = "#e6f2fd")
  ),
  value_box(
    title = "Sum Arrivals Fee",
    value = textOutput(outputId = "club_arrivals_sumfee"),
    theme = bslib::value_box_theme(bg = "#e6f2fd", fg = "#9B9B4d")
  ),
  value_box(
    title = "Sum Departures Fee",
    value = textOutput(outputId = "club_departures_sumfee"),
    theme = bslib::value_box_theme(bg = "#5F4B8B", fg = "#e6f2fd")
  )
)
