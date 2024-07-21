seasonInput <- checkboxGroupInput(width = "100%",
  inline = T,
  inputId = "season_year",
  label = NULL,
  selected = transfers_df$season %>% unique(),
  choices = transfers_df$season %>% unique()
)

uncheckSeasonInput <- actionButton(inputId = "unc_season", "Uncheck Seasons")

sidebar_season <- card(
  card_header("Select Season"),
  seasonInput,
  uncheckSeasonInput
)


sidebar_loan_opt <- card(
  radioButtons(
    "loanstat",
    label = "Exclude Loans?",
    choices = c("Yes", "No"),
    selected = "No",
    inline = T
  )
)

arr_dep <- checkboxGroupInput(
  inputId = "arrdep",
  label = "Transfers Type",
  choices = c("Arrivals", "Departures"),
  selected = c("Arrivals", "Departures")
)

sidebar_club <- card(
  min_height = "1200px",
  card_header("Compare Up to Six Clubs"),
  uiOutput(outputId = "teamInput",fill = T), # sourced from server from renderUI
 # arr_dep
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
    title = "Total Arrivals Fee",
    value = textOutput(outputId = "arrivals_sumfee"),
    theme = bslib::value_box_theme(bg = "#e6f2fd", fg = "#9B9B4d")
  ),
  value_box(
    title = "Total Departures Fee",
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
    title = "Total Arrivals Fee",
    value = textOutput(outputId = "club_arrivals_sumfee"),
    theme = bslib::value_box_theme(bg = "#e6f2fd", fg = "#9B9B4d")
  ),
  value_box(
    title = "Total Departures Fee",
    value = textOutput(outputId = "club_departures_sumfee"),
    theme = bslib::value_box_theme(bg = "#5F4B8B", fg = "#e6f2fd")
  )
)


age_slider <-
  card(
    sliderInput(
      inputId = "age_slider",
      label = h4("Age Range"),
      min = min(transfers_df$player_age, na.rm = T),
      max = max(transfers_df$player_age, na.rm = T),
      value = c(min(transfers_df$player_age, na.rm = T), max(transfers_df$player_age, na.rm = T))
      )
  )

fee_slider <-
  card(
    sliderInput(step = 4,
      inputId = "fee_slider",
      label = h4("Fee Range"),
      min = min(transfers_df$transfer_fee),
      max = max(transfers_df$transfer_fee),
      value = c(
        min(transfers_df$transfer_fee),
        max(transfers_df$transfer_fee)
        )
    )
  )
