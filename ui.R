source("uiHelpers.R")

ui <- page_navbar(
  id = "nav",
  title = "Premier League Transfers Dashboard",
  fillable = F,
  sidebar = sidebar(
    sidebar_season, # select season
    sidebar_loan_opt, # exclude loans?
    conditionalPanel(
      condition = "input.nav == 'Teams'", sidebar_club)
  ),
  nav_spacer(),
  nav_panel(
    title = "Transfers Overview",

    # value boxes
    layout_column_wrap(
      fill = FALSE,
      width = "150px",
      !!!vbs
    ),

    # map
    card(
      div(class = "card-header", "Top 10 Transfers Traffic by Country"),
      leafletOutput("top_map"),
      min_height = "400px"
      ),

    # plot tf by  season and loan
    card(
      fill = T,
      plotOutput("plot_tfbs")
  ),
),

  nav_panel(
    title = "Teams",


    layout_column_wrap(
      fill = FALSE,
      width = "150px",
      !!!vbs_club
    ),

    # plot: transfers fee agg by club
    card(
      girafeOutput("ccfp")
    ),
    card(fill = T,
         girafeOutput("plot_pp")
    ),

    # plot: top 10 players
    card(
      girafeOutput("t10p")
    )

  ),


  nav_panel(
    "Table",
    card(full_screen = T,
      dataTableOutput("tf_table")
    )
  )




)






