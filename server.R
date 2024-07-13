

library(shiny)



server <- function(input, output, session) {

  # LOAN?
  stat_loan <- reactive({

      if (input$loanstat == "No"){
      is_loan_input <- c(TRUE, FALSE)
    } else {
      is_loan_input <- c(FALSE)
    }

    return(is_loan_input)
  })


  # VALUE BOXES ####


  ### arrival count
  count_arr <- reactive({

    ca <- transfers_df %>%
    filter(transfer_type == "Arrivals") %>%
    filter(season %in% c(input$season_year)) %>%
    filter(is_loan %in% stat_loan()) %>%
    nrow()
    return(ca)
    })

  output$arrivals_count <- renderText({
    invalidateLater(1000)
    count_arr()
  })

  ### departure count
  count_dep <- reactive({
    cd <- transfers_df %>%
      filter(transfer_type != "Arrivals") %>%
      filter(season %in% c(input$season_year)) %>%
      filter(is_loan %in% stat_loan()) %>%
      nrow()
    return(cd)
  })

  output$departures_count <- renderText({
    invalidateLater(1000)
    count_dep()
  })

  ### arrival fee total
  sf_arr <- reactive({
    sfa <- transfers_df %>%
      filter(transfer_type == "Arrivals") %>%
      filter(season %in% c(input$season_year)) %>%
      filter(is_loan %in% stat_loan()) %>%
      summarise(total_transfer_arrivals = sum(transfer_fee)) %>% pull()
    sfa <-
      scales::number(
        sfa,
        big.mark = "",
        scale = 1/1e6,
        suffix = " M",
        prefix = "€ "
      )
    return(sfa)
  })

  output$arrivals_sumfee <- renderText({
    sf_arr()
  })

  ### departure fee total
  sf_dep <- reactive({
    sfd <- transfers_df %>%
      filter(transfer_type != "Arrivals") %>%
      filter(season %in% c(input$season_year)) %>%
      filter(is_loan %in% stat_loan()) %>%
      summarise(total_transfer_arrivals = sum(transfer_fee)) %>% pull()
    sfd <-
      scales::number(
        sfd,
        big.mark = "",
        scale = 1/1e6,
        suffix = " M",
        prefix = "€ "
        )
    return(sfd)
  })

  output$departures_sumfee <- renderText({
    sf_dep()
  })



  # MAP ####


  transfers_transformations <- reactive({
    top10country_byfee_arrivals1 <-
      transfers_df %>%
      filter(
        season %in% c(input$season_year),
        transfer_type == "Arrivals"
      ) %>%
      filter(is_loan %in% stat_loan()) %>%
      group_by(country_2) %>%
      summarise(sum_fee = sum(transfer_fee)) %>%
      ungroup() %>%
      arrange(desc(sum_fee)) %>%
      slice_head(n = 10)

    top10country_byfee_departures1 <-
      transfers_df %>%
      filter(
        season %in% c(input$season_year),
        transfer_type == "Departures"
      ) %>%
      filter(is_loan %in% stat_loan()) %>%
      group_by(country_2) %>%
      summarise(sum_fee = sum(transfer_fee)) %>%
      ungroup() %>%
      arrange(desc(sum_fee)) %>%
      slice_head(n = 10)

    top10country_byfee_arrivals2 <-
      top10country_byfee_arrivals1 %>%
      left_join(transfers_df %>% filter(transfer_type=="Arrivals"), by = "country_2") %>%
      select(country_2, centroid_longitude, centroid_latitude, transfer_type, sum_fee) %>% distinct()

    top10country_byfee_departures2 <-
      top10country_byfee_departures1 %>%
      left_join(transfers_df %>% filter(transfer_type=="Departures"), by = "country_2") %>%
      select(country_2, centroid_longitude, centroid_latitude, transfer_type, sum_fee) %>% distinct()

    top10country_byfee <-
      bind_rows(top10country_byfee_arrivals2, top10country_byfee_departures2) %>%
      arrange(desc(sum_fee))

    # if(input$season_year != 0){
    #   return(top10country_byfee)
    # } else {
    #   return(print(input$season_year))
    # }

    return(top10country_byfee)

    })



  # prep data for leaflet
  ## create a new dataframe with starting long lat and ending long lat.
  top_map <-
    reactive({

      count_country <- transfers_transformations() %>% select(country_2) %>% n_distinct() - 1
      df4lines_1 <-
        transfers_transformations()[transfers_transformations()$country_2=="England", c("country_2","centroid_longitude", "centroid_latitude")] %>% distinct()
      df4lines_1 <- df4lines_1[rep(1, count_country),]
      ## rename
      df4lines_1 <-
        rename_with(df4lines_1, ~ gsub("centroid", "start", .x)) %>%
        rename_with(~ gsub("_2", "", .x))
      ## prepare for the end long lat
      df4lines_2 <-
        transfers_transformations() %>%
        filter(country_2 != "England") %>%
        select(1, 2, 3) %>%
        distinct() %>%
        rename_with(~ gsub("centroid", "end", .x))
      ## combine them
      comb_df4lines <- bind_cols(df4lines_1, df4lines_2)

      country_polylines <-
        generate_add_polylines_code(
          data = comb_df4lines,
          start_lng_col = "start_longitude",
          start_lat_col = "start_latitude",
          end_lng_col = "end_longitude",
          end_lat_col = "end_latitude"
        )
      second_chunk <- paste0(country_polylines, collapse = "\n")
      # That will be the second chunk of the code. Let's go back to the first
      # chunk. It is the code for transfer map with added pipe and wrapped as a
      # single string.

      first_chunk <-
        "leaflet(data = transfers_transformations()) %>%
        addTiles() %>%
        addMarkers(
          lng = ~centroid_longitude,
          lat = ~centroid_latitude,
          label = ~country_2,
          icon = ~tf_icons[transfer_type]
        ) %>% "

      combined_chunks <-
        paste0(first_chunk, second_chunk, collapse = "\n")

      # eval the combined chunk of texts as one code group to generate code for map

      top_country_map <- eval(parse(text = combined_chunks))

      return(top_country_map)
  })

  output$top_map <- renderLeaflet({

    if(is.null(input$season_year) || length(input$season_year) == 0){
      plot.new()
      title("No options selected")
    } else {
      top_map()
    }

  })


  # PLOT TRANSFER COUNT BREAKDOWN ####


  ptfbs <- reactive({

    df <-
      transfers_df %>%
      filter(season %in% c(input$season_year)) %>%
      filter(transfer_type %in% c("Arrivals", "Departures")) %>%
      filter(is_loan %in% stat_loan()) %>%
      mutate(is_loan = ifelse(is_loan == T, "LOANS", "NOT LOANS")) %>%
      group_by(season, is_loan) %>%
      mutate(sumtf = sum(transfer_fee))

    group_color <- c("#9B9B4D", "#5F4B8B")
    names(group_color) <- c("Arrivals", "Departures")

    xlab <- "Season"
    ylab <- "Number of Transfers"
    tlab <- "Transfer Count by Season"
    stlab <- glue::glue(
      "On <span style= 'color:{\"#9B9B4D\"}'>**Arrivals** </span>and <span style='color:{\"#5F4B8B\"}'>**Departures** </span>")

    theme_setting <-
      theme_light(base_size = 11)+
      theme(
        plot.title.position = "panel",
        plot.subtitle = ggtext::element_markdown(), # used to let the html style in title to be colored
        title = element_text(size = rel(1.5)),
        strip.text.x.top = element_text(size = rel(1.4), face = "bold", colour = "black"),
        axis.line.x = element_blank(),
      )

    p_tfcount_byseason <-
      ggplot(df, aes(x = season, fill = transfer_type))+
      geom_histogram(stat = "count", position = "dodge", show.legend = F)+
      facet_wrap(vars(is_loan))+
      labs(x = xlab, y = ylab, title = tlab, subtitle =  stlab)+
      scale_fill_manual(values = group_color)+
      theme_setting

    return(p_tfcount_byseason)
  })

  output$plot_tfbs <- renderPlot({

    if(is.null(input$season_year) || length(input$season_year) == 0){
      plot.new()
      title("No options selected")
    } else {
      ptfbs()
    }
     })


  # UNCHECK BUTTON ####

    observeEvent(input$unc_season, {
      updateCheckboxGroupInput(session = session,inputId = "season_year", selected = character(0))
    })


  # PLOT COUNT BY LOAN ####

  ptfbl <- reactive({

    df <-
      transfers_df %>%
      filter(season %in% c(input$season_year)) %>%
      filter(transfer_type %in% c("Arrivals", "Departures")) %>%
      group_by(is_loan) %>%
      summarise(count = n(), sum_fee = sum(transfer_fee))

    xlab <- "Is This Transfer a Loan?"
    ylab <- "Number of Transfers"
    tlab <- "Count of Transfer by Loan"
    group_color <- c("#e6f2fd", "#40739e")

    theme_setting <-
      theme_light(base_size = 11)+
      theme(
        plot.title.position = "panel",
        title = element_text(size = rel(1.5))
      )

    plot_tfbl <- ggplot(
      data = df,
      aes(x = is_loan, y = count, fill = group_color)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = tlab , x = xlab, y = ylab)+
      scale_fill_manual(values = group_color)+
      coord_flip()+
      theme_setting

    if(stat_loan() == TRUE){
      return(plot_tfbl)
    } else {
      return(NULL)
    }


  })

  output$plot_tfbl <- renderPlot({ ptfbl() })



  # PLOT CLUB COMPARISON ####

  dfsf <- reactive({
    df_season_filtered <-
      transfers_df %>%
      filter(season %in% c(input$season_year))
    return(df_season_filtered)
  })

  # render UI for choosing team based on selected season ####
  output$teamInput <- renderUI({
    selectizeInput(
      inputId = "club_name",
      label = "Choose club:",
      selected = dfsf()$team_name %>% unique() %>% sample(2),
      choices =  dfsf()$team_name %>% unique(),
      multiple = T,
      options = list(maxItems = 6)
    )
  })

  dfsncf <- reactive({
    if (input$loanstat == "No"){
      is_loan_input <- c(TRUE, FALSE)
    } else {
      is_loan_input <- c(FALSE)
    }

    df_season_n_club_filtered <-
      dfsf() %>%
      filter(team_name %in% c(input$club_name)) %>%
      filter(transfer_type %in% c(input$arrdep)) %>%
      filter(is_loan %in% is_loan_input) %>%
    return(df_season_n_club_filtered)
  })

  dfccf <- reactive({
    df_club_count_fee <-
      dfsncf() %>%
      group_by(team_name, transfer_type) %>%
      summarise(totfee = sum(transfer_fee),
                maxfee = max(transfer_fee),
                avgfee = round(mean(transfer_fee), 2),
                minfee = min(transfer_fee),
                tcount = n())
    return(df_club_count_fee)
  })

  plot_ccf <- reactive({
    xt <- "Number of Transfers"
    yt <- "Total Transfer Fee"
    mt <- "Transfer Fee Aggregates by Club"
    st <- glue::glue(
      "On <span style= 'color:{\"#9B9B4D\"}'>**Arrivals** </span>and <span style='color:{\"#5F4B8B\"}'>**Departures** </span>")

    group_color <- c("#9B9B4D", "#5F4B8B")
    names(group_color) <- c("Arrivals", "Departures")

    theme_setting <-
      theme_light(base_size = 12)+
      theme(
        plot.title.position = "panel",
        plot.subtitle = ggtext::element_markdown(),
        title = element_text(size = rel(1.3))
      )

    pcc <- dfccf() %>%
      mutate(
        maxfee = scales::number(
          maxfee, scale = 1/1e6, suffix = " M"),
        avgfee = scales::number(
          avgfee, scale = 1/1e6, suffix = " M")
      ) %>%
      mutate(
        tooltip_label = glue(
          "
          Highest {transfer_type} Fee: {maxfee}
          Average {transfer_type} Fee: {avgfee}
          "
        )
      ) %>%
      ggplot(
        aes(
          x = tcount,
          y = totfee,
          col = transfer_type,
          size = avgfee))+
      geom_point_interactive(
        position = position_jitterdodge(),
        show.legend = F,
        aes(tooltip = tooltip_label, data_id = transfer_type))+
      labs(title = mt, subtitle = st, x = xt, y = yt)+
      facet_wrap(vars(team_name))+
      scale_color_manual(values = group_color)+
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(space = T)))+
      theme_setting

    gpcc <- girafe(
      ggobj = pcc,
      options = list(
        opts_hover(css = ""),
        opts_hover_inv(css = "opacity:0.1;")
      )

    )
    return(gpcc)
  })


  output$ccfp <- renderGirafe({
    plot_ccf()
  })



  # PLOT TOP 10 PLAYERS ####

  plot_top10players <- reactive({
    df_dir <-
      dfsncf() %>%
      arrange(desc(transfer_fee)) %>%
      head(n = 10) %>%
      mutate(tf_direction = ifelse(
        transfer_type == "Departures", transfer_fee * -1, transfer_fee
      ))

    xt <- "Transfer Fee"
    yt <- ""
    mt <- "Top 10 Player Transfer"


    theme_setting <-
      theme_light(base_size = 4)+
      theme(
        plot.title.position = "panel",
        plot.subtitle = ggtext::element_markdown(),
        title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.8))
      )

    plot_dir <-
      df_dir %>%
      mutate(
        transfer_fee_label = scales::number(
          transfer_fee, scale = 1/1e6, suffix = " M")
      ) %>%
      ggplot(
        aes(
          x = tf_direction,
          y = reorder(player_name, transfer_fee),
          fill = team_name
        )
      ) +
      geom_col_interactive(
        alpha = 0.55,
        aes(tooltip = team_name, data_id = team_name)) +
      geom_text(
        aes(
          label = player_name,
          hjust = ifelse(tf_direction > 0, 1.05, -0.05),
        )
      )+
      geom_text(
        aes(
          label = transfer_fee_label,
          hjust = ifelse(tf_direction > 0, -0.25, 1.2)
        )
      )+
      theme_setting+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()

      )+
      labs(y = yt, title = mt, x = xt, fill = "Team")+
      coord_cartesian(
        xlim =
          c(
            min(df_dir$tf_direction)-0.4*mean(df_dir$transfer_fee),
            max(df_dir$tf_direction)+0.4*mean(df_dir$transfer_fee))
      )+
      scale_fill_manual(values = club_colors)

    gplotdir <- girafe(
      ggobj = plot_dir,
      options = list(
        opts_hover(css = ""),
        opts_hover_inv(css = "opacity:0.05")
      ))
    return(gplotdir)
  })
  output$t10p <- renderGirafe({ plot_top10players() })

  # PLOT: POSITION SHOPPINGS TRENDS ####

  dfpos <- reactive({
    df_pos <-
      dfsncf() %>%
      group_by(player_position, transfer_type, team_name) %>%
      summarize(total_fee = sum(transfer_fee),
                total_transfer = n()) %>%
      ungroup() %>%
      arrange(desc(total_fee)) %>%
      mutate(
        total_fee_label = scales::number(
          total_fee, scale = 1/1e6, suffix = " M"),
        total_transfer = glue::glue("{total_transfer} Transfers"))
    return(df_pos)
  })

  plotpos <- reactive({
    xt <- "Player Position"
    yt <- "Total Transfer Fee"
    mt <- "Transfer Activity by Player Position"
    st <- glue::glue(
      "On <span style= 'color:{\"#9B9B4D\"}'>**Arrivals** </span>and <span style='color:{\"#5F4B8B\"}'>**Departures** </span>")

    group_color <- c("#9B9B4D", "#5F4B8B")
    names(group_color) <- c("Arrivals", "Departures")

    theme_setting <-
      theme_light(base_size = 11)+
      theme(
        plot.title.position = "panel",
        plot.subtitle = ggtext::element_markdown(),
        title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.3)),
        panel.spacing = unit(x = 0.5, units =  "cm"),
        axis.text.x = element_blank()
      )

    pp <-
      dfpos() %>%
      mutate(
        tooltip_label = glue(
          "
          {total_fee_label} for {total_transfer}
          "
        )
      ) %>%
      ggplot(
      aes(y = player_position, x = total_fee, fill = transfer_type))+
      geom_col_interactive(
        position = position_stack(),
        show.legend = F,
        aes(tooltip = tooltip_label, data_id = transfer_type))+
      facet_wrap(. ~ team_name)+
      labs(title = mt, x = xt, y = yt, subtitle = st) +
      scale_fill_manual(values = group_color)+
      theme_minimal() +
      theme_setting+
      coord_cartesian(
        xlim =
          c(
            min(0 - (1 * median(dfpos()$total_fee))),
            max(dfpos()$total_fee)+ 5*mean(dfpos()$total_fee))
      )+
      scale_x_continuous(labels = label_number(scale = 1/1e6, suffix = "M"))+
      # add margin between position
      theme(axis.text.y = element_text(margin = margin(t = 45, b = 45), size = 9))

    gpp <- girafe(
      ggobj = pp,
      options = list(
        opts_hover(css = ""),
        opts_hover_inv(css = "opacity:0.1;")
      ))
    return(gpp)
  })

  output$plot_pp <- renderGirafe({
    plotpos()
  })

  # VB2 ####
  # dfsncf() is dataframe that has been filtered with season, club,  ttype, and isloan
  ### arrival count
  clubcount_arr <- reactive({
    n_arr <- dfsncf() %>%
      filter(transfer_type == "Arrivals") %>%
      nrow()
    return(n_arr)
  })
  output$club_arrivals_count <- renderText({
    invalidateLater(1000)
    clubcount_arr()
    })

  ### departure count
  clubcount_dep <- reactive({
    ccd <- dfsncf() %>%
      filter(transfer_type != "Arrivals") %>%
      nrow()
    return(ccd)
  })

  output$club_departures_count <- renderText({
    invalidateLater(1000)
    clubcount_dep()
  })

  ### arrival fee total
  csf_arr <- reactive({
    csfa <- dfsncf() %>%
      filter(transfer_type == "Arrivals") %>%
      summarise(total_transfer_arrivals = sum(transfer_fee)) %>% pull()
    csfa <-
      scales::number(
        csfa,
        big.mark = "",
        scale = 1/1e6,
        suffix = " M",
        prefix = "€ "
      )
    return(csfa)
  })

  output$club_arrivals_sumfee <- renderText({
    csf_arr()
  })

  ### departure fee total
  csf_dep <- reactive({
    csfd <- dfsncf() %>%
      filter(transfer_type != "Arrivals") %>%
      summarise(total_transfer_arrivals = sum(transfer_fee)) %>% pull()
    csfd <-
      scales::number(
        csfd,
        big.mark = "",
        scale = 1/1e6,
        suffix = " M",
        prefix = "€ "
      )
    return(csfd)
  })

  output$club_departures_sumfee <- renderText({
    csf_dep()
  })

  # TABLE  ####
  output$tf_table <- renderDataTable({

    table_filtered <-
    transfers_df %>%
      filter(season %in% c(input$season_year)) %>%
      filter(is_loan %in% stat_loan()) %>%
      filter(
        player_age >= input$age_slider[1] &
        player_age <= input$age_slider[2])

    return(table_filtered)
  })
}

