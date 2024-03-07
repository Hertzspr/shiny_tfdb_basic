library(shiny)
library(bslib)
library(leaflet)
library(scales)
library(ggiraph)
library(htmltools)
library(glue)
library(shinyjs)
library(tidyverse)


# DATA RELATED ####
transfers <- readRDS(file = "transfers_df.RDS")
df_long_lat <- readRDS(file = "df_long_lat.RDS")


transfers_df <- transfers %>% left_join(df_long_lat, by = c("country_2" = "c2_name"), keep = F)
tibble(

)

# LEAFLET RELATED ####

tf_icons <-
  iconList(
    Arrivals = makeIcon(
      iconUrl = "in.png",
      iconWidth = 35,
      iconHeight = 35,
      iconRetinaUrl = "in@2x.png"),
    Departures = makeIcon(
      iconUrl = "out.png",
      iconWidth = 35,
      iconHeight = 35,
      iconRetinaUrl = "out@2x.png")
  )

# generate polylines code ####

generate_add_polylines_code <-
  function(
    data,
    start_lng_col,
    start_lat_col,
    end_lng_col,
    end_lat_col) {

    code_lines <- c()
    for (i in 1:(nrow(data))-1) {

      line_code <- paste0(
        "addPolylines(lng = c(",
        data[i, start_lng_col],
        ", ",
        data[i, end_lng_col],
        "), lat = c(",
        data[i, start_lat_col],
        ", ",
        data[i, end_lat_col],
        ")) %>% ")

      line_code_end <- paste0(
        "addPolylines(lng = c(",
        data[nrow(data), start_lng_col],
        ", ",
        data[nrow(data), end_lng_col],
        "), lat = c(",
        data[nrow(data), start_lat_col],
        ", ",
        data[nrow(data), end_lat_col],
        ")) ")

      code_lines[i] <- line_code
      final_code <- c(code_lines, line_code_end)
    }
    return(final_code)
  }


# club colors ####

club_colors <- c(
  "Chelsea FC" = "royalblue",
  "Manchester City" = "skyblue",
  "Manchester United" = "red",
  "Arsenal FC" = "firebrick",
  "Liverpool FC" = "darkred",
  "Tottenham Hotspur" = "darkblue",
  "Everton FC" = "royalblue4",
  "Newcastle United" = "black",
  "Fulham FC" = "darkseagreen",
  "West Ham United" = "darkred",
  "Aston Villa" = "darkred",
  "Crystal Palace" = "darkorange",
  "Burnley FC" = "darkred",
  "AFC Bournemouth" = "red4",
  "Brighton & Hove Albion" = "darkblue",
  "Wolverhampton Wanderers" = "gold",
  "Sheffield United" = "black",
  "Brentford FC" = "darkred",
  "Nottingham Forest" = "darkgreen",
  "Luton Town" = "orange"
)


