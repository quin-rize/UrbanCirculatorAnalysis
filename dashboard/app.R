# Urban Circulators -- Shiny app
#
# Lets a user switch between St. Louis, Kansas City, and Cincinnati, see
# an interactive map (with a street basemap and a yearly time slider) of
# home values along that city's circulator route, and a home-value trend
# line over the full monthly history.
#
# This app does NOT hit the network or recompute anything itself -- it
# just loads the plots/data that zip_cleaning.R already built and saved
# to shiny_app_data.rds. Run zip_cleaning.R first (or whenever the
# underlying Zillow/ArcGIS data should be refreshed), then run this file.
#
# Required packages: shiny, bslib, plotly, leaflet, leaflegend, sf, dplyr,
# scales
# (ggplot2 is needed to build shiny_app_data.rds, but not to run the app
# itself -- the trend chart is a pre-built ggplot object, and the map is
# built fresh each time from combined_data using leaflet, not ggplot2.)

library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(leaflegend)
library(sf)
library(dplyr)

app_data <- readRDS("shiny_app_data.rds")
city_data <- app_data$cities
year_value_cols <- app_data$year_value_cols # tibble(year, col), one row per year

city_choices <- setNames(
  names(city_data),
  vapply(city_data, function(city) city$label, character(1))
)

year_min <- min(year_value_cols$year)
year_max <- max(year_value_cols$year)

ui <- page_sidebar(
  title = "Urban Circulators: Transit Lines & Home Values",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    selectInput("city", "City", choices = city_choices, selected = city_choices[[1]]),
    hr(),
    p(
      "Median home values (Zillow ZHVI) for zip codes near three U.S. ",
      "streetcar/trolley lines, compared against nearby zips without one."
    ),
    p(
      class = "text-muted small",
      "Zip codes the circulator actually runs through are outlined in ",
      "red on the map and drawn in warm colors on the trend chart."
    )
  ),
  navset_card_tab(
    nav_panel(
      "Map",
      card(
        full_screen = TRUE,
        card_header(textOutput("map_title")),
        sliderInput(
          "year", "Year",
          min = year_min, max = year_max, value = year_max,
          step = 1, sep = "", animate = animationOptions(interval = 1200, loop = FALSE)
        ),
        leafletOutput("map", height = "550px")
      )
    ),
    nav_panel(
      "Home Value Trend",
      card(
        full_screen = TRUE,
        card_header(textOutput("trend_title")),
        plotlyOutput("trend", height = "500px")
      )
    ),
    nav_panel(
      "About",
      card(
        card_header("About this project"),
        markdown(
          "This app accompanies an analysis of whether new urban circulator
          transit lines (streetcars/trolleys) are associated with changes in
          nearby home values, using Zillow's Zip Code Home Value Index
          (ZHVI) and zip-level boundary data from Esri's ArcGIS Living
          Atlas.

          Three circulator lines are compared: the **Delmar Loop Trolley**
          (St. Louis, MO), the **KC Streetcar** (Kansas City, MO), and the
          **Cincinnati Connector** (Cincinnati, OH). For each city, home
          values in the zip codes the line runs through are compared
          against nearby zip codes without a circulator.

          The map shows each year's December (year-end) home value; the
          trend chart shows the full monthly history.

          Full methodology and a difference-in-differences write-up
          accompany this app in the project repository."
        )
      )
    )
  )
)

server <- function(input, output, session) {
  selected_city <- reactive({
    req(input$city)
    city_data[[input$city]]
  })

  output$map_title <- renderText({
    paste(selected_city()$label, "- Home Values Along the Route")
  })

  output$trend_title <- renderText({
    paste(selected_city()$label, "- Home Value Trend")
  })

  # Base map: basemap tiles + initial view only. Polygons are added/
  # updated separately below via leafletProxy(), so switching years
  # doesn't rebuild the whole map widget -- just swaps the polygon layer.
  output$map <- renderLeaflet({
    city <- selected_city()
    leaflet(options = leafletOptions(minZoom = 9)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(city$xlim[1], city$ylim[1], city$xlim[2], city$ylim[2])
  })

  # Fixed per-city color domain, spanning every year's values -- not just
  # whichever year is currently selected. Pegging this to the city's own
  # min/max (and only recomputing when the city changes, not the year)
  # is what makes toggling years show real appreciation: a zip's color
  # now reflects its value against a constant scale, instead of the
  # scale silently re-stretching to fit each year's own range.
  city_color_domain <- reactive({
    city <- selected_city()
    all_values <- unlist(lapply(year_value_cols$col, function(col) city$combined_data[[col]]))
    range(all_values, na.rm = TRUE)
  })

  # Re-draws the choropleth polygons whenever the selected city or year
  # changes. Also fires once on startup (regular reactive observers run
  # as soon as their dependencies are available), so the map isn't empty
  # before the user touches anything.
  observe({
    city <- selected_city()
    req(input$year)

    value_col <- year_value_cols$col[year_value_cols$year == input$year]
    req(length(value_col) == 1)

    zips <- city$combined_data
    values <- zips[[value_col]]
    domain <- city_color_domain()
    pal <- colorNumeric("viridis", domain = domain, na.color = "#cccccc")
    is_route <- as.character(zips$ZIP_CODE) %in% city$route_zips

    leafletProxy("map") %>%
      clearGroup("zips") %>%
      clearGroup("route") %>%
      clearControls() %>%
      addPolygons(
        data = zips,
        group = "zips",
        fillColor = pal(values),
        fillOpacity = 0.75,
        color = ifelse(is_route, "red", "white"),
        weight = ifelse(is_route, 3, 1),
        label = paste0(
          as.character(zips$ZIP_CODE), ": ",
          ifelse(is.na(values), "no data", scales::label_dollar()(values))
        )
      ) %>%
      addPolylines(data = city$route_line, group = "route", color = "#FFAE00", weight = 3) %>%
      # addLegend() defaults to lowest value at top, which reads
      # backwards for a "more valuable = higher" mental model.
      # leaflegend::addLegendNumeric() supports decreasing = TRUE to put
      # the highest value at top; values = domain (the fixed range, not
      # this year's own values) keeps the legend itself from rescaling
      # as you move the slider, matching the pegged fill scale above.
      addLegendNumeric(
        pal = pal, values = domain, decreasing = TRUE,
        position = "bottomright", title = "Home Value (USD)",
        orientation = "vertical", shape = "rect",
        width = 20, height = 140,
        numberFormat = scales::label_dollar(scale_cut = scales::cut_short_scale())
      )
  })

  output$trend <- renderPlotly({
    # tooltip = "text" tells plotly to show only the clean, pre-built
    # tooltip_text string (see build_trend_plot() in zip_cleaning.R)
    # instead of its default dump of every mapped aesthetic.
    ggplotly(selected_city()$trend_plot, tooltip = "text")
  })
}

shinyApp(ui, server)
