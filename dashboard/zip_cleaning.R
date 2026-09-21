library(tidyverse)
library(sf)
library(janitor)
library(arcgislayers)
# Run this script with its own folder (/dashboard) as the working
# directory -- e.g. open the repo's .Rproj and run from here, or in
# RStudio use Session > Set Working Directory > To Source File Location.
# Source data (the Zillow export, stl_zips.csv) is read from ../data;
# everything this script builds (mo_combined.rds, shiny_app_data.rds,
# etc.) is written back into this folder, alongside app.R.
# data from:
# https://www.zillow.com/research/data/
# https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_ZIP_Code_Areas_anaylsis/FeatureServer/0
#   (this is the live feature service behind the "USA ZIP Code Boundaries"
#   ArcGIS Hub item, id d6f7ee6129e241cc9b6f75978e47128b -- the Hub "explore"
#   page doesn't offer a stable direct shapefile download, and the full
#   national layer is 1+ GB, which is the real reason the local .shp kept
#   being a hassle)
# https://www.ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?zip=631
# https://www.zip-codes.com/city/mo-kansas-city.asp
#Shape file with neighborhood coordinate information.
# rgdal + broom::tidy() on sp objects are retired/deprecated (rgdal was
# sunset in 2023). And rather than re-downloading a giant national
# shapefile and managing its .shp/.shx/.dbf sidecar files locally, we
# query the live ArcGIS Feature Service directly for just the state we
# need. arcgislayers handles the request, pagination, and reprojection,
# and returns an sf object -- same ZIP_CODE/PO_NAME/STATE/OBJECTID
# columns the rest of this script already expects. 
zip_service_url <- paste0(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/",
  "USA_ZIP_Code_Areas_anaylsis/FeatureServer/0"
)
zip_layer <- arc_open(zip_service_url)

mo_shape <- arc_select(zip_layer, where = "STATE = 'MO'", crs = 4326)
# Save as .rds, not .csv -- geometry can't survive a CSV round trip.
saveRDS(mo_shape, "mo_combined.rds")

#Get data on areas around loop trolley.
library(tidyverse)
library(ggthemes)
library(stringr)
library(lubridate)
library(plotly)
library(ggspatial)

# Zillow's ZHVI export is a wide file: a handful of ID columns followed by
# one column per month, and that column count grows every time the file is
# refreshed. Keeping read.csv() (rather than read_csv()) here is deliberate:
# base R's read.csv() prefixes date columns like "2024-06-30" with "X" (via
# make.names()), and the rest of this script keys off that "X..." naming --
# switching readers would silently rename every date column and break every
# plot below.
#
# Read once and reuse for every state, instead of re-parsing the same CSV
# from disk per city (the OH section used to do its own separate read()).
zip_data_raw <- read.csv("../data/zip_data.csv") %>%
  # The 2000-2010 drop is a fixed historical window (won't grow), so a
  # literal range is fine here -- unlike the pivots below, which need to
  # cover however many months exist *today*.
  select(-c(SizeRank, RegionType, StateName, X2000.01.31:X2010.12.31))

# This project doesn't need to be a live-updating dashboard, and re-running
# the pipeline against whatever "latest" column happened to exist that day
# made every run (and every re-download of zip_data.csv) subtly different.
# Freeze the dataset at end-of-year 2025: drop any month past 2025-12-31 up
# front so every downstream calculation (latest_value_col, year_value_cols,
# the trend charts) is anchored to a fixed, reproducible cutoff. This also
# trims a chunk of columns we don't use, which is most of the "save some
# space" ask below -- the rest is in build_city_data()'s keep_value_cols.
FINAL_YEAR <- 2025
raw_date_cols <- zip_data_raw %>% select(starts_with("X")) %>% names()
raw_date_col_dates <- as.Date(str_remove_all(raw_date_cols, "X"), "%Y.%m.%d")
cols_past_cutoff <- raw_date_cols[raw_date_col_dates > as.Date(paste0(FINAL_YEAR, "-12-31"))]
zip_data_raw <- zip_data_raw %>%
  select(-all_of(cols_past_cutoff)) %>%
  # Also drop everything except the two states this project uses -- the
  # national file has thousands of rows we never touch.
  filter(State %in% c("MO", "OH"))

# Most recent month present in the (now 2025-capped) file, instead of a
# hardcoded date string -- keeps every choropleth's "current" value
# pointed at 2025-12-31 without hardcoding that string in five places.
latest_value_col <- zip_data_raw %>%
  select(starts_with("X")) %>%
  names() %>%
  max()

# Year -> column name lookup, for the Shiny map's yearly time slider (the
# underlying data is monthly, but a slider with one stop per year reads a
# lot better than one with ~180 stops). Prefer each year's December
# reading; fall back to that year's latest available month if December
# isn't there yet -- true for the current year, and possibly the very
# first year of coverage.
date_col_lookup <- tibble(col = zip_data_raw %>% select(starts_with("X")) %>% names()) %>%
  mutate(
    date = as.Date(str_remove_all(col, "X"), "%Y.%m.%d"),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m"))
  ) %>%
  arrange(date)

year_value_cols <- date_col_lookup %>%
  group_by(year) %>%
  summarise(
    col = if (any(month == 12)) col[month == 12][1] else col[which.max(date)],
    .groups = "drop"
  ) %>%
  arrange(year)

zip_data <- zip_data_raw %>%
  filter(State == "MO") %>%
  mutate(RegionName = factor(RegionName))

oh_zip_data <- zip_data_raw %>%
  filter(State == "OH") %>%
  mutate(RegionName = factor(RegionName))

# ---- Shared building blocks -------------------------------------------
# STL/KC/Cincy were three near-identical copies of: join zip polygons to
# home values, pull out the "on the route" zips, draw a choropleth, and
# draw a trend line. These three helpers are that one process; each city
# below just supplies the pieces that actually differ (which zips, what
# the route looks like, map extent, titles).

# Turn a route's lon/lat vertices into a single LINESTRING, so the whole
# route draws with one geom_sf() layer instead of a geom_segment() per
# leg (STL needed 2, KC needed 5, Cincy needed 8 -- all hand-typed).
make_route <- function(lon, lat, crs = 4326) {
  st_sfc(st_linestring(cbind(lon, lat)), crs = crs)
}

# Join a state's zip polygons to home-value data, restrict to the zips
# this city cares about, and split out the ones that sit along the route
# (for the red highlight). Warns instead of silently dropping zips that
# don't match anything -- useful for the KC list in particular, since
# that one comes from a scrape.
#
# keep_value_cols, if supplied, slims the joined data down to just those
# columns (plus ZIP_CODE/PO_NAME/geometry -- geometry is "sticky" and
# survives select() on an sf object automatically). The map only ever
# needs one column per year (year_value_cols), not the full monthly
# history, so this is the other half of "save some space": every one of
# combined/route_zip_data/map_plot/the Shiny export drops from ~180
# monthly columns down to ~15 yearly ones.
build_city_data <- function(shape_sf, home_value_data, city_zips, route_zips, city_label,
                             keep_value_cols = NULL) {
  city_zips <- as.character(city_zips)
  route_zips <- as.character(route_zips)

  combined <- shape_sf %>%
    left_join(home_value_data, by = c("ZIP_CODE" = "RegionName")) %>%
    filter(ZIP_CODE %in% city_zips)

  if (!is.null(keep_value_cols)) {
    combined <- combined %>% select(ZIP_CODE, PO_NAME, all_of(keep_value_cols))
  }

  missing <- setdiff(city_zips, as.character(combined$ZIP_CODE))
  if (length(missing) > 0) {
    warning(
      city_label, " zip list has zip(s) not found in the shape/home-value data: ",
      paste(missing, collapse = ", ")
    )
  }

  list(
    combined = combined,
    route_zip_data = combined %>% filter(ZIP_CODE %in% route_zips)
  )
}

# The choropleth: base fill layer, red-outlined "on the route" zips, the
# route line itself, and the color scale/theme/labels shared by all three.
build_choropleth <- function(combined, route_zip_data, route_line,
                              xlim, ylim, title, subtitle, route_linewidth = 1.2,
                              route_color = "#FFAE00", highlight_linewidth = 0.9) {
  ggplot() +
    geom_sf(aes(fill = .data[[latest_value_col]], zip = ZIP_CODE, name = PO_NAME),
            data = combined) +
    geom_sf(aes(fill = .data[[latest_value_col]], zip = ZIP_CODE, name = PO_NAME),
            data = route_zip_data, color = "red", linewidth = highlight_linewidth) +
    geom_sf(data = route_line, color = route_color, linewidth = route_linewidth, lineend = "round") +
    coord_sf(xlim = xlim, ylim = ylim) +
    # viridis: colorblind-safe and reads as more polished than the old
    # manual grey88-to-darkblue gradient. Labels use the same dynamic
    # "$250K"/"$1M" short-scale formatting as the trend charts, computed
    # from the data actually being drawn, instead of a fixed
    # 250k/500k/750k/1mil legend that stops covering the real range as
    # home values rise past $1M.
    scale_fill_viridis_c(
      name = "Home Value (USD)",
      labels = scales::label_dollar(scale_cut = scales::cut_short_scale())
    ) +
    # Scale bar + north arrow: small addition that makes this read as an
    # actual cartographic figure rather than a scatter of colored
    # polygons -- standard expectation for a map in a published write-up.
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering(),
                           height = grid::unit(1, "cm"), width = grid::unit(1, "cm")) +
    theme_map() +
    theme(legend.position = "right", title = element_text(hjust = 0.5)) +
    labs(title = title, subtitle = subtitle)
}

# The home-value trend line for a curated subset of zips, with an
# optional vertical marker (e.g. the line's opening date). Zips in
# `route_zips` (the ones the circulator actually runs through) get
# pulled to the front of the legend, drawn in a warm red/orange ramp,
# and given a thicker line; everyone else fades to gray. The warm
# colors intentionally echo the orange route line / red highlight used
# for the same zips on the choropleth.
build_trend_plot <- function(home_value_data, trend_zips, route_zips = character(0), title,
                              marker_date = NULL, marker_yend = NULL,
                              marker_color = "black") {
  trend_longer <- home_value_data %>%
    pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "HomeValue") %>%
    filter(RegionName %in% trend_zips) %>%
    mutate(Date = str_remove_all(Date, "X"),
           Date = as.Date(Date, "%Y.%m.%d"))

  present_zips <- as.character(unique(trend_longer$RegionName))
  route_levels <- present_zips[present_zips %in% as.character(route_zips)]
  other_levels <- setdiff(present_zips, route_levels)

  # Explicit droplevels + reordered factor: without this, RegionName's
  # levels() would still include every zip in the state (leftover from
  # the state-wide factor() call up top), which would both break
  # scale_color_manual (needs exactly one color per level present) and
  # leave the legend order unpredictable.
  trend_longer <- trend_longer %>%
    mutate(
      RegionName = factor(RegionName, levels = c(route_levels, other_levels)),
      on_route = RegionName %in% route_levels,
      # ggplotly()'s default tooltip dumps every mapped aesthetic
      # (colour hex, linewidth, on_route, ...) into one messy box. Build
      # one clean string per point instead, and have the caller tell
      # ggplotly() to show ONLY this ("text") aesthetic.
      tooltip_text = paste0(
        "Zip ", RegionName, if_else(on_route, " (on route)", ""), "\n",
        format(Date, "%b %Y"), ": ", scales::label_dollar()(HomeValue)
      )
    )

  highlight_palette <- if (length(route_levels) > 0) {
    colorRampPalette(c("#E69F00", "#B2182B"))(length(route_levels))
  } else {
    character(0)
  }
  muted_palette <- if (length(other_levels) > 0) {
    grey.colors(length(other_levels), start = 0.55, end = 0.8)
  } else {
    character(0)
  }
  color_values <- setNames(c(highlight_palette, muted_palette), c(route_levels, other_levels))

  p <- ggplot(trend_longer) +
    geom_line(aes(x = Date, y = HomeValue, group = RegionName,
                  color = RegionName, linewidth = on_route, text = tooltip_text)) +
    scale_color_manual(values = color_values, name = "Zip Code") +
    # Thicker line for route zips; hide this legend since the color
    # legend (with route zips called out in warm colors, listed first)
    # already carries that information without a second, redundant key.
    scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.6), guide = "none") +
    theme_classic() +
    # Dynamic y-axis: breaks/labels are computed from whatever the highest
    # zip's value actually is over this data's date range, using scales'
    # short-scale formatter ("$250K", "$1M") instead of a fixed set of
    # breaks someone has to remember to raise every time a zip crosses
    # into a new price bracket -- which is exactly what the old hardcoded
    # "100k/200k/300k/400k"-style breaks quietly stopped doing.
    scale_y_continuous(
      name = "Home Value (USD)",
      labels = scales::label_dollar(scale_cut = scales::cut_short_scale())
    ) +
    # This now spans ~15 years of monthly data -- explicit yearly breaks
    # keep the x-axis readable instead of ggplot's default date breaks
    # cramming/overlapping at that span.
    scale_x_date(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
    ggtitle(title, subtitle = "Zillow.com")

  if (!is.null(marker_date)) {
    # Same idea: if no explicit height is given, run the marker line all
    # the way to the current max instead of a stale hardcoded value that
    # would fall short of the top of the (now dynamic) axis.
    if (is.null(marker_yend)) {
      marker_yend <- max(trend_longer$HomeValue, na.rm = TRUE)
    }
    p <- p + geom_segment(aes(x = marker_date, y = 0, xend = marker_date, yend = marker_yend),
                          color = marker_color)
  }

  list(plot = p, data = trend_longer)
}



#St. Louis###################################################################
#Bring back geographic data, join them together.

mo_sf <- readRDS("mo_combined.rds") %>%
  mutate(ZIP_CODE = factor(ZIP_CODE))

# read_csv() auto-detects and strips a UTF-8 BOM (the source of the old
# mangled "..Zip.Code" column name), and clean_names() gives a predictable
# snake_case column name no matter how the source file spells/spaces it.
stl_zips <- read_csv("../data/stl_zips.csv") %>%
  clean_names()

#Actual Coordinates is (-90.3325, 38.6682)
#Loop Trolley(38.656430, -90.309739) to (38.653941, -90.284413) to (38.644572, -90.285615)
stl_route <- make_route(
  lon = c(-90.309739, -90.284413, -90.285615),
  lat = c(38.656430, 38.653941, 38.644572)
)
stl_route_zips <- c("63130", "63112")
# The comparison zips here match the paper's chosen control group exactly
# (63114, 63133, 63108) -- these three were picked for a reason (matched
# demographics/geography, City/County balance against the treated pair),
# and the DiD analysis depends on that reasoning. This used to say
# "63113" instead of 63114 and included an unrelated 63144 (Brentwood) --
# a transcription typo, confirmed by checking City/CountyName in
# zip_data.csv: 63113 is a different zip entirely (Saint Louis City, not
# the paper's Overland/St. Louis County control), and 63144 was never
# part of the paper's control group. Fixed to match the paper.
stl_trend_zips <- c("63130", "63112", "63133", "63114", "63108")

stl_result <- build_city_data(mo_sf, zip_data, stl_zips$zip_code, stl_route_zips, "St. Louis",
                               keep_value_cols = year_value_cols$col)
stl_combined_data <- stl_result$combined
trolley_zips <- stl_result$route_zip_data

#Making basic Chloropleth Map
p1 <- build_choropleth(
  stl_combined_data, trolley_zips, stl_route,
  xlim = c(-90.41, -90.23), ylim = c(38.58, 38.73),
  title = "The Delmar Loop Trolley", subtitle = "St. Louis, MO, Zip Codes"
)
ggplotly(p1)

#Making Line Graph
stl_trend <- build_trend_plot(
  zip_data, stl_trend_zips, route_zips = stl_route_zips,
  title = "Median Home Values, Selected St. Louis Zip Codes",
  marker_date = as.Date("2018-11-01"), marker_color = "blue"
)
stl_zip_longer <- stl_trend$data
stl_trend$plot

#KCMO##################################################################
# The zip-codes.com scrape stopped returning a usable table -- exactly
# the fragility flagged when this was written. Kansas City zips run
# roughly 64101-64199, with plenty of numbers unused in between, so
# rather than hand-typing ~70 codes (and risking a fat-fingered digit),
# generate every code in that range and let mo_sf -- the actual ZCTA
# boundary data we're already pulling from ArcGIS -- tell us which ones
# are real. The boundary data is the authoritative source here, so this
# is more trustworthy than a manually curated list, and it needs zero
# upkeep if KC's zips are ever redrawn.
kc_zip_candidates <- sprintf("641%02d", 1:99)
kc_zips <- mo_sf %>%
  st_drop_geometry() %>%
  filter(ZIP_CODE %in% kc_zip_candidates) %>%
  pull(ZIP_CODE) %>%
  as.character() %>%
  unique()

kc_route <- make_route(
  lon = c(-94.584042, -94.582943, -94.584588, -94.580953, -94.580495, -94.584588),
  lat = c(39.084913, 39.105484, 39.109600, 39.110325, 39.108820, 39.109600)
)
kc_route_zips <- c("64106", "64108", "64105")
# 64116 (Clay County) swapped for 64111 (Westport/Country Club Plaza,
# Jackson County -- the same county as the route zips). Checked against
# City/CountyName in zip_data.csv while vetting comparison zips for
# analysis.qmd: 64116 crosses a county line the route zips don't, which
# is exactly the kind of mismatch the original paper's own control-group
# picks (63108/63114/63133, all matched to Saint Louis City/County) were
# careful to avoid. 64111 keeps the same "walkable district the
# streetcar doesn't reach" logic without the county mismatch.
kc_trend_zips <- c("64106", "64108", "64105", "64111", "64124", "64127")

# The sanity check inside build_city_data() should rarely fire now that
# kc_zips is derived straight from mo_sf's own ZIP_CODE values (every
# candidate that survives the filter above is already a real polygon),
# but it's still worth keeping: it'll catch a real KC zip that mo_sf has
# but Zillow doesn't cover.
kc_result <- build_city_data(mo_sf, zip_data, kc_zips, kc_route_zips, "KC (64101-64199 range)",
                              keep_value_cols = year_value_cols$col)
kc_combined_data <- kc_result$combined
streetcar_zips <- kc_result$route_zip_data

#Make KC Chloropleth Map
p2 <- build_choropleth(
  kc_combined_data, streetcar_zips, kc_route,
  xlim = c(-94.63, -94.50), ylim = c(39.07, 39.15),
  title = "The KC Streetcar", subtitle = "Kansas City, MO, Zip Codes"
)
ggplotly(p2, tooltip = c("zip", "fill"))

#Make KC linegraph
kc_trend <- build_trend_plot(
  zip_data, kc_trend_zips, route_zips = kc_route_zips,
  title = "Median Home Values, Selected Kansas City Zip Codes",
  marker_date = as.Date("2016-05-31")
)
kc_zip_longer <- kc_trend$data
kc_trend$plot

save(stl_combined_data, stl_zip_longer, trolley_zips,
     kc_combined_data, kc_zip_longer, streetcar_zips,
     file = "urbancircluators.Rdata")

#Cincy##############################################################
# (tidyverse/sf/janitor/arcgislayers already loaded above -- no need to
# reload; zip_layer is the same live feature-service connection opened
# at the top of this script; oh_zip_data was already derived above from
# the single shared zip_data_raw read)
oh_shape <- arc_select(zip_layer, where = "STATE = 'OH'", crs = 4326)
saveRDS(oh_shape, "oh_combined.rds")

oh_sf <- readRDS("oh_combined.rds") %>%
  mutate(ZIP_CODE = factor(ZIP_CODE))

cincy_zips <- as.character(c(45202, 45203, 45204,
                             45205, 45206, 45207, 45208,
                45209, 45211, 45212, 45213, 45214, 45215, 45216,
                45217, 45219, 45220, 45223, 45224, 45225, 45226,
                45227, 45229, 45230, 45231, 45232, 45233, 45237,
                45238, 45239, 45243, 45248))

# The 8 hand-typed segments formed one continuous, closed loop (each
# segment's end was the next one's start, and the last came back to the
# first) -- so this is really a single ring geometry, not 8 lines.
cincy_route <- make_route(
  lon = c(-84.510558, -84.508958, -84.511575, -84.517972,
          -84.520461, -84.518904, -84.516069, -84.512869, -84.510558),
  lat = c(39.098185, 39.098439, 39.108788, 39.107858,
          39.117666, 39.117894, 39.106887, 39.107324, 39.098185)
)
cincy_route_zips <- c("45202")
cincy_trend_zips <- c("45202", "45203", "45212", "45219", "45206")

cincy_result <- build_city_data(oh_sf, oh_zip_data, cincy_zips, cincy_route_zips, "cincy_zips",
                                 keep_value_cols = year_value_cols$col)
cincy_combined_data <- cincy_result$combined
connector_zips <- cincy_result$route_zip_data

#Making Basic Chloropleth Map
p3 <- build_choropleth(
  cincy_combined_data, connector_zips, cincy_route,
  xlim = c(-84.54, -84.49), ylim = c(39.086, 39.13),
  title = "The Cincinnati Connector", subtitle = "Cincinnati, OH Zip Codes"
)
ggplotly(p3)

#Linegraph
cincy_trend <- build_trend_plot(
  oh_zip_data, cincy_trend_zips, route_zips = cincy_route_zips,
  title = "Median Home Values, Selected Cincinnati Zip Codes",
  marker_date = as.Date("2016-09-30")
)
cincy_zip_longer <- cincy_trend$data
cincy_trend$plot

save(cincy_combined_data, connector_zips, cincy_zip_longer,
     file = "urbancirculators2.Rdata")

# ---- Shiny app data export ----------------------------------------------
# Package each city's already-built plots and underlying data into one
# object the Shiny app can load directly -- so the app starts instantly
# instead of re-running ArcGIS queries, CSV parsing, and ggplot
# construction every time someone opens it. Re-run this script and this
# block whenever the source data (Zillow export, ArcGIS boundaries)
# should be refreshed; the app itself never touches the network.
#
# combined_data keeps every year's value column (not just
# latest_value_col), and route_zips/route_line/xlim/ylim are included
# alongside the pre-built map_plot/trend_plot -- the Shiny app's
# interactive map is built with leaflet (for basemap tiles + a smooth
# yearly time slider), which needs these raw ingredients to redraw the
# choropleth per year, rather than the static ggplot2 map_plot (that one's
# still here for pulling into the write-up as a static figure via
# ggsave()).
city_data <- list(
  stl = list(
    label = "St. Louis - Delmar Loop Trolley",
    map_plot = p1,
    trend_plot = stl_trend$plot,
    combined_data = stl_combined_data,
    route_zips = stl_route_zips,
    route_line = stl_route,
    xlim = c(-90.41, -90.23),
    ylim = c(38.58, 38.73),
    trend_data = stl_zip_longer
  ),
  kc = list(
    label = "Kansas City - KC Streetcar",
    map_plot = p2,
    trend_plot = kc_trend$plot,
    combined_data = kc_combined_data,
    route_zips = kc_route_zips,
    route_line = kc_route,
    xlim = c(-94.63, -94.50),
    ylim = c(39.07, 39.15),
    trend_data = kc_zip_longer
  ),
  cincy = list(
    label = "Cincinnati - Cincinnati Connector",
    map_plot = p3,
    trend_plot = cincy_trend$plot,
    combined_data = cincy_combined_data,
    route_zips = cincy_route_zips,
    route_line = cincy_route,
    xlim = c(-84.54, -84.49),
    ylim = c(39.086, 39.13),
    trend_data = cincy_zip_longer
  )
)

app_data <- list(cities = city_data, year_value_cols = year_value_cols)
# xz compression trades a bit of save/load time for a meaningfully
# smaller file on disk -- worth it now that this is a one-time export
# rather than something regenerated on every app start.
saveRDS(app_data, "shiny_app_data.rds", compress = "xz")

# ---- Note: zip 63113 (MO) has no ZHVI data before 2017 ------------------
# RESOLVED: this turned out not to be a Zillow data-coverage story. 63113
# was never supposed to be part of the St. Louis analysis at all -- it
# was a transcription typo for 63114 (the paper's actual control zip;
# see the comment at stl_trend_zips above, confirmed via City/CountyName
# in zip_data.csv). 63113 is a different zip in a different part of the
# city with no particular relevance to the Loop Trolley study. Now that
# stl_trend_zips uses 63114, this data gap is moot -- leaving this note
# for the record in case 63113 shows up again somewhere by mistake.
#
# (Original hypothesis, kept for reference: Zillow's ZHVI doesn't backfill
# a value for every zip across its full historical range -- low sales-
# volume/low-coverage geographies get suppressed rather than published
# with a shaky number, per Zillow's methodology writeup. 63113 covers the
# Ville/JeffVanderLou area, a historically high-vacancy part of the city,
# which does fit that profile -- but it's irrelevant here either way
# since that zip isn't part of this analysis.)
