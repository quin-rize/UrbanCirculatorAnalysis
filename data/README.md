# Data

## `zip_data_subset.csv`

The full Zillow Home Value Index (ZHVI) export is a national file covering
every U.S. zip code (~120MB) and isn't committed here. This repo's analysis
(`/analysis/analysis.qmd`) only ever uses 16 zip codes across three cities,
so `zip_data_subset.csv` contains just those rows -- same columns, same
values as the source file, about 500x smaller:

- **St. Louis:** 63130, 63112 (treated); 63114, 63133, 63108 (comparison)
- **Kansas City:** 64106, 64108, 64105 (treated); 64111, 64124, 64127
  (comparison)
- **Cincinnati:** 45202 (treated); 45203, 45212, 45219, 45206 (comparison)

If you want to extend the analysis to different zip codes, download the
full ZHVI file (Zip Code, all homes, Zillow's smoothed/seasonally adjusted
series) from [Zillow Research](https://www.zillow.com/research/data/) and
either point `analysis.qmd` at it directly or re-run the extraction filter
in the repo's history to build a wider subset.

## `stl_zips.csv`

A small curated list of St. Louis-area zip codes, used by
`/dashboard/zip_cleaning.R` to build the St. Louis choropleth map. Kept as
its own file (rather than folded into the code) since it's editorial input,
not a derived value.

## Regenerating the full dashboard

`/dashboard/zip_cleaning.R` builds the interactive dashboard's underlying
data from two sources:

1. The full Zillow ZHVI export above (not the subset -- the dashboard's
   choropleth maps need every zip in each metro area, not just the 16
   used in the causal analysis). Download it and save it here as
   `zip_data.csv`.
2. Zip code boundary polygons, pulled live at run time from Esri's ArcGIS
   Living Atlas "USA ZIP Code Boundaries" feature service (no download
   needed -- `zip_cleaning.R` queries it directly via the `arcgislayers`
   package).

Run `zip_cleaning.R` with `/dashboard` as the working directory once both
are in place; it writes `shiny_app_data.rds` back into `/dashboard`, which
`app.R` reads on startup.
