# Urban Circulators and Home Values

Do heritage trolleys and modern streetcars raise nearby home values enough to
justify their cost? I test this with panel fixed-effects regressions and an
event-study design across three U.S. "urban circulator" lines -- St. Louis's
Delmar Loop Trolley, the KC Streetcar, and the Cincinnati Bell Connector --
using Zillow's Zip Code Home Value Index.

**The finding:** none of the three shows a statistically detectable increase
in nearby home values. Where there's a large-looking pre/post gap
(Cincinnati, mildly Kansas City), an event-study check shows it's a
pre-existing trend the project didn't add to, not an effect of the project
itself -- a distinction a simple two-period difference-in-differences,
which is what the original 2023 version of this analysis used, can't catch.

**[Read the full analysis](./analysis/analysis.html)** <!-- once this repo is on GitHub, swap this for an htmlpreview.github.io link (or a GitHub Pages link) so it renders in-browser instead of downloading -->

**[Live dashboard](#)** <!-- add the shinyapps.io link here after deploying -->

## How this evolved

- **2022** -- As a Show-Me Institute intern, I wrote a short piece arguing
  the KC Streetcar's expansion was crowding out its much more heavily-used
  (and starved-for-funding) bus system: ["As Kansas City's Streetcar
  Expands, Its Buses
  Suffer"](https://showmeinstitute.org/article/transportation/as-kansas-citys-streetcar-expands-its-buses-suffer/).
- **2023** -- As an econ undergrad, I tried to test the property-value side
  of that argument for St. Louis specifically, with a naive two-period DiD
  (`/legacy`). It found no significant effect, but couldn't check its own
  key assumption (parallel pre-trends) beyond eyeballing a line chart.
- **Now** -- I revisited it with the tools I'd actually use professionally:
  fixed-effects panel regression, clustered standard errors, and an
  event-study design that tests parallel trends directly instead of
  assuming them. It turned up a real confound in the pooled model (see
  below) that the 2023 version had no way to catch, and lands, with much
  more rigor, in the same place the 2022 piece did.

## Methods

- Two-way fixed-effects DiD (`fixest::feols`), zip and date/event-month
  fixed effects, standard errors clustered by zip
- Event-study specification (`i()` interactions) to test parallel
  pre-trends explicitly, city-by-city and pooled
- A pooled three-city panel on a shared event-time clock (months relative
  to each city's own opening date), with a disclosed, lightly-vetted
  comparison-group swap for Kansas City
- Diagnostic breakdown that traces a pooled pre-trend violation to a
  specific city (Cincinnati) rather than reporting the pooled coefficient
  at face value

## Repo structure

```
analysis/     the current analysis: analysis.qmd (source) + analysis.html (rendered)
dashboard/    interactive Shiny app (app.R) and the pipeline that builds its data (zip_cleaning.R)
data/         small committed data files + instructions for the large source files
legacy/       the original 2023 paper and regression code, kept for the record
```

## Running this yourself

The rendered `analysis.html` is self-contained -- just open it, no setup
needed. To re-run `analysis.qmd`, you'll need R with `tidyverse`, `fixest`,
`broom`, and `scales`; it reads `data/zip_data_subset.csv`, which is
committed, so it runs immediately after cloning.

The dashboard needs more: see `data/README.md` for how to get the full
Zillow export and regenerate `dashboard/shiny_app_data.rds` via
`dashboard/zip_cleaning.R`, or just use the live link above.

## Author

Quin Rizer
