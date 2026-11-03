# Pistachio Branch Carbon Budget Model

An R Shiny app that estimates daily pistachio carbon supply, respiration, growth demand, and net carbon budget from hourly CIMIS weather data and leaf and fruit counts.

## Run locally

Install R 4.1 or later, then clone this repository and open `pistachiocb.Rproj` in RStudio, or set your R working directory to the repository folder.

Install the app dependencies once:

```r
install.packages(c(
  "dplyr", "readr", "bigleaf", "tibble", "rlang", "magrittr",
  "shiny", "plotly"
))
```

The model requires dplyr 1.1.0 or later. Start the app from the repository folder:

```r
shiny::runApp(".")
```

The app loads the model functions directly from `R/`; installing the project as an R package is not required.

## Weather input

Upload an hourly CIMIS CSV with these column names:

| Column | Format or unit |
| --- | --- |
| `Date` | Month/day/year, such as `04/01/2026` |
| `Hour (PST)` | Numeric HHMM, such as `1300` |
| `Sol Rad (Ly/day)` | Solar radiation in Langley/day |
| `Air Temp (F)` | Air temperature in degrees Fahrenheit |

Set the leaf and fruit counts, then click **Run model**. The app provides an overview, component plots and tables, daily results, hourly inputs, and a downloadable budget CSV. The final carbon budget can be displayed in micromoles C/day or milligrams C/day.

Weather data is supplied by the user; no weather dataset is bundled in this repository.

## Model

The model converts radiation and temperature, estimates absorbed light and cumulative growing degree days, calculates leaf assimilation, and scales respiration and growth demand using leaf and fruit growth curves. The final budget includes leaves and fruit; shoot and storage terms are not explicitly included.

## Project files

- `app.R`: Shiny interface and server.
- `R/`: carbon budget calculations and physiological submodels.
- `DESCRIPTION`, `NAMESPACE`, and `man/`: R package metadata and documentation.
- `pistachiocb.Rproj`: RStudio project.

RStudio session files, local environment files, and `rsconnect/` deployment records are excluded from version control.

## License

MIT, as declared in `DESCRIPTION`. See [LICENSE.md](LICENSE.md).
