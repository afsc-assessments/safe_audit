# SAFE audit bundle

Files included:

- `npfmcSAFE.R` — scraper/crawler functions
- `safe_audit.qmd` — Quarto report using the scraper
- `README_SAFE_audit.md` — this note

The NOAA path now crawls both:

- annual NOAA groundfish pages for 2013 onward
- the NOAA `1998–2012` historical archive page, including any older year pages or direct document links it exposes
- NOAA `resource/data` cover pages are resolved to their underlying downloadable files when possible, so the catalog prefers terminal PDFs over wrapper pages

## Basic use

In R:

```r
source("npfmcSAFE.R")

safe_catalog <- build_safe_catalog(crawl_noaa = TRUE, quietly = FALSE)
write_safe_outputs(safe_catalog, out_dir = "outputs")
```

To render the report:

```bash
quarto render safe_audit.qmd
```

or inside R:

```r
quarto::quarto_render("safe_audit.qmd")
```

## Outputs written by the report

- `outputs/safe_catalog.csv`
- `outputs/safe_stock_inventory.csv`
- `outputs/safe_anomalies.csv`
- `outputs/safe_filename_collisions.csv`
- `outputs/safe_year_source_counts.csv`
- `outputs/safe_resolved_noaa_terminal_links.csv`

## Packages used

The R script expects:

- rvest
- xml2
- dplyr
- stringr
- purrr
- tidyr
- tibble
- readr
- ggplot2

The Quarto document also expects:

- gt
- scales
