# npfmcSAFE.R
# Scraper + crawler for NPFMC SAFE page and NOAA groundfish historical pages
#
# Main functions:
#   scrape_npfmc_safe()
#   scrape_noaa_groundfish_index()
#   crawl_noaa_groundfish_years()
#   build_safe_catalog()
#   write_safe_outputs()
#
# The code is designed to be sourced from an R session or used in a Quarto doc.

suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(ggplot2)
})

NPFMC_SAFE_URL <- "https://www.npfmc.org/library/safe-reports/"
NOAA_GROUNDFISH_INDEX_URL <- "https://www.fisheries.noaa.gov/alaska/population-assessments/north-pacific-groundfish-stock-assessments-and-fishery-evaluation"
REFM_HISTORIC_URL <- "https://apps-afsc.fisheries.noaa.gov/REFM/stocks/Historic_Assess.htm"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x
}

clean_text <- function(x) {
  x %>%
    str_replace_all("\u00a0", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

extract_year <- function(x) {
  suppressWarnings(as.integer(str_extract(x %||% "", "\\b(?:19|20)\\d{2}\\b")))
}

extract_dir_year <- function(url) {
  suppressWarnings(as.integer(str_match(url %||% "", "/SAFE/((?:19|20)\\d{2})/")[, 2]))
}

extract_path_year <- function(url) {
  suppressWarnings(as.integer(str_match(url %||% "", "/((?:19|20)\\d{2})(?:/|$)")[, 2]))
}

extract_filename <- function(url) {
  ifelse(is.na(url) | url == "", NA_character_, basename(url))
}

strip_url_fragment <- function(url) {
  str_replace(url %||% "", "#.*$", "")
}

has_document_extension <- function(url) {
  str_detect(url %||% "", regex("\\.(pdf|doc|docx|xls|xlsx|zip)(?:$|[?#])", TRUE))
}

is_noaa_cover_page <- function(url) {
  str_detect(url %||% "", regex("/resource/(data|document)/", TRUE)) &
    !has_document_extension(url)
}

is_same_page_anchor <- function(url, page_url) {
  !is.na(url) &
    url != "" &
    str_detect(url, "#") &
    strip_url_fragment(url) == strip_url_fragment(page_url)
}

safe_get_attr <- function(node, attr) {
  out <- xml_attr(node, attr)
  ifelse(length(out) == 0, NA_character_, out)
}

infer_section_from_text <- function(text) {
  case_when(
    str_detect(text, regex("Groundfish SAFEs", TRUE)) ~ "Groundfish SAFEs",
    str_detect(text, regex("Crab SAFEs", TRUE)) ~ "Crab SAFEs",
    str_detect(text, regex("Scallop SAFEs", TRUE)) ~ "Scallop SAFEs",
    str_detect(text, regex("Cook Inlet Salmon SAFEs", TRUE)) ~ "Cook Inlet Salmon SAFEs",
    TRUE ~ NA_character_
  )
}

nearest_section <- function(node) {
  hits <- xml_find_all(node, "preceding::*[self::h1 or self::h2 or self::h3 or self::h4 or self::strong]")
  txt <- clean_text(xml_text(hits, trim = TRUE))
  txt <- txt[txt != ""]
  hit <- txt[str_detect(txt, regex("Groundfish SAFEs|Crab SAFEs|Scallop SAFEs|Cook Inlet Salmon SAFEs", TRUE))]
  if (length(hit)) tail(hit, 1) else NA_character_
}

nearest_year_context <- function(node) {
  hits <- xml_find_all(node, "preceding::*[self::p or self::b or self::strong or self::h1 or self::h2 or self::h3 or self::h4]")
  txt <- clean_text(xml_text(hits, trim = TRUE))
  txt <- txt[txt != ""]
  yrs <- extract_year(txt)
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs)) tail(yrs, 1) else NA_integer_
}

table_context <- function(node) {
  td <- xml_find_first(node, "ancestor::td[1] | ancestor::th[1]")
  tr <- xml_find_first(node, "ancestor::tr[1]")
  tbl <- xml_find_first(node, "ancestor::table[1]")

  if (inherits(td, "xml_missing") || inherits(tr, "xml_missing") || inherits(tbl, "xml_missing")) {
    return(tibble(col_header = NA_character_, row_text = NA_character_))
  }

  cells <- xml_find_all(tr, "./th|./td")
  idx <- which(xml_path(cells) == xml_path(td))
  idx <- if (length(idx)) idx[1] else NA_integer_

  header_row <- xml_find_first(tbl, ".//tr[2] | .//tr[1]")
  headers <- xml_find_all(header_row, "./th|./td") %>%
    xml_text(trim = TRUE) %>%
    clean_text()

  col_header <- if (!is.na(idx) && idx <= length(headers)) headers[idx] else NA_character_
  row_text <- clean_text(xml_text(tr, trim = TRUE))

  tibble(col_header = col_header, row_text = row_text)
}

stock_dictionary <- tribble(
  ~pattern, ~stock_group, ~stock,
  "pollock", "groundfish", "Pollock",
  "pacific cod|\\bpcod\\b", "groundfish", "Pacific cod",
  "sablefish", "groundfish", "Sablefish",
  "yellowfin sole", "groundfish", "Yellowfin sole",
  "greenland turbot|turbot", "groundfish", "Greenland turbot",
  "arrowtooth flounder|\\batf\\b", "groundfish", "Arrowtooth flounder",
  "kamchatka flounder", "groundfish", "Kamchatka flounder",
  "northern rock sole|rock sole", "groundfish", "Northern rock sole",
  "flathead sole", "groundfish", "Flathead sole",
  "alaska plaice|plaice", "groundfish", "Alaska plaice",
  "other flatfish|\\boflat\\b", "groundfish", "Other flatfish",
  "pacific ocean perch|\\bpop\\b", "groundfish", "Pacific ocean perch",
  "northern rockfish|\\bnork\\b", "groundfish", "Northern rockfish",
  "blackspotted.*rougheye|rougheye.*blackspotted|\\brebs\\b|\\bbsre\\b", "groundfish", "Rougheye/blackspotted complex",
  "shortraker", "groundfish", "Shortraker rockfish",
  "dusky", "groundfish", "Dusky rockfish",
  "dem\\. shelf rockfish|demersal shelf rockfish|\\bdsr\\b", "groundfish", "Demersal shelf rockfish",
  "thornyhead", "groundfish", "Thornyheads",
  "other rockfish|\\borock\\b", "groundfish", "Other rockfish",
  "atka mackerel|\\batka\\b", "groundfish", "Atka mackerel",
  "skates|\\bskate\\b", "groundfish", "Skates",
  "sharks", "groundfish", "Sharks",
  "octopus|\\bocto\\b", "groundfish", "Octopus",
  "shallow-water flatfish|shallowflat", "groundfish", "Shallow-water flatfish",
  "deep-water flatfish|deepflat", "groundfish", "Deep-water flatfish",
  "rex sole|\\brex\\b", "groundfish", "Rex sole",
  "forage fish", "groundfish", "Forage fish",
  "grenadier", "groundfish", "Grenadier",
  "snow crab", "crab", "Eastern Bering Sea snow crab",
  "bristol bay red king crab|bbrkc", "crab", "Bristol Bay red king crab",
  "tanner crab", "crab", "Tanner crab",
  "pribilof islands red king crab|pirkc", "crab", "Pribilof Islands red king crab",
  "pribilof islands blue king crab|pibkc", "crab", "Pribilof Islands blue king crab",
  "saint matthew island blue king crab|smbkc", "crab", "Saint Matthew Island blue king crab",
  "norton sound red king crab|nsrkc", "crab", "Norton Sound red king crab",
  "pribilof islands golden king crab|pigkc", "crab", "Pribilof Islands golden king crab",
  "west aleutian islands red king crab|wairkc", "crab", "West Aleutian Islands red king crab",
  "aleutian islands golden king crab|aigkc", "crab", "Aleutian Islands golden king crab",
  "scallop", "scallop", "Scallop",
  "cook inlet salmon", "salmon", "Cook Inlet salmon",
  "halibut discard mortality", "other", "Halibut discard mortality rates"
)

infer_stock <- function(label, filename = NA_character_) {
  hay <- paste(label %||% "", filename %||% "")
  hit <- stock_dictionary %>%
    filter(str_detect(hay, regex(pattern, TRUE))) %>%
    slice(1)

  if (!nrow(hit)) {
    tibble(stock_group = NA_character_, stock = NA_character_)
  } else {
    transmute(hit, stock_group, stock)
  }
}

infer_area <- function(label, filename = NA_character_, col_header = NA_character_) {
  hay <- paste(label %||% "", filename %||% "", col_header %||% "")
  case_when(
    str_detect(hay, regex("\\bBSAI\\b|Bering Sea and Aleutian Islands|Aleutian Islands", TRUE)) ~ "BSAI",
    str_detect(hay, regex("\\bGOA\\b|Gulf of Alaska", TRUE)) ~ "GOA",
    str_detect(hay, regex("BSAI/GOA|AK Sablefish|Alaska", TRUE)) ~ "BSAI/GOA",
    str_detect(hay, regex("Cook Inlet", TRUE)) ~ "Cook Inlet",
    TRUE ~ NA_character_
  )
}

infer_product_type <- function(label, filename = NA_character_, url = NA_character_) {
  hay <- paste(label %||% "", filename %||% "", url %||% "")
  case_when(
    str_detect(hay, regex("definitions", TRUE)) ~ "definitions",
    str_detect(hay, regex("report card", TRUE)) ~ "report_card",
    str_detect(hay, regex("\\bESP\\b", TRUE)) ~ "ESP",
    str_detect(hay, regex("\\bESR\\b|ecosystem status report", TRUE)) ~ "ESR",
    str_detect(hay, regex("appendix", TRUE)) ~ "appendix",
    str_detect(hay, regex("introduction|intro chapt", TRUE)) ~ "introduction",
    str_detect(hay, regex("economic safe|\\becon\\b", TRUE)) ~ "economic",
    str_detect(hay, regex("/resource/(data|document)/", TRUE)) ~ "document",
    str_detect(hay, regex("population-assessments|historical", TRUE)) ~ "historical_index",
    str_detect(hay, regex("safe|\\.pdf$", TRUE)) ~ "document",
    TRUE ~ "unknown"
  )
}

infer_true_year <- function(section, label, filename, url, product_type) {
  y_label <- extract_year(label)
  y_file <- extract_year(filename)
  y_dir <- extract_dir_year(url)

  y <- coalesce(y_label, y_file, y_dir)

  if (identical(section, "Groundfish SAFEs") &&
      !is.na(y_dir) && y_dir == 2025 &&
      is.na(y_label) && is.na(y_file) &&
      product_type == "document") {
    y <- 2025L
  }

  y
}

infer_role <- function(section, dir_year, label, product_type) {
  if (identical(section, "Groundfish SAFEs") && identical(dir_year, 2025L)) {
    if (str_detect(label %||% "", regex("\\b2026\\b", TRUE))) return("2026_update_material")
    if (product_type == "ESR") return("ecosystem_status_report")
    if (product_type == "definitions") return("definitions")
    if (product_type == "historical_index") return("historical_index")
    return("2025_catch_report_or_wrapper")
  }

  case_when(
    product_type == "historical_index" ~ "historical_index",
    product_type == "definitions" ~ "definitions",
    product_type == "ESR" ~ "ecosystem_status_report",
    product_type == "ESP" ~ "ecosystem_profile",
    product_type == "report_card" ~ "report_card",
    product_type == "appendix" ~ "appendix",
    product_type == "introduction" ~ "introduction",
    product_type == "economic" ~ "economic",
    product_type == "document" ~ "assessment_or_safe",
    TRUE ~ "unknown"
  )
}

flag_anomalies <- function(section, label, filename, dir_year, true_year) {
  flags <- character()

  if (!is.na(dir_year) && !is.na(true_year) && dir_year != true_year) {
    flags <- c(flags, "dir_year_differs_from_true_year")
  }

  if (identical(section, "Groundfish SAFEs") &&
      identical(dir_year, 2025L) &&
      str_detect(label %||% "", regex("\\b2026\\b", TRUE))) {
    flags <- c(flags, "2026_material_under_2025")
  }

  if (identical(filename, "GOApcod.pdf") &&
      str_detect(label %||% "", regex("\\b2026\\b", TRUE))) {
    flags <- c(flags, "goapcod_filename_reused")
  }

  tibble(
    is_anomalous = length(flags) > 0,
    anomaly_flags = paste(flags, collapse = "; ")
  )
}

classify_npfmc_link <- function(node, base_url = NPFMC_SAFE_URL) {
  label <- clean_text(xml_text(node, trim = TRUE))
  href <- safe_get_attr(node, "href")
  url <- url_absolute(href %||% "", base_url)
  filename <- extract_filename(url)

  ctx <- table_context(node)
  section <- nearest_section(node)

  stock_info <- infer_stock(label, filename)
  area <- infer_area(label, filename, ctx$col_header)
  product_type <- infer_product_type(label, filename, url)
  dir_year <- extract_dir_year(url)
  true_year <- infer_true_year(section, label, filename, url, product_type)
  role <- infer_role(section, dir_year, label, product_type)
  anomaly <- flag_anomalies(section, label, filename, dir_year, true_year)

  tibble(
    source = "NPFMC",
    section = section,
    page_url = base_url,
    parent_page = base_url,
    label = label,
    url = url,
    filename = filename,
    source_year_dir = dir_year,
    true_year = true_year,
    area = area,
    stock_group = stock_info$stock_group,
    stock = stock_info$stock,
    product_type = product_type,
    document_role = role,
    col_header = ctx$col_header,
    row_text = ctx$row_text,
    is_anomalous = anomaly$is_anomalous,
    anomaly_flags = anomaly$anomaly_flags
  )
}

scrape_npfmc_safe <- function(url = NPFMC_SAFE_URL) {
  read_html(url) %>%
    html_elements("a") %>%
    map_dfr(classify_npfmc_link, base_url = url) %>%
    filter(
      str_detect(url, regex("SAFE|population-assessments|CommentReview/DownloadFile", TRUE)) |
        str_detect(label, regex("SAFE|assessment|ESR|ESP|report card|appendix", TRUE))
    ) %>%
    filter(!is.na(url), url != "") %>%
    distinct(url, label, .keep_all = TRUE)
}

classify_noaa_index_link <- function(node, base_url = NOAA_GROUNDFISH_INDEX_URL) {
  label <- clean_text(xml_text(node, trim = TRUE))
  href <- safe_get_attr(node, "href")
  url <- url_absolute(href %||% "", base_url)

  tibble(
    label = label,
    url = url,
    year = extract_year(label),
    link_type = case_when(
      str_detect(label, regex("^\\d{4} North Pacific Groundfish Stock Assessments$", TRUE)) ~ "year_page",
      str_detect(label, regex("1998.?2012", TRUE)) ~ "historical_bundle",
      str_detect(label, regex("Halibut Discard Mortality", TRUE)) ~ "supporting_document",
      TRUE ~ "other"
    )
  )
}

scrape_noaa_groundfish_index <- function(url = NOAA_GROUNDFISH_INDEX_URL) {
  read_html(url) %>%
    html_elements("a") %>%
    map_dfr(classify_noaa_index_link, base_url = url) %>%
    filter(link_type != "other") %>%
    distinct(url, label, .keep_all = TRUE) %>%
    arrange(desc(year), label)
}

is_relevant_noaa_link <- function(label, url, filename = extract_filename(url)) {
  hay <- paste(label %||% "", filename %||% "", url %||% "")

  is_candidate <- str_detect(
    hay,
    regex(
      paste(
        c(
          stock_dictionary$pattern,
          "introduction",
          "ecosystem status report",
          "\\bESR\\b",
          "economic",
          "halibut discard",
          "discard mortality",
          "stock assessment",
          "fishery evaluation",
          "\\bSAFE\\b",
          "assessment"
        ),
        collapse = "|"
      ),
      TRUE
    )
  )

  !str_detect(hay, regex("archive|1998.?2012|historical bundle", TRUE)) & is_candidate
}

is_relevant_refm_link <- function(label, url, filename = extract_filename(url)) {
  hay <- paste(label %||% "", filename %||% "", url %||% "")

  str_detect(
    hay,
    regex(
      paste(
        c(
          stock_dictionary$pattern,
          "assessment",
          "\\bSAFE\\b",
          "economic status",
          "ecosystem",
          "halibut discard",
          "prohibited species",
          "appendix",
          "\\bEA\\b"
        ),
        collapse = "|"
      ),
      TRUE
    )
  ) &
    !str_detect(hay, regex("west coast|seminar|species|directory|site map|archive home", TRUE))
}

classify_refm_historic_option <- function(node, base_url = REFM_HISTORIC_URL) {
  label <- clean_text(xml_text(node, trim = TRUE))
  href <- safe_get_attr(node, "value")
  url <- url_absolute(href %||% "", base_url)
  filename <- extract_filename(url)
  context_year <- nearest_year_context(node)
  stock_info <- infer_stock(label, filename)
  area <- infer_area(label, filename, label)
  product_type <- infer_product_type(label, filename, url)

  tibble(
    source = "NOAA_REFM",
    section = "Groundfish SAFEs",
    page_url = base_url,
    parent_page = base_url,
    label = label,
    url = url,
    filename = filename,
    source_year_dir = coalesce(extract_path_year(url), extract_dir_year(url)),
    true_year = coalesce(extract_year(label), extract_path_year(url), extract_dir_year(url), context_year),
    area = area,
    stock_group = coalesce(stock_info$stock_group, "groundfish"),
    stock = stock_info$stock,
    product_type = product_type,
    document_role = infer_role("Groundfish SAFEs", extract_dir_year(url), label, product_type),
    col_header = NA_character_,
    row_text = NA_character_,
    is_anomalous = FALSE,
    anomaly_flags = "",
    link_type = case_when(
      is.na(url) | url == "" ~ "other",
      identical(strip_url_fragment(url), strip_url_fragment(base_url)) ~ "other",
      TRUE ~ "document"
    )
  )
}

classify_refm_historic_anchor <- function(node, base_url = REFM_HISTORIC_URL) {
  label <- clean_text(xml_text(node, trim = TRUE))
  href <- safe_get_attr(node, "href")
  url <- url_absolute(href %||% "", base_url)
  filename <- extract_filename(url)
  context_year <- nearest_year_context(node)
  stock_info <- infer_stock(label, filename)
  area <- infer_area(label, filename, label)
  product_type <- infer_product_type(label, filename, url)

  tibble(
    source = "NOAA_REFM",
    section = "Groundfish SAFEs",
    page_url = base_url,
    parent_page = base_url,
    label = label,
    url = url,
    filename = filename,
    source_year_dir = coalesce(extract_path_year(url), extract_dir_year(url)),
    true_year = coalesce(extract_year(label), extract_path_year(url), extract_dir_year(url), context_year),
    area = area,
    stock_group = coalesce(stock_info$stock_group, "groundfish"),
    stock = stock_info$stock,
    product_type = product_type,
    document_role = infer_role("Groundfish SAFEs", extract_dir_year(url), label, product_type),
    col_header = NA_character_,
    row_text = NA_character_,
    is_anomalous = FALSE,
    anomaly_flags = "",
    link_type = case_when(
      is.na(url) | url == "" ~ "other",
      has_document_extension(url) ~ "document",
      str_detect(url, regex("repository\\.library\\.noaa\\.gov/view/noaa/", TRUE)) ~ "document",
      TRUE ~ "other"
    )
  )
}

scrape_refm_historic_archive <- function(url = REFM_HISTORIC_URL) {
  page <- read_html(url)

  option_docs <- page %>%
    html_elements("option") %>%
    map_dfr(classify_refm_historic_option, base_url = url)

  anchor_docs <- page %>%
    html_elements("a") %>%
    map_dfr(classify_refm_historic_anchor, base_url = url)

  bind_rows(option_docs, anchor_docs) %>%
    filter(
      link_type == "document",
      !is.na(true_year),
      is_relevant_refm_link(label, url, filename)
    ) %>%
    select(-link_type) %>%
    distinct(url, label, .keep_all = TRUE) %>%
    arrange(desc(true_year), label)
}

classify_noaa_page_link <- function(node, page_url, year = NA_integer_, parent_page = NOAA_GROUNDFISH_INDEX_URL) {
  label <- clean_text(xml_text(node, trim = TRUE))
  href <- safe_get_attr(node, "href")
  url <- url_absolute(href %||% "", page_url)
  filename <- extract_filename(url)
  stock_info <- infer_stock(label, filename)
  area <- infer_area(label, filename, label)
  product_type <- infer_product_type(label, filename, url)
  inferred_year <- coalesce(year, extract_year(label), extract_year(filename), extract_year(url))

  link_type <- case_when(
    is.na(url) | url == "" ~ "other",
    is_same_page_anchor(url, page_url) ~ "page_anchor",
    has_document_extension(url) ~ "document",
    is_noaa_cover_page(url) ~ "cover_page",
    str_detect(url, regex("/population-assessments/", TRUE)) &&
      !is.na(extract_year(label %||% url)) ~ "year_page",
    str_detect(paste(label, url), regex("stock assessment|fishery evaluation|\\bSAFE\\b", TRUE)) ~ "document",
    TRUE ~ "other"
  )

  role <- case_when(
    str_detect(label, regex("introduction", TRUE)) ~ "introduction",
    product_type == "ESR" ~ "ecosystem_status_report",
    str_detect(label, regex("economic", TRUE)) ~ "economic",
    TRUE ~ "assessment_or_safe"
  )

  tibble(
    source = "NOAA",
    section = "Groundfish SAFEs",
    page_url = page_url,
    parent_page = parent_page,
    label = label,
    url = url,
    filename = filename,
    source_year_dir = extract_dir_year(url),
    true_year = inferred_year,
    area = area,
    stock_group = stock_info$stock_group,
    stock = stock_info$stock,
    product_type = product_type,
    document_role = role,
    col_header = NA_character_,
    row_text = NA_character_,
    is_anomalous = FALSE,
    anomaly_flags = "",
    link_type = link_type
  )
}

scrape_noaa_groundfish_page <- function(page_url,
                                        year = NA_integer_,
                                        parent_page = NOAA_GROUNDFISH_INDEX_URL,
                                        quietly = FALSE) {
  raw_links <- read_html(page_url) %>%
    html_elements("a") %>%
    map_dfr(classify_noaa_page_link, page_url = page_url, year = year, parent_page = parent_page) %>%
    filter(
      link_type %in% c("document", "cover_page"),
      !is.na(true_year),
      is_relevant_noaa_link(label, url, filename)
    )

  resolve_noaa_documents(raw_links, quietly = quietly) %>%
    distinct(url, label, .keep_all = TRUE)
}

scrape_noaa_historical_archive <- function(archive_url, quietly = FALSE) {
  archive_links <- read_html(archive_url) %>%
    html_elements("a") %>%
    map_dfr(classify_noaa_page_link, page_url = archive_url, parent_page = NOAA_GROUNDFISH_INDEX_URL)

  direct_docs <- archive_links %>%
    filter(
      link_type %in% c("document", "cover_page"),
      !is.na(true_year),
      dplyr::between(true_year, 1998L, 2012L),
      is_relevant_noaa_link(label, url, filename)
    )

  direct_docs <- resolve_noaa_documents(direct_docs, quietly = quietly)

  year_pages <- archive_links %>%
    filter(
      link_type == "year_page",
      !is.na(true_year),
      dplyr::between(true_year, 1998L, 2012L)
    ) %>%
    distinct(url, true_year)

  nested_docs <- map2_dfr(year_pages$url, year_pages$true_year, function(u, y) {
    if (!quietly) message("Crawling NOAA archive year page: ", y)
    tryCatch(
      scrape_noaa_groundfish_page(u, year = y, parent_page = archive_url, quietly = quietly),
      error = function(e) {
        warning("Failed on historical year ", y, ": ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
  })

  bind_rows(direct_docs, nested_docs) %>%
    distinct(url, label, .keep_all = TRUE)
}

crawl_noaa_groundfish_years <- function(index = NULL,
                                        years = NULL,
                                        quietly = FALSE) {
  if (is.null(index)) {
    index <- scrape_noaa_groundfish_index()
  }

  year_pages <- index %>%
    filter(link_type == "year_page", !is.na(year))

  if (!is.null(years)) {
    year_pages <- year_pages %>% filter(year %in% years)
  }

  out <- map2_dfr(year_pages$url, year_pages$year, function(u, y) {
    if (!quietly) message("Crawling NOAA year page: ", y)
    tryCatch(
      scrape_noaa_groundfish_page(u, year = y, quietly = quietly),
      error = function(e) {
        warning("Failed on year ", y, ": ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
  })

  historical_bundle <- index %>%
    filter(link_type == "historical_bundle") %>%
    pull(url)

  historical_docs <- map_dfr(historical_bundle, function(u) {
    if (!quietly) message("Crawling NOAA historical archive: ", u)
    tryCatch(
      scrape_noaa_historical_archive(u, quietly = quietly),
      error = function(e) {
        warning("Failed on historical archive ", u, ": ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
  })

  bind_rows(out, historical_docs)
}

resolve_noaa_cover_page <- function(record, quietly = FALSE) {
  cover_url <- record$url[[1]]

  if (!quietly) message("Resolving NOAA cover page: ", cover_url)

  candidates <- tryCatch(
    read_html(cover_url) %>%
      html_elements("a") %>%
      map_dfr(function(node) {
        target_label <- clean_text(xml_text(node, trim = TRUE))
        href <- safe_get_attr(node, "href")
        target_url <- url_absolute(href %||% "", cover_url)
        target_filename <- extract_filename(target_url)
        target_stock <- infer_stock(target_label, target_filename)
        target_area <- infer_area(target_label, target_filename, target_label)

        tibble(
          target_label = target_label,
          target_url = target_url,
          target_filename = target_filename,
          target_true_year = coalesce(
            extract_year(target_label),
            extract_year(target_filename),
            extract_path_year(target_url),
            extract_dir_year(target_url)
          ),
          target_stock_group = target_stock$stock_group,
          target_stock = target_stock$stock,
          target_area = target_area
        )
      }),
    error = function(e) {
      warning("Failed to resolve cover page ", cover_url, ": ", conditionMessage(e), call. = FALSE)
      tibble()
    }
  )

  if (!nrow(candidates)) {
    return(tibble())
  }

  base_year <- record$true_year[[1]]
  base_stock <- record$stock[[1]]
  base_area <- record$area[[1]]

  candidates <- candidates %>%
    filter(
      !is.na(target_url),
      target_url != "",
      !is_same_page_anchor(target_url, cover_url),
      has_document_extension(target_url)
    ) %>%
    mutate(
      score = 0L +
        if_else(!is.na(base_year) & !is.na(target_true_year) & target_true_year == base_year, 4L, 0L) +
        if_else(!is.na(base_year) & is.na(target_true_year), 1L, 0L) +
        if_else(str_detect(target_url, regex("apps-afsc\\.fisheries\\.noaa\\.gov/.*/Plan_Team/", TRUE)), 3L, 0L) +
        if_else(str_detect(target_url, regex("\\.pdf(?:$|[?#])", TRUE)), 2L, 0L) +
        if_else(!is.na(base_stock) & !is.na(target_stock) & target_stock == base_stock, 2L, 0L) +
        if_else(!is.na(base_area) & !is.na(target_area) & target_area == base_area, 1L, 0L) +
        if_else(str_detect(target_label, regex("download|full report|assessment|SAFE", TRUE)), 1L, 0L)
    ) %>%
    arrange(desc(score), target_url) %>%
    slice_head(n = 1)

  if (!nrow(candidates)) {
    return(tibble())
  }

  target_url <- candidates$target_url[[1]]
  target_filename <- candidates$target_filename[[1]]

  record %>%
    mutate(
      url = target_url,
      filename = target_filename,
      source_year_dir = coalesce(extract_path_year(target_url), extract_dir_year(target_url)),
      area = coalesce(area, candidates$target_area[[1]]),
      stock_group = coalesce(stock_group, candidates$target_stock_group[[1]]),
      stock = coalesce(stock, candidates$target_stock[[1]]),
      product_type = infer_product_type(label, target_filename, target_url),
      link_type = "document"
    )
}

resolve_noaa_documents <- function(records, quietly = FALSE) {
  if (!nrow(records)) {
    return(records %>% select(-any_of("link_type")))
  }

  direct_docs <- records %>%
    filter(link_type == "document") %>%
    select(-link_type)

  cover_pages <- records %>%
    filter(link_type == "cover_page")

  resolved_cover_docs <- map_dfr(seq_len(nrow(cover_pages)), function(i) {
    resolve_noaa_cover_page(cover_pages[i, , drop = FALSE], quietly = quietly)
  }) %>%
    select(-any_of("link_type"))

  bind_rows(direct_docs, resolved_cover_docs) %>%
    filter(is_relevant_noaa_link(label, url, filename)) %>%
    distinct(url, label, .keep_all = TRUE)
}

build_safe_catalog <- function(crawl_noaa = TRUE,
                               noaa_years = NULL,
                               quietly = FALSE) {
  npfmc <- scrape_npfmc_safe()

  if (!crawl_noaa) {
    return(npfmc %>% arrange(desc(true_year), stock, label))
  }

  noaa_index <- scrape_noaa_groundfish_index()
  noaa <- crawl_noaa_groundfish_years(index = noaa_index, years = noaa_years, quietly = quietly)
  refm <- tryCatch(
    scrape_refm_historic_archive(),
    error = function(e) {
      warning("Failed on REFM historic archive: ", conditionMessage(e), call. = FALSE)
      tibble()
    }
  )

  bind_rows(npfmc, noaa, refm) %>%
    mutate(
      stock_group = coalesce(stock_group, case_when(
        section == "Groundfish SAFEs" ~ "groundfish",
        section == "Crab SAFEs" ~ "crab",
        section == "Scallop SAFEs" ~ "scallop",
        section == "Cook Inlet Salmon SAFEs" ~ "salmon",
        TRUE ~ NA_character_
      ))
    ) %>%
    distinct(source, url, label, .keep_all = TRUE) %>%
    arrange(desc(true_year), source, stock, label)
}

summarise_safe_catalog <- function(df) {
  list(
    stock_inventory = df %>%
      filter(!is.na(stock)) %>%
      count(section, stock_group, stock, area, sort = TRUE),

    anomalies = df %>%
      filter(is_anomalous) %>%
      arrange(section, stock, filename),

    filename_collisions = df %>%
      filter(!is.na(filename), !is.na(true_year)) %>%
      distinct(source, filename, true_year) %>%
      add_count(filename, name = "n_true_years") %>%
      filter(n_true_years > 1) %>%
      arrange(filename, true_year),

    year_source_counts = df %>%
      filter(!is.na(true_year)) %>%
      count(source, section, true_year, name = "n_docs")
  )
}

plot_safe_docs_by_year <- function(df) {
  df %>%
    filter(!is.na(true_year)) %>%
    mutate(plot_source = if_else(source == "NPFMC", "NPFMC", "NOAA")) %>%
    count(plot_source, true_year, name = "n_docs") %>%
    ggplot(aes(true_year, n_docs, color = plot_source, shape = plot_source)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.2) +
    scale_color_manual(values = c("NOAA" = "#1b6ca8", "NPFMC" = "#c76b29")) +
    labs(
      x = "Effective year",
      y = "Document count",
      title = "SAFE-linked documents by effective year",
      subtitle = "NPFMC versus NOAA entry points, with REFM historic links grouped under NOAA",
      color = "Source",
      shape = "Source"
    ) +
    theme_minimal(base_size = 11)
}

derive_stock_fmp <- function(df) {
  df %>%
    mutate(
      fmp_hay = paste(label %||% "", filename %||% "", url %||% "", area %||% "", stock %||% ""),
      fmp = case_when(
        str_detect(fmp_hay, regex("\\bGOA\\b|Gulf of Alaska", TRUE)) ~ "GOA",
        str_detect(fmp_hay, regex("\\bAI\\b|Aleutian Islands|Bogoslof", TRUE)) ~ "BSAI",
        str_detect(fmp_hay, regex("\\bEBS\\b|Eastern Bering Sea|Bering Sea and Aleutian Islands|\\bBSAI\\b", TRUE)) ~ "BSAI",
        stock %in% c("Sablefish", "Grenadier") ~ "GOA",
        area == "GOA" ~ "GOA",
        area == "BSAI" ~ "BSAI",
        area == "BSAI/GOA" ~ "GOA",
        TRUE ~ NA_character_
      ),
      fmp_subarea = case_when(
        fmp == "GOA" & stock == "Sablefish" ~ "Both",
        fmp == "GOA" & stock == "Grenadier" ~ "Both",
        fmp == "GOA" ~ "GOA",
        fmp == "BSAI" & str_detect(fmp_hay, regex("\\bAI\\b|Aleutian Islands|Bogoslof", TRUE)) ~ "AI",
        fmp == "BSAI" & str_detect(fmp_hay, regex("\\bEBS\\b|Eastern Bering Sea", TRUE)) ~ "EBS",
        fmp == "BSAI" ~ "Both",
        TRUE ~ NA_character_
      ),
      stock_fmp_label = case_when(
        !is.na(fmp) & !is.na(fmp_subarea) ~ paste0(stock, " (", fmp, ": ", fmp_subarea, ")"),
        !is.na(fmp) ~ paste0(stock, " (", fmp, ")"),
        TRUE ~ stock
      )
    ) %>%
    select(-fmp_hay)
}

plot_stock_coverage <- function(df, n = 20) {
  df_fmp <- derive_stock_fmp(df)

  top_stocks <- df_fmp %>%
    filter(!is.na(stock)) %>%
    count(stock_fmp_label, sort = TRUE) %>%
    slice_head(n = n)

  df_fmp %>%
    semi_join(top_stocks, by = "stock_fmp_label") %>%
    filter(!is.na(true_year), !is.na(stock), !is.na(fmp), !is.na(fmp_subarea)) %>%
    count(fmp, fmp_subarea, stock_fmp_label, true_year, name = "n_docs") %>%
    ggplot(aes(true_year, reorder(stock_fmp_label, true_year), size = n_docs, color = fmp_subarea)) +
    geom_point(alpha = 0.85) +
    facet_grid(fmp ~ ., scales = "free_y", space = "free_y") +
    labs(
      x = "Effective year",
      y = NULL,
      title = "Coverage by stock and year",
      subtitle = paste("Top", n, "stock-by-FMP groups, with GOA and BSAI sub-categories"),
      color = "Sub-area"
    ) +
    theme_minimal(base_size = 11)
}

write_safe_outputs <- function(df, out_dir = ".") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sums <- summarise_safe_catalog(df)
  resolved_noaa_terminal_links <- df %>%
    filter(
      source %in% c("NOAA", "NOAA_REFM"),
      !str_detect(url, fixed("fisheries.noaa.gov/resource/data"))
    ) %>%
    arrange(desc(true_year), stock, label)

  write_csv(df, file.path(out_dir, "safe_catalog.csv"))
  write_csv(sums$stock_inventory, file.path(out_dir, "safe_stock_inventory.csv"))
  write_csv(sums$anomalies, file.path(out_dir, "safe_anomalies.csv"))
  write_csv(sums$filename_collisions, file.path(out_dir, "safe_filename_collisions.csv"))
  write_csv(sums$year_source_counts, file.path(out_dir, "safe_year_source_counts.csv"))
  write_csv(resolved_noaa_terminal_links, file.path(out_dir, "safe_resolved_noaa_terminal_links.csv"))

  invisible(sums)
}
