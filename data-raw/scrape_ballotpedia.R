# Scrape 2026 primary candidate parties from Ballotpedia's per-county pages.
#
# Ballotpedia loads each county's ballot via JavaScript and CloudFront blocks the
# default headless user-agent, so we drive headless Chrome via {chromote} with a
# spoofed normal UA. Each county page has <div class="widget-data-list"> blocks:
#   <p><b>Office</b></p><ul><li><a title="Name (Office, KY, candidate 2026)">Name</a> (R)</li>...
#
# Output: data-raw/ballotpedia_2026_raw.csv  (county, office_bp, candidate, party)
# written incrementally so partial progress survives. Matching to the unresolved
# list happens in match_ballotpedia.R.
#
# Usage:
#   Rscript data-raw/scrape_ballotpedia.R        # all counties
#   Rscript data-raw/scrape_ballotpedia.R 3      # first 3 counties (test)

suppressMessages({ library(httr); library(rvest); library(chromote); library(dplyr); library(stringr); library(readr); library(xml2) })
options(chromote.chrome = "C:/Program Files/Google/Chrome/Application/chrome.exe")
realUA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
out_csv <- "data-raw/ballotpedia_2026_raw.csv"
limit <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))

# County page URLs from the hub (httr is not blocked for static HTML).
hub <- GET("https://ballotpedia.org/Kentucky_local_election_coverage,_2026",
           user_agent("Mozilla/5.0 (research; KY civic data app; rkahne@gmail.com)"), timeout(45))
links <- read_html(content(hub, "text", encoding = "UTF-8")) %>% html_elements("a") %>% html_attr("href")
county_urls <- unique(links[!is.na(links) & str_detect(links, "_County,_Kentucky,_elections,_2026$")])
county_urls <- paste0("https://ballotpedia.org", county_urls)
if (!is.na(limit)) county_urls <- head(county_urls, limit)
cat("counties to scrape:", length(county_urls), "\n")

parse_county <- function(html, county) {
  pg <- read_html(html)
  lis <- pg %>% html_elements("div.widget-data-list ul li")
  if (length(lis) == 0) return(tibble())
  a    <- lis %>% html_element("a")
  name <- a %>% html_text2()
  ttl  <- a %>% html_attr("title")
  txt  <- lis %>% html_text2()
  tibble(county   = county,
         office_bp = str_match(ttl, "\\(([^()]*?),\\s*Kentucky,\\s*candidate")[, 2],
         candidate = name,
         party    = str_match(txt, "\\((R|D)\\)")[, 2]) %>%
    filter(!is.na(candidate), !is.na(party)) %>%
    mutate(party = if_else(party == "R", "REP", "DEM")) %>%
    distinct()
}

b <- ChromoteSession$new(); on.exit(try(b$close(), silent = TRUE), add = TRUE)
b$Network$setUserAgentOverride(userAgent = realUA)
ready_js <- "(function(){var d=document.querySelectorAll('.widget-data-list li a');return d.length;})()"

all <- list()
if (file.exists(out_csv)) file.remove(out_csv)
for (i in seq_along(county_urls)) {
  cn <- str_match(county_urls[i], "/([A-Za-z_]+)_County")[, 2] %>% str_replace_all("_", " ")
  res <- tryCatch({
    b$Page$navigate(county_urls[i])
    n <- 0
    for (t in 1:15) { Sys.sleep(1); n <- b$Runtime$evaluate(ready_js)$result$value; if (!is.null(n) && n > 0) break }
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    parse_county(html, cn)
  }, error = function(e) { message("  ! ", cn, ": ", conditionMessage(e)); tibble() })
  all[[i]] <- res
  cat(sprintf("  [%3d/%3d] %-16s %3d candidates\n", i, length(county_urls), cn, nrow(res)))
  write_csv(bind_rows(all), out_csv)   # incremental save
  Sys.sleep(1)
}
cat("\nTotal candidate rows:", nrow(bind_rows(all)), "-> ", out_csv, "\n")
