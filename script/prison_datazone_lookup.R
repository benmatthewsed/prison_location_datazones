# load packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(readxl)
library(here)


# download postcode to SIMD lookup ----------------------------------------

download.file(url = "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-postcode-look-up-file/documents/simd-2020-postcode-lookup-v5/simd-2020-postcode-lookup-v5/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bpostcode%2Blookup%2B-%2Bupdated%2B2023%25232.xlsx",
              destfile = here("sssp007", "data", "simd_postcode_lookup.xlsx"),
              mode = "wb")

postcode_lookup <- read_xlsx(here("sssp007", "data", "simd_postcode_lookup.xlsx"), sheet = 2)



# scrape prisons address data ---------------------------------------------

# get list of prisons
sps <- read_html("http://www.sps.gov.uk/Corporate/Prisons/Prisons.aspx/")

sps_nodes <- html_nodes(sps, ".prison-name")

# make URL for each prison using right prefix
prisons <- 
  tibble(prison = html_text(sps_nodes)) %>% 
  mutate(prefix = case_when(
    prison == "Cornton Vale" ~ "HMP-YOI",
    prison == "Polmont" ~ "HMP-YOI",
    prison == "Grampian" ~ "HMP-YOI",
    prison == "Stirling" ~ "HMP-YOI",
    str_detect(prison, "Centre") ~ "",
    TRUE ~ "HMP"
  )) %>% 
  mutate(prison = str_replace(prison, " ", "-"),
         url_prison = if_else(str_detect(prison, "Centre"),
                              str_remove(prison, "-"),
                              prison),
         url = case_when(
           !str_detect(prison, "Centre") ~ glue::glue("http://www.sps.gov.uk/Corporate/Prisons/{url_prison}/{prefix}-{prison}.aspx"),
           TRUE ~ glue::glue("http://www.sps.gov.uk/Corporate/Prisons/{url_prison}/{prefix}{prison}.aspx")
           ),
         url = if_else(str_detect(prison, "Centre"),
                          glue::glue("http://www.sps.gov.uk/Corporate/Prisons/{url_prison}/{prefix}{prison}.aspx"),
                          url))




prisons$url[[3]]

prisons$url[[12]]

prisons$url[[17]]

safe_read_html <- purrr::safely(read_html)

# read html
prisons <- 
  prisons %>% 
  mutate(html = map(url, read_html))


# scrape addresses
prisons <- 
  prisons %>% 
  mutate(address_node = map2(html, 
                             "#prison-main-r .clearfix:nth-child(4) strong",
                             html_nodes),
         address = map(address_node, html_text))

# extract postcodes
prisons <- 
  prisons %>% 
  select(prison, address) %>% 
  unnest() %>% 
  mutate(postcode = str_sub(address,
                            str_length(address) - 7,
                            str_length(address)),
         postcode = str_replace(postcode, "\r", "")) %>% 
  select(-address)



# merge with datazones ---------------------------------------------------------

prisons <- left_join(prisons, postcode_lookup, by = c("postcode" = "Postcode"))

data_zones <- tibble(data_zone = unique(postcode_lookup$DZ))

data_zone_prison <- left_join(data_zones, prisons, by = c("data_zone" = "DZ"))

data_zone_prison <- 
  data_zone_prison %>% 
  select(data_zone, prison) %>% 
  mutate(has_prison = if_else(is.na(prison), 0, 1))



# save file as csv --------------------------------------------------------

write_csv(data_zone_prison,
          here("data", "data_zone_prison.csv"))
