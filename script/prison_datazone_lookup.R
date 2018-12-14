
# load packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(readxl)
library(here)


# download postcode to SIMD lookup ----------------------------------------

download.file("https://www2.gov.scot/Resource/0053/00534448.xlsx",
              destfile = here("data", "simd_postcode_lookup.xlsx"),
              mode = "wb")

postcode_lookup <- read_xlsx(here("data", "simd_postcode_lookup.xlsx"), sheet = 2)



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
    TRUE ~ "HMP"
  )) %>% 
  mutate(prison = str_replace(prison, " ", "-"),
         url = glue::glue("http://www.sps.gov.uk/Corporate/Prisons/{prison}/{prefix}-{prison}.aspx"))

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
