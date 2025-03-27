ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE))
}
library(stringr)
library(dplyr)

ensure_package(httr2)
ensure_package(rvest)
ensure_package(datasets)
ensure_package(purrr)
ensure_package(DT)

get_eia_sep <- function(state, abbr){
  state_formatted <- str_to_lower(state) |> str_replace_all("\\s", "")
  
  dir_name <- file.path("data", "mp02")
  file_name <- file.path(dir_name, state_formatted)
  
  dir.create(dir_name, showWarnings=FALSE, recursive=TRUE)
  
  if(!file.exists(file_name)){
    BASE_URL <- "https://www.eia.gov"
    REQUEST <- request(BASE_URL) |> 
      req_url_path("electricity", "state", state_formatted)
    
    RESPONSE <- req_perform(REQUEST)
    
    resp_check_status(RESPONSE)
    
    writeLines(resp_body_string(RESPONSE), file_name)
  }
  
  TABLE <- read_html(file_name) |> 
    html_element("table") |> 
    html_table() |>
    mutate(Item = str_to_lower(Item))
  
  if("U.S. rank" %in% colnames(TABLE)){
    TABLE <- TABLE |> rename(Rank = `U.S. rank`)
  }
  
  CO2_MWh <- TABLE |> 
    filter(Item == "carbon dioxide (lbs/mwh)") |>
    pull(Value) |> 
    str_replace_all(",", "") |>
    as.numeric()
  
  PRIMARY <- TABLE |> 
    filter(Item == "primary energy source") |> 
    pull(Rank)
  
  RATE <- TABLE |>
    filter(Item == "average retail price (cents/kwh)") |>
    pull(Value) |>
    as.numeric()
  
  GENERATION_MWh <- TABLE |>
    filter(Item == "net generation (megawatthours)") |>
    pull(Value) |>
    str_replace_all(",", "") |>
    as.numeric()
  
  data.frame(CO2_MWh               = CO2_MWh, 
             primary_source        = PRIMARY,
             electricity_price_MWh = RATE * 10, # / 100 cents to dollars &
             # * 1000 kWh to MWH 
             generation_MWh        = GENERATION_MWh, 
             state                 = state, 
             abbreviation          = abbr
  )
}

EIA_SEP_REPORT <- map2(state.name, state.abb, get_eia_sep) |> list_rbind()

ensure_package(scales)
ensure_package(DT)

EIA_SEP_REPORT |> 
  select(-abbreviation) |>
  arrange(desc(CO2_MWh)) |>
  mutate(CO2_MWh = number(CO2_MWh, big.mark=","), 
         electricity_price_MWh = dollar(electricity_price_MWh), 
         generation_MWh = number(generation_MWh, big.mark=",")) |>
  rename(`Pounds of CO2 Emitted per MWh of Electricity Produced`=CO2_MWh, 
         `Primary Source of Electricity Generation`=primary_source, 
         `Average Retail Price for 1000 kWh`=electricity_price_MWh, 
         `Total Generation Capacity (MWh)`= generation_MWh, 
         State=state) |>
  datatable()