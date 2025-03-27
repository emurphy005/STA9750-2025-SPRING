#Task 1 - download EIA Data w/ instructor code 
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

#Task 2 
#Which state has the most expensive retail electricity 
Most_expensive_state <- EIA_SEP_REPORT|>
  arrange(desc(electricity_price_MWh))|>
  slice(1)
print(Most_expensive_state)
#most expensive state is Hawaii 
#Which state has the dirtiest mix 
dirtiest_mix <- EIA_SEP_REPORT|>
  filter(primary_source == 'Coal')|>
  arrange(desc(CO2_MWh))|>
  slice(1)
print(dirtiest_mix)
#west virginia has the dirtiest mix 
#Average pounds of CO2 are emmitted per MWH electirictie produced 
CO_per_MWh_produced <- sum(EIA_SEP_REPORT$CO2_MWh, na.rm = TRUE) /
  sum(EIA_SEP_REPORT$generation_MWh, na.rm = TRUE)
print(CO_per_MWh_produced)
#1.016615e-05
#Rarest primary energy source in the US? What is the associated cost of electricity and where is it used?
rarest_energy <- EIA_SEP_REPORT |>
  group_by(primary_source)|>
  summarise(total_MWh = sum(generation_MWh, na.rm = TRUE))|>
  arrange(total_MWh)|>
  slice(1)
print(rarest_energy)
rare_cost <- EIA_SEP_REPORT|>
  filter(primary_source == 'Petroleum')
print(rare_cost)
#petroleum in Hawaii and cost $1,444
#Texas vs NY -> CO2 intensity = total CO2 emmissions/total generation capacity 
#TX C02 Intensity/NY CO2 Intensity 
TX <- EIA_SEP_REPORT|>
  filter(state == c("Texas"))
NY <- EIA_SEP_REPORT|>
  filter(state == c("New York"))
CO2_Intensity_TX <-TX$CO2_MWh / TX$generation_MWh
CO2_Intensity_NY <- NY$CO2_MWh / NY$generation_MWh

percent_dirier <- CO2_Intensity_TX / CO2_Intensity_NY
print(percent_dirier * 100)
#NY is 37.12% cleaner than TX

#Downloading the 2023 Annual Database Energy Consumption report 
ensure_package(readxl)
# Create 'data/mp02' directory if not already present
DATA_DIR <- file.path("data", "mp02")
dir.create(DATA_DIR, showWarnings=FALSE, recursive=TRUE)

NTD_ENERGY_FILE <- file.path(DATA_DIR, "2023_ntd_energy.xlsx")

if(!file.exists(NTD_ENERGY_FILE)){
  DS <- download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-10/2023%20Energy%20Consumption.xlsx", 
                      destfile=NTD_ENERGY_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_ENERGY_FILE)$size == 0)){
    cat("I was unable to download the NTD Energy File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_ENERGY_RAW <- read_xlsx(NTD_ENERGY_FILE)
#File Clean up 
ensure_package(tidyr)
to_numeric_fill_0 <- function(x){
  x <- if_else(x == "-", NA, x)
  replace_na(as.numeric(x), 0)
}

NTD_ENERGY <- NTD_ENERGY_RAW |> 
  select(-c(`Reporter Type`, 
            `Reporting Module`, 
            `Other Fuel`, 
            `Other Fuel Description`)) |>
  mutate(across(-c(`Agency Name`, 
                   `Mode`,
                   `TOS`), 
                to_numeric_fill_0)) |>
  group_by(`NTD ID`, `Mode`, `Agency Name`) |>
  summarize(across(where(is.numeric), sum), 
            .groups = "keep") |>
  mutate(ENERGY = sum(c_across(c(where(is.numeric))))) |>
  filter(ENERGY > 0) |>
  select(-ENERGY) |>
  ungroup()
# Display 10 random rows
slice_sample(NTD_ENERGY , n=10)

#Task 3 - recode the Mode column
NTD_ENERGY <- NTD_ENERGY |>
  mutate(Mode=case_when(
    Mode == "HR" ~ "Heavy Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Cars",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail and Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybuses",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"))
NTD_ENERGY

#2023 Annual Database Service by Agency
library(readr)
NTD_SERVICE_FILE <- file.path(DATA_DIR, "2023_service.csv")
if(!file.exists(NTD_SERVICE_FILE)){
  DS <- download.file("https://data.transportation.gov/resource/6y83-7vuw.csv", 
                      destfile=NTD_SERVICE_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_SERVICE_FILE)$size == 0)){
    cat("I was unable to download the NTD Service File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_SERVICE_RAW <- read_csv(NTD_SERVICE_FILE) 

NTD_SERVICE <- NTD_SERVICE_RAW |>
  mutate(`NTD ID` = as.numeric(`_5_digit_ntd_id`)) |> 
  rename(Agency = agency, 
         City   = max_city, 
         State  = max_state,
         UPT    = sum_unlinked_passenger_trips_upt, 
         MILES  = sum_passenger_miles) |>
  select(matches("^[A-Z]", ignore.case=FALSE)) |>
  filter(MILES > 0)
NTD_SERVICE

#Task 4 
#Which transit service has the most UPT annually?
most_UPT_annually <- NTD_SERVICE |>
  arrange(desc(UPT))|>
  slice(1)
print(most_UPT_annually)
#MTA has the most UPT annually 

#average trip length of a trip on MTA NYC
MTA_data <- NTD_SERVICE |>
  filter(Agency == c("MTA New York City Transit"))
NYC_ave_trip_length <- MTA_data$MILES /MTA_data$UPT
print(NYC_ave_trip_length)
#3.644089 miles 

#Which transit service in NYC has the longest average trip length?
NYC_longest <- NTD_SERVICE |>
  filter(grepl("Brooklyn|New York", City, ignore.case = TRUE))|>
  group_by(Agency) |>
  summarise(Ave_trip_length = sum(MILES)/sum (UPT), .groups = "drop")|>
  arrange(desc(Ave_trip_length))
print(NYC_longest)
#MTA Long Island railroad 

#Which state has the fewest total miles travelled by public transit?
fewest_total_miles <- NTD_SERVICE |>
  group_by(State)|>
  summarise(Total_Miles_State = sum(MILES, na.rm = TRUE), .groups = "drop")|>
  arrange(Total_Miles_State) |>
  slice(1)
print(fewest_total_miles)
#NH

#Are all states represented in this data? If no, which ones are missing? 
states_included <- unique(NTD_SERVICE$State)
missing_states <- setdiff(state.abb, states_included)
print(missing_states)
state.name[match(missing_states, state.abb)]
#Arizona, Arkansas, California, Colorado, Hawaii, Iowa, Kansas, Louisiana, Missouri,
#Montana, Nebraska, Nevada, New Mexico, North Dakota, Oklahoma, South Dakota, Texas, Utah, Wyoming

#Task 5: Joining Tables 
#fixing names in the tables 
NTD_Energy_updated <- NTD_ENERGY|>
  rename(Agency = `Agency Name`)
NTD_Energy_updated
NTD_SERVICE_updated <- NTD_SERVICE |>
  rename(abbreviation = State)
NTD_SERVICE_updated

#joining NTD_Service and NTD_energy
joined_data <- NTD_SERVICE_updated|>
  left_join(NTD_Energy_updated, by = c("Agency" = "Agency", "NTD ID" =  "NTD ID"))
joined_data

#pivoting fuel types to columns 
joined_data_long <- joined_data |>
  pivot_longer(cols = c(`Bio-Diesel`, `Bunker Fuel`, `C Natural Gas`, `Diesel Fuel`, `Electric Battery`, `Electric Propulsion`, Ethanol, Methonal, Gasoline, Hydrogen, Kerosene, `Liquified Nat Gas`,`Liquified Petroleum Gas`),
               names_to = "Fuel_Type",
               values_to = "Usage")|>
  filter(Usage > 0) 

joined_data_long

#joinging EIA Sep Report 
final_data <- joined_data_long |>
  left_join(EIA_SEP_REPORT, by = "abbreviation")|>
  select(Agency, City, abbreviation, UPT, MILES, `NTD ID`, Mode, Fuel_Type, Usage, CO2_MWh)|>
  distinct()
final_data

#Computing total emissions for each row (in pounds) (x10)
co2_factors <- c(
  "Diesel Fuel" = 1634.5,
  "Gasoline" = 1677.9,  #motor gasoline
  "Electric Battery" = 0,
  "Bio-Diesel" = 2251.3,
  "Electric Propulsion" = 0,
  "C Natural Gas" = 1166.5,
  "Liquified Petroleum Gas" = 2251.3,
  "Hydrogen" = 260.3 #steam
)

Total_C02_Emissions <- final_data|>
  mutate(CO2_Emissions = case_when(
  Fuel_Type =="Diesel Fuel" ~ 1634.5,
  Fuel_Type == "Gasoline" ~ 1677.9,  #motor gasoline
  Fuel_Type == "Electric Battery" ~ 0,
  Fuel_Type == "Bio-Diesel" ~ 2251.3,
  Fuel_Type == "Electric Propulsion" ~ 0,
  Fuel_Type == "C Natural Gas" ~ 1166.5,
  Fuel_Type == "Liquified Petroleum Gas" ~ 2251.3,
  Fuel_Type == "Hydrogen" ~ 260.3, #steam
  TRUE ~ 0 
    )
  )|>
  mutate(total_CO2 = CO2_Emissions*Usage)

Total_C02_Emissions

total_emissions_per_agency <- Total_C02_Emissions |>
  group_by(Agency)|>
  summarise(Total_CO2 = sum(CO2_Emissions, na.rm = TRUE),
  total_upt = sum(UPT),                  # Sum UPT across modes
  total_miles = sum(MILES))|>
  arrange(desc(Total_CO2))

total_emissions_per_agency

#compute per UPT and per Mile 
updated_final <- total_emissions_per_agency |>
  mutate(
    CO2_per_UPT = Total_CO2/total_upt,
    CO2_perMile = Total_CO2/total_miles
  )
updated_final

#categorize by size (based on UPT)
total_size <- updated_final |>
  arrange(total_upt) |>
  mutate(
    Agency_Size = ntile(total_upt, 3))
total_size

total_size<- total_size |>
  mutate(
    Agency_Size = case_when(
      Agency_Size == 1 ~ "Small",
      Agency_Size == 2 ~ "Medium",
      Agency_Size == 3 ~ "Large",
      TRUE ~ "Unknown"
    )
  )
total_size

#separate by size 
#small
small_agencies <- total_size|>
  filter(Agency_Size == "Small")
small_agencies
#medium
medium_agencies <- total_size |>
  filter(Agency_Size == "Medium")
medium_agencies

#large
large_agency <- total_size |>
  filter(Agency_Size =="Large")

#Most efficient small agency 
most_efficient_upt_small <- small_agencies |>
  arrange(CO2_per_UPT)
most_efficient_upt_small
#Pennsylvania Department of Transportation, Harrisburg PA because of electric propulsion as their fuel source

#Most efficient medium agency 
most_efficient_upt_medium <- medium_agencies |>
  arrange(CO2_per_UPT)
most_efficient_upt_medium
# Port Authority Transit Corporation, Camden NJ because of electric propulsion as their fuel source

#Most efficient large agency 
most_efficient_upt_large <- large_agency |>
  arrange(CO2_per_UPT)
most_efficient_upt_large
#Port Authority Trans-Hudson Corporation, NY, NY because of electric propulsion as their fuel source

test4 <- final_data |>
  filter(Agency == "Pennsylvania Department of Transportation")
test4

#most admissions avoided

mpg <- 27.1
co2_per_gallon <- 19.6
#small
small_agencies <- small_agencies |>
  mutate(
    driving_emissions = (total_miles/mpg) * co2_per_gallon,
    emissions_avoided = driving_emissions - Total_CO2
  )
most_avoided_small <- small_agencies |>
  arrange(desc(emissions_avoided))
most_avoided_small
#Hampton Jitney, Inc. 

#medium
medium_agencies <- medium_agencies |>
  mutate(
    driving_emissions = (total_miles/mpg) * co2_per_gallon,
    emissions_avoided = driving_emissions - Total_CO2
  )
most_avoided_medium <- medium_agencies |>
  arrange(desc(emissions_avoided))

most_avoided_medium
#Municipality of Anchorage  

#Large
large_agency <- large_agency |>
  mutate(
    driving_emissions = (total_miles/mpg) * co2_per_gallon,
    emissions_avoided = driving_emissions - Total_CO2
  )
most_avoided_large <- large_agency |>
  arrange(desc(emissions_avoided))

most_avoided_large
#MTA New York City Transit  
emissions_avoided_data <- data.frame(
  category = c("Trees Planted (747,189 trees)", 
             "Cars Off the Road (3,537 cars)", 
             "Homes Powered (2,170 homes)", 
             "Flights Avoided (18,070 flights)"),
  emissions_avoided = c(747189, 3537, 2170, 18070))

library(ggplot2)
library(dplyr)
library(ggtext)

ggplot(emissions_avoided_data, aes(x = reorder(category, emissions_avoided), y = emissions_avoided, fill = category)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Simplified bars
  coord_flip() +  # Horizontal bars
  labs(title = "Emissions Avoided by Agency", 
       subtitle = "How much CO2 emissions were avoided by transit usage?",
       x = "", y = "Emissions Avoided (units)") +
  theme_minimal() +  # Clean theme with minimal style
  geom_text(aes(label = scales::comma(emissions_avoided)), vjust = -0.5, size = 5)  # Add text labels

#Most improved small agency 
avg_emission_per_mile_small <- mean(small_agencies$CO2_perMile, na.rm = TRUE)
most_improved_small <- small_agencies |>
  mutate(
    efficiency_improvement_mile = avg_emission_per_mile_small - CO2_perMile
  )
most_improved_efficiency_mile_small <-most_improved_small |>
  filter(!is.na(efficiency_improvement_mile)) |>
  arrange(desc(efficiency_improvement_mile)) |>
  slice(1)
most_improved_efficiency_mile_small

#Most improved medium agency 
avg_emission_per_mile_md <- mean(medium_agencies$CO2_perMile, na.rm = TRUE)
most_improved_md <- medium_agencies |>
  mutate(
    efficiency_improvement_mile = avg_emission_per_mile_md - CO2_perMile
  )
most_improved_efficiency_mile_md <-most_improved_md |>
  filter(!is.na(efficiency_improvement_mile)) |>
  arrange(desc(efficiency_improvement_mile)) |>
  slice(1)
most_improved_efficiency_mile_md

#Most improved large agency 
avg_emission_per_mile_lg <- mean(large_agency$CO2_perMile, na.rm = TRUE)
most_improved_lg <- large_agency |>
  mutate(
    efficiency_improvement_mile = avg_emission_per_mile_lg - CO2_perMile
  )
most_improved_efficiency_mile_lg <-most_improved_lg |>
  filter(!is.na(efficiency_improvement_mile)) |>
  arrange(desc(efficiency_improvement_mile)) |>
  slice(1)
most_improved_efficiency_mile_lg

#dirtiest mix
dirtiest_electric <- Total_C02_Emissions |>
  filter(Fuel_Type == "Gasoline")|>
  arrange(desc(total_CO2))
dirtiest_electric

#small range
sm_lowest <- min(small_agencies$total_upt, na.rm = TRUE)
sm_largest <- max(small_agencies$total_upt, na.rm = TRUE)
print(sm_lowest)
print(sm_largest)

#medium range
md_lowest <- min(medium_agencies$total_upt, na.rm = TRUE)
md_largest <- max(medium_agencies$total_upt, na.rm = TRUE)
print(md_lowest)
print(md_largest)

#large range
lg_lowest <- min(large_agency$total_upt, na.rm = TRUE)
lg_largest <- max(large_agency$total_upt, na.rm = TRUE)
print(lg_lowest)
print(lg_largest)


library(ggplot2)
library(dplyr)

#small
emissions_data <- data.frame(
  Category = c("Emissions Avoided", "Trees Planted", "Cars Off Road", "New Emissions Value"),  # Adding the new category
  Emissions = c(59563931, 1180000, 5963, 35823050)  # Values for emissions avoided, tree planting, cars off the road, and the new value
)
ggplot(emissions_data, aes(x = Category, y = Emissions, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("skyblue", "green", "orange", "red")) +  # Different colors for each category
  labs(title = "CO2 Emissions Avoided vs. Environmental Equivalents", 
       y = "CO2 Emissions / Equivalents", 
       x = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_y_log10(labels = scales::comma)

#medium
emissions_data <- data.frame(
  Category = c("Emissions Avoided", "Trees Planted", "Cars Off Road"),  # Different environmental comparisons
  Emissions = c(59563931, 1180000, 5963)  # Values for emissions avoided, tree planting, and cars off the road
)

ggplot(emissions_data, aes(x = Category, y = Emissions, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("skyblue", "green", "orange")) +  # Colors for different categories
  labs(title = "CO2 Emissions Avoided vs. Environmental Equivalents", 
       y = "CO2 Emissions / Equivalents", 
       x = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_y_log10(labels = scales::comma) 

#large
emissions_data <- data.frame(
  Category = c("Emissions Avoided", "Trees Planted", "Cars Off Road", "New Emissions Value"),  # Adding the new category
  Emissions = c(59563931, 1180000, 5963, 55494771930)  # Values for emissions avoided, tree planting, cars off the road, and the new value
)

ggplot(emissions_data, aes(x = Category, y = Emissions, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("skyblue", "green", "orange", "red")) +  # Different colors for each category
  labs(title = "CO2 Emissions Avoided vs. Environmental Equivalents", 
       y = "CO2 Emissions / Equivalents", 
       x = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_y_log10(labels = scales::comma)