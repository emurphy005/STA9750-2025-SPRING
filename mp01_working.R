if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}
#renaming the CSV file
nyc_payroll<- read_csv("data/mp01/nyc_payroll_export.csv")

#converting columsn to more conventional formatting
library(dbplyr)
library(stringr)
payroll <- nyc_payroll |>
  mutate(agency_name = str_to_title(agency_name),
         last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         work_location_borough = str_to_title(work_location_borough),
         title_description = str_to_title(title_description),
         leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
         )
head(payroll)

#filter Eric Adams
if (!require("DT")) install.packages('DT')
xfun::session_info('DT')
library(DT)
e_adams <- payroll |>
  filter(first_name == "Eric",
         last_name == "Adams",
         mid_init == "L")|>
  rename(`Fiscal Year` = fiscal_year,
         `Position` = title_description,
         `Agency` = agency_name,
         ) |>
  arrange(`Fiscal Year`, Position) |>
  select(`Fiscal Year`, Position, Agency, base_salary)
  
  
e_adams

#Total Compensation 
total_employee_compensation <- payroll|>
  mutate(
    Total_Compensation = case_when(
      pay_basis == "per Annum" ~ base_salary,
      pay_basis == "per Hour" ~ base_salary * (regular_hours + ot_hours * 1.5),
      pay_basis == "per Day" ~ base_salary * (regular_hours/7.5)
    )
  )
#job title has the highest base rate of pay
job_title_max_bas <- total_employee_compensation |>
  filter(base_salary == max(base_salary)) |>
  select(fiscal_year, agency_name, title_description, base_salary)|>
  slice_head(n=1)

job_title_max_bas

#Which individual & in what year had the single highest city total payroll
highest_city_payroll <- total_employee_compensation |>
  filter(Total_Compensation == max(Total_Compensation))
highest_city_payroll

#Which individual worked the most overtime hours in this data set?
max_overtime <- total_employee_compensation |>
  filter(ot_hours == max(ot_hours)) |>
  select(fiscal_year, agency_name, last_name, first_name, ot_hours)

max_overtime

#Which agency has the highest average total annual payroll (base and overtime pay per employee)?
ave_total_payroll <- total_employee_compensation |>
  group_by(agency_name) |>
  summarise(highest_ave = max(mean(Total_Compensation))) |>
  

ave_total_payroll

#Which agency has the most employees on payroll in each year?
most_employees_payroll <- total_employee_compensation |>
  group_by(fiscal_year, agency_name)|>
  tally()|>
  slice_max(n, n=1)

most_employees_payroll

#Which agency has the highest overtime usage (compared to regular hours)
highest_overtime_usage <- total_employee_compensation |>
  group_by(agency_name) |>
  summarise(overtime = sum(ot_hours))

highest_overtime_usage

#How much has the city’s aggregate payroll grown over the past 10 years
payroll_growth <- total_employee_compensation |>
  group_by(fiscal_year) |>
  sum(Total_Compensation)
payroll_growth

#Policy 1 capping salaries 
one_mayor <- total_employee_compensation |>
  filter(
    title_description == 'Mayor') |>
  group_by(fiscal_year) |>
  slice_head(n = 1)|>
  summarise(Total_Mayoral_Pay = sum(Total_Compensation))

datatable(one_mayor, options=list(searching=FALSE, 
                                  paging=FALSE,
                                  info=FALSE)) 


one_mayor 


employees_above_mayoral_pay <- total_employee_compensation |>
  left_join(one_mayor, by = "fiscal_year" ) |>
  filter(Total_Compensation > Total_Mayoral_Pay)|>
  select(fiscal_year, agency_name, last_name, first_name, Total_Compensation)


employees_above_mayoral_pay

sum_above_mayoral_pay <- employees_above_mayoral_pay |>
  summarise(Total_Above_Mayor = sum(Total_Compensation))

sum_above_mayoral_pay

edit_compensation <- total_employee_compensation|>
  select(fiscal_year, last_name, first_name, agency_name, title_description, Total_Compensation)

datatable(edit_compensation, options=list(searching=FALSE, 
                                          paging=FALSE,
                                          info=FALSE))

mayorsum <- one_mayor|>
  summarise(sum(Total_Mayoral_Pay))

mayorsum 


affected_agencies <- employees_above_mayoral_pay|>
  distinct(agency_name)


#Policy2
#Overtime hours per agency & job title
#get hourly rate for all employees
payroll_data <- payroll|>
  mutate(
    hourly_rate = case_when(
      pay_basis == "per Annum" ~ base_salary / 1950, #37.5 hours a week for 52 weeks
      pay_basis == "per Day" ~ base_salary / 7.5,
      pay_basis == "per Hour" ~ base_salary,
      TRUE ~NA_real_
    )
  )
payroll_data
# Step 2: Total Overtime Hours Per Agency + Job Title
overtime_summary <- payroll_data|>
  group_by(agency_name, title_description)|>
  summarise(
    total_ot_hours = sum(ot_hours, 2, na.rm = TRUE),
    avg_hourlry_rate = mean(hourly_rate, 2, na.rm = TRUE),
    .groups = "drop"
  )|>
  filter(total_ot_hours > 0)



# Step 3: Identify Top Job Titles Driving Overtime Costs
job_title_overtime <- overtime_summary|>
  mutate(
    ot_cost = total_ot_hours * avg_hourlry_rate *1.5
  )|>
  select(agency_name, title_description, total_ot_hours, ot_cost)|>
  arrange(desc(ot_cost))|>
  rename(
    `Agency` = agency_name,
    `Position` = title_description,
    `Total OT Hours` = total_ot_hours,
    `OT Cost` = ot_cost
  )|>
  slice_head(n=10)

job_title_overtime

# Step 4: Calculate Full-Time Employees (FTEs) Needed to Replace Overtime
overtime_summary <- overtime_summary |>
  mutate(
    fte_needed = total_ot_hours / 1950
  )

overtime_summary

num_of_fte <- overtime_summary |>
  summarise(total_fte_needed = sum(fte_needed, na.rm = TRUE))

num_of_fte


# Step 4: Compute Cost Savings
overtime_summary <- overtime_summary |>
  mutate(
    ot_cost = total_ot_hours * avg_hourlry_rate *1.5,
    regular_cost = total_ot_hours * avg_hourlry_rate,
    potential_savings = ot_cost - regular_cost
  )

savings<-overtime_summary |>
  summarise(totalsav = sum(potential_savings, 0, na.rm = TRUE))

savings

# Step 5: Aggregate Savings to Agency Level
agency_savings <- overtime_summary |>
  group_by(agency_name)|>
  summarise(
    total_savings = sum(potential_savings, na.rm = TRUE),
    total_fte_needed = sum(fte_needed, na.rm = TRUE),
    .groups = "drop"
  )|>
  arrange(desc(total_savings))

agency_savings

library(ggplot2)
# Step 1: Count Active and Ceased Employees by Agency and Job Title
turnover_analysis <- total_employee_compensation|>
  group_by(fiscal_year, agency_name, title_description)|>
  summarise(
    active_count = sum(leave_status_as_of_june_30 =="Active", na.rm = TRUE),
    ceased_count = sum(leave_status_as_of_june_30 == "Ceased", na = TRUE),
    avg_compensation = mean(Total_Compensation, na.rm = TRUE),
    .groups = "drop"
  )|>
  filter(active_count > 0)

turnover_analysis

# Step 2: Calculate Turnover Rate
turnover_rates <- turnover_analysis |>
  mutate(
    turnover_rate = ceased_count / (active_count + ceased_count),
    estimating_hiring_cost = avg_compensation * .35, #average hiring cost in NYC public sector 20%-30%
    potential_savings = turnover_rate * estimating_hiring_cost * active_count # savings by reducing turnover 
  )|>
  arrange(desc(turnover_rate))
  
turnover_rates

# Step 3: Aggregate Savings to Agency Level
agency_savings_hiring <- turnover_rates |>
  group_by(agency_name)|>
  summarise(
    total_savings = sum(potential_savings, na.rm = TRUE),
    avg_turnover_rate = mean(turnover_rate, na.rm = TRUE),
    .groups = "drop"
  )|>
  arrange(desc(total_savings))

agency_savings_hiring

# Step 4: Identify Job Titles Driving the Most Turnover
top_turnover_titles <- turnover_rates |>
  arrange(desc(turnover_rate))|>
  head(10)

top_turnover_titles

# Step 5: Visualize Savings by Agency
ggplot(agency_savings, aes(x = reorder(agency_name, total_savings), y = total_savings))+
  geom_bar(stat = "identity", fill = "steelblue")+
  coord_flip()+
  labs(title = "Estimated Savings from Reducing Turnover by Agency",
       x = "Agency", y = "Potential Savings ($)")+
  theme_minimal()

# Step 6: Display Summary Tables
datatable(turnover_rates, options=list(searching=FALSE, 
                                       paging=FALSE,
                                       scrollY = "400px",
                                       info=FALSE)
          )# Show job title turnover rates

datatable(top_turnover_titles, options = list(pageLength = 10))  # Show top turnover job titles
datatable(agency_savings, options = list(pageLength = 10))  # Show agency-level savings






