#' util_format_household_data: process household data for REACH visit 1 and 5
#'
#' This function process household data
#'
#'
#' @param household_data Household extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' household_data_formatted <- util_format_household_data(household_data)
#'
#' @seealso [util_redcap_parent_v1()], [util_redcap_parent_v5()]
#'
#' @export

util_format_household_data <- function(household_data) {

  # rename columns
  names(household_data)[names(household_data) == "demo_self_report_feet"] <- "demo_parent2_rep_height_ft"
  names(household_data)[names(household_data) == "demo_self_report_inches"] <- "demo_parent2_rep_height_in"
  names(household_data)[names(household_data) == "demo_self_report_weight"] <- "demo_parent2_rep_weight_lbs"

  # calculate parent age
  household_data[['demo_parent_birthdate']] <- lubridate::as_date(household_data[['demo_parent_birthdate']])
  household_data[['visit_date']] <- lubridate::as_date(household_data[['visit_date']])

  household_data[['demo_parent_age']] <- round(lubridate::interval(household_data[['demo_parent_birthdate']], household_data[['visit_date']])/lubridate::years(1), 1)


  # remove birthdate and timestamp variables
  household_data <- household_data[, !grepl("birthdate", names(household_data))]

  # fix a '000' participant entry
  household_data['demo_parent2_rep_height_ft'] <- ifelse(household_data[['demo_parent2_rep_height_ft']] == '000', NA, as.numeric(household_data[['demo_parent2_rep_height_ft']]))

  household_data['demo_parent2_rep_height_in'] <- ifelse(household_data[['demo_parent2_rep_height_in']] == '000', NA, as.numeric(household_data[['demo_parent2_rep_height_in']]))

  household_data['demo_parent2_rep_weight_lbs'] <- ifelse(household_data[['demo_parent2_rep_weight_lbs']] == '000', NA, as.numeric(household_data[['demo_parent2_rep_weight_lbs']]))

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data['demo_parent2_rep_height_m'] <- ((household_data[['demo_parent2_rep_height_ft']]*12) + household_data[['demo_parent2_rep_height_in']])*0.0254

  # convert parent2 lbs variable in kg
  household_data['demo_parent2_rep_weight_kg'] <- household_data[['demo_parent2_rep_weight_lbs']]*0.453592

  # calculate parent2 BMI (kg/m2)
  household_data['demo_parent2_rep_bmi'] <- household_data[['demo_parent2_rep_weight_kg']]/household_data[['demo_parent2_rep_height_m']]^2

  # food assistance programs
  names(household_data)[names(household_data) == 'demo_programs___0'] <- 'demo_assist_program_no'
  names(household_data)[names(household_data) == 'demo_programs___1'] <- 'demo_assist_program_snap'
  names(household_data)[names(household_data) == 'demo_programs___2'] <- 'demo_assist_program_wic'
  names(household_data)[names(household_data) == 'demo_programs___3'] <- 'demo_assist_program_tnaf'
  names(household_data)[names(household_data) == 'demo_programs___4'] <- 'demo_assist_program_medicaid'
  names(household_data)[names(household_data) == 'demo_programs___5'] <- 'demo_assist_program_liheap'
  names(household_data)[names(household_data) == 'demo_programs___6'] <- 'demo_assist_program_pfr_lunch'
  names(household_data)[names(household_data) == 'demo_programs___7'] <- 'demo_assist_program_fr_lunch'
  names(household_data)[names(household_data) == 'demo_programs___8'] <- 'demo_assist_program_other'

  # growing food
  names(household_data)[names(household_data) == 'demo_grow_own_food___0'] <- 'demo_grow_fruit'
  names(household_data)[names(household_data) == 'demo_grow_own_food___1'] <- 'demo_grow_veg'
  names(household_data)[names(household_data) == 'demo_grow_own_food___2'] <- 'demo_grow_spreads'
  names(household_data)[names(household_data) == 'demo_grow_own_food___3'] <- 'demo_grow_nutseeds'
  names(household_data)[names(household_data) == 'demo_grow_own_food___4'] <- 'demo_grow_milk'
  names(household_data)[names(household_data) == 'demo_grow_own_food___5'] <- 'demo_grow_cheese'
  names(household_data)[names(household_data) == 'demo_grow_own_food___6'] <- 'demo_grow_butter'
  names(household_data)[names(household_data) == 'demo_grow_own_food___7'] <- 'demo_grow_eggs'
  names(household_data)[names(household_data) == 'demo_grow_own_food___8'] <- 'demo_grow_redmeat'
  names(household_data)[names(household_data) == 'demo_grow_own_food___9'] <- 'demo_grow_poultry'
  names(household_data)[names(household_data) == 'demo_grow_own_food___10'] <- 'demo_grow_none'

  # return data
  return(household_data)

}
