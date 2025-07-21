#' util_format_household_data: process household data for Apple
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
#' @export

util_format_household_data <- function(household_data) {

  # rename columns
  names(household_data)[names(household_data) == 'demo_parent_dob'] <- 'demo_parent_birthdate'
  names(household_data)[names(household_data) == 'demo_relationship'] <- 'demo_child_relationship'
  names(household_data)[names(household_data) == 'demo_birthorder'] <- 'demo_birth_number'

  names(household_data)[names(household_data) == 'demo_parent_separate'] <- 'demo_parent_seperations'
  names(household_data)[names(household_data) == 'demo_foster'] <- 'demo_child_foster'

  names(household_data)[names(household_data) == 'demo_partner_household'] <- 'demo_partner'
  names(household_data)[names(household_data) == 'demo_parent_marital'] <- 'demo_marital_status'
  names(household_data)[names(household_data) == 'demo_household_income'] <- 'demo_income'
  names(household_data)[names(household_data) == 'demo_parent_edu'] <- 'demo_education_parent'
  names(household_data)[names(household_data) == 'demo_parent_edu_other'] <- 'demo_parent_education_other'
  names(household_data)[names(household_data) == 'demo_parent_employ'] <- 'demo_employed'
  names(household_data)[names(household_data) == 'demo_parent_workhours'] <- 'demo_work_hours'
  names(household_data)[names(household_data) == 'demo_parent_retired'] <- 'demo_retired'
  names(household_data)[names(household_data) == 'demo_partner_employ'] <- 'demo_partner_employment'
  names(household_data)[names(household_data) == 'demo_partner_workhours'] <- 'demo_partner_workhours'
  names(household_data)[names(household_data) == 'demo_partner_retired'] <- 'demo_partner_retired'

  names(household_data)[names(household_data) == 'demo_mother_weightgain'] <-'demo_weight_past_month'

  names(household_data)[names(household_data) == 'demo_buying'] <- 'demo_buys_food'
  names(household_data)[names(household_data) == 'demo_delivery'] <- 'demo_eat_out'
  names(household_data)[names(household_data) == 'demo_dinner'] <- 'demo_dinner_together'
  names(household_data)[names(household_data) == 'demo_lunch'] <- 'demo_prepared_lunch'
  names(household_data)[names(household_data) == 'demo_condition'] <-  'demo_food_condition'
  names(household_data)[names(household_data) == 'demo_condition_yes'] <-  'demo_food_condition_desc'

  names(household_data)[names(household_data) == 'demo_foodpantry'] <- 'demo_foodpantry'
  names(household_data)[names(household_data) == 'demo_foodpantry_yes'] <- 'demo_foodpantry_times'
  names(household_data)[names(household_data) == 'demo_money'] <- 'demo_food_costs'
  names(household_data)[names(household_data) == 'demo_shopping'] <- 'demo_store_choice'

  names(household_data) <- gsub('demo_assistance', 'demo_programs', names(household_data))

  # add mom education
  household_data['demo_education_mom'] <- ifelse(household_data[['demo_child_relationship']] == 0, household_data[['demo_education_parent']], NA)

  # calculate parent age
  household_data[['demo_parent_birthdate']] <- lubridate::as_date(household_data[['demo_parent_birthdate']])
  household_data[['visit_date']] <- lubridate::as_date(household_data[['visit_date']])

  household_data[['demo_parent_age']] <- round(lubridate::interval(household_data[['demo_parent_birthdate']], household_data[['visit_date']])/lubridate::years(1), 1)


  # remove birthdate and timestamp variables
  household_data <- household_data[, !grepl('birthdate', names(household_data))]

  # household people
  names(household_data)[names(household_data) == 'demo_household_people___1'] <- 'demo_live_mother'
  names(household_data)[names(household_data) == 'demo_household_people___2'] <- 'demo_live_father'
  names(household_data)[names(household_data) == 'demo_household_people___3'] <- 'demo_live_siblings'
  names(household_data)[names(household_data) == 'demo_household_people___4'] <- 'demo_live_uncle'
  names(household_data)[names(household_data) == 'demo_household_people___5'] <- 'demo_live_aunt'
  names(household_data)[names(household_data) == 'demo_household_people___6'] <- 'demo_live_grandma'
  names(household_data)[names(household_data) == 'demo_household_people___7'] <- 'demo_live_grandpa'
  names(household_data)[names(household_data) == 'demo_household_people___8'] <- 'demo_live_cousins'
  names(household_data)[names(household_data) == 'demo_household_people___9'] <- 'demo_live_other'

  # food assistance programs
  names(household_data)[names(household_data) == 'demo_programs___0'] <- 'demo_assist_program_snap'
  names(household_data)[names(household_data) == 'demo_programs___1'] <- 'demo_assist_program_wic'
  names(household_data)[names(household_data) == 'demo_programs___2'] <- 'demo_assist_program_tnaf'
  names(household_data)[names(household_data) == 'demo_programs___3'] <- 'demo_assist_program_medicaid'
  names(household_data)[names(household_data) == 'demo_programs___4'] <- 'demo_assist_program_liheap'
  names(household_data)[names(household_data) == 'demo_programs___5'] <- 'demo_assist_program_pfr_lunch'
  names(household_data)[names(household_data) == 'demo_programs___6'] <- 'demo_assist_program_fr_lunch'
  names(household_data)[names(household_data) == 'demo_programs___7'] <- 'demo_assist_program_other'

  # growing food
  names(household_data)[names(household_data) == 'demo_grow___0'] <- 'demo_grow_fruit'
  names(household_data)[names(household_data) == 'demo_grow___1'] <- 'demo_grow_veg'
  names(household_data)[names(household_data) == 'demo_grow___2'] <- 'demo_grow_spreads'
  names(household_data)[names(household_data) == 'demo_grow___3'] <- 'demo_grow_nutseeds'
  names(household_data)[names(household_data) == 'demo_grow___4'] <- 'demo_grow_milk'
  names(household_data)[names(household_data) == 'demo_grow___5'] <- 'demo_grow_cheese'
  names(household_data)[names(household_data) == 'demo_grow___6'] <- 'demo_grow_butter'
  names(household_data)[names(household_data) == 'demo_grow___7'] <- 'demo_grow_eggs'
  names(household_data)[names(household_data) == 'demo_grow___8'] <- 'demo_grow_redmeat'
  names(household_data)[names(household_data) == 'demo_grow___9'] <- 'demo_grow_poultry'

  # return data
  return(household_data)

}
