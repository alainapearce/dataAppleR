#' util_merged_anthro: merges and formats anthropometrics data
#'
#' This function merges anthropometrics data across visits and formats/calculates necessary values
#'
#'
#' @param visit1_anthro visit 1 anthropometrics data.frame
#' @param visit5_anthro visit 2 anthropometrics data.frame
#' @param merged_household merged household data.frame
#' @param date_data data.frame with all visit date information
#'
#' @examples
#'
#' # process data
#' merge_anthro_data <- util_merged_anthro(child_v1_data$anthro_data$data, child_v5_data$anthro_data$data, merged_qs_list$household_all, date_data)
#'
#' @seealso [proc_redcap()], [util_merge_questionnaires()]
#'
#' @export
#'


util_merged_anthro <- function(visit1_anthro, visit5_anthro, merged_household, date_data) {

  anthro_all <- rbind.data.frame(visit1_anthro, visit5_anthro)

  # Extract parent 2 BMI from household_data and stack
  parent2_anthro_all <- merged_household[c('participant_id','session_id', 'demo_parent2_rep_bmi','demo_child_relationship')]

  # may need to update with double entered values
  merged_anthro <- merge(anthro_all, parent2_anthro_all, by=c('participant_id', 'session_id'), all = TRUE)
  merged_anthro <- merge(merged_anthro, date_data[c('participant_id', 'sex', 'v1_age', 'v5_age')], by=c('participant_id'), all = TRUE)

  merged_anthro['age'] <- ifelse(merged_anthro[['session_id']] == 'ses-1', merged_anthro[['v1_age']], merged_anthro[['v5_age']])

  merged_anthro <- merged_anthro[!grepl('v1_age|v5_age', names(merged_anthro))]

  # compute bmi variables
  merged_anthro['child_bmi'] <- round(merged_anthro['child_weight_average'] /((merged_anthro['child_height_average'] / 100) ^ 2), digits = 2)

  merged_anthro['child_bmi_z'] <- round(childsds::sds(value = merged_anthro[['child_bmi']], age = merged_anthro[['age']], sex = merged_anthro[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'SDS', male = 'male', female = 'female'), digits = 2)

  merged_anthro['child_bmi_p'] <- round((childsds::sds(value = merged_anthro[['child_bmi']], age = merged_anthro[['age']], sex = merged_anthro[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'perc', male = 'male', female = 'female')) * 100, digits = 2)

  merged_anthro['parent1_bmi'] <- round(merged_anthro['parent1_weight_average_kg'] / ((merged_anthro['parent1_height_average_cm'] / 100) ^ 2), digits = 2)

  # Define parental BMI values and method
  ## parent1_sex ('female' or 'male') indicates the parent with measured anthro; demo_child_relationship (0 = bio-mom, 1 = bio-dad) indicates parent that reported height/weight for bio parent *not* at visit in household demo form
  ## parent1_sex and demo_child_relationship should indicate the same parent, but referencing both in ifelse statements in case of scenario where this is not true
  merged_anthro['maternal_anthro_method'] <- ifelse(merged_anthro['parent1_sex'] == 'female', ifelse(merged_anthro['demo_child_relationship'] == 0, 'measured', NA), 'reported')

  merged_anthro['maternal_bmi'] <- ifelse(merged_anthro[['maternal_anthro_method']] == 'measured', merged_anthro[['parent1_bmi']], ifelse(merged_anthro[['maternal_anthro_method']] == 'reported', merged_anthro[['demo_parent2_rep_bmi']], NA))

  merged_anthro['paternal_anthro_method'] <- ifelse(merged_anthro['parent1_sex'] == 'male', ifelse(merged_anthro['demo_child_relationship'] == 1, 'measured', NA), 'reported')

  merged_anthro['paternal_bmi'] <- ifelse(merged_anthro[['paternal_anthro_method']] == 'measured', merged_anthro[['parent1_bmi']], ifelse(merged_anthro[['paternal_anthro_method']] == 'reported', merged_anthro[['demo_parent2_rep_bmi']], NA))

  # fix names
  names(merged_anthro) <- gsub('demo_', '', names(merged_anthro))

  merged_anthro <- merged_anthro[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'child_relationship', 'age', 'sex', names(merged_anthro)[grepl('child|parent|maternal|paternal', names(merged_anthro))])]
  merged_anthro <- merged_anthro[!grepl('child_relationship.1', names(merged_anthro))]

  # return data
  return(merged_anthro)

}
