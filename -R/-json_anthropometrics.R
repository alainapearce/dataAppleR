#' json_anthropometrics: Generates a json file for anthropometric data
#'
#' This function generates a json file for anthropometric data
#'
#' @return A string with data stored in JSON format containing meta-data anthropometric data
#'
#'
#' @export

json_anthropometrics <- function() {

  anthro_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date (YYYY-MM-DD) visit was completed'),
    child_relationship = list(Description = 'Child relationship of adult completing the household demographics form where parent2 anthropometrics were reported. Also reported in household.tsv.',
                                   Levels = list ('0' = 'Biological mother',
                                                  '1' = 'Biological father',
                                                  '2' = 'Non-biological mother',
                                                  '3' = 'Non-biological father')),
    age = list( Description = 'Child age. Used to calculate child_bmi_p and child_bmi_z. Also reported in demographics.tsv',
                      unit = "years"),
    sex = list( Description = 'Child sex. Used to calculate child_bmi_p and child_bmi_z. Also reported in demographics.tsv',
                Levels = list ('female' = 'female',
                               'male' = 'male')),
    child_height_1_cm = list( Description = 'child height measurement 1',
                              Unit = "cm"),
    child_height_2_cm = list( Description = 'child height measurement 2',
                              Unit = "cm"),
    child_weight_1_kg = list( Description = 'child weight measurement 1',
                              Unit = "kg"),
    child_weight_2_kg = list( Description = 'child weight measurement 2',
                              Unit = "kg"),
    child_height_average = list( Description = 'average of child_height_1_cm and child_height_2_cm',
                                 Unit = "cm",
                                 Derivative = TRUE),
    child_weight_average = list( Description = 'average of child_weight_1_kg and child_weight_2_kg',
                                 Unit = "kg",
                                 Derivative = TRUE),
    parent1_sex = list( Description = 'Sex of parent being measured',
                        Levels = list ('female' = 'female',
                                       'male' = 'male')),
    parent1_height_1_cm = list( Description = 'parent height measurement 1',
                                Unit = "cm"),
    parent1_height_2_cm = list( Description = 'parent height measurement 2',
                                Unit = "cm"),
    parent1_weight_1_kg = list( Description = 'parent weight measurement 1',
                                Unit = "kg"),
    parent1_weight_2_kg = list( Description = 'parent weight measurement 2',
                                Unit = "kg"),
    parent1_height_average_cm = list( Description = 'average of parent1_height_1_cm and parent1_height_2_cm',
                                      Unit = "cm",
                                      Derivative = TRUE),
    parent1_weight_average_kg = list( Description = 'average of parent1_weight_1_kg and parent1_weight_2_kg',
                                      Unit = "kg",
                                      Derivative = TRUE),
    parent2_reported_bmi = list( Description = 'Parent2 (i.e., biological parent not at visit) BMI calculated from parent1 reported metrics (see household.tsv). Also reported in household.tsv',
                                 Unit = "kg/(m^2)",
                                 Derivative = TRUE),
    child_bmi = list( Description = 'Child BMI. Calculated in R using child_height_average and child_average_weight',
                      Unit = "kg/(m^2)",
                      Derivative = TRUE),
    child_bmi_p = list( Description = 'Child BMI-for-age-and-sex percentile based on the LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Calculated in R using childsds package',
                        Derivative = TRUE),
    child_bmi_z = list( Description = 'Child BMI-for-age-age-and-sex z-score baesd on the LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Calculated in R using childsds package',
                        Derivative = TRUE),
    parent1_bmi = list( Description = 'BMI of parent with measured height and weight. Calculated in R using parent1_height_average_cm and parent1_weight_average_kg',
                        Unit = "kg/(m^2)",
                        Derivative = TRUE),
    maternal_anthro_method = list(Description = 'Method used to determine maternal anthropometrics. Equal to "measured" if parent1_sex == "female" and equal to "reported" if "demo_child_relationship" == 1 (biological father)',
                                  Levels = list ('measured' = 'measured by researcher',
                                                 'reported' = 'reported by biological father on household demographic form'),
                                  Derivative = TRUE),
    maternal_bmi = list(Description = 'Maternal BMI. Equal to parent1_bmi if maternal_anthro_method == "measured" and equal to parent2_reported_bmi if maternal_anthro_method == "reported"',
                        Unit = "kg/(m^2)",
                        Derivative = TRUE),
    paternal_anthro_method = list(Description = 'Method used to determine paternal anthropometrics. Equal to "measured" if parent1_sex == "male" and equal to "reported" if "demo_child_relationship" == 0 (biological mom)',
                                  Levels = list ('measured' = 'measured by researcher',
                                                 'reported' = 'reported by mother on household demographic form'),
                                  Derivative = TRUE),
    paternal_bmi = list(Description = 'Paternal BMI. Equal to parent1_bmi if paternal_anthro_method == "measured" and equal to parent2_reported_bmi if paternal_anthro_method == "reported"',
                        Unit = "kg/(m^2)",
                        Derivative = TRUE)
  )

  # convert formatting to JSON
  anthro_json <- RJSONIO::toJSON(anthro_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(anthro_json, asText = TRUE))){
    print('MRI visit JSON file may be invalid')
  }

  return(anthro_json)

}
