#' json_infancy: Generates a json file for infancy data
#'
#' This function generates a json file for infant-related data collected in the visit 1 demographics form
#'
#' @return A string with data stored in JSON format containing meta-data for infant-related data
#'
#'
#' @export

json_infancy <- function() {

  infancy_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Questions related to the child\'s birth and infancy that parents answered as part of the REDCap form visit_1_demographics'),
    participant_id = list( Description = 'participant id number'),
    visit_date = list( Description = 'Date of visit where parent completed form',
                       Unit = 'YYYY-MM-DD'),
    demo_birth_length = list( Description = 'What was your child\'s birth length, in inches?',
                              Unit = "inches"),
    demo_birthweight_pounds_component = list( Description = 'Birth weight in pounds extracted from \'What was your childs birth weight? Please report in pounds and ounces.\'',
                                              Derivative = TRUE,
                                              Unit = 'poulds'),
    demo_birthweight_ounces_component = list( Description = 'Birth weight in ounces extracted from \'What was your childs birth weight? Please report in pounds and ounces.\'',
                                              Derivative = TRUE,
                                              Unit = 'ounces'),
    demo_premature = list( Description = 'Was your child born premature?'),
    demo_premature_weeks = list( Description = 'By how many weeks?'),
    demo_feeding = list( Description = 'Was your child primarily breast-fed or primarily formula fed?'),
    demo_exclusive_feeding = list( Description = 'If your child was breast-fed, for how many months was he/she exclusively (only) fed breast milk?'),
    birthweight_ounces_total = list( Description = 'Child birthweight computed from demo_birthweight_pounds_component and demo_birthweight_ounces_component (16*demo_birthweight_pounds_component + demo_birthweight_ounces_component)',
                                     Derivative = TRUE,
                                     Unit = "ounces")
    )

  # convert formatting to JSON
  infancy_json <- RJSONIO::toJSON(infancy_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(infancy_json, asText = TRUE))){
    print('Infancy JSON file may be invalid')
  }

  return(infancy_json)

}
