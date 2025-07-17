#' json_v1_infancy: Generates a json file for visit 1 infancy data
#'
#' This function generates a json file for visit 1 infancy data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v1_infancy <- function() {

  infancy_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'A compilation of demographic variables about birth and infant feeding practices.'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected; except: for stable demo variables collected only at baseline (ethnicity, race) and risk_status (determined at baseline), values are repeated for ses-1 and ses-2)',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    demo_birth_length = list( Description = 'What was your childs birth length, in inches?',
                              Unit = 'in'),
    demo_birthweight_pounds = list( Description = 'Childs birth weight (pounds):',
                                    Unit = 'lbs'),
    demo_birthweight_ounces = list( Description = 'Child birth weight (ounces):',
                                    Unit = 'oz'),
    demo_premature = list( Description = 'Was your child born premature (born before 37 weeks)?',
                 Levels = list ('0' = 'No',
                                '1' = 'Yes')),
    demo_premature_weeks = list( Description = 'By how many weeks? (The normal gestation period is 40 weeks)',
                                 Unit = 'weeks'),
    demo_feeding = list( Description = 'Was your child primarily breast-fed or primarily formula fed?',
                         Levels = list ('0' = 'No',
                                        '1' = 'Yes')),
    demo_exclusive_feeding = list( Description = 'What was the total duration, in months, that your child was exclusively (only) breastfed? Please enter the duration, in months, that your child was only fed breastmilk and no formula or solid food was consumed.',
                                   Unit = 'months'),
    demo_tot_breastfeeding = list( Description = 'What was the total duration, in months, that your child was breastfed? Please enter the duration, in months, that your child was fed both breast milk and formula and/or solid food.',
                                   Unit = 'months'),
    demo_solid_food = list( Description = 'At what age, in months, was your child first introduced to solid food?',
                            Unit = "months"),
    birthweight_ounces_total = list( Description = 'Total birth weight in ounces',
                                     Unit = 'oz',
                                     Derivative = TRUE)

  )

  # convert formatting to JSON
  infancy_v1_json <- RJSONIO::toJSON(infancy_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(infancy_v1_json, asText = TRUE))){
    print('Visit 1 infancy data JSON file may be invalid')
  }

  return(infancy_v1_json)

}
