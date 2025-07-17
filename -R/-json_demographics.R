#' json_demographics: Generates a json file for demographics.tsv
#'
#' This function generates a json file for demographics.tsv
#'
#' @return A string with data stored in JSON format containing meta-data for demographic variables
#'
#'
#' @export

json_demographics <- function() {

  demo_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'A compilation of demographic and anthropometric variables commonly reported in papers or used as covariates.'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected; except: for stable demo variables collected only at baseline (ethnicity, race) and risk_status (determined at baseline), values are repeated for ses-1 and ses-2)',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    risk_status = list( Description = 'Familial risk status for obesity based on maternal_bmi at ses-1 (found in anthropometrics.tsv)',
                        Levels = list('low-risk' = 'maternal bmi at visit 1: 18.5-25',
                                      'high-risk' = 'maternal bmi at visit 1: >= 30.0'),
                        Derivative = TRUE),
    child_bmi_criteria = list( Description = 'Does child BMI percentile meet criteria for the study (BMI percentile < 95th)',
                               Levels = list ('1' = 'Eligible based on BMI percentile',
                                              '0' = 'Not eligible based on BMI percentile')),
    date_session_start = list( Description = 'Date at the start of baseline or follow-up sessions (i.e., date at V1 for ses-1 and V5 for ses-2). Values correspond to child_protocol_1_date and child_protocol_5_date in participants.tsv'),
    sex = list( Description = 'Parent-reported child sex. Can also be found in puberty.tsv',
                Levels = list ('female' = 'female',
                               'male' = 'male')),
    age = list( Description = 'Child age at the start of the session (i.e., age at V1 for ses-1 and V5 for ses-2). Computed as difference between date_session_start and child date of birth (not shared)',
                Unit = 'years',
                Derivative = TRUE),
    ethnicity = list( Description = 'Parent-reported child ethnicity. Collected at ses-1 only but values applied to ses-1 and ses-2',
                      Levels = list ('0' = 'Not Hispanic or Latino',
                                     '1' = 'Hispanic or Latino')),
    race = list( Description = 'Parent-reported child race. Collected at ses-1 only but values applied to ses-1 and ses-2',
                 Levels = list ('0' = 'American Indian/Alaskan Native',
                                '1' = 'Asian',
                                '2' = 'Black or African American',
                                '3' = 'White',
                                '4' = 'Hawaiian/Pacific Islander',
                                '5' = 'Other')),
    income = list( Description = 'Parent-reported family income. Can also be found in household.tsv. Parent\'s were asked: "Including ALL sources (such as social security income, child support payments, government assistance, dividends from investments, etc.) what was your household\'s combined yearly income last year BEFORE taxes?"',
                   Levels = list ('0' = 'Less than $20,000',
                                  '1' = '$21,000 - $35,000',
                                  '2' = '$36,000 - $50,000',
                                  '3' = '$51,000 - $75,000',
                                  '4' = '$76,000 - $100,000',
                                  '5' = '$100,000 +')),
    education_mom = list( Description = 'Parent-reported maternal education. Can also be found in household.tsv',
                          Levels = list ('0' = 'None',
                                         '1' = 'Preschool',
                                         '2' = 'Kindergarten',
                                         '3' = '1st grade',
                                         '4' = '2nd grade',
                                         '5' = '3rd grade',
                                         '6' = '4th grade',
                                         '7' = '5th grade',
                                         '8' = '6th grade',
                                         '9' = '7th grade',
                                         '10' = '8th grade',
                                         '11' = '9th grade',
                                         '12' = '10th grade',
                                         '13' = '11th grade',
                                         '14' = 'GED',
                                         '15' = 'High School graduate',
                                         '16' = 'Some college but less than 1 year',
                                         '17' = 'One or more years of college at a 2-year program, No degree obtained',
                                         '18' = 'One year of college at a 4-year program, No degree obtained',
                                         '19' = 'Two years of college at a 4-year program, No degree obtained',
                                         '20' = 'Three years of college at a 4-year program, No degree obtained',
                                         '21' = 'Associates degree (AA, AS)',
                                         '22' = 'Bachelor\'s Degree (BA, AB, BS)',
                                         '23' = 'Master\'s degree (MA, MS, MEng, MEd, MSW, MBA, MPH)',
                                         '24' = 'Professional Degree (MD, DDS, DVM, LLB, JD, DPT, PharmD)',
                                         '25' = 'Doctoral Degree (PhD, EdD, PsyD)',
                                         '26' = 'N/A')),
    child_bmi = list( Description = 'Child BMI. Calculated in R using child_height_average and child_average_weight. Can also be found in anthropometrics.tsv',
                      Unit = "kg/(m^2)",
                      Derivative = TRUE),
    child_bmi_p = list( Description = 'Child BMI-for-age percentile based on the LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Calculated in R using childsds package. Can also be found in anthropometrics.tsv',
                        Derivative = TRUE),
    child_bmi_z = list( Description = 'Child BMI-for-age z-score baesd on the LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Calculated in R using childsds package. Can also be found in anthropometrics.tsv',
                        Derivative = TRUE),
    maternal_bmi = list( Description = 'Maternal BMI. Can also be found in anthropometrics.tsv',
                         Derivative = TRUE,
                         Unit = 'kg/m^2'),
    maternal_anthro_method = list( Description = 'Method used to determine maternal_bmi. Can also be found in anthropometrics.tsv',
                                   Levels = list('measured' = 'BMI calculated from height and weight measurements',
                                                 'reported' = 'BMI calculated from parent-reported height and weight values'))

  )

  # convert formatting to JSON
  demo_json <- RJSONIO::toJSON(demo_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(demo_json, asText = TRUE))){
    print('Demographics JSON file may be invalid')
  }

  return(demo_json)

}
