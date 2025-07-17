#' json_audit: Generates a json file for the Alcohol Use Disorders Identification Test
#'
#' This function generates a json file for the scored Alcohol Use Disorders Identification Test and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data the Alcohol Use Disorders Identification Test
#'
#'
#' @export

json_audit <- function() {

  audit_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Alcohol Use Disorders Identification Test. Participants were provided the following instructions: "Please check the answer that is correct for you"',
      Reference = 'Saunders JB, Aasland OG, Babor TF, De La Fuente JR, Grant M. Development of the Alcohol Use Disorders Identification Test (AUDIT): WHO Collaborative Project on Early Detection of Persons with Harmful Alcohol Consumption-II. Addiction. 1993;88(6):791-804. doi:10.1111/j.1360-0443.1993.tb02093.x',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/8329970/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                    Levels = list ('ses-1' = 'session 1 / baseline',
                                   'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    audit1 = list( Description = 'How often do you have a drink containing alcohol?',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Monthly or less',
                                 '2' = 'Two to four times a month',
                                 '3' = 'Two to three times a week',
                                 '4' = 'Four or more times a week')),
    audit2 = list( Description = 'How many drinks containing alcohol do you have on a typical day when you are drinking?',
                  Levels = list ('0' = '1 or 2',
                                 '1' = '3 or 4',
                                 '2' = '5 or 6',
                                 '3' = '7 to 9',
                                 '4' = '10 or more')),
    audit3 = list( Description = 'How often do you have six or more drinks on one occasion?',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Less than monthly',
                                 '2' = 'Monthly',
                                 '3' = 'Weekly',
                                 '4' = 'Daily or almost daily')),
    audit4 = list( Description = 'How often during the last year have you found that you were not able to stop drinking once you had started?',
                   Levels = list ('0' = 'Never',
                                  '1' = 'Less than monthly',
                                  '2' = 'Monthly',
                                  '3' = 'Weekly',
                                  '4' = 'Daily or almost daily')),
    audit5 = list( Description = 'How often during the last year have you failed to do what was normally expected from you because of drinking?',
                   Levels = list ('0' = 'Never',
                                  '1' = 'Less than monthly',
                                  '2' = 'Monthly',
                                  '3' = 'Weekly',
                                  '4' = 'Daily or almost daily')),
    audit6 = list( Description = 'How often during the last year have you needed a first drink in the morning to get yourself going after a heavy drinking session?',
                   Levels = list ('0' = 'Never',
                                  '1' = 'Less than monthly',
                                  '2' = 'Monthly',
                                  '3' = 'Weekly',
                                  '4' = 'Daily or almost daily')),
    audit7 = list( Description = 'How often during the last year have you had a feeling of guilt or remorse after drinking?',
                   Levels = list ('0' = 'Never',
                                  '1' = 'Less than monthly',
                                  '2' = 'Monthly',
                                  '3' = 'Weekly',
                                  '4' = 'Daily or almost daily')),
    audit8 = list( Description = 'How often during the last year have you been unable to remember what happened the night before because you had been drinking?',
                   Levels = list ('0' = 'Never',
                                  '1' = 'Less than monthly',
                                  '2' = 'Monthly',
                                  '3' = 'Weekly',
                                  '4' = 'Daily or almost daily')),
    audit9 = list( Description = 'Have you or someone else been injured as a result of your drinking?',
                  Levels = list ('0' = 'No',
                                 '2' = 'Yes, but not in the last year',
                                 '4' = 'Yes, during the last year')),
    audit10 = list( Description = 'Has a relative or friend, doctor or other health worker been concerned about your drinking or suggested you cut down?',
                    Levels = list ('0' = 'No',
                                   '2' = 'Yes, but not in the last year',
                                   '4' = 'Yes, during the last year')),
    audit_total = list( Description = 'Audit score',
                       Derivative = TRUE),
    audit_cat = list( Description = 'Audit category',
                        Derivative = TRUE,
                        Levels = list ('Likely Harmful Consumption' = 'Likely Harmful Consumption (audit_total >= 8)',
                                       'Not Harmful Consumption' = 'Not Harmful Consumption (audit_total < 8)')))

  # convert formatting to JSON
  audit_json <- RJSONIO::toJSON(audit_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(audit_json, asText = TRUE))){
    print('audit JSON file may be invalid')
  }

  return(audit_json)

}
