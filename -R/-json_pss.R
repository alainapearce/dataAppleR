#' json_pss: Generates a json file for the 10-item Perceived Stress Scale
#'
#' This function generates a json file for the scored 10-item Perceived Stress Scale and raw participant responses.
#' This function provides accurate json files ONLY if data is processed using score_pss function in dataprepr and is only accurate for data collected in Study REACH
#'
#' @return A string with data stored in JSON format containing meta-data for the 10-item Perceived Stress Scale
#'
#'
#' @export

json_pss <- function() {

  pss_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '10-item Perceived Stress Scale. Participants were provided the following instructions: "The questions in this scale ask you about your feelings and thoughts during the last month. In each case, you will be asked to indicate by marking how often you felt or thought a certain way."',
      Reference = 'Cohen, S. and Williamson, G. Perceived Stress in a Probability Sample of the United States. Spacapan, S. and Oskamp, S. (Eds.) The Social Psychology of Health. Newbury Park, CA: Sage, 1988.',
      TermURL = 'https://psycnet.apa.org/record/1988-98838-002'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    pss1 = list( Description = 'In the last month, how often have you been upset because of something that happened unexpectedly?',
                  Levels = list ('0' = '0 = Never',
                                 '1' = '1 = Almost Never',
                                 '2' = '2 = Sometimes',
                                 '3' = '3 = Fairly Often',
                                 '4' = '4 = Very Often')),
    pss2 = list( Description = 'In the last month, how often have you felt that you were unable to control the important things in your life?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss3 = list( Description = 'In the last month, how often have you felt nervous and "stressed"?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss4 = list( Description = 'In the last month, how often have you felt confident about your ability to handle your personal problems?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss5 = list( Description = 'In the last month, how often have you felt that things were going your way?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss6 = list( Description = 'In the last month, how often have you found that you could not cope with all the things that you had to do?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss7 = list( Description = 'In the last month, how often have you been able to control irritations in your life?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss8 = list( Description = 'In the last month, how often have you felt that you were on top of things?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss9 = list( Description = 'In the last month, how often have you been angered because of things that were outside of your control?',
                 Levels = list ('0' = '0 = Never',
                                '1' = '1 = Almost Never',
                                '2' = '2 = Sometimes',
                                '3' = '3 = Fairly Often',
                                '4' = '4 = Very Often')),
    pss10 = list( Description = 'In the last month, how often have you felt difficulties were piling up so high that you could not overcome them?',
                  Levels = list ('0' = '0 = Never',
                                 '1' = '1 = Almost Never',
                                 '2' = '2 = Sometimes',
                                 '3' = '3 = Fairly Often',
                                 '4' = '4 = Very Often')),
    pss_total = list( Description = 'Total percieved stress',
                      Derivative = TRUE),
    pss_helplessness = list( Description = 'Perceived helplessness',
                             Derivative = TRUE,
                             Reference = 'Taylor JM. Psychometric analysis of the Ten-Item Perceived Stress Scale. Psychol Assess. 2015 Mar;27(1):90-101. doi: 10.1037/a0038100. Epub 2014 Oct 27. PMID: 25346996.'),
    pss_selfefficacy = list( Description = 'Perceived self-efficacy; items are not reverse-scored for this subscale, such that higher positive score represents higher coping ability.',
                              Derivative = TRUE,
                             Reference = 'Taylor JM. Psychometric analysis of the Ten-Item Perceived Stress Scale. Psychol Assess. 2015 Mar;27(1):90-101. doi: 10.1037/a0038100. Epub 2014 Oct 27. PMID: 25346996.'))

  # convert formatting to JSON
  pss_json <- RJSONIO::toJSON(pss_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(pss_json, asText = TRUE))){
    print('pss JSON file may be invalid')
  }

  return(pss_json)

}
