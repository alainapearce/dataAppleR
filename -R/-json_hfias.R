#' json_hfias: Generates a json file for the Household Food Insecurity Access Scale
#'
#' This function generates a json file for the scored Household Food Insecurity Access Scale and raw participant responses.

#'
#' @return A string with data stored in JSON format containing meta-data for the Household Food Insecurity Access Scale
#'
#'
#' @export

json_hfias <- function() {

  hfias_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Household Food Insecurity Access Scale. Participants were provided the following instructions: "Each of the following questions is asked with a recall period of four weeks (30 days). You are first asked an occurrence question - that is, whether the condition in the question happened at all in the past four weeks (yes or no). If "yes" to the occurrence question, a frequency-of-occurrence question is asked. Please respond with, rarely (once or twice), sometimes (three to ten times) or often (more than ten times) in the past four weeks."',
      Reference = 'oates, Jennifer, Anne Swindale and Paula Bilinsky. 2007. Household Food Insecurity Access Scale (HFIAS) for Measurement of Household Food Access: Indicator Guide (v. 3). Washington, D.C.:FHI 360/FANTA.',
      TermURL = 'https://www.fantaproject.org/sites/default/files/resources/HFIAS_ENG_v3_Aug07.pdf'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    hfias_1 = list( Description = '1. In the past four weeks, did you worry that your household would not have enough food?',
                 Levels = list ('0' = 'No',
                                '1' = 'Yes')),
    hfias_1a = list( Description = '1a. How often did this happen?',
                   Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                  '1' = 'Sometimes (three to ten times in the past four weeks)',
                                  '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_2 = list( Description = '2. In the past four weeks, were you or any household member not able to eat the kinds of foods you preferred because of a lack of resources?',
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes')),
    hfias_2a = list( Description = '2a. How often did this happen?',
                   Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                  '1' = 'Sometimes (three to ten times in the past four weeks)',
                                  '2' = 'Often (more than ten times in the past four weeks)')),

    hfias_3 = list( Description = '3. In the past four weeks, did you or any household member have to eat a limited variety of foods due to a lack of resources?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_3a = list( Description = '3a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),

    hfias_4 = list( Description = '4. In the past four weeks, did you or any household member have to eat some foods that you really did not want to eat because of a lack of resources to obtain other types of food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_4a = list( Description = '4a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_5 = list( Description = '5. In the past four weeks, did you or any household member have to eat a smaller meal than you felt you needed because there was not enough food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_5a = list( Description = '5a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_6 = list( Description = '6. In the past four weeks, did you or any other household member have to eat fewer meals in a day because there was not enough food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_6a = list( Description = '6a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_7 = list( Description = '7. In the past four weeks, was there ever no food to eat of any kind in your household because of lack of resources to get food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_7a = list( Description = '7a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_8 = list( Description = '8. In the past four weeks, did you or any household member go to sleep at night hungry because there was not enough food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_8a = list( Description = '8a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_9 = list( Description = '9. In the past four weeks, did you or any household member go a whole day and night without eating anything because there was not enough food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    hfias_9a = list( Description = '9a. How often did this happen?',
                     Levels = list ('0' = 'Rarely (once or twice in the past four weeks)',
                                    '1' = 'Sometimes (three to ten times in the past four weeks)',
                                    '2' = 'Often (more than ten times in the past four weeks)')),
    hfias_score_cont = list( Description = 'Household Food Insecurity Access score: Sum of the frequency-of-occurrence during the past four weeks for the 9 food insecurity-related conditions',
                      Derivative = TRUE),
    hfias_score_cat = list( Description = 'Household Food Insecurity Access category',
                            Levels = list ('Food Secure' = 'Food Secure',
                                           'Mildly Food Insecure Access' = 'Mildly Food Insecure Access)',
                                           'Moderately Food Insecure Access' = 'Moderately Food Insecure Access',
                                           'Severely Food Insecure Access' = 'Severely Food Insecure Access'),
                            Derivative = TRUE),
    hfias_anx_domain = list( Description = 'Experiences anxiety',
                             Levels = list ('1' = 'yes (response == 1 to hfias_1)',
                                            '0' = 'no (response == 0 to hfias_1)'),
                            Derivative = TRUE),
    hfias_quality_domain = list( Description = 'Experiences Insufficient Quality',
                                 Levels = list ('1' = 'yes (response == 1 to hfias_2, hfias_3 or hfias_4)',
                                                '0' = 'no (response == 0 to hfias_2, hfias_3 and hfias_4)'),
                            Derivative = TRUE),
    hfias_intake_domain = list( Description = 'Experiences insufficient food intake and physical consequences',
                                Levels = list ('1' = 'yes (response == 1 to hfias_5, hfias_6, hfias_7, hfias_8, or hfias_9)',
                                               '0' = 'no (response == 0 to hfias_5, hfias_6, hfias_7, hfias_8, and hfias_9)'),
                            Derivative = TRUE))

  # convert formatting to JSON
  hfias_json <- RJSONIO::toJSON(hfias_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(hfias_json, asText = TRUE))){
    print('hfias JSON file may be invalid')
  }

  return(hfias_json)

}
