#' json_toolbox_beh: Generates a json file for the NIH toolbox behavioral response data (data in rawdata/beh)
#'
#' This function generates a json file for cleaned NIH toolbox assessment (response) data
#'
#' @return A string with data stored in JSON format containing meta-data for NIH toolbox assessment (response) data
#'
#'
#' @export

json_toolbox_beh <- function() {

  toolbox_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'NIH Toolbox Assessment data. Children were administered 3 tasks from the NIH toolbox: Flanker Inhibitory Control and Attention Test (FLANKER), Dimensional Change Card Sort Test (CARTSORT), List Sorting Working Memory Test (LISTSORT).',
      Reference = '',
      TermURL = 'https://nihtoolbox.zendesk.com/hc/en-us'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    PIN = list( Description = 'unique ID assigned to participant on iPad at administration'),
    DeviceID = list( Description = 'device identifier located on the "Help&Support" in the app'),
    Assessment.Name = list( Description = ''),
    InstOrdr = list( Description = 'Administration position (1st, 2nd, etc.) of an Instrument within a Module (MdlOrdr). '),
    InstSctn = list( Description = 'Grouping field within an instrument which can be used to present items together during administration. Items with the same InstruSctn value were administered together in the same block.'),
    ItmOrdr = list( Description = 'The position of an item within an item block (InstSctn). This may or may not match the order of administration based on how a study was set-up.'),
    Inst = list( Description = 'instrument name provided by NIH TOOLBOX'),
    Test = list( Description = 'Test name derived from Inst',
                 Levels = list ('FLANKER' = 'NIH Toolbox Flanker Inhibitory Control and Attention Test',
                                'CARDSORT' = 'NIH Toolbox Dimensional Change Card Sort Test',
                                'LISTSORT' = 'NIH Toolbox List Sorting Working Memory Test')),
    Test_Ages = list( Description = 'Test version derived from Inst'),
    Locale = list( Description = 'Localization value (Tag for the Identification of Languages) for an instrument',
                   Levels = list ('en-US' = 'US English',
                                  'es' = 'Spanish')),
    ItemID = list( Description = 'ID of the item administered'),
    Response = list( Description = 'Position within response options of participant selection. For example, the response option listed first is coded as 1. If a participant skipped an item, the Rspnse field will be blank'),
    Score = list( Description = 'Score of the participant selection. Please see data dictionary'),
    Theta = list( Description = 'Instrument score'),
    TScore = list( Description = 'Transformed score based on a formula (10*Theta +50 for PROMIS and NeuroQOL instruments, different transformation for NIH Toolbox Emotional instruments)'),
    SE = list( Description = 'Standard Error for administered instrument'),
    DataType = list( Description = 'Response type (e.g. integer, string)'),
    Position = list( Description = 'Administration order of item'),
    ResponseTime = list( Description = 'timespan between item presented and the response selected (please refer to instruments scores documentation)'),
    DateCreated = list( Description = 'time stamp when an item response was submitted'),
    InstStarted = list( Description = 'time stamp when first item is accessed'),
    InstEnded = list( Description = 'time stamp when last response is submitted'),
    App.Version = list( Description = 'App version'),
    iPad.Version = list( Description = 'Ipad version'),
    Firmware.Version = list( Description = 'Firmware Version'),
    registration_age = list( Description = 'Registration data: participant age at the time of assessment (same participant may have a different age on the exports for multiple/future assessments)'),
    registration_education = list( Description = 'Registration data: participant education',
                                   Levels = list ('1' = 'None',
                                                  '2' = 'Preschool',
                                                  '3' = 'Kindergarten',
                                                  '4' = '1st grade',
                                                  '5' = '2nd grade',
                                                  '6' = '3rd grade',
                                                  '7' = '4th grade',
                                                  '8' = '5th grade',
                                                  '9' = '6th grade',
                                                  '10' = '7th grade',
                                                  '11' = '8th grade',
                                                  '12' = '9th grade',
                                                  '13' = '10th grade',
                                                  '14' = '11th grade',
                                                  '16' = 'High school graduate',
                                                  '18' = 'Some college credit but less than 1 year',
                                                  '20' = 'Associates degree (e.g., AA, AS)',
                                                  '21' = 'Bachelor\'s degree (e.g., BA, AB, BS)',
                                                  '22' = 'Masters degree (e.g., MA, MS, MEng, MEd, MSW, MBA)',
                                                  '23' = 'Professional degree (e.g., MD, DDS, DVM, LLB, JD)',
                                                  '24' = 'Doctorate degree (e.g., PhD, EdD)',
                                                  '25' = 'One or more years of college at a 2-year program, no degree',
                                                  '26' = 'One year of college at a 4-year program, no degree',
                                                  '27' = 'Two years of college at a 4-year program, no degree',
                                                  '28' = 'Three years of college at a 4-year program, no degree',
                                                  '999' = 'Unknown'
                                                  )),
    registration_mothers_education = list( Description = 'Registration data: education of participant\'s mother',
                                           Levels = list ('1' = 'None',
                                                          '2' = 'Preschool',
                                                          '3' = 'Kindergarten',
                                                          '4' = '1st grade',
                                                          '5' = '2nd grade',
                                                          '6' = '3rd grade',
                                                          '7' = '4th grade',
                                                          '8' = '5th grade',
                                                          '9' = '6th grade',
                                                          '10' = '7th grade',
                                                          '11' = '8th grade',
                                                          '12' = '9th grade',
                                                          '13' = '10th grade',
                                                          '14' = '11th grade',
                                                          '16' = 'High school graduate',
                                                          '18' = 'Some college credit but less than 1 year',
                                                          '20' = 'Associates degree (e.g., AA, AS)',
                                                          '21' = 'Bachelor\'s degree (e.g., BA, AB, BS)',
                                                          '22' = 'Masters degree (e.g., MA, MS, MEng, MEd, MSW, MBA)',
                                                          '23' = 'Professional degree (e.g., MD, DDS, DVM, LLB, JD)',
                                                          '24' = 'Doctorate degree (e.g., PhD, EdD)',
                                                          '25' = 'One or more years of college at a 2-year program, no degree',
                                                          '26' = 'One year of college at a 4-year program, no degree',
                                                          '27' = 'Two years of college at a 4-year program, no degree',
                                                          '28' = 'Three years of college at a 4-year program, no degree',
                                                          '999' = 'Unknown'
                                           )),
    registration_gender = list( Description = 'Registration data: gender of participant (e.g. male, female)'),
    registration_handedness = list( Description = 'Registration data: dominant hand',
                                           Levels = list ('1' = 'Right',
                                                          '2' = 'Left')),
    registration_race = list( Description = 'Registration data: participant race in bitwise values - Each response is assigned a numeric value (i.e., 1, 2, 4, 8, 16, 32, 64). These values are summed and the total is stored as the score for the item ID. Therefore, if a participant has a score of 9, you know that he or she endorsed the first and fourth responses (scores 1 and 8 summed).',
                              Levels = list ('1' = 'White',
                                             '2' = 'Black or African American',
                                             '4' = 'Asian',
                                             '8' = 'American Indian or Alaska Native',
                                             '16' = 'Native Hawaiian or Other Pacific Islanders',
                                             '32' = 'Other',
                                             '64' = 'Not provided'
                                             )),
    registration_ethnicity = list( Description = 'Registration data: participant ethnicity',
                                   Levels = list ('1' = 'Not Hispanic or Latino',
                                                  '2' = 'Hispanic or Latino',
                                                  '-1' = 'Not provided'))
  )


  # convert formatting to JSON
  toolbox_json <- RJSONIO::toJSON(toolbox_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(toolbox_json, asText = TRUE))){
    print('NIH toolbox beh JSON file may be invalid')
  }

  return(toolbox_json)

}
