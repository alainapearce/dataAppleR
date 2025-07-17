#' json_researcher_notes: Generates a json file for researcher_notes
#'
#'
#' @return A string with data stored in JSON format containing meta-data for researcher_notes
#'
#'
#' @export

json_researcher_notes <- function() {

  notes_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Researcher notes about visits and tasks'),

    participant_id = list( Description = 'participant id number'),
    child_setup_v1_notes = list( Description = 'researcher notes from child visit protocol 1'),
    child_heightweight_ses1_notes = list( Description = 'researcher notes from child viist protocol 1 for child height and weight'),
    dexa_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) dexa scan'),
    meal_protocol_1_notes = list( Description = 'research notes regarding meal during child visit protocol 1'),
    stq_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) child screen time questionnaire'),
    flanker_7yr_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) the NIH toolbox Flanker task for 7-year-olds'),
    flanker_8yr_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) the NIH toolbox Flanker task for 8-year-olds'),
    dccs_7yr_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) the NIH toolbox dimensional card sorting task for 7-year-olds'),
    dccs_8yr_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) the NIH toolbox dimensional card sorting task for 8-year-olds'),
    listsort_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) the NIH toolbox list sorting task'),
    rrv_task_notes = list( Description = 'researcher notes regarding RRV task'),
    mock_fmri_notes = list( Description = 'researcher notes regarding mock fMRI training'),
    heightweight_ses1_notes = list( Description = 'researcher notes regarding session-1 (baseline / V1) parent and child height and weight'),
    meal_intake_protocol_1_notes = list( Description = 'researcher notes from child visit protocol 1 meal intake values'),
    child_protocol_1_notes = list( Description = 'researcher notes from child visit protocol 1'),
    sst_beh_notes = list( Description = 'researcher notes regarding SST behavioral task'),
    mock_fmri_2_notes = list( Description = 'researcher notes regarding second/day-of mock fMRI'),
    mri_anatomy_notes = list( Description = 'researcher notes regarding anatomical scan'),
    mri_sst_notes = list( Description = 'researcher notes regarding SST functional scan'),
    mri_resting_state_notes = list( Description = 'researcher notes regarding resting state scan'),
    child_protocol_2_notes = list( Description = 'researcher notes from child visit protocol 2'),
    child_setup_3_notes = list( Description = 'researcher notes from child visit protocol 3'),
    meal_protocol_3_notes = list( Description = 'research notes regarding meal during child visit protocol 3'),
    eah_protocol_3_notes = list( Description = 'research notes regarding EAH during child visit protocol 3'),
    space_game_notes = list( Description = 'researcher notes regarding the Space Game task'),
    pit_ses1_protocol_3_notes = list( Description = 'researcher notes regarding session-1 (baseline / V1) PIT task completed during child protocol visit 3'),
    eah_intake_protocol_3_notes = list( Description = 'researcher notes from child visit protocol 3 EAH intake values'),
    meal_intake_protocol_3_notes = list( Description = 'researcher notes from child visit protocol 3 meal intake values'),
    child_protocol_3_notes = list( Description = 'researcher notes from child visit protocol 3'),
    child_setup_4_notes = list( Description = 'researcher notes from child visit protocol 4'),
    meal_protocol_4_notes = list( Description = 'research notes regarding meal during child visit protocol 4'),
    eah_protocol_4_notes = list( Description = 'research notes regarding EAH during child visit protocol 4'),
    pit_ses1_protocol_4_notes = list( Description = 'researcher notes regarding session-1 (baseline / V1) PIT task completed during child protocol visit 4'),
    wasi_notes = list( Description = 'researcher notes regarding WASI'),
    eah_intake_protocol_4_notes = list( Description = 'researcher notes from child visit protocol 4 EAH intake values'),
    meal_intake_protocol_4_notes = list( Description = 'researcher notes from child visit protocol 4 meal intake values'),
    child_protocol_4_notes = list( Description = 'researcher notes from child visit protocol 4'),
    child_setup_5_notes = list( Description = 'researcher notes from child visit protocol 5'),
    dexa_ses2_notes = list( Description = 'research notes regarding session-2 (follow-up / V5) dexa scan'),
    meal_protocol_5_notes = list( Description = 'research notes regarding meal during child visit protocol 5'),
    flanker_8yr_ses2_notes = list( Description = 'research notes regarding session-2 (follow-up / V5) the NIH toolbox Flanker task for 8-year-olds'),
    dccs_8yr_ses2_notes = list( Description = 'research notes regarding session-2 (follow-up / V5) the NIH toolbox dimensional card sorting task for 8-year-olds'),
    listsort_ses2_notes = list( Description = 'research notes regarding session-2 (follow-up / V5) the NIH toolbox list sorting task'),
    pit_ses2_notes = list( Description = 'researcher notes regarding session-2 (follow-up / V5) PIT task'),
    heightweight_ses2_notes = list( Description = 'researcher notes regarding session-2 (follow-up / V5) parent and child height and weight'),
    eah_protocol_5_notes = list( Description = 'research notes regarding EAH during child visit protocol 5'),
    eah_intake_protocol_5_notes = list( Description = 'researcher notes from child visit protocol 5 EAH intake values'),
    meal_intake_protocol_5_notes = list( Description = 'researcher notes from child visit protocol 5 meal intake values'),
    child_protocol_5_notes = list( Description = 'researcher notes from child visit protocol 5')
  )

  # convert formatting to JSON
  notes_json <- RJSONIO::toJSON(notes_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(notes_json, asText = TRUE))){
    print('notes_json file may be invalid')
  }

  return(notes_json)

}
