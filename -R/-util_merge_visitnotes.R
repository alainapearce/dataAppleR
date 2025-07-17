#' util_merged_visitnotes: merges and formates visit notes
#'
#' This function merges visit notes in wide format and re-names variables
#'
#'
#' @param visit1_notes visit 1 notes data.frame
#' @param visit2_notes visit 2 notes data.frame
#' @param visit3_notes visit 3 notes data.frame
#' @param visit4_notes visit 4 notes data.frame
#' @param visit5_notes visit 5 notes data.frame
#'
#' @examples
#'
#' # process data
#' visit_notes_data <- util_merged_visitnotes(visit1_notes, visit2_notes, visit3_notes, visit4_notes, visit5_notes)
#'
#' @seealso [proc_redcap()]
#'
#' @export
#'


util_merged_visitnotes <- function(visit1_notes, visit2_notes, visit3_notes, visit4_notes, visit5_notes) {

  researcher_notes <- merge(visit1_notes[grepl('participant_id|v1', names(visit1_notes))], visit2_notes[grepl('participant_id|v2', names(visit2_notes))], by=c('participant_id'), all = TRUE)

  researcher_notes <- merge(researcher_notes, visit3_notes[grepl('participant_id|v3', names(visit3_notes))], by=c('participant_id'), all = TRUE)
  researcher_notes <- merge(researcher_notes, visit4_notes[grepl('participant_id|v4', names(visit4_notes))], by=c('participant_id'), all = TRUE)
  researcher_notes <- merge(researcher_notes, visit5_notes[grepl('participant_id|v5', names(visit5_notes))], by=c('participant_id'), all = TRUE)

  # update names
  names(researcher_notes)[names(researcher_notes) == 'v1_notes'] <- 'child_protocol_1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v2_notes'] <- 'child_protocol_2_notes'
  names(researcher_notes)[names(researcher_notes) == 'v3_notes'] <- 'child_protocol_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_notes'] <- 'child_protocol_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_notes'] <- 'child_protocol_5_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_pre_notes'] <- 'child_setup_1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v3_pre_notes'] <- 'child_setup_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_pre_notes'] <- 'child_setup_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_pre_notes'] <- 'child_setup_5_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_test_meal_notes'] <- 'meal_protocol_1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v3_test_meal_notes'] <- 'meal_protocol_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_test_meal_notes'] <- 'meal_protocol_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_test_meal_notes'] <- 'meal_protocol_5_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_meal_intake_notes'] <- 'meal_intake_protocol_1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v3_meal_intake_notes'] <- 'meal_intake_protocol_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_meal_intake_notes'] <- 'meal_intake_protocol_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_meal_intake_notes'] <- 'meal_intake_protocol_5_notes'

  names(researcher_notes)[names(researcher_notes) == 'v3_eah_notes'] <- 'eah_protocol_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_eah_notes'] <- 'eah_protocol_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_eah_notes'] <- 'eah_protocol_5_notes'

  names(researcher_notes)[names(researcher_notes) == 'v3_eah_intake_notes'] <- 'eah_intake_protocol_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_eah_intake_notes'] <- 'eah_intake_protocol_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_eah_intake_notes'] <- 'eah_intake_protocol_5_notes'

  names(researcher_notes)[names(researcher_notes) == 'v3_pit_task_notes'] <- 'pit_ses1_protocol_3_notes'
  names(researcher_notes)[names(researcher_notes) == 'v4_pit_task_notes'] <- 'pit_ses1_protocol_4_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_pit_task_notes'] <- 'pit_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_dxa_notes'] <- 'dexa_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_dxa_notes'] <- 'dexa_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_child_heightweight_notes'] <- 'child_heightweight_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v1_parent_heightweight_notes'] <- 'heightweight_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_heightweight_notes'] <- 'heightweight_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_flanker_7yr_notes'] <- 'flanker_7yr_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v1_flanker_8yr_notes'] <- 'flanker_8yr_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_flanker_8yr_notes'] <- 'flanker_8yr_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_dccs_7yr_notes'] <- 'dccs_7yr_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v1_dccs_8yr_notes'] <- 'dccs_8yr_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_dccs_8yr_notes'] <- 'dccs_8yr_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_listsort_notes'] <- 'listsort_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_listsort_notes'] <- 'listsort_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v1_stq_notes'] <- 'stq_ses1_notes'
  names(researcher_notes)[names(researcher_notes) == 'v5_stq_notes'] <- 'stq_ses2_notes'

  names(researcher_notes)[names(researcher_notes) == 'v2_mock_fmri_notes'] <- 'mock_fmri_2_notes'


  names(researcher_notes) <- gsub('^v1_|^v2_|^v3_|^v4_|^v5_', '', names(researcher_notes))

  # return data
  return(researcher_notes)

}
