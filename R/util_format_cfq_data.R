#' util_format_cfq_data: clean up the administration of the Child Feeding Questionnaire
#'
#' This function cleans up the administration of the Child Feeding Questionnaire (skipped questions about child weight concerns so numbers are off)
#'
#'
#' @param cfq_data rank extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' cfq_data_formatted <- util_format_cfq_data(cfq_data)
#'
#' @seealso [util_redcap_parent_v1()]
#'
#' @export


util_format_cfq_data <- function(cfq_data) {

  # rename columns
  names(cfq_data)[names(cfq_data) == 'cfq_29'] <- 'cfq_31'
  names(cfq_data)[names(cfq_data) == 'cfq_28'] <- 'cfq_30'
  names(cfq_data)[names(cfq_data) == 'cfq_27'] <- 'cfq_29'
  names(cfq_data)[names(cfq_data) == 'cfq_26'] <- 'cfq_28'
  names(cfq_data)[names(cfq_data) == 'cfq_25'] <- 'cfq_27'
  names(cfq_data)[names(cfq_data) == 'cfq_24'] <- 'cfq_26'
  names(cfq_data)[names(cfq_data) == 'cfq_23'] <- 'cfq_25'
  names(cfq_data)[names(cfq_data) == 'cfq_22'] <- 'cfq_24'
  names(cfq_data)[names(cfq_data) == 'cfq_21'] <- 'cfq_23'
  names(cfq_data)[names(cfq_data) == 'cfq_20'] <- 'cfq_22'
  names(cfq_data)[names(cfq_data) == 'cfq_19'] <- 'cfq_21'
  names(cfq_data)[names(cfq_data) == 'cfq_18'] <- 'cfq_20'
  names(cfq_data)[names(cfq_data) == 'cfq_17'] <- 'cfq_19'
  names(cfq_data)[names(cfq_data) == 'cfq_16'] <- 'cfq_18'
  names(cfq_data)[names(cfq_data) == 'cfq_15'] <- 'cfq_17'
  names(cfq_data)[names(cfq_data) == 'cfq_14'] <- 'cfq_16'
  names(cfq_data)[names(cfq_data) == 'cfq_13'] <- 'cfq_15'
  names(cfq_data)[names(cfq_data) == 'cfq_12'] <- 'cfq_14'

  cfq_data['cfq_12'] <- 6
  cfq_data['cfq_13'] <- 6

  cfq_data <- cfq_data[c(names(cfq_data)[1:13],names(cfq_data)[32:33], names(cfq_data)[14:31])]

  # return data
  return(cfq_data)

}
