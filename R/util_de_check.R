#' util_de_check: Checks double-entry data that have not yet been merged in REDCap
#'
#' This function will compare double-entry data that have not yet merged in REDCap
#'
#' @param unmerged_data data.frame of unmerged double entry data (should have ids similar to 001--1, 001--2)
#'
#' @return Two data.frames:
#' \itemize{
#'  \item{1) merged_de_data: merged data.frame after confirming matching values}
#'  \item{2) unmerged_de_data: data.frame containing participants with unmatched or non-double-entered data}
#'  \item{3) single_de_data: data.frame contianing participant with only 1 set of entered values so no comparison could be made}
#'  }
#'
#' @examples
#'
#' # process data
#' data_de_list <- util_de_check(unmerged_data)
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_de_check <- function(unmerged_data) {

  # get unique participant id strings
  id_list <- c(unmerged_data[['participant_id']])
  id_list <- gsub('--1|--2', '', id_list)
  id_list <- unique(id_list)

  de_check_fn <- function(id_str, unmerged_data){
    de_data <- unmerged_data[grepl(id_str, unmerged_data[['participant_id']]), ]

    if (nrow(de_data) == 1){
      check_good = 'only 1 entered'
    } else if (nrow(de_data) == 2) {
      check_list <- sapply(names(de_data)[!grepl('_id', names(de_data))], function(x) ifelse(de_data[de_data['participant_id'] == paste0(id_str, '--1'), x] == de_data[de_data['participant_id'] == paste0(id_str, '--2'), x], TRUE, FALSE))

      if (sum(isFALSE(check_list)) == 0){
        check_good = TRUE
      } else {
        check_good = FALSE
      }
    } else {
      # not 1 or two
      check_good = FALSE
    }

    return(check_good)
  }

  # get logical indicator of ids with matching data
  id_check_good <- sapply(id_list, function(x) de_check_fn(x, unmerged_data))

  if (sum(grepl('TRUE', id_check_good)) > 0){

    # extract matching participants and process ID so will align with already merged data
    unmerged_data_good <- unmerged_data[grepl('TRUE', id_check_good) & grepl('--1', unmerged_data[['participant_id']]), ]

    # fix participant ID
    unmerged_data_good['participant_id'] <- gsub('--1', '', unmerged_data_good[['participant_id']])
    unmerged_data_good['participant_id'] <- paste0("sub-",  unmerged_data_good[['participant_id']])

  } else {
    unmerged_data_good <- 'no matching data to merge'
  }

  if (sum(grepl('FALSE', id_check_good)) > 0){
    unmerged_data_miss <- unmerged_data[grepl('FALSE', id_check_good), ]

  } else {
    unmerged_data_miss <- 'no participants with 2 entrees that have miss-match values'
  }

  if (sum(grepl('only', id_check_good)) > 0 ){
    single_data_good <- unmerged_data[grepl('only', id_check_good), ]
  } else {
    single_data_good <- 'no participant only had 1 set of values entered'
  }

  # return data
  return(list(merged_de_data = unmerged_data_good,
              unmerged_de_data = unmerged_data_miss,
              single_de_data = single_data_good))

}
