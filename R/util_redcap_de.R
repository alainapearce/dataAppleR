#' util_redcap_de: Organize double-entry data from REDCap
#'
#' This function organizes REDCap double entry data data
#'
#' @param redcap_api (logical) execute REDCap API. Default = FALSE.
#' @param redcap_de_data REDCap double-entry data from a prior API call
#' @inheritParams util_redcap_parent_v1
#'
#' @return Will return a list including data that has been double-entered and checked along with the metadata for:
#' \itemize{
#'  \item{hfi_data}
#'  \item{dxa}
#'  \item{intake_v1}
#'  \item{intake_v2}
#'  \item{intake_v3}
#'  \item{intake_v4}
#'  \item{intake_v5}
#' }
#'
#'
#' @export

util_redcap_de <- function(redcap_api = FALSE, redcap_de_data, date_data) {

  #### Set up/initial checks #####

  # check that data is passed if redcap_api = FALSE
  if (isFALSE(redcap_api)){

    # check that redcap_de_data exist and is a data.frame
    de_data_arg <- methods::hasArg(redcap_de_data)

    if (isTRUE(de_data_arg)) {
      if (!is.data.frame(redcap_de_data)) {
        stop('redcap_de_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(de_data_arg)) {
      stop('redcap_de_data must be a data.frame with recap_api = FALSE')
    }

  } else {
    # get data from REDCap directly (only will work if have access and keys setup)
    Sys.setenv(reach_de_redcap_key = keyring::key_get('reach-de_redcap_key'))
    redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                       token = Sys.getenv('reach_de_redcap_key'))

    redcap_de_data <- redcap_de[['data']]
    redcap_de_dict <- redcap_de[['dictionary']]

    # remove '.factor'
    redcap_de_data <- redcap_de_data[, !grepl('.factor', names(redcap_de_data))]
  }

  # update name of participant ID column
  names(redcap_de_data)[names(redcap_de_data) == 'record_id'] <- 'participant_id'

  ## Extract data ####
  checked_data <- redcap_de_data[!grepl('--', redcap_de_data$participant_id), ]

  # make a grepl string including all merged ids separate by '|'
  merged_ids_grepl <- paste0(checked_data[['participant_id']], collapse = '|')

  # get vector indicator of unmerged ids
  unmerged_ids <- sapply(redcap_de_data[['participant_id']], function(x) !grepl(merged_ids_grepl, x))

  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {
    unmerged_data <- redcap_de_data[unmerged_ids, ]
  }

  # Make ID column bids compliant: add 'sub_'
  checked_data$participant_id <- paste0('sub-', checked_data$participant_id)

  ## DXA data - visit 1 ####
  # visit 1 data
  dxa <- checked_data[grepl('participant_id|dxa|left|right|spine|pelvis|subtotal|head|total|trunk|android|gynoid|fat|vat|lean', names(checked_data))]

  # remove extra columns and re-order
  dxa <- dxa[!grepl('check|dxa_id|dob|sex|ethnicity|age|intake|complete', names(dxa))]

  # fix names
  names(dxa) <- gsub('^v1_|_v1$|dxa_', '', names(dxa))
  names(dxa)[names(dxa) == 'scan_date'] <- 'visit_date'

  dxa['visit_date'] <- lubridate::as_date(dxa[['visit_date']])

  # re-order
  dxa <- dxa[c('participant_id', 'visit_date', 'height', 'weight', names(dxa)[!grepl('_id|visit|^height|^weight', names(dxa))])]

  # make numeric
  dxa[!grepl('_id|date', names(dxa))] <- sapply(dxa[!grepl('_id|date', names(dxa))], function(x) as.numeric(x))

  dxa <- util_format_dxa(dxa)

  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    dxa_unmerged <- unmerged_data[grepl('participant_id|dxa|left|right|spine|pelvis|subtotal|head|total|trunk|android|gynoid|fat|vat|lean', names(unmerged_data))]

    # remove extra columns and re-order
    dxa_unmerged <- dxa_unmerged[!grepl('check|dxa_id|dob|sex|ethnicity|age|intake|complete', names(dxa_unmerged))]

    # fix names
    names(dxa_unmerged) <- gsub('^v1_|_v1$|dxa_', '', names(dxa_unmerged))
    names(dxa_unmerged)[names(dxa_unmerged) == 'scan_date'] <- 'visit_date'
    dxa_unmerged['visit_date'] <- lubridate::as_date(dxa_unmerged[['visit_date']])

    # re-order
    dxa_unmerged <- dxa_unmerged[c('participant_id', 'visit_date', 'height', 'weight', names(dxa_unmerged)[!grepl('_id|visit|^height|^weight', names(dxa_unmerged))])]

    # make numeric
    dxa_unmerged[!grepl('_id|date', names(dxa_unmerged))] <- sapply(dxa_unmerged[!grepl('_id|date', names(dxa_unmerged))], function(x) as.numeric(x))

    dxa_unmerged <- util_format_dxa(dxa_unmerged)

    # check unmerged values
    data_de_list <- util_de_check(dxa_unmerged)

    if (is.data.frame(data_de_list$merged_de_data)) {
      dxa <- rbind.data.frame(dxa, data_de_list$merged_de_data)
      dxa <- dxa[order(dxa[['participant_id']]), ]
    }
  }

  #dxa_json <- json_dxa()
  dxa_json <- NA

  ## intake data - visit 1 ####
  intake_v1 <- checked_data[grepl('_id|_v1$', names(checked_data))]

  names(intake_v1) <- gsub('_v1', '', names(intake_v1))
  intake_v1 <- intake_v1[!grepl('dxa_id', names(intake_v1))]

  intake_v1 <- util_format_intake_data(intake_v1)

  # merge with date data for v1
  intake_v1 <- merge(intake_v1, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v1)[names(intake_v1) == 'v1_date'] <- 'visit_date'
  intake_v1['visit_date'] <- lubridate::as_date(intake_v1[['visit_date']])

  intake_v1['visit'] <- 1

  intake_v1['preload'] <- intake_v1['preload'] - 1

  # re-order
  intake_v1 <- intake_v1[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v1)[!grepl('_id|visit|preload', names(intake_v1))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v1_unmerged <- unmerged_data[grepl('_id|_v1$', names(unmerged_data))]

    # remove extra columns and re-order
    names(intake_v1_unmerged) <- gsub('_v1', '', names(intake_v1_unmerged))
    intake_v1_unmerged <- intake_v1_unmerged[!grepl('dxa_id', names(intake_v1_unmerged))]

    intake_v1_unmerged <- util_format_intake_data(intake_v1_unmerged)


    # check unmerged values
    intake_v1_de_list <- util_de_check(intake_v1_unmerged)

    if (is.data.frame(intake_v1_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v1_de_merged <- merge(intake_v1_de_list$merged_de_data, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v1_de_merged)[names(intake_v1_de_merged) == 'v1_date'] <- 'visit_date'
      intake_v1_de_merged['visit_date'] <- lubridate::as_date(intake_v1_de_merged[['visit_date']])

      intake_v1_de_merged['visit'] <- 1

      intake_v1_de_merged['preload'] <- intake_v1_de_merged['preload'] - 1

      # re-order
      intake_v1_de_merged <- intake_v1_de_merged[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v1_de_merged)[!grepl('_id|visit|preload', names(intake_v1_de_merged))])]

      # combine with exisitng merged data
      intake_v1 <- rbind.data.frame(intake_v1, intake_v1_de_merged)
      intake_v1 <- intake_v1[order(intake_v1[['participant_id']]), ]
    }
  }

  ## intake data - visit 2 ####
  intake_v2 <- checked_data[grepl('_id|_v2$', names(checked_data))]

  names(intake_v2) <- gsub('_v2', '', names(intake_v2))
  intake_v2 <- intake_v2[!grepl('dxa_id', names(intake_v2))]

  intake_v2 <- util_format_intake_data(intake_v2)

  # merge with date data for v2
  intake_v2 <- merge(intake_v2, date_data[c('participant_id', 'v2_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v2)[names(intake_v2) == 'v2_date'] <- 'visit_date'
  intake_v2['visit_date'] <- lubridate::as_date(intake_v2[['visit_date']])

  intake_v2['visit'] <- 2

  intake_v2['preload'] <- intake_v2['preload'] - 1

  # re-order
  intake_v2 <- intake_v2[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v2)[!grepl('_id|visit|preload', names(intake_v2))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v2_unmerged <- unmerged_data[grepl('_id|_v2$', names(unmerged_data))]

    # remove extra columns and re-order
    names(intake_v2_unmerged) <- gsub('_v2', '', names(intake_v2_unmerged))
    intake_v2_unmerged <- intake_v2_unmerged[!grepl('dxa_id', names(intake_v2_unmerged))]

    intake_v2_unmerged <- util_format_intake_data(intake_v2_unmerged)


    # check unmerged values
    intake_v2_de_list <- util_de_check(intake_v2_unmerged)

    if (is.data.frame(intake_v2_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v2_de_merged <- merge(intake_v2_de_list$merged_de_data, date_data[c('participant_id', 'v2_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v2_de_merged)[names(intake_v2_de_merged) == 'v2_date'] <- 'visit_date'
      intake_v2_de_merged['visit_date'] <- lubridate::as_date(intake_v2_de_merged[['visit_date']])

      intake_v2_de_merged['visit'] <- 2

      intake_v2_de_merged['preload'] <- intake_v2_de_merged['preload'] - 1


      # re-order
      intake_v2_de_merged <- intake_v2_de_merged[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v2_de_merged)[!grepl('_id|visit|preload', names(intake_v2_de_merged))])]

      # combine with exisitng merged data
      intake_v2 <- rbind.data.frame(intake_v2, intake_v2_de_merged)
      intake_v2 <- intake_v2[order(intake_v2[['participant_id']]), ]
    }
  }

  ## intake data - visit 3 ####
  intake_v3 <- checked_data[grepl('_id|_v3$', names(checked_data))]

  names(intake_v3) <- gsub('_v3', '', names(intake_v3))
  intake_v3 <- intake_v3[!grepl('dxa_id', names(intake_v3))]

  intake_v3 <- util_format_intake_data(intake_v3)

  # merge with date data for v3
  intake_v3 <- merge(intake_v3, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v3)[names(intake_v3) == 'v3_date'] <- 'visit_date'
  intake_v3['visit_date'] <- lubridate::as_date(intake_v3[['visit_date']])

  intake_v3['visit'] <- 3

  intake_v3['preload'] <- intake_v3['preload'] - 1


  # re-order
  intake_v3 <- intake_v3[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v3)[!grepl('_id|visit|preload', names(intake_v3))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v3_unmerged <- unmerged_data[grepl('_id|_v3$', names(unmerged_data))]

    # remove extra columns and re-order
    names(intake_v3_unmerged) <- gsub('_v3', '', names(intake_v3_unmerged))
    intake_v3_unmerged <- intake_v3_unmerged[!grepl('dxa_id', names(intake_v3_unmerged))]

    intake_v3_unmerged <- util_format_intake_data(intake_v3_unmerged)


    # check unmerged values
    intake_v3_de_list <- util_de_check(intake_v3_unmerged)

    if (is.data.frame(intake_v3_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v3_de_merged <- merge(intake_v3_de_list$merged_de_data, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v3_de_merged)[names(intake_v3_de_merged) == 'v3_date'] <- 'visit_date'
      intake_v3_de_merged['visit_date'] <- lubridate::as_date(intake_v3_de_merged[['visit_date']])

      intake_v3_de_merged['visit'] <- 3

      intake_v3_de_merged['preload'] <- intake_v3_de_merged['preload'] - 1


      # re-order
      intake_v3_de_merged <- intake_v3_de_merged[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v3_de_merged)[!grepl('_id|visit|preload', names(intake_v3_de_merged))])]

      # combine with exisitng merged data
      intake_v3 <- rbind.data.frame(intake_v3, intake_v3_de_merged)
      intake_v3 <- intake_v3[order(intake_v3[['participant_id']]), ]
    }
  }

  ## intake data - visit 4 ####
  intake_v4 <- checked_data[grepl('_id|_v4$', names(checked_data))]

  names(intake_v4) <- gsub('_v4', '', names(intake_v4))
  intake_v4 <- intake_v4[!grepl('dxa_id', names(intake_v4))]

  intake_v4 <- util_format_intake_data(intake_v4)

  # merge with date data for v4
  intake_v4 <- merge(intake_v4, date_data[c('participant_id', 'v4_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v4)[names(intake_v4) == 'v4_date'] <- 'visit_date'
  intake_v4['visit_date'] <- lubridate::as_date(intake_v4[['visit_date']])

  intake_v4['visit'] <- 4

  intake_v4['preload'] <- intake_v4['preload'] - 1


  # re-order
  intake_v4 <- intake_v4[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v4)[!grepl('_id|visit|preload', names(intake_v4))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v4_unmerged <- unmerged_data[grepl('_id|_v4$', names(unmerged_data))]

    # remove extra columns and re-order
    names(intake_v4_unmerged) <- gsub('_v4', '', names(intake_v4_unmerged))
    intake_v4_unmerged <- intake_v4_unmerged[!grepl('dxa_id', names(intake_v4_unmerged))]

    intake_v4_unmerged <- util_format_intake_data(intake_v4_unmerged)


    # check unmerged values
    intake_v4_de_list <- util_de_check(intake_v4_unmerged)

    if (is.data.frame(intake_v4_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v4_de_merged <- merge(intake_v4_de_list$merged_de_data, date_data[c('participant_id', 'v4_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v4_de_merged)[names(intake_v4_de_merged) == 'v4_date'] <- 'visit_date'
      intake_v4_de_merged['visit_date'] <- lubridate::as_date(intake_v4_de_merged[['visit_date']])

      intake_v4_de_merged['visit'] <- 4

      intake_v4_de_merged['preload'] <- intake_v4_de_merged['preload'] - 1


      # re-order
      intake_v4_de_merged <- intake_v4_de_merged[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v4_de_merged)[!grepl('_id|visit|preload', names(intake_v4_de_merged))])]

      # combine with exisitng merged data
      intake_v4 <- rbind.data.frame(intake_v4, intake_v4_de_merged)
      intake_v4 <- intake_v4[order(intake_v4[['participant_id']]), ]
    }
  }

  ## intake data - visit 5 ####
  intake_v5 <- checked_data[grepl('_id|_v5$|chips|mms|brownies|brownies|cookies|starburst|fritos', names(checked_data))]

  names(intake_v5) <- gsub('_v5', '', names(intake_v5))
  intake_v5 <- intake_v5[!grepl('dxa_id|fhfi', names(intake_v5))]

  intake_v5 <- util_format_intake_data(intake_v5)

  # merge with date data for v5
  intake_v5 <- merge(intake_v5, date_data[c('participant_id', 'v5_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v5)[names(intake_v5) == 'v5_date'] <- 'visit_date'
  intake_v5['visit_date'] <- lubridate::as_date(intake_v5[['visit_date']])

  intake_v5['visit'] <- 5

  intake_v5['preload'] <- intake_v5['preload'] - 1


  # re-order
  intake_v5 <- intake_v5[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v5)[!grepl('_id|visit|preload', names(intake_v5))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v5_unmerged <- unmerged_data[grepl('_id|_v5$|chips|mms|brownies|brownies|cookies|starburst|fritos', names(unmerged_data))]

    # remove extra columns and re-order
    names(intake_v5_unmerged) <- gsub('_v5', '', names(intake_v5_unmerged))
    intake_v5_unmerged <- intake_v5_unmerged[!grepl('dxa_id|fhfi', names(intake_v5_unmerged))]

    intake_v5_unmerged <- util_format_intake_data(intake_v5_unmerged)


    # check unmerged values
    intake_v5_de_list <- util_de_check(intake_v5_unmerged)

    if (is.data.frame(intake_v5_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v5_de_merged <- merge(intake_v5_de_list$merged_de_data, date_data[c('participant_id', 'v5_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v5_de_merged)[names(intake_v5_de_merged) == 'v5_date'] <- 'visit_date'
      intake_v5_de_merged['visit_date'] <- lubridate::as_date(intake_v5_de_merged[['visit_date']])

      intake_v5_de_merged['visit'] <- 5

      intake_v5_de_merged['preload'] <- intake_v5_de_merged['preload'] - 1


      # re-order
      intake_v5_de_merged <- intake_v5_de_merged[c('participant_id', 'preload', 'visit', 'visit_date', names(intake_v5_de_merged)[!grepl('_id|visit|preload', names(intake_v5_de_merged))])]

      # combine with exisitng merged data
      intake_v5 <- rbind.data.frame(intake_v5, intake_v5_de_merged)
      intake_v5 <- intake_v5[order(intake_v5[['participant_id']]), ]
    }
  }

  intake_json <- json_intake()
  intake_v5_json <- json_intake_v5()


  return(list(dxa = list(data = dxa, meta = dxa_json),
              intake_v1 = list(data = intake_v1, meta = intake_json),
              intake_v2 = list(data = intake_v2, meta = intake_json),
              intake_v3 = list(data = intake_v3, meta = intake_json),
              intake_v4 = list(data = intake_v4, meta = intake_json),
              intake_v5 = list(data = intake_v5, meta = intake_v5_json)
  ))

}

