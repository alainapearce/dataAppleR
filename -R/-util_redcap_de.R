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
#'  \item{dxa_v1}
#'  \item{dxa_v5}
#'  \item{intake_v1}
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
  dxa_v1 <- checked_data[grepl('participant_id|^dxa.*v1$|^left.*v1$|right.*v1$|^v1.*v1$', names(checked_data))]

  # remove extra columns and re-order
  dxa_v1 <- dxa_v1[!grepl('check|dxa_id|visit_number|dob|sex|ethnicity|age', names(dxa_v1))]

  # add session column
  dxa_v1['session_id'] <- 'ses-1'

  # add visit number
  dxa_v1['visit_protocol'] <- 1

  # fix names
  names(dxa_v1) <- gsub('^v1_|_v1$|dxa_', '', names(dxa_v1))
  names(dxa_v1)[names(dxa_v1) == 'scan_date'] <- 'visit_date'

  dxa_v1['visit_date'] <- lubridate::as_date(dxa_v1[['visit_date']])

  # re-order
  dxa_v1 <- dxa_v1[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'height', 'weight', names(dxa_v1)[!grepl('_id|visit|^height|^weight', names(dxa_v1))])]

  # make numeric
  dxa_v1[!grepl('_id|date', names(dxa_v1))] <- sapply(dxa_v1[!grepl('_id|date', names(dxa_v1))], function(x) as.numeric(x))

  dxa_v1 <- util_format_dxa(dxa_v1)

  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    dxa_v1_unmerged <- unmerged_data[grepl('participant_id|^dxa.*v1$|^left.*v1$|right.*v1$|^v1.*v1$', names(unmerged_data))]

    # remove extra columns and re-order
    dxa_v1_unmerged <- dxa_v1_unmerged[!grepl('check|dxa_id|visit_number|dob|sex|ethnicity|age', names(dxa_v1_unmerged))]

    # add session column
    dxa_v1_unmerged['session_id'] <- 'ses-1'

    # add visit number
    dxa_v1_unmerged['visit_protocol'] <- 1

    # fix names
    names(dxa_v1_unmerged) <- gsub('^v1_|_v1$|dxa_', '', names(dxa_v1_unmerged))
    names(dxa_v1_unmerged)[names(dxa_v1_unmerged) == 'scan_date'] <- 'visit_date'
    dxa_v1_unmerged['visit_date'] <- lubridate::as_date(dxa_v1_unmerged[['visit_date']])

    # re-order
    dxa_v1_unmerged <- dxa_v1_unmerged[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'height', 'weight', names(dxa_v1_unmerged)[!grepl('_id|visit|^height|^weight', names(dxa_v1_unmerged))])]

    # make numeric
    dxa_v1_unmerged[!grepl('_id|date', names(dxa_v1_unmerged))] <- sapply(dxa_v1_unmerged[!grepl('_id|date', names(dxa_v1_unmerged))], function(x) as.numeric(x))

    dxa_v1_unmerged <- util_format_dxa(dxa_v1_unmerged)

    # check unmerged values
    data_de_list <- util_de_check(dxa_v1_unmerged)

    if (is.data.frame(data_de_list$merged_de_data)) {
      dxa_v1 <- rbind.data.frame(dxa_v1, data_de_list$merged_de_data)
      dxa_v1 <- dxa_v1[order(dxa_v1[['participant_id']]), ]
    }
  }

  ## DXA data - visit 5####
  # visit 5 data
  dxa_v5 <- checked_data[grepl('participant_id|^dxa.*v5$|^left.*v5$|right.*v5$|^v1.*v5$', names(checked_data))]

  # remove extra columns and re-order
  dxa_v5 <- dxa_v5[!grepl('check|dxa_id|visit_number|dob|sex|ethnicity|age', names(dxa_v5))]

  # add session column
  dxa_v5['session_id'] <- 'ses-2'

  # add visit number
  dxa_v5['visit_protocol'] <- 5

  # fix names
  names(dxa_v5) <- gsub('^v1_|_v5$|dxa_', '', names(dxa_v5))
  names(dxa_v5)[names(dxa_v5) == 'scan_date'] <- 'visit_date'

  dxa_v5['visit_date'] <- lubridate::as_date(dxa_v5[['visit_date']])

  # re-order
  dxa_v5 <- dxa_v5[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'height', 'weight', names(dxa_v5)[!grepl('_id|visit|^height|^weight', names(dxa_v5))])]

  # make numeric
  dxa_v5[!grepl('_id|date', names(dxa_v5))] <- sapply(dxa_v5[!grepl('_id|date', names(dxa_v5))], function(x) as.numeric(x))

  dxa_v5 <- util_format_dxa(dxa_v5)

  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    dxa_v5_unmerged <- unmerged_data[grepl('participant_id|^dxa.*v5$|^left.*v5$|right.*v5$|^v1.*v5$', names(unmerged_data))]

    # remove extra columns and re-order
    dxa_v5_unmerged <- dxa_v5_unmerged[!grepl('check|dxa_id|visit_number|dob|sex|ethnicity|age', names(dxa_v5_unmerged))]

    # add session column
    dxa_v5_unmerged['session_id'] <- 'ses-2'

    # add visit number
    dxa_v5_unmerged['visit_protocol'] <- 5

    # fix names
    names(dxa_v5_unmerged) <- gsub('^v1_|_v5$|dxa_', '', names(dxa_v5_unmerged))
    names(dxa_v5_unmerged)[names(dxa_v5_unmerged) == 'scan_date'] <- 'visit_date'
    dxa_v5_unmerged['visit_date'] <- lubridate::as_date(dxa_v5_unmerged[['visit_date']])

    # re-order
    dxa_v5_unmerged <- dxa_v5_unmerged[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'height', 'weight', names(dxa_v5_unmerged)[!grepl('_id|visit|^height|^weight', names(dxa_v5_unmerged))])]

    # make numeric
    dxa_v5_unmerged[!grepl('_id|date', names(dxa_v5_unmerged))] <- sapply(dxa_v5_unmerged[!grepl('_id|date', names(dxa_v5_unmerged))], function(x) as.numeric(x))

    dxa_v5_unmerged <- util_format_dxa(dxa_v5_unmerged)

    # remove rows with all NA
    dxa_v5_unmerged <- dxa_v5_unmerged[!(rowSums(is.na(dxa_v5_unmerged)) == ncol(dxa_v5_unmerged) - 3), ]

    # check unmerged values
    data_de_list <- util_de_check(dxa_v5_unmerged)

    if (is.data.frame(data_de_list$merged_de_data)) {
      dxa_v5 <- rbind.data.frame(dxa_v5, data_de_list$merged_de_data)
      dxa_v5 <- dxa_v5[order(dxa_v5[['participant_id']]), ]
    }
  }

  dxa_json <- json_dxa()

  ## intake data - visit 1 ####
  intake_v1 <- checked_data[grepl('_id|plate_v1', names(checked_data))]

  # remove extra columns and re-order
  intake_v1 <- intake_v1[!grepl('dxa_id', names(intake_v1))]

  intake_v1['session_id'] <- 'ses-2'
  intake_v1['visit_protocol'] <- 1

  # merge with date data for v1
  intake_v1 <- merge(intake_v1, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v1)[names(intake_v1) == 'v1_date'] <- 'visit_date'
  intake_v1['visit_date'] <- lubridate::as_date(intake_v1[['visit_date']])

  # fix names
  names(intake_v1) <- gsub('_v1', '', names(intake_v1))

  # re-order
  intake_v1 <- intake_v1[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(intake_v1)[!grepl('_id|visit', names(intake_v1))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v1_unmerged <- unmerged_data[grepl('_id|plate_v1', names(unmerged_data))]

    # remove extra columns and re-order
    intake_v1_unmerged <- intake_v1_unmerged[!grepl('dxa_id', names(intake_v1_unmerged))]

    intake_v1_unmerged['session_id'] <- 'ses-1'
    intake_v1_unmerged['visit_protocol'] <- 1

    # fix names
    names(intake_v1_unmerged) <- gsub('_v1', '', names(intake_v1_unmerged))

    # check unmerged values
    intake_v1_de_list <- util_de_check(intake_v1_unmerged)

    if (is.data.frame(intake_v1_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v1_de_merged <- merge(intake_v1_de_list$merged_de_data, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v1_de_merged)[names(intake_v1_de_merged) == 'v1_date'] <- 'visit_date'
      intake_v1_de_merged['visit_date'] <- lubridate::as_date(intake_v1_de_merged[['visit_date']])

      # re-order
      intake_v1_de_merged <- intake_v1_de_merged[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(intake_v1_de_merged)[!grepl('_id|visit', names(intake_v1_de_merged))])]

      # remove rows with all NA
      intake_v1_de_merged <- intake_v1_de_merged[!(rowSums(is.na(intake_v1_de_merged)) == ncol(intake_v1_de_merged) - 4), ]

      # combine with exisitng merged data
      intake_v1 <- rbind.data.frame(intake_v1, intake_v1_de_merged)
      intake_v1 <- intake_v1[order(intake_v1[['participant_id']]), ]
    }
  }

  intake_v1_json <- json_v1_intake()

  ## intake data - visit 3 ####
  intake_v3 <- checked_data[grepl('_id|plate_v3|^ad.*v3$', names(checked_data))]

  # remove extra columns and re-order
  intake_v3 <- intake_v3[!grepl('dxa_id', names(intake_v3))]

  intake_v3['session_id'] <- 'ses-1'
  intake_v3['visit_protocol'] <- 3

  # merge with date data for v3
  intake_v3 <- merge(intake_v3, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v3)[names(intake_v3) == 'v3_date'] <- 'visit_date'
  intake_v3['visit_date'] <- lubridate::as_date(intake_v3[['visit_date']])

  #fix names
  names(intake_v3)[names(intake_v3) == 'ad_cond_eah_v3'] <- 'advertisement_condition'
  names(intake_v3) <- gsub('_v3', '', names(intake_v3))

  # re-order
  intake_v3 <- intake_v3[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_v3)[!grepl('_id|visit|advertisement_condition', names(intake_v3))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v3_unmerged <- unmerged_data[grepl('_id|plate_v3|^ad.*v3$', names(unmerged_data))]

    # remove extra columns and re-order
    intake_v3_unmerged <- intake_v3_unmerged[!grepl('dxa_id', names(intake_v3_unmerged))]

    intake_v3_unmerged['session_id'] <- 'ses-1'
    intake_v3_unmerged['visit_protocol'] <- 3

    #fix names
    names(intake_v3_unmerged)[names(intake_v3_unmerged) == 'ad_cond_eah_v3'] <- 'advertisement_condition'
    names(intake_v3_unmerged) <- gsub('_v3', '', names(intake_v3_unmerged))

    # check unmerged values
    intake_v3_de_list <- util_de_check(intake_v3_unmerged)

    if (is.data.frame(intake_v3_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v3_de_merged <- merge(intake_v3_de_list$merged_de_data, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v3_de_merged)[names(intake_v3_de_merged) == 'v3_date'] <- 'visit_date'
      intake_v3_de_merged['visit_date'] <- lubridate::as_date(intake_v3_de_merged[['visit_date']])

      # re-order
      intake_v3_de_merged <- intake_v3_de_merged[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_v3_de_merged)[!grepl('_id|visit|advertisement_condition', names(intake_v3_de_merged))])]

      # remove rows with all NA
      intake_v3_de_merged <- intake_v3_de_merged[!(rowSums(is.na(intake_v3_de_merged)) >= ncol(intake_v3_de_merged) - 4), ]

      # combine with exisitng merged data
      intake_v3 <- rbind.data.frame(intake_v3, intake_v3_de_merged)
      intake_v3 <- intake_v3[order(intake_v3[['participant_id']]), ]
    }
  }

  ## intake data - visit 4 ####
  intake_v4 <- checked_data[grepl('_id|plate_v4|^ad.*v4$', names(checked_data))]

  # remove extra columns and re-order
  intake_v4 <- intake_v4[!grepl('dxa_id', names(intake_v4))]

  intake_v4['session_id'] <- 'ses-1'
  intake_v4['visit_protocol'] <- 4

  #fix names
  names(intake_v4)[names(intake_v4) == 'ad_cond_eah_v4'] <- 'advertisement_condition'
  names(intake_v4) <- gsub('_v4', '', names(intake_v4))

  # merge with date data for v4
  intake_v4 <- merge(intake_v4, date_data[c('participant_id', 'v4_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v4)[names(intake_v4) == 'v4_date'] <- 'visit_date'
  intake_v4['visit_date'] <- lubridate::as_date(intake_v4[['visit_date']])

  # re-order
  intake_v4 <- intake_v4[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_v4)[!grepl('_id|visit|advertisement_condition', names(intake_v4))])]

  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v4_unmerged <- unmerged_data[grepl('_id|plate_v4|^ad.*v4$', names(unmerged_data))]

    # remove extra columns and re-order
    intake_v4_unmerged <- intake_v4_unmerged[!grepl('dxa_id', names(intake_v4_unmerged))]

    intake_v4_unmerged['session_id'] <- 'ses-1'
    intake_v4_unmerged['visit_protocol'] <- 4

    #fix names
    names(intake_v4_unmerged)[names(intake_v4_unmerged) == 'ad_cond_eah_v4'] <- 'advertisement_condition'
    names(intake_v4_unmerged) <- gsub('_v4', '', names(intake_v4_unmerged))

    # check unmerged values
    intake_v4_de_list <- util_de_check(intake_v4_unmerged)

    if (is.data.frame(intake_v4_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v4_de_merged <- merge(intake_v4_de_list$merged_de_data, date_data[c('participant_id', 'v4_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v4_de_merged)[names(intake_v4_de_merged) == 'v4_date'] <- 'visit_date'
      intake_v4_de_merged['visit_date'] <- lubridate::as_date(intake_v4_de_merged[['visit_date']])

      # re-order
      intake_v4_de_merged <- intake_v4_de_merged[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_v4_de_merged)[!grepl('_id|visit|advertisement_condition', names(intake_v4_de_merged))])]

      # remove rows with all NA
      intake_v4_de_merged <- intake_v4_de_merged[!(rowSums(is.na(intake_v4_de_merged)) >= ncol(intake_v4_de_merged) - 4), ]

      # combine with exisitng merged data
      intake_v4 <- rbind.data.frame(intake_v4, intake_v4_de_merged)
      intake_v4 <- intake_v4[order(intake_v4[['participant_id']]), ]
    }
  }

  ## intake data - visit 5 ####
  intake_v5 <- checked_data[grepl('_id|plate_v5', names(checked_data))]

  # remove extra columns and re-order
  intake_v5 <- intake_v5[!grepl('dxa_id', names(intake_v5))]

  intake_v5['session_id'] <- 'ses-2'
  intake_v5['visit_protocol'] <- 5

  #fix names
  names(intake_v5) <- gsub('_v5', '', names(intake_v5))
  intake_v5['advertisement_condition'] <- NA

  # merge with date data for v5
  intake_v5 <- merge(intake_v5, date_data[c('participant_id', 'v5_date')], by = 'participant_id', all.x = TRUE)
  names(intake_v5)[names(intake_v5) == 'v5_date'] <- 'visit_date'
  intake_v5['visit_date'] <- lubridate::as_date(intake_v5[['visit_date']])

  # re-order
  intake_v5 <- intake_v5[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_v5)[!grepl('_id|visit|advertisement_condition', names(intake_v5))])]


  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {

    intake_v5_unmerged <- unmerged_data[grepl('_id|plate_v5', names(unmerged_data))]

    # remove extra columns and re-order
    intake_v5_unmerged <- intake_v5_unmerged[!grepl('dxa_id', names(intake_v5_unmerged))]

    intake_v5_unmerged['session_id'] <- 'ses-2'
    intake_v5_unmerged['visit_protocol'] <- 5

    #fix names
    names(intake_v5_unmerged) <- gsub('_v5', '', names(intake_v5_unmerged))
    intake_v5_unmerged['advertisement_condition'] <- NA

    # check unmerged values
    intake_v5_de_list <- util_de_check(intake_v5_unmerged)

    if (is.data.frame(intake_v5_de_list$merged_de_data)) {
      # get date info for newly merged data
      intake_v5_de_merged <- merge(intake_v5_de_list$merged_de_data, date_data[c('participant_id', 'v5_date')], by = 'participant_id', all.x = TRUE)
      names(intake_v5_de_merged)[names(intake_v5_de_merged) == 'v5_date'] <- 'visit_date'
      intake_v5_de_merged['visit_date'] <- lubridate::as_date(intake_v5_de_merged[['visit_date']])

      # re-order
      intake_v5_de_merged <- intake_v5_de_merged[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_v5_de_merged)[!grepl('_id|visit|advertisement_condition', names(intake_v5_de_merged))])]

      # remove rows with all NA
      intake_v5_de_merged <- intake_v5_de_merged[!(rowSums(is.na(intake_v5_de_merged)) >= ncol(intake_v5_de_merged) - 4), ]

      # combine with exisitng merged data
      intake_v5 <- rbind.data.frame(intake_v5, intake_v5_de_merged)
      intake_v5 <- intake_v5[order(intake_v5[['participant_id']]), ]
    }
  }

  intake_v3v4v5_json = json_v3v4v5_intake()

  return(list(dxa_v1 = list(data = dxa_v1, meta = dxa_json),
              dxa_v5 = list(data = dxa_v5, meta = dxa_json),
              intake_v1 = list(data = intake_v1, meta = intake_v1_json),
              intake_v3 = list(data = intake_v3, meta = intake_v3v4v5_json),
              intake_v4 = list(data = intake_v4, meta = intake_v3v4v5_json),
              intake_v5 = list(data = intake_v5, meta = intake_v3v4v5_json)
  ))

}

