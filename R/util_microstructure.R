#' util_microstructure: Process raw microstructure coding data
#'
#' This function loads the .txt raw data files from ObserverXT. Cleaning the data involves:
#' \itemize{
#'  \item{ 1) separating event log names to get visit, participant, and coder information}
#' \item{ 2) selecting relevant data columns}
#' \item{ 3) removing all events that are not needed/old/duplicate}
#' \item{ 4) re-ordering and re-name data columns}
#' \item{ 5) making wide behavioral data for summary metrics (nbites, bite rate)}
#' \item{ 6) separating event logs by coder and padding for equal entrees - need to fix mismatched number of bites/sips in a different step}
#' \item{ 7) creating variable labels, levels, and dictionaries}
#' }
#'
#'
#' @inheritParams base_wd
#' @inheritParams overwrite
#'
#'
#' @return A list containing 2 data lists:
#' 1) beh_wide contains - data: data.frame summary metrics in wide format by code, dict: data dictionary with variable descriptions
#' 2) event contains - data: event data by coder and time, dict: data dictionary with variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ps1_microstructure <- util_microstructure(base_wd_path)
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
util_microstructure <- function(base_wd, file_str, overwrite = FALSE) {

  #### 1. Set up/initial checks #####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  # check that base_wd exist and is a string
  file_arg <- methods::hasArg(file_str)

  if (isTRUE(file_arg)) {
    if (!is.character(file_str)) {
      stop('file_str must be entered as a string')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  # 2. load and process data file ####
  micro_raw_dir <- file.path(base_wd, 'raw')

  micro_data <- read.table(file_str, sep = ',', fileEncoding = 'utf-16le', header = TRUE)
  micro_data <- micro_data[!grepl('X', names(micro_data))]

  #fix naming
  names(micro_data) <- tolower(names(micro_data))

  names(micro_data)[names(micro_data) == 'date_time_absolute_dmy_hmsf'] <- 'date_time'
  names(micro_data)[names(micro_data) == 'date_dmy'] <- 'date'
  names(micro_data)[names(micro_data) == 'time_absolute_hms'] <- 'time'
  names(micro_data)[names(micro_data) == 'time_absolute_f'] <- 'time_frames'
  names(micro_data)[names(micro_data) == 'time_relative_hmsf'] <- 'time_hmsf'
  names(micro_data)[names(micro_data) == 'time_relative_hms'] <- 'time_hms'
  names(micro_data)[names(micro_data) == 'time_relative_f'] <- 'time_relative_frames'
  names(micro_data)[names(micro_data) == 'time_relative_sf'] <- 'time_relative'
  names(micro_data)[names(micro_data) == 'duration_sf'] <- 'duration'

  names(micro_data)[names(micro_data) == 'modifier_1'] <- 'grapes'
  names(micro_data)[names(micro_data) == 'modifier_2'] <- 'carrots'
  names(micro_data)[names(micro_data) == 'modifier_3'] <- 'mac'
  names(micro_data)[names(micro_data) == 'modifier_4'] <- 'graham'
  names(micro_data)[names(micro_data) == 'modifier_5'] <- 'broccoli'
  names(micro_data)[names(micro_data) == 'modifier_6'] <- 'hand'
  names(micro_data)[names(micro_data) == 'modifier_7'] <- 'fork'

  names(micro_data) <- gsub('\\.', '_', names(micro_data))

  # get participant ID
  micro_data['observation'] <- gsub('-', '_', micro_data[['observation']])
  micro_data['observation'] <- gsub('no_preload|no_prelooad', 'no-preload', micro_data[['observation']])
  micro_data['observation'] <- gsub('sub_050\037', 'sub_050', micro_data[['observation']])

  sub_str <- data.frame(t(rbind.data.frame(strsplit(micro_data[['observation']], '_'))))
  names(sub_str) <- c('sub_str', 'id', 'meal_str', 'preload_condition', 'coder')


  micro_data['participant_id'] <- paste0(sub_str[['sub_str']], '-', sub_str[['id']])
  micro_data['preload_condition'] <- sub_str['preload_condition']
  micro_data['coder'] <- sub_str['coder']

  # get n coders
  micro_data['n_coders'] <- sapply(micro_data[['participant_id']], function(x) length(unique(micro_data[micro_data['participant_id'] == x, 'observation'])))

  coder_info <- function(id){
    data <- micro_data[micro_data['participant_id'] == id, ]

    obs_unique <- unique(data['observation'])

    if(nrow(obs_unique) == 1) {
      return(rep(1, nrow(data)))
    } else {

      coder_order <- c(rep(1, nrow(data[data['observation'] == obs_unique[1, ], ])), rep(2, nrow(data[data['observation'] == obs_unique[2, ], ])))

      return(coder_order)
    }
  }

  micro_data['coder_order'] <- unlist(sapply(unique(micro_data[['participant_id']]), function(x) coder_info(x), USE.NAMES = FALSE))

  ## concatenate foods
  micro_data['graham'] <- gsub('graham crackers', 'graham', micro_data[['graham']])
  micro_data['mac'] <- gsub('mac and cheese', 'mac', micro_data[['mac']])

  micro_data['meal_food'] <- paste0(micro_data[['grapes']], micro_data[['carrots']], micro_data[['mac']], micro_data[['graham']], micro_data[['broccoli']])

  micro_data['meal_food'] <- gsub('macgraham', 'mac_graham', micro_data[['meal_food']])
  micro_data['meal_food'] <- gsub('macbroccoli', 'mac_broccoli', micro_data[['meal_food']])
  micro_data['meal_food'] <- gsub('grapemac', 'grape_mac', micro_data[['meal_food']])

  hed_foods <- c('mac', 'graham')
  led_foods <- c('grape', 'carrot', 'broccoli')

  # food ed
  micro_data[['meal_food_ed']] <- ifelse(is.na(micro_data[['meal_food']]), NA, ifelse(micro_data[['meal_food']] %in% hed_foods, 'h_ed', ifelse(micro_data[['meal_food']] %in% led_foods, 'l_ed', ifelse(grepl('mac', micro_data[['meal_food']]) & grepl('graham', micro_data[['meal_food']]), 'h_ed', ifelse(grepl('grape', micro_data[['meal_food']]) & grepl('carrot', micro_data[['meal_food']]), 'l_ed', ifelse(grepl('grape', micro_data[['meal_food']]) & grepl('broccoli', micro_data[['meal_food']]), 'l_ed', ifelse(grepl('carrot', micro_data[['meal_food']]) & grepl('broccoli', micro_data[['meal_food']]), 'l_ed', 'mixed_ed')))))))

  # food/sip
  micro_data[['meal_food_sip']] <- ifelse(is.na(micro_data[['meal_food']]) | micro_data[['meal_food']] == '', ifelse(micro_data[['behavior']] == 'Sips', 'sip', micro_data[['meal_food']]), micro_data[['meal_food']])

  # 3. process/save in raw data for each participant ####

  # add if statement to check for file/overwrite option
  raw_save <- function(id, micro_raw_dir){
    data <- micro_data[micro_data['participant_id'] == id, ]

    write.csv(data, file = file.path(micro_raw_dir, id, paste0(id, '_micro-events.csv')))

    return('raw saved')
  }

  save_msg <- sapply(unique(micro_data[['participant_id']]), function(x) raw_save(x, micro_raw_dir))

  # 4. make wide ####

  wide_dat <- function(id){
    data <- micro_data[micro_data['participant_id'] == id, ]

    obs_unique <- unique(data['observation'])
    preload <- data[1, 'preload_condition']

    if(nrow(obs_unique) == 1) {
      meal_dur <- data[data[['behavior']] == 'Meal Duration', 'duration']
      latency <- data[data[['behavior']] == 'Latency to First Bite', 'duration']
      nbites <- nrow(data)
      nsips <- nrow(data[data[['behavior']] == 'Sips' & !is.na(data[['time_relative']]), ])

      wide_data <- data.frame(c(id, preload, nbites, NA, nsips, NA, meal_dur[1], NA, latency[1], NA))

    } else {

      meal_dur1 <- data[data[['behavior']] == 'Meal Duration' & data['observation'] == obs_unique[1, ], 'duration']
      meal_dur2 <- data[data[['behavior']] == 'Meal Duration' & data['observation'] == obs_unique[2, ], 'duration']

      latency1 <- data[data[['behavior']] == 'Latency to First Bite' & data['observation'] == obs_unique[1, ], 'duration']
      latency2 <- data[data[['behavior']] == 'Latency to First Bite' & data['observation'] == obs_unique[2, ], 'duration']

      nbites1 <- nrow(data[data['observation'] == obs_unique[1, ], ])
      nbites2 <- nrow(data[data['observation'] == obs_unique[2, ], ])

      nsips1 <- nrow(data[data[['behavior']] == 'Sips' & !is.na(data[['time_relative']]) & data['observation'] == obs_unique[1, ], ])
      nsips2 <- nrow(data[data[['behavior']] == 'Sips' & !is.na(data[['time_relative']]) & data['observation'] == obs_unique[2, ], ])

      wide_data <- data.frame(c(id, preload, nbites1, nbites2, nsips1, nsips2, meal_dur1[1], meal_dur1[1], latency1[1], latency2[1]))
    }

    return(wide_data)
  }

  beh_wide_data <- data.frame(t(rbind.data.frame(sapply(unique(micro_data[['participant_id']]), function(x) wide_dat(x)))))
  names(beh_wide_data) <- c('participant_id', 'preload_condition', 'nbites_c1', 'nbites_c2', 'nsips_c1', 'nsips_c2', 'meal_dur_c1', 'meal_dur_c2', 'bite_latency_c1', 'bite_latency_c2')
  rownames(beh_wide_data) <- NULL

  # make numeric
  beh_wide_data[!grepl('id|preload', names(beh_wide_data))] <- sapply(beh_wide_data[!grepl('id|preload', names(beh_wide_data))], function(x) as.numeric(x))

  #convert to minutes
  beh_wide_data['meal_dur_c1'] <- beh_wide_data[['meal_dur_c1']]/60
  beh_wide_data['meal_dur_c2'] <- beh_wide_data[['meal_dur_c2']]/60

  #generate other data by coder
  beh_wide_data[['bite_rate_c1']] <- beh_wide_data[['nbites_c1']]/beh_wide_data[['meal_dur_c1']]
  beh_wide_data[['bite_rate_c2']] <- beh_wide_data[['nbites_c2']]/beh_wide_data[['meal_dur_c2']]

  # beh_wide_data[['bite_rate_active_c1']] <- beh_wide_data[['nbites_c1']]/beh_wide_data[['total_active_eating_c1']]
  # beh_wide_data[['bite_rate_active_c2']] <- beh_wide_data[['nbites_c2']]/beh_wide_data[['total_active_eating_c2']]

  beh_wide_data[['sip_rate_c1']] <- beh_wide_data[['nsips_c1']]/beh_wide_data[['meal_dur_c1']]
  beh_wide_data[['sip_rate_c2']] <- beh_wide_data[['nsips_c2']]/beh_wide_data[['meal_dur_c2']]

  # beh_wide_data[['sip_rate_active_c1']] <- beh_wide_data[['nsips_c1']]/beh_wide_data[['total_active_eating_c1']]
  # beh_wide_data[['sip_rate_active_c2']] <- beh_wide_data[['nsips_c2']]/beh_wide_data[['total_active_eating_c2']]

  #clean up order
  beh_wide_data <- beh_wide_data[c('participant_id', 'preload_condition', names(beh_wide_data)[grepl('c1', names(beh_wide_data))], names(beh_wide_data)[grepl('c2', names(beh_wide_data))])]


  ## 5 - Event Data by Coder ####
  micro_data_event <- micro_data[micro_data[['behavior']] == 'Bite' | micro_data[['behavior']] == 'Sips', ]

  # get coder datasets
  micro_dat_event_c1 <- micro_data_event[micro_data_event[['coder_order']] == 1, ]
  micro_dat_event_c2 <- micro_data_event[micro_data_event[['coder_order']] == 2, ]

  #get event number by id
  micro_dat_event_c1[['event_num']] <- unlist(sapply(unique(micro_dat_event_c1[['participant_id']]), function(x) seq(1, nrow(micro_dat_event_c1[micro_dat_event_c1[['participant_id']] == x, ]), 1), USE.NAMES = FALSE))

  micro_dat_event_c2[['event_num']] <- unlist(sapply(unique(micro_dat_event_c2[['participant_id']]), function(x) seq(1, nrow(micro_dat_event_c2[micro_dat_event_c2[['participant_id']] == x, ]), 1), USE.NAMES = FALSE))

  #code switches
  switch_fn <- function(food, event){
    if (event == 1 | is.na(food[[event]])) {
      switch <- NA
    } else if (food[[event]] != food[[event - 1]]){
      switch <- 1
    } else {
      switch <- 0
    }
  }

  switch_wrapper_food <- function(id, food_var, dat){
    id_dat <- dat[dat[['participant_id']] == id, ]

    food_list <- id_dat[[food_var]]

    switch <- sapply(id_dat[['event_num']], function(x) switch_fn(food_list, x))
  }

  # foods
  micro_dat_event_c1[['switch']] <- unlist(sapply(unique(micro_dat_event_c1[['participant_id']]), function(x) switch_wrapper_food(x, 'meal_food', micro_dat_event_c1), USE.NAMES = FALSE))

  micro_dat_event_c2[['switch']] <- unlist(sapply(unique(micro_dat_event_c2[['participant_id']]), function(x) switch_wrapper_food(x, 'meal_food', micro_dat_event_c2), USE.NAMES = FALSE))

  # foods ed cat
  micro_dat_event_c1[['switch_ed']] <- unlist(sapply(unique(micro_dat_event_c1[['participant_id']]), function(x) switch_wrapper_food(x, 'meal_food_ed', micro_dat_event_c1), USE.NAMES = FALSE))

  micro_dat_event_c2[['switch_ed']] <- unlist(sapply(unique(micro_dat_event_c2[['participant_id']]), function(x) switch_wrapper_food(x, 'meal_food_ed', micro_dat_event_c2), USE.NAMES = FALSE))

  # bite/sip
  micro_dat_event_c1[['switch_wsip']] <- unlist(sapply(unique(micro_dat_event_c1[['participant_id']]), function(x) switch_wrapper_food(x, 'meal_food_sip', micro_dat_event_c1), USE.NAMES = FALSE))

  micro_dat_event_c2[['switch_wsip']] <- unlist(sapply(unique(micro_dat_event_c2[['participant_id']]), function(x) switch_wrapper_food(x, 'meal_food_sip', micro_dat_event_c2), USE.NAMES = FALSE))

  #merge - figure out -- need to pad w/NAs
  # micro_dat_event_wide <- merge(micro_dat_event_c1[c(1:2, 4:20)], micro_dat_event_c2[c(1, 4:20)], by = c('id', 'event_num'), all = TRUE)
  #
  # #organize
  # micro_dat_event_wide <- micro_dat_event_wide[c(1, 3:5, 20:21, 2, 6:9, 17:19, 22:25, 33:35, 11:16, 27:32)]

  ## 6. Add switches to behavior summary output ####
  beh_wide_data[c('nswitch_c1', 'nswitch_ed_c1', 'nswitch_wsip_c1')]  <- t(sapply(unique(micro_dat_event_c1[['participant_id']]), function(x) colSums(micro_dat_event_c1[micro_dat_event_c1[['participant_id']] == x, c('switch', 'switch_ed', 'switch_wsip')], na.rm = TRUE), USE.NAMES = FALSE))

  beh_wide_data[!is.na(beh_wide_data[['nbites_c2']]), c('nswitch_c2', 'nswitch_ed_c2', 'nswitch_wsip_c2')]  <- t(sapply(unique(micro_dat_event_c2[['participant_id']]), function(x) colSums(micro_dat_event_c2[micro_dat_event_c2[['participant_id']] == x, c('switch', 'switch_ed', 'switch_wsip')], na.rm = TRUE), USE.NAMES = FALSE))

  #clean up order
  beh_wide_data <- beh_wide_data[c('participant_id', 'preload_condition', names(beh_wide_data)[grepl('c1', names(beh_wide_data))], names(beh_wide_data)[grepl('c2', names(beh_wide_data))])]


  ## make list of data frame and associated labels
  meal_micro <- list(
    beh_wide_data = list(data = beh_wide_data, meta = NA),
    event_data = list(data = micro_dat_event_c1, meta = NA))

  ## want an export options??

  return(meal_micro)
}
