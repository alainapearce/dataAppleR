#' proc_tasks: Process task data from untouchedRaw to create bids compliant files
#'
#' This function: \itemize{
#' \item{1) copies task data from untouchedRaw into bids/sourcedata for all tasks (food view, pit, sst, nih toolbox, spacegame), using util_task_untouched_to_source(all_tasks = TRUE)}
#' \item{2) processes task sourcedata and exports cleaned dataframes into bids/rawdata for the following tasks: rrv, sst, foodview nih-toolbox, pit, spacegame, using task-specific util_task_{task-name} functions}
#' \item{3) exports JSON meta-data files for tasks organized into rawdata (rrv, sst, foodview), using write_task_jsons()}
#' }
#'
#' @param base_wd (string) full path to directory that contains both the untouchedRaw and bids directories
#' @param overwrite (logical) data should be overwritten. Default = FALSE
#' @param all_tasks (logical) all tasks should be moved to sourcedata. Default = FALSE. If all_tasks = FALSE, user must specify tasks to process in task_vector
#' @param task_vector (vector) tasks to process. Must be included if all_tasks = FALSE. Options include: c('sst', 'foodview', 'spacegame', 'nih_toolbox', 'rrv', 'pit') (optional)
#' @param overwrite_jsons (logical) overwrite existing jsons in rawdata. Default = FALSE
#'
#' @examples
#'
#' \dontrun{
#' # organize task data for all tasks in untouchedRaw into sourcedata and rawdata
#' proc_task(base_wd = base_wd, all_tasks = TRUE)
#'
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata and raw data
#' proc_task(base_wd = base_wd, task_vector = c('spacegame', 'nih_toolbox'))
#'
#' }
#'
#'
#' @export

proc_tasks <- function(base_wd, overwrite = FALSE, all_tasks = FALSE, task_vector, overwrite_jsons = FALSE) {

  #### Set up/initial checks ####

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

  # check that task options correctly specified
  task_vector_arg <- methods::hasArg(task_vector)

  if (isFALSE(all_tasks) & isFALSE(task_vector_arg)) {
    stop('Tasks to process not specified: Specify tasks by setting all_tasks = TRUE or providing vector to task_vector parameter')
  }

  if (isTRUE(all_tasks) & isTRUE(task_vector_arg)) {
    stop('all_tasks = TRUE and task_vector parameter were provided. Use only 1 of these options')
  }

  if (isTRUE(task_vector_arg)) {
    if (!is.vector(task_vector)) {
      stop('Input to task_vector must be vector (e.g., task_vector = c("rrv"")')
    } else {
      for (task in task_vector) {
        if (!task %in% c('sst','foodview','spacegame','nih_toolbox','rrv','pit')) {
          stop(paste(task, 'is not an option for task_vector'))
        }
      }
    }
  } else {
    task_vector = c()
  }


  #### FoodView task ####

  if (isTRUE(all_tasks) | 'foodview' %in% task_vector) {
    print('-- copying Food View')

    foodview_dir <- file.path(base_wd, 'untouchedRaw', 'foodview_task')

    # get list of available subjects
    foodview_list <- list.files(foodview_dir, pattern = '.txt')
    foodview_list <- as.data.frame(foodview_list[!(grepl('onsets', foodview_list))])
    names(foodview_list) <- 'filename'

    #get list of subject IDs
    foodview_list[['id']] <- as.numeric(sapply(foodview_list[['filename']], function(x) substr(x, unlist(gregexpr('-', x))+1, unlist(gregexpr('.txt', x))-1), simplify = TRUE))

    foodview_list[['sub_str']] <- sapply(foodview_list[['id']], function(x) sprintf('sub-%03d', x), simplify = TRUE)

    #organize data into BIDS sourcedata
    foodview_list[['sourcedata_done']] <- sapply(foodview_list[['id']], function(x) util_copy_to_source(task_dir = foodview_dir, task_str = 'foodview', sub_id = x, ses_str = 'ses-1', overwrite = overwrite), simplify = TRUE)

    #process raw data
    foodview_list[['rawproc_done']] <- sapply(foodview_list[['sub_str']], function(x) util_task_foodview(sub_str = x, ses_str = 'ses-1', bids_wd = file.path(base_wd, 'bids'), overwrite = overwrite), simplify = TRUE)

  }


  #### Stop Signal Task ####
  if (isTRUE(all_tasks) | 'sst' %in% task_vector) {
    print('-- copying SST')

    sst_dir <- file.path(base_wd, 'untouchedRaw', 'sst')

    # get list of available subjects
    sst_list <- list.files(sst_dir, pattern = '.txt')
    sst_list <- as.data.frame(sst_list[(grepl('prac', sst_list))])
    names(sst_list) <- 'filename'

    #get list of subject IDs
    sst_list[['id']] <- sapply(sst_list[['filename']], function(x) ifelse(grepl('37_1st', x), '37-1', substr(x, unlist(gregexpr('-', x))+1, unlist(gregexpr('.txt', x))-1)), simplify = TRUE)

    sst_list[['sub_str']] <- sapply(sst_list[['id']], function(x) ifelse(grepl('37-1', x), 'sub-037-1', ifelse(grepl('37', x), 'sub-037-2', sprintf('sub-%03d', as.numeric(x)))), simplify = TRUE)

    sst_list[sst_list['id'] == '37-1', 'id'] <- '37'

    sst_list['id'] <- as.numeric(sst_list[['id']])

    #organize data into BIDS sourcedata
    sst_list[['sourcedata_done']] <- sapply(sst_list[['id']], function(x) util_copy_to_source(task_dir = sst_dir, task_str = 'stop', sub_id = x, ses_str = 'ses-1', overwrite = overwrite), simplify = TRUE)

    #process raw data
    sst_list[['rawproc_done']] <- sapply(sst_list[['sub_str']], function(x) util_task_sst(sub_str = x, ses_str = 'ses-1', bids_wd = file.path(base_wd, 'bids'), overwrite = overwrite), simplify = TRUE)

  }


  #### Space Game ####
  if (isTRUE(all_tasks) | 'spacegame' %in% task_vector) {
    print('-- copying Space Game')

    # Get list of subs with spacegame files in untouchedRaw based on filenames
    space_dir <- file.path(base_wd, 'untouchedRaw', 'space_game') # set spacegame dir

    # get list of available subjects
    space_list <- as.data.frame(list.files(space_dir, pattern = 'mbmfNovelStakes', full.names = FALSE))
    names(space_list) <- 'filename'

    #get list of subject IDs
    space_list[['id']] <- sapply(space_list[['filename']], function(x) ifelse(grepl('026cancelled', x), '26-1', ifelse(grepl('059-01', x), '059', ifelse(grepl('069-09', x), '069', ifelse(grepl('117-09', x), '117', substr(x, unlist(gregexpr('_', x))[1]+1, unlist(gregexpr('-', x))[1] -1))))), simplify = TRUE)

    space_list[['sub_str']] <- sapply(space_list[['id']], function(x) ifelse(grepl('26-1', x), 'sub-026-1', paste0('sub-', x)), simplify = TRUE)

    space_list[space_list['id'] == '26-1', 'id'] <- '26'

    space_list['id'] <- as.numeric(space_list[['id']])

    #organize data into BIDS sourcedata
    space_list[['sourcedata_done']] <- sapply(space_list[['id']], function(x) util_copy_to_source(task_dir = space_dir, task_str = 'mbmfNovelStakes', sub_id = x, ses_str = 'ses-1', overwrite = overwrite), simplify = TRUE)

    #process raw data
    #foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'basel ine', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)

  }

  #### NIH toolbox ####
  if (isTRUE(all_tasks) | 'nih_toolbox' %in% task_vector) {
    print('-- copying NIH toolbox')

    nih_list <- list()
    # for each session
    if (length(ses_str) == 2){
      file_paths <- c(file.path(base_wd, 'untouchedRaw', 'nih-toolbox', 'V1'), file.path(base_wd, 'untouchedRaw', 'nih-toolbox', 'V5'))
      nih_list <- as.data.frame(unlist(sapply(file_paths, function(x) list.dirs(x, recursive = FALSE, full.names = FALSE), simplify = TRUE, USE.NAMES = FALSE)))
      names(nih_list) <- 'id_folder'

      nih_list[['ses']] <- c(rep('ses-1', length(list.dirs(file_paths[1], recursive = FALSE, full.names = FALSE))), rep('ses-2', length(list.dirs(file_paths[2], recursive = FALSE, full.names = FALSE))))

    } else {
      # define directory with data
      if (ses_str == 'ses-1') {
        nih_dir <- file.path(base_wd, 'untouchedRaw', 'nih-toolbox', 'V1')
      } else {
        nih_dir <- file.path(base_wd, 'untouchedRaw', 'nih-toolbox', 'V5')
      }

      # get list of subject directories
      nih_list <- as.data.frame(list.dirs(nih_dir, recursive = FALSE, full.names = FALSE))
      names(nih_list) <- 'id_folder'

      nih_list[['ses']] <- ses_str
    }

    nih_list[['ses_folder']] <- ifelse(nih_list[['ses']] == 'ses-1', 'V1', 'V5')

    #get list of subject IDs
    nih_list[['id']] <- as.numeric(sapply(nih_list[['id_folder']], function(x) substr(x, unlist(gregexpr('_', x))+1, nchar(x)), simplify = TRUE))

    nih_list[['sub_str']] <- sapply(nih_list[['id']], function(x) sprintf('sub-%03d', as.numeric(x)), simplify = TRUE)

    #organize data into BIDS sourcedata
    nih_list[['sourcedata_done']] <- mapply(util_copy_to_source, sub_id = nih_list[['id']], task_dir = file.path(base_wd, 'untouchedRaw', 'nih-toolbox', nih_list[['ses_folder']], nih_list[['id_folder']]), ses_str = nih_list[['ses']], MoreArgs = list(task_str = 'nih', overwrite = overwrite))

    #process raw data
    nih_list[['rawproc_done']] <- mapply(util_task_nihtoolbox, sub_str = nih_list[['sub_str']], ses_str = nih_list[['ses']], MoreArgs = list(bids_wd = file.path(base_wd, 'bids'), overwrite = overwrite))

    # generate derivatives file
    phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
    raw_beh_wd <- file.path(base_wd, 'bids', 'rawdata')

    #create derivative file
    if (!dir.exists(phenotype_wd)) {
      dir.create(phenotype_wd, recursive = TRUE)
    }

    nih_events_dat <- do.call('rbind', mapply(read.table, file = file.path(raw_beh_wd, nih_list[['sub_str']], nih_list[['ses']], 'beh', paste0(nih_list[['sub_str']], '_', nih_list[['ses']], '_task-nih_toolbox_events.tsv')), MoreArgs = list(sep = '\t', header = TRUE), SIMPLIFY = FALSE))

    write.table(nih_events_dat, file.path(phenotype_wd, 'nih_toolbox_events.tsv'), sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )

    nih_scores_dat <- do.call('rbind', mapply(read.table, file = file.path(raw_beh_wd, nih_list[['sub_str']], nih_list[['ses']], 'beh', paste0(nih_list[['sub_str']], '_', nih_list[['ses']], '_task-nih_toolbox_scores.tsv')), MoreArgs = list(sep = '\t', header = TRUE), SIMPLIFY = FALSE))

    write.table(nih_scores_dat, file.path(phenotype_wd, 'nih_toolbox_scores.tsv'), sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )

  }

  #### RRV ####
  if (isTRUE(all_tasks) | 'rrv' %in% task_vector) {

    print('-- copying RRV')

    rrv_dir <- file.path(base_wd, 'untouchedRaw', 'rrv_task')

    # get list of subject directories
    rrv_list <- as.data.frame(list.dirs(rrv_dir, recursive = FALSE, full.names = FALSE))
    names(rrv_list) <- 'id_folder'

    #get list of subject IDs
    rrv_list[['id']] <- as.numeric(sapply(rrv_list[['id_folder']], function(x) substr(x, unlist(gregexpr('_', x))+1, nchar(x)), simplify = TRUE))

    rrv_list[['sub_str']] <- sapply(rrv_list[['id']], function(x) sprintf('sub-%03d', as.numeric(x)), simplify = TRUE)

    #organize data into BIDS sourcedata
    rrv_list[['sourcedata_done']] <- mapply(util_copy_to_source, sub_id = rrv_list[['id']], task_dir = file.path(rrv_dir, rrv_list[['id_folder']]), MoreArgs = list(task_str = 'rrv', ses_str = 'ses-1', overwrite = overwrite))

    #process raw data
    #foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'basel ine', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
  }


  #### PIT ####

  if (isTRUE(all_tasks) | 'pit' %in% task_vector) {
    print('-- copying PIT task')

    # for each session
    for (ses_str in c('ses-1', 'ses-2')) {

      # define directory with pit data
      if (ses_str == 'ses-1') {
        pit_dir <- file.path(base_wd, 'untouchedRaw', 'pit_task', 'V4_PIT')
      } else {
        pit_dir <- file.path(base_wd, 'untouchedRaw', 'pit_task', 'V5_PIT')
      }

      # get list of available subjects
      pit_list <- as.data.frame(list.files(pit_dir, pattern = '.psydat'))
      names(pit_list) <- 'filename'

      #get list of subject IDs
      pit_list[['id']] <- as.numeric(sapply(pit_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))[1]-1), simplify = TRUE))

      pit_list[['sub_str']] <- sapply(pit_list[['id']], function(x) sprintf('sub-%03d', x), simplify = TRUE)
      #organize data into BIDS sourcedata
      pit_list[['sourcedata_done']] <- sapply(pit_list[['id']], function(x) util_copy_to_source(task_dir = pit_dir, task_str = 'pit', sub_id = x, ses_str = ses_str, overwrite = overwrite), simplify = TRUE)

      #process raw data
      #foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)

    }
  }
}
