


#' Create folders to contain batch and process files
#'
#' @param output_dir Path to output/project folder
#'
#' @return None
#' @export
#'
#' @examples start_batchman(output_dir = './')
start_batchman <- function(output_dir = './'){
  dir.create(paste0(output_dir, 'batches'), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(output_dir, 'processes'), showWarnings = FALSE, recursive = TRUE)
}




#' Creates Registry (csv file) for running a process on a batch of files
#'
#' @param output_dir the path to contain the process registry file.
#' @param source_files the list of files to be processed.
#'
#' @return the path to the process registry file.
#' @export
#'
#' @examples create_registry('./data/L2/demo/', './data/L1/subject_01/response.csv')
create_registry <- function(output_dir, source_files){

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  registry_filename <- paste0(output_dir, '/registry.csv')
  registry <- tibble::tibble(source_file = source_files,
                             status = 'todo',
                             session = NA,
                             start_date = NA,
                             duration = NA)

  readr::write_csv(x = registry,
            file = registry_filename)

  invisible(registry_filename)

}


#' Updates the process registry file
#'
#' @param registry_filename path to the process registry file.
#' @param source_filenames vector of the filenames to be processed.
#' @param keys  keys of the process registry table to be updated.
#' @param values corresponding values to be used for the update.
#'
#' @return None
#' @export
#'
#' @examples update_registry(..., keys = 'status', values = 'done')
update_registry <- function(registry_filename, source_filenames, keys, values){

  # load data
  registry <- read.csv(registry_filename)

  # update data
  for (i in 1:length(keys)) {
    registry[registry$source_file %in% source_filenames, keys[i]] <- values[i]
  }

  # save data
  readr::write_csv(registry, registry_filename)

}




#' Select files to processes next from the process registry file.
#'
#' @param registry_filename path to the process registry file.
#' @param batch_size number of files to be processed at once when calling the execution function.
#'
#' @return vector of filenames
#' @export
#'
#' @examples select_process(registry_filename, batch_size = 3)
select_process <- function(registry_filename, batch_size = 3){

  # read process registry
  registry <- read.csv(registry_filename)

  # pick one case to fit
  registry |>
    filter(status == 'todo') |>
    head(batch_size) |>
    pull(source_file)

}






#' Wrapper function to track and monitor the execution of a process
#'
#' @param filename path to file to be processed.
#' @param process_name name of the function that does the processing.
#' @param output_dir  path to file that will store the results.
#'
#' @return NA
#' @export
#'
#' @examples process_wrapper(filename='./subject_01/response.csv', process_name='process_q1', output_dir = './subject_01/results/')
process_wrapper <- function(filename, process_name, output_dir) {

  # load file
  input_data <- read.csv(filename)

  # start timer
  start_date <- now()

  # run process
  df <- eval(parse(text = paste0(process_name, ".process(input_data)")))

  # measure duration
  duration <- as.numeric(difftime(now(), start_date, units = "secs"))

  # did it complete correctly?

  # save output to file
  readr::write_csv(x = df, file = paste0(output_dir, '/output.csv'), append = TRUE)

  # update registry (set status to "done")
  registry_filename <- paste0(output_dir, '/registry.csv')
  update_registry(registry_filename,
                  filename,
                  keys = c("status","start_date", "duration"),
                  values = c("done", as.character(start_date), as.character(duration)))

}




#' Run a batch of processes
#'
#' @param batch_filename Path to the batch of processes to be executed.
#' @param batch_size Number of files to be processed per call (default=100).
#'
#' @return None
#' @export
#'
#' @examples run_batch("demo.yaml", 1000)
run_batch <- function(batch_filename, batch_size = 100) {

  # read process flow definition file
  dfy <- yaml::read_yaml(batch_filename)
  process_name <- dfy$process

  # source the process
  source(file = paste0('./processes/', process_name, '.R'),
         verbose = FALSE)

  output_dir <- paste0(dfy$output_dir, dfy$name, '/')
  registry_filename <- paste0(output_dir, 'registry.csv')


  # --- orchestration:
  # 1) select cases to be processed
  cases_to_process <- select_process(registry_filename,
                                     batch_size)

  # 2) if there are cases to fit...
  while (length(cases_to_process) > 0) {

    # save session info (packages, software version, etc.)
    session_id <- save_session_info(output_dir)

    # tag cases_to_process as having status "doing"; preventing other cores to run same code
    update_registry(registry_filename,
                            cases_to_process,
                            keys = c("status","session"),
                            values = c("doing", session_id))

    # TODO: parallel loops
    for (case in cases_to_process) {
      process_wrapper(filename = case,
                      process_name = process_name,
                      output_dir = output_dir)


    }

    # select cases to be processed for next iteration
    cases_to_process <- select_process(registry_filename,
                                       batch_size)

  }
}





#' Initialize Batch; creates or updates registry of processes
#'
#' @param batch_filename Name of the yaml batch file to be initialized.
#' @param update_mode Indicates the behavior to adopt if a batch was already initiated in the past.
#'
#' @return logical indicating if all processes within batch have been executed.
#' @export
#'
#' @examples initialize_batch("demo.yaml", "update")
initialize_batch <- function(batch_filename, update_mode = 'update'){

  # --- setting up registry and folder structure ---
  dfy <- yaml::read_yaml(batch_filename)
  query <- stringr::str_replace(dfy$source, "dataset", paste0('"', dfy$dataset, '"'))
  # TODO: more generally parse dfy to replace vars in source expression

  # TODO: make sure process.R exists.

  # run query to get list of files
  source_filenames <- eval(parse(text = query))

  # check if registry already exists
  output_dir <- paste0(dfy$output_dir, dfy$name, '/')
  registry_filename <- paste0(output_dir, 'registry.csv')


  if (!file.exists(registry_filename)) {

    # create empty process registry
    tmp <- create_registry(output_dir, source_filenames)
    process_completed <- FALSE

  } else {

    if (update_mode == 'update') {

      # check if its up to date (i.e., contains all source files)
      # registry <- read_csv(registry_filename, show_col_types = FALSE)
      registry <- read.csv(registry_filename)

      new_registry <- data.frame(source_file = source_filenames)

      old_source_filenames <- registry$source_file
      files_to_remove <- setdiff(old_source_filenames, source_filenames) # files in old no longer in new
      files_to_add <- setdiff(source_filenames, old_source_filenames) # files in new not present in old

      # if there are any changes; keep only entries for the new list:
      if (length(files_to_add) | length(files_to_remove)) {
        registry <- dplyr::left_join(new_registry, registry, by = 'source_file')
      }
      # set status of new entries to "todo"
      registry <- registry |>
        dplyr::mutate(stauts = ifelse(is.na(status), "todo", status))

      # save new process registry
      readr::write_csv(x = registry,
                file = registry_filename)

    } else if (update_mode == 'overwrite') {

      file.remove(registry_filename)
      registry_filename <- create_registry(output_dir, source_filenames)

    }

    # check content of registry: is there anything to be executed?
    # registry <- read_csv(registry_filename, show_col_types = FALSE)
    registry <- read.csv(registry_filename)
    process_completed <- !any(registry$status == 'todo')

  }
  invisible(process_completed)
}


#' Get path to all batch files in the batches folder
#'
#' @param path Path to the folder containing batch.yaml files.
#'
#' @return vector containing batch filenames.
#' @export
#'
#' @examples get_batch_filenames()
get_batch_filenames <- function(path = './batches/'){
  list.files(path = path,
             pattern = '.yaml',
             full.names = TRUE)
}

