


#' Save information about platform and packages used during session
#'
#' @param output_dir path to directory in which to save the session info.
#'
#' @return hash/unique id of the session
#' @export
#'
#' @examples save_session_info("./")
save_session_info <- function(output_dir = '../'){

  algo = "sha1"
  # create unique id for a system setup
  session_info_text <- paste(capture.output(sessionInfo()), collapse = '')
  session_info_id <- substr(digest::digest(session_info_text, algo = algo),1, 6)

  # collect detailed system setup info
  session_info <- devtools::session_info()
  session_platform_info <- as.data.frame(session_info$platform)
  session_packages_info <- as.data.frame(session_info$packages)

  # add time of execution
  now <- now()
  id <- substr(digest::digest(now, algo = algo),1, 6)

  session_platform_info <- session_platform_info |>
    mutate(id = as.character(id), now = now) |>
    relocate(id, now, .before = 1)

  session_packages_info <- session_packages_info |>
    mutate(id = as.character(id), now = now) |>
    relocate(id, now, .before = 1)


  # if model_registry does not exist yet, create it, otherwise update
  session_info_filename <- paste0(output_dir, '/session_info.csv')
  session_packages_filename <- paste0(output_dir, '/session_packages.csv')


  append_csv(x = session_platform_info, file = session_info_filename)
  append_csv(x = session_packages_info, file = session_packages_filename)


  id
}



#' Append data frame to CSV file
#'
#'If csv file already exists, don't include column names; otherwise do.
#'Uses readr:write_csv()
#'
#' @param x data frame
#' @param file name of the csv file to be created or appended.
#'
#' @return None
#' @export
#'
#' @examples append_csv(cars, 'cars.csv')
append_csv <- function(x, file){
  readr::write_csv(x = x, file = file,
                   append = file.exists(file))
}

