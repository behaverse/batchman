
## ---- templates ----
# There are functions that create boilerplate documents.

#' Create new process file (boilerplate)
#'
#' @param process_name the name of the process to be created. Determines the name of the R file and the name of its functions.
#'
#' @return None
#' @export
#'
#' @examples new_process("demo")
new_process <- function(process_name = "process_name", path = '../') {

  # ---- define template ----
  text <- paste0('

# ---- load required packages ----
require(tidyverse)

',process_name,'.info <- function() {
# some information about this process (in markdown)
"
## About
This script is designed to score the resonses to the BIS/BAS Scale as implemented in Behaverse.
You can learn more about this scale [here](https://www.psy.miami.edu/faculty/ccarver/bisbas.html), [here](https://en.wikiversity.org/wiki/Behavioral_Inhibition_and_Behavioral_Activation_System_(BIS/BAS)_Scales) and [here](https://arc.psych.wisc.edu/self-report/behavioral-activation-and-behavioral-inhibition-scales-bai/).

It is important to note that there are different versions of this questionnaire and depending on the version the number of items and the scoring method may be different.

Here we use the scoring as definied in the [original version](https://www.psy.miami.edu/faculty/ccarver/bisbas.html).
However, we used responses options that ranged from 1 to 7 rather than from 1 to 4. We decided to score each item with values from 0 (=min) to 1 (=max) rather than 1 to 4 as in the original scale and we also decided to calculate the mean of these item scores rather than the sum as this provides an overal score that is easier to interpret.
"
}

', process_name,'.process <- function(df) {

  # ---- preprocessing ----
  df <- df |>
  filter(!is.na(option_data_type)) |>
  mutate(stimulus_id = as.numeric(str_remove_all(block_name, "x_bisbas_v2021.03_bisbas_q_") )) |>

  # assigning items to constructs;
  # TODO: link constructs to theory/ontology
  mutate(construct = case_when(
    stimulus_id %in% c(3,9,12,21) ~ "bas_drive",
    stimulus_id %in% c(5,10,15,20) ~ "bas_funseeking",
    stimulus_id %in% c(4,7,14,18,23) ~ "bas_rewardresponsiveness",
    stimulus_id %in% c(2,8,13,16,19,22,24) ~ "bis",
    stimulus_id %in% c(1,6,11,17) ~ "fillers")) |>

  # re-code responses to numeric values consistent with scoring function
  # 7 becomes 1, 1 becomes 7
  mutate(response_score = 8 - response_option_index) |>
  # reverse code some items
  mutate(response_score = ifelse(stimulus_id %in% c(2,22),
                                  8 - response_option_index,
                                  response_score)) |>

  # change scale so numbers go from 0 to 1 (original scale is 1 to 4)
  mutate(response_score = (response_score - 1)/6)


  # ---- summarize ----
  df |>
    filter(!is.na(construct)) |>
    group_by(subject_id, instrument_name, timeline_name, construct) |>
    summarise(response_numeric_mean = mean(response_score),
            response_time_median = median(response_time),
            n = n(),
            .groups = "drop")

}

', process_name,'.codebook <- function() {
  # codebook: describes variables created above
  tribble(
    ~variable, ~definition,
    "response_numeric_mean",   "Mean of the responses...",
    "response_time_median",   "Median of response times to ...",
    "n",   "Number of samples used in computation."
  )

}

', process_name,'.data_sample <- function() {
  # include a sample dataset to "test" function
  # not using dput() as it seems cumbersome
  # instead saving a data_sample manually


  # for example:
  ## load the data from one participant...
  # df <- readr::read_csv("./data/P500-L1m/subject_001/session_06/x_bisbas_v2021.03/response_1.csv")
  ## save that data as a data sample
  data_sample_filename <- "', path, 'processes/', process_name, '.RData"
  # save(df, file = data_sample_filename)

  # load data and return data sample
  load(data_sample_filename)
  df
}



')

  # ---- create file ----
  fileConn <- file(paste0(path, "processes/", process_name, ".R"))
  writeLines(text, fileConn)
  close(fileConn)

}





#' Create new batch file (boilerplate)
#'
#' @param batch_name the name of the process to be created. Determines the name of the yaml file to be created.
#'
#' @return None
#' @export
#'
#' @examples new_batch("demo")
new_batch <- function(batch_name, path = '../') {

  # ---- define template ----
  text <- '
name: batch_name
dataset: P500-L1m
process: process_name
output_dir: ./data/L2/
source: list_instrument_filenames(dataset, "my_instrument_name")
'

  # ---- create file ----
  fileConn <- file(paste0(path, "batches/", batch_name, ".yaml"))
  writeLines(text, fileConn)
  close(fileConn)

}


