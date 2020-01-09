#' Find columns that are "empty" (only one unique variable)
#'
#' @param dataset dataframe
#'
#' @return character
#' @export
identify_empty_columns <- function(dataset) {
  dataset %>%
    gather() %>%
    group_by(key) %>%
    unique() %>%
    summarize(count = n()) %>%
    filter(count == 1) %>%
    select(key) %>%
    pull()
}

#' Remove columns that are "empty" (only one unique variable)
#'
#' @param dataset dataframe
#'
#' @return dataframe
#' @export
remove_extra_columns <- function(dataset) {
  dataset %>%
    select(-one_of(identify_empty_columns(.)))
}


#' Count how many items are in a group
#'
#' @param dataset dataframe
#' @param ... character
#'
#' @return dataframe
#' @export
count_group <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    summarize(count = n()) %>%
    mutate(prop = count / sum(count)) %>%
    arrange(-count)
}

#' Count how many items are in a group
#'
#' @param dataset dataframe
#' @param ... character
#'
#' @return integer
#' @export
pull_count <- function(dataset) {
  dataset %>%
    summarize(count = n()) %>%
    pull(count)
}