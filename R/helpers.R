#' Find columns that are "empty" (only one unique variable)
#'
#' @param dataset dataframe
#'
#' @return character
#' @export
identify_empty_columns <- function(dataset) {
  dataset %>%
    tidyr::gather() %>%
    dplyr::group_by(key) %>%
    base::unique() %>%
    dplyr::summarize(count = n()) %>%
    dplyr::filter(count == 1) %>%
    dplyr::select(key) %>%
    dplyr::pull()
}

#' Remove columns that are "empty" (only one unique variable)
#'
#' @param dataset dataframe
#'
#' @return dataframe
#' @export
remove_extra_columns <- function(dataset) {
  dataset %>%
    dplyr::select(-one_of(identify_empty_columns(.)))
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
    dplyr::group_by(...) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::mutate(prop = count / sum(count)) %>%
    dplyr::arrange(-count)
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
    dplyr::summarize(count = n()) %>%
    dplyr::pull(count)
}