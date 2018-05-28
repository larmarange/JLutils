#' Transform data from long format to period format
#' 
#' @param .data a data.frame
#' @param .id a character, column containing individual ids
#' @param .start a character, column containing time variable indicating the beginning of each row
#' @param .stop a character, column containing time variable indicating the end of each row. If not provided, it will be derived from the dataset, considering that each row ends at the beginning of the next one.
#' @param .by a character vector, co-variables to consider (optionnal)
#' @examples 
#' \dontrun{
#' load(url("https://larmarange.github.io/analyse-R/data/care_trajectories.RData"))
#' care_trajectories
#' long_to_periods(care_trajectories, .id = "id", .start = "month")
#' long_to_periods(care_trajectories, .id = "id", .start = "month", 
#'                 .by = c("sex", "age", "care_status"))
#' }
#' @export
long_to_periods <- function(.data, .id, .start, .stop = NULL, .by = NULL){
  if (!requireNamespace("dplyr")) 
    stop("dplyr package is required. Please install it.")
  `%>%` <- dplyr::`%>%`
  cl <- class(.data)
  if (length(.start) != 1) stop(".start should contain only one column name")
  if (length(.stop) > 1) stop(".stop should contain only one column name or be NULL")
  .data$start <- .data[[.start]]
  .data <- .data %>% 
    dplyr::arrange_(.id, .start) %>%
    dplyr::group_by_at(c(.id, .by))
  .data$.grp <- .data %>% dplyr::group_indices()
  if (is.null(.stop)) {
    .data <- .data %>%
      dplyr::group_by_at(.id) %>%
      dplyr::mutate(stop = dplyr::lead(start)) %>%
      dplyr::filter(!is.na(stop)) # cleaning required
  } else {
    .data$stop <- .data[[.stop]]
  }
  .data <- .data %>%
    dplyr::group_by_at(.id) %>%
    dplyr::mutate(
      .prev_grp = dplyr::lag(.grp),
      .prev_stop = dplyr::lag(stop)
    )
  periods <- .data %>%
    dplyr::filter(is.na(.prev_grp) | .grp != .prev_grp | start != .prev_stop)
  periods <- periods %>%
    dplyr::group_by_at(.id) %>%
    dplyr::mutate(.next_prev_stop = dplyr::lead(.prev_stop))
  # trick: using the next value of .prev_stop allows to identify the new value of stop in periods
  # if no next value, stop remains unchanged
  
  periods <- merge(
    periods,
    .data %>% dplyr::group_by_at(.id) %>% dplyr::summarise(.last_stop = max(stop, na.rm = TRUE)),
    by = .id,
    all.x = TRUE
  )
  periods <- periods %>%
    dplyr::mutate(stop = ifelse(!is.na(.next_prev_stop), .next_prev_stop, .last_stop))
  
  periods <- periods[, c(.id, "start", "stop", .by)]
  
  if ("tbl_dt" %in% cl) {
    periods <- dtplyr::tbl_dt(periods)
  } else if ("tbl_df" %in% cl) {
    periods <- dplyr::as_tibble(periods)
  } else if ("data.table" %in% cl) {
    periods <- data.table::as.data.table(periods)
  } else {
    periods <- as.data.frame(periods)
  }
  
  return(periods)
}