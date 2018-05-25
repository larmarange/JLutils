#' Transform data from long format to period format
#' 
#' @param .data a data.frame
#' @param .id a character, column containing individual ids
#' @param .time a character, column containing time variable
#' @param .status a character vector, a status variable (optionnal)
#' @param .by a character vector, co-variables to consider (optionnal)
#' @examples 
#' \dontrun{
#' load(url("https://larmarange.github.io/analyse-R/data/care_trajectories.RData"))
#' care_trajectories
#' long_to_periods(care_trajectories, .id = "id", .time = "month")
#' long_to_periods(care_trajectories, .id = "id", .time = "month", .by = c("sex", "age", "care_status"))
#' long_to_periods(care_trajectories, .id = "id", .time = "month", .by = c("sex", "age"), .status = ("care_status"))
#' }
#' @export
long_to_periods <- function(.data, .id, .time, .status = NULL, .by = NULL){
  if (!requireNamespace("dplyr")) 
    stop("dplyr package is required. Please install it.")
  `%>%` <- dplyr::`%>%`
  if (length(.time) > 1) stop(".time should contain only one column name")
  if (length(.status) > 1) stop(".status should contain only one column name")
  .data <- .data %>% 
    dplyr::arrange_(.id, .time) %>%
    dplyr::group_by_at(c(.id, .by, .status))
  .data$.grp <- .data %>% dplyr::group_indices()
  .data <- .data %>%
    dplyr::group_by_at(.id) %>%
    dplyr::mutate(.prev_grp = dplyr::lag(.grp))
  periods <- .data %>%
    dplyr::filter(is.na(.prev_grp) | .grp != .prev_grp)
  periods$.start <- periods[[.time]]
  periods <- periods %>%
    dplyr::group_by_at(.id) %>%
    dplyr::mutate(.next_start = dplyr::lead(.start))
  .data$.timevar <- .data[[.time]]
  periods <- merge(
    periods,
    .data %>% dplyr::group_by_at(.id) %>% dplyr::summarise(.last_time = dplyr::last(.timevar)),
    by = .id,
    all.x = TRUE
  )
  periods <- periods %>%
    dplyr::mutate(.stop = ifelse(is.na(.next_start), .last_time, .next_start))
  
  if (!is.null(.status)) {
    periods$.from <- periods[[.status]]
    periods <- periods %>%
      dplyr::group_by_at(.id) %>%
      mutate(.to = dplyr::lead(.from))
    periods[is.na(periods$.to),]$.to <- periods[is.na(periods$.to),]$.from
    periods <- periods[, c(.id, ".start", ".stop", ".from", ".to", .by)]
  } else {
    periods <- periods[, c(.id, ".start", ".stop", .by)]
  }
  class(periods) <- class(.data)
  return(periods)
}