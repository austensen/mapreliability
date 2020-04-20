#' Classification reliability tables
#'
#' Get a table of reliability results for a classification of a set of
#' estimates.
#'
#' @param data Dataframe
#' @param est Estimate column
#' @param moe Margin of error column
#' @param n_classes Number of classes
#' @param class_breaks_low A vector of lower break points for a custom classification
#' @param conf_level Confidence level used for MOEs (default = 90\%)
#' @param quiet Whether to raise warnings about dropped observations with
#'   missing data
#'
#' @import rlang
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' bk_hh_income <- tidycensus::get_acs(
#'   geography = "tract",
#'   state = "NY",
#'   county = "Kings",
#'   year = 2018,
#'   survey = "acs5",
#'   variables = "B19013_001", # Median Household Income
#'   key = Sys.getenv("CENSUS_API")
#' )
#'
#' reliability_table_equal(bk_hh_income, estimate, moe, 4)
#'
#' reliability_table_quant(bk_hh_income, estimate, moe, 4)
#'
#' reliability_table_custom(bk_hh_income, estimate, moe, c(0, 40000, 80000))
#'
#' }
#'
#' @name reliability_table
NULL

#' @rdname reliability_table
#' @export
reliability_table_equal <- function(data, est, moe, n_classes, conf_level = 0.90, quiet = FALSE) {

  data <- prep_input_data(data, as_label(enquo(est)), as_label(enquo(moe)), quiet = quiet)

  z_score <- stats::qnorm((1.0-conf_level)/2, lower.tail = FALSE)

  data %>%
    dplyr::mutate(
      grp_breaks = cut(.data[["est"]], n_classes, dig.lab = 10),
      grp_lo = stringr::str_replace(.data[["grp_breaks"]], "\\((.+),.*", "\\1") %>% as.numeric(),
      grp_hi = stringr::str_replace(.data[["grp_breaks"]], "[^,]*,([^]]*)\\]", "\\1") %>% as.numeric(),
      grp_num = as.integer(.data[["grp_breaks"]])
    ) %>%
    calc_reliability(n_classes, z_score)
}

#' @export
#' @rdname reliability_table
reliability_table_quant <- function(data, est, moe, n_classes, conf_level = 0.90, quiet = FALSE) {

  data <- prep_input_data(data, as_label(enquo(est)), as_label(enquo(moe)), quiet = quiet)

  z_score <- stats::qnorm((1.0-conf_level)/2, lower.tail = FALSE)

  grp_qu_lo_vals <- stats::quantile(data[["est"]], ((100/n_classes)*((1:n_classes)-1))/100)
  grp_qu_lo_vals[1] <- -Inf
  grp_qu_hi_vals <- stats::quantile(data[["est"]], ((100/n_classes)*((1:n_classes)))/100)
  grp_qu_hi_vals[n_classes] <- Inf

  data %>%
    dplyr::mutate(
      grp_num = purrr::map_int(.data[["est"]], ~which(. >= grp_qu_lo_vals & . < grp_qu_hi_vals)),
      grp_lo = stats::quantile(.data[["est"]], ((100/n_classes)*(.data[["grp_num"]]-1))/100),
      grp_hi = stats::quantile(.data[["est"]], ((100/n_classes)*(.data[["grp_num"]]))/100)
    ) %>%
    calc_reliability(n_classes, z_score)
}

#' @export
#' @rdname reliability_table
reliability_table_custom <- function(data, est, moe, class_breaks_low, conf_level = 0.90, quiet = FALSE) {

  data <- prep_input_data(data, as_label(enquo(est)), as_label(enquo(moe)), quiet = quiet)

  z_score <- stats::qnorm((1.0-conf_level)/2, lower.tail = FALSE)

  n_classes <- length(class_breaks_low)

  class_breaks_high <- c(class_breaks_low[2:n_classes], max(data[["est"]]))

  grp_cus_lo_vals <- class_breaks_low
  grp_cus_lo_vals[1] <- -Inf
  grp_cus_hi_vals <- dplyr::lead(class_breaks_low)
  grp_cus_hi_vals[n_classes] <- Inf

  data %>%
    dplyr::mutate(
      grp_num = purrr::map_int(.data[["est"]], ~which(. >= grp_cus_lo_vals & . < grp_cus_hi_vals)),
      grp_lo = class_breaks_low[.data[["grp_num"]]],
      grp_hi = class_breaks_high[.data[["grp_num"]]]
    ) %>%
    calc_reliability(n_classes, z_score)
}


# takes a dataframe, and the names of the estimate and moe columns as string,
# and renames the columns to a standard "est" and "moe", then removes rows with
# missing values for the estimate or moe and optionally raises a warning about
# it the number of dropped observations.
prep_input_data <- function(data, est, moe, quiet = FALSE) {
  data <- data %>%
    # remove "sf" from class for spatial dataframe
    tibble::as_tibble() %>%
    # rename columns to standard "est" and "moe"
    dplyr::select(est = .data[[est]], moe = .data[[moe]])

  data_no_missing <- data %>% dplyr::filter(!is.na(.data[["est"]]), !is.na(.data[["moe"]]))

  dropped_rows <- nrow(data) - nrow(data_no_missing)
  if (!quiet && dropped_rows != 0) {
    rlang::warn(stringr::str_glue(
      "There are {dropped_rows} observations with missing estimates or MOE that have been removed."
    ))
  }

  data_no_missing
}

# dataframe with cols: est, moe, grp_num, grp_lo, grp_hi
calc_reliability <- function(data, n_classes, z_score) {
  data %>%
    dplyr::mutate(
      grp_prob = dplyr::case_when(
        grp_num == n_classes ~ 100-(stats::pnorm((.data[["est"]] - .data[["grp_lo"]])/(.data[["moe"]]/z_score))*100),
        grp_num == 1         ~ 100-(stats::pnorm((.data[["grp_hi"]] - .data[["est"]])/(.data[["moe"]]/z_score))*100),
        TRUE                 ~ (100-(stats::pnorm((.data[["grp_hi"]] - .data[["est"]])/(.data[["moe"]]/z_score))*100)
                               + 100-(stats::pnorm((.data[["est"]] - .data[["grp_lo"]])/(.data[["moe"]]/z_score))*100))
      )
    ) %>%
    dplyr::group_by(.data[["grp_lo"]]) %>%
    dplyr::summarise(
      grp_count = n(),
      grp_cum_prob = sum(.data[["grp_prob"]])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      grp_avg_prob = .data[["grp_cum_prob"]] / .data[["grp_count"]],
      grp_tot_count = sum(.data[["grp_count"]]),
      grp_tot_prob = sum(.data[["grp_cum_prob"]]) / .data[["grp_tot_count"]]
    ) %>%
    dplyr::select(
      class_breaks = .data[["grp_lo"]],
      count = .data[["grp_count"]],
      tot_count = .data[["grp_tot_count"]],
      reliability = .data[["grp_avg_prob"]],
      tot_reliability = .data[["grp_tot_prob"]]
    )
}
