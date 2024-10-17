#' Title
#'
#' @param data
#' @param column
#' @param conf_int
#'
#' @return
#' @export
#'
#' @examples
summarise_to_boxplot <- function(data, column, conf_int = 0.95){

  low_bound <- (1-conf_int)/2
  high_bound <- 1-low_bound

  data %>%
    rename(data_column = !!column) %>%
    summarise(
      y_min = quantile(data_column, low_bound, na.rm=T),
      y_25 = quantile(data_column, 0.25, na.rm=T),
      y_median = median(data_column, na.rm=T),
      y_75 = quantile(data_column, 0.75, na.rm=T),
      y_max = quantile(data_column, high_bound, na.rm=T),
      y_mean = mean(data_column, na.rm = T),
      n = n()
    )
}
