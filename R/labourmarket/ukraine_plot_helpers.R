#' Title
#'
#' @param data data.frame
#' @param jaottelu character
#' @param osuus boolean
#' @param variable character
#' @param grouping character
#' @param ylabtxt character
#' @param alpha_u numeric
#' @param font_size numeric
#'
#' @return
#' @export
#'
#' @examples
Ukraina_kuvaaja <- function(data, jaottelu, osuus, variable, grouping, ylabtxt, alpha_u, font_size) {
  #function(data.frame, str, boolean, str, str, str)

  data <- Ukraina_aggregaattori(data, jaottelu)
  Ukraina_colours <- readRDS(file = file.path("cache", "dr_colours.Rds"))
  col_pos <- "stack"

  if (osuus) {

    data$n <- round((data$n/data$n_total)*100, digits = 1)

  }

  if (jaottelu == "none") {
    lookup = c("lukumäärä"="n_total")
    Ukraina_colours <- colors["orange"]
  } else if (grouping == "ikäryhmä" & osuus == FALSE) {
    lookup = c("lukumäärä"="n", "ikäryhmä"="age_group")
  } else if (grouping == "sukupuoli" & osuus == FALSE) {
    lookup = c("lukumäärä"="n")
    col_pos <- "dodge"
    Ukraina_colours <- c(colors["light_blue"], colors["orange"])
  } else if (grouping == "ikäryhmä" & osuus == TRUE) {
    lookup = c("prosenttia"="n", "ikäryhmä"="age_group")
  } else if (grouping == "sukupuoli" & osuus == TRUE) {
    lookup = c( "prosenttia"="n")
    #col_pos <- "dodge"
    Ukraina_colours <- c(colors["light_blue"], colors["orange"])
  } else if (grouping == "ala" & osuus == FALSE & jaottelu != "ammatti") {
    lookup = c( "lukumäärä"="n") #, "ala" = "toimiala"
  } else if (jaottelu == "ammatti" & osuus == FALSE) {
    lookup = c("lukumäärä"="n") #, "ala" = "nimi_fi"
  }

  p <- data %>%
    rename(any_of(lookup)) %>%
    ggplot(aes(x = aika))
  if (jaottelu == "none") {
    p <- p + geom_col(aes_string(y = variable, fill = grouping), fill = colors["orange"], alpha = alpha_u, position = col_pos)
  } else {
    p <- p + geom_col(aes_string(y = variable, fill = grouping), alpha = alpha_u, position = col_pos)
  }
  p <- p + scale_x_date(name = "", date_breaks = "1 month", date_labels = "%m/%Y") +
    scale_fill_manual(values = Ukraina_colours) +
    scale_y_continuous(name = ylabtxt, labels = tuhaterotin) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(size = font_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 13),
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotlyp <- ggplotly(p) %>%
    layout(legend = list(orientation = "h", x = 0.5, y = -0.5, xanchor = 'center', title=list(text='')))
}

Ukraina_aggregaattori <- function(data, param) {

  switch(param,
         none = distinct(data, aika, n_total),
         ikäryhmä = data %>%
           group_by(aika, n_total, age_group) %>%
           summarise(n = sum(n)),
         sukupuoli = data %>%
           group_by(aika, n_total, sukupuoli) %>%
           summarise(n = sum(n)),
         toimiala = data %>%
           filter(aika > ymd("2022-04-01")) %>%
           filter(toimiala_nimi %in% top) %>%
           mutate(ala = toimiala_nimi ),
         ammatti = data %>%
           filter(aika > ymd("2022-04-01")) %>%
           filter(t3_nimi %in% top) %>%
           mutate(ala = t3_nimi)
  )

}
