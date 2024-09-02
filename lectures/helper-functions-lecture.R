#' lecture_ggplot_theme
#' 
lecture_ggplot_theme_1 <- 
  ggplot2::theme(
    plot.background = element_rect(fill = "#F9F7F7", colour = "#F9F7F7"),
    panel.background = element_rect(fill = "#F9F7F7"),
    axis.line.x = element_line(colour = "#3D3C42"),
    axis.line.y = ggplot2::element_blank(),
    panel.grid.minor = element_line(colour = "#F9F7F7"),
    panel.grid.major = element_line(colour = "#F9F7F7"),
    axis.ticks.y = ggplot2::element_line(linewidth = 0),
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  )

lecture_ggplot_theme_barplot <- 
  ggplot2::theme(
    plot.background = element_rect(fill = "#F9F7F7", colour = "#F9F7F7"),
    panel.background = element_rect(fill = "#F9F7F7"),
    axis.line.x = element_line(colour = "#3D3C42"),
    axis.line.y = ggplot2::element_blank(),
    panel.grid.minor = element_line(colour = "#F9F7F7"),
    panel.grid.major = element_line(colour = "#F9F7F7"),
    legend.text = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "none"
  )

lecture_ggplot_theme_moderation_plot <- 
  ggplot2::theme(
    plot.background = element_rect(fill = "#F9F7F7", colour = "#F9F7F7"),
    panel.background = element_rect(fill = "#F9F7F7"),
    axis.line.x = element_line(colour = "#3D3C42"),
    axis.line.y = ggplot2::element_blank(),
    panel.grid.minor = element_line(colour = "#F9F7F7"),
    panel.grid.major = element_line(colour = "#F9F7F7"),
    legend.background = element_rect(fill = "#F9F7F7", color = "black"),
    strip.background = element_rect(fill = "#F9F7F7", color = "black")
  )

# ggplot2 variables
plot_alpha <- .50
plot_fill <- "#3F72AF"
plot_color <- "#112D4E"
ggplot2::theme
#' scaled_dnorm
#' 
#' 
scaled_dnorm <- function(x, mean = 0, sd = 1, scale) dnorm(x, mean, sd) * scale

#' geom_norm_density
#' 
#' 
geom_norm_density <- function(mean, sd, fun, args, y_offset, color, fill,
                              alpha, scale) {
  ggplot2::geom_area(
    stat = "function",
    fun = fun,
    args = list(mean = mean, sd = sd, scale = scale),
    xlim = c(mean - 3 * sd, mean + 3 * sd),
    position = ggplot2::position_nudge(y = y_offset),
    color = color,
    fill = fill,
    alpha = alpha
  )
}

#' calc_t_confint
#' 
#' 
calc_t_confint <- function(x, estimate, se, df) {
  
  t <- (x - estimate)/se
  dt(t, df = df)
  
} 
