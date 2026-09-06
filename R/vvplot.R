#' @importFrom ggplot2 ggplot aes geom_point geom_abline
#' @importFrom ggplot2 labs scale_x_continuous scale_y_continuous theme_light
#' @importFrom scales label_number
#' @importFrom rlang .data
#' @export

vvplot <- function (b.x, se.x, n.x, b.y, se.y, n.y, k.x = 0, k.y = 0,
                       xlab = "Exposure",
                       ylab = "Outcome",
                       main = "V-V Plot",
                       point_color = "blue",
                       line_color = "red",
                       point_shape = 16,
                       point_size = 1,
                       line_width = 1,
                       line_type = "solid"
)
{
  v.x <- 1/(b.x^2 + (n.x - 2 - k.x) * se.x^2)
  v.x <- v.x/sum(v.x)
  v.y <- 1/(b.y^2 + (n.y - 2 - k.y) * se.y^2)
  v.y <- v.y/sum(v.y)
  lb <- min(v.x, v.y)
  ub <- max(v.x, v.y)
  plot.data <- data.frame(v.x = v.x, v.y = v.y)
  ggplot(plot.data, aes(x = v.x, y = v.y)) +
    geom_point(size = point_size,
               color = point_color,
               shape = point_shape) +
    geom_abline(intercept = 0, slope = 1,
                color = line_color,
                lwd = line_width,
                lty = line_type) +
    coord_fixed(xlim = c(lb, ub), ylim = c(lb, ub)) +
    labs(x = xlab, y = ylab, title = main) +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    scale_y_continuous(labels = label_number(accuracy = 0.001)) +
    theme_light()
}
