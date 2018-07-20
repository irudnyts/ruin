#' @export
plot_path <- function(path_object) {

  data <- data.frame(path_object@path)

  aestetic <- ggplot2::aes_string(x = "time", y = "X")

  ggplot2::ggplot(data = data) + geom_line(mapping = aestetic)

}
