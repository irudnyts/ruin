#' @export
plot_path <- function(path_object) {

  # validate arguments
  #---------------------------------------------------------------------------

  stopifnot(

    is(object = path_object,
       class2 = c("PathCramerLundberg",
                  "PathCramerLundbergCapitalInjections",
                  "PathSparreAndersen",
                  "PathSparreAndersenCapitalInjections")),

    "path" %in% slotNames(path_object)

  )

  # plot
  #---------------------------------------------------------------------------

  data <- data.frame(path_object@path)

  aestetic <- ggplot2::aes_string(x = "time", y = "X")

  return(ggplot2::ggplot(data = data) + ggplot2::geom_line(mapping = aestetic))

}
