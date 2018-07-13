#' @export
setGeneric(
  name = "simulate_path",
  def = function(model,
                 max_time_horizon = NULL,
                 max_simulation_time = NULL,
                 seed = NULL) {
    standardGeneric("simulate_path")
  },
  signature = c("model")

)
