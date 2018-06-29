#' @export
setGeneric(name = "simulate_process",
           def = function(model,
                          max_jumps_number,
                          max_time_horizon,
                          max_simulation_time,
                          seed)
               standardGeneric("simulate_process"),
           signature = "model"
           )
