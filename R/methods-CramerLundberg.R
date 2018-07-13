#' @include AllClass.R
#' @include AllGeneric.R
NULL

#' @export
CramerLundberg <- function(initial_capital = NULL,
                           premium_rate = NULL,
                           claim_poisson_arrival_rate = NULL,
                           claim_size_generator = NULL,
                           claim_size_parameters = NULL) {

  # set default arguments
  #-----------------------------------------------------------------------------

  if(is.null(initial_capital))
    initial_capital <- 0

  if(is.null(premium_rate))
    premium_rate <- 1

  if(is.null(claim_poisson_arrival_rate))
    claim_poisson_arrival_rate <- 1

  if(is.null(claim_size_generator))
    claim_size_generator <- rexp

  if(is.null(claim_size_parameters))
    claim_size_parameters <- list(rate = 1)

  # validate arguments
  #-----------------------------------------------------------------------------

  stopifnot(

    is.numeric(initial_capital) &&
      length(initial_capital) == 1 &&
      isFALSE(is.na(initial_capital)),

    is.numeric(premium_rate) &&
      length(premium_rate) == 1 &&
      isFALSE(is.na(premium_rate)),

    is.numeric(claim_poisson_arrival_rate) &&
      length(claim_poisson_arrival_rate) == 1 &&
      isFALSE(is.na(claim_poisson_arrival_rate)) &&
      claim_poisson_arrival_rate > 0,

    is.function(claim_size_generator),

    is.vector(claim_size_parameters)

  )

  # generate an object and return it
  #-----------------------------------------------------------------------------

  model <- new(
    Class = "CramerLundberg",
    initial_capital = initial_capital,
    premium_rate = premium_rate,
    claim_poisson_arrival_rate = claim_poisson_arrival_rate,
    claim_size_mixing_parameter = claim_size_mixing_parameter,
    claim_size_light_tail_generator = claim_size_light_tail_generator,
    claim_size_light_tail_parameters = claim_size_light_tail_parameters,
    claim_size_heavy_tail_generator = claim_size_heavy_tail_generator,
    claim_size_heavy_tail_parameters = claim_size_heavy_tail_parameters,
    capital_injection_poisson_rate = capital_injection_poisson_rate,
    capital_injection_generator = capital_injection_generator,
    capital_injection_parameters = capital_injection_parameters
  )

  return(model)

}

setMethod(
  f = "simulate_path",
  signature = c(model = "CramerLundberg"),
  definition = function(model,
                        max_time_horizon = NULL,
                        max_simulation_time = NULL,
                        seed = NULL) {


  }
)
