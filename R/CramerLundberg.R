#' @include generics.R
NULL

setClass(Class = "CramerLundberg",
         slots = list(
             initial_capital = "numeric",
             premium_rate = "numeric",
             claim_poisson_arrival_rate = "numeric",
             claim_size_distribution = "numeric",
             claim_size_parameters = "numeric"
         ))

#' @export
CramerLundberg <- function(initial_capital = NULL,
                                   premium_rate = NULL,
                                   claim_poisson_arrival_rate = NULL,
                                   claim_size_distribution = NULL,
                                   claim_size_parameters = NULL) {
    model <- new(
        Class = "CramerLundberg",
        initial_capital = initial_capital,
        premium_rate = premium_rate,
        claim_poisson_arrival_rate = claim_poisson_arrival_rate,
        claim_size_mixing_parameter = claim_size_mixing_parameter,
        claim_size_light_tail_distribution = claim_size_light_tail_distribution,
        claim_size_light_tail_parameters = claim_size_light_tail_parameters,
        claim_size_heavy_tail_distribution = claim_size_heavy_tail_distribution,
        claim_size_heavy_tail_parameters = claim_size_heavy_tail_parameters,
        capital_injection_poisson_rate = capital_injection_poisson_rate,
        capital_injection_distribution = capital_injection_distribution,
        capital_injection_parameters = capital_injection_parameters
    )
}

setMethod(
    f = "simulate_process",
    signature = c(model = "CramerLundberg"),
    definition = function(model,
                          max_jumps_number = NULL,
                          max_time_horizon = NULL,
                          max_simulation_time = NULL) {

        browser()

    }
)
