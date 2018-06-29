#' @include generics.R
NULL

setClass(Class = "CramerLundbergExtended",
         slots = list(
             initial_capital = "numeric",
             premium_rate = "numeric",
             claim_poisson_arrival_rate = "numeric",
             claim_size_mixing_parameter = "function",
             claim_size_light_tail_distribution = "function",
             claim_size_light_tail_parameters = "list",
             claim_size_heavy_tail_distribution = "function",
             claim_size_heavy_tail_parameters = "list",
             capital_injection_poisson_rate = "numeric",
             capital_injection_distribution = "function",
             capital_injection_parameters = "list"
         ))

#' @export
CramerLundbergExtended <- function(initial_capital = NULL,
                                   premium_rate = NULL,
                                   claim_poisson_arrival_rate = NULL,
                                   claim_size_mixing_parameter = NULL,
                                   claim_size_light_tail_distribution = NULL,
                                   claim_size_light_tail_parameters = NULL,
                                   claim_size_heavy_tail_distribution = NULL,
                                   claim_size_heavy_tail_parameters = NULL,
                                   capital_injection_poisson_rate = NULL,
                                   capital_injection_distribution = NULL,
                                   capital_injection_parameters = NULL) {
    model <- new(
        Class = "CramerLundbergExtended",
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
    signature = c(model = "CramerLundbergExtended"),
    definition = function(model) {

        browser()

    }
)


