#' @export
ruin_probability <- function(model,
                             time_horizon,
                             simulation_number = 10000) {

    processes <- pbapply::pbreplicate(
        n = simulation_number,
        expr = do.call(simulate_path,
                       list(model = model,
                            max_time_horizon = time_horizon)),
        simplify = FALSE
    )

    ruined <- sapply(processes, function(prc) prc@is_ruined)

    # p <- mean(ruined)
    # std <- sd(ruined)
    #
    # z <- qnorm(0.5 + ci_level / 2)
    #
    # list(
    #     lower_bound = p - z * std / sqrt(simulation_number),
    #     estimate = p,
    #     upper_bound = p + z * std / sqrt(simulation_number)
    # )

    mean(ruined)

}
