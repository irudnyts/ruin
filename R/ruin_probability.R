#' @export
ruin_probability <- function(model,
                             time_horizon,
                             simulation_number = NULL,
                             ci_level = NULL,
                             parallel = TRUE) {

    # set default arguments
    #---------------------------------------------------------------------------

    if(is.null(simulation_number))
        simulation_number <- 10000

    if(is.null(ci_level))
        ci_level <- 0.95

    # validate arguments
    #---------------------------------------------------------------------------

    # simulate
    #---------------------------------------------------------------------------

    if(parallel) {

        # detect the number of cores
        ncores <- parallel::detectCores()

        # parallelize for Windows
        if(.Platform[["OS.type"]] == "windows") {

            # XXX

            cluster <- parallel::makePSOCKcluster(ncores)

            parallel::clusterSetRNGStream(cl = cluster)

            processes <- parallel::parLapply(
                cl = cluster,
                X = rep(time_horizon, simulation_number),
                fun = function(x) simulate_path(
                    model = model,
                    max_time_horizon = x
                )
            )

        # parallelize for Unix
        } else if(.Platform[["OS.type"]] == "unix") {

            processes <- parallel::mclapply(
                X = rep(time_horizon, simulation_number),
                FUN = function(x) simulate_path(
                    model = model,
                    max_time_horizon = x
                ),
                mc.set.seed = TRUE,
                mc.cores = ncores
            )

        }


    } else {

        processes <- replicate(
            n = simulation_number,
            expr = do.call(simulate_path,
                           list(model = model,
                                max_time_horizon = time_horizon)),
            simplify = FALSE
        )

    }

    # define which processes have been ruined
    ruined <- sapply(processes, function(prc) prc@is_ruined)

    p <- mean(ruined)
    std <- sd(ruined)

    z <- qnorm(0.5 + ci_level / 2)

    rval <- list(
        ruin_probability = c(
            lower_bound = p - z * std / sqrt(simulation_number),
            estimate = p,
            upper_bound = p + z * std / sqrt(simulation_number)
        ),
        simulated_processes = processes
    )

    return(rval)

}
