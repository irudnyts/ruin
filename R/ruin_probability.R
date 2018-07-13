#' @export
ruin_probability <- function(model,
                             time_horizon,
                             simulation_number = NULL,
                             ci_level = NULL,
                             parallel = NULL,
                             return_paths = NULL) {

    # set default arguments
    #---------------------------------------------------------------------------

    if(is.null(simulation_number))
        simulation_number <- 10000

    if(is.null(ci_level))
        ci_level <- 0.95

    if(is.null(parallel))
        parallel <- TRUE

    if(is.null(return_paths))
        return_paths <- FALSE

    # validate arguments
    #---------------------------------------------------------------------------

    stopifnot(
        isS4(model),

        is.numeric(time_horizon) &&
            length(time_horizon) == 1 &&
            isFALSE(is.na(time_horizon)) &&
            time_horizon > 0,

        is.numeric(simulation_number) &&
            length(simulation_number) == 1 &&
            isFALSE(is.na(simulation_number)) &&
            simulation_number > 0,

        is.numeric(ci_level) &&
            length(ci_level) == 1 &&
            isFALSE(is.na(ci_level)) &&
            ci_level >= 0 &&
            ci_level <= 1,

        is.logical(parallel) &&
            isFALSE(is.na(parallel)) &&
            length(parallel) == 1,

        is.logical(return_paths) &&
            isFALSE(is.na(return_paths)) &&
            length(return_paths) == 1

    )

    # simulate
    #---------------------------------------------------------------------------

    if(parallel) {

        # detect the number of cores
        ncores <- parallel::detectCores()

        # parallelize for Windows
        if(.Platform[["OS.type"]] == "windows") {

            # set up a cluster
            cluster <- parallel::makePSOCKcluster(ncores)

            # set a RNG stream
            parallel::clusterSetRNGStream(cl = cluster)

            # export "model" variable to cluster workers
            parallel::clusterExport(cl = cluster,
                                    varlist = "model",
                                    envir = environment())

            # run simulate_path simulation_number times
            processes <- parallel::parLapply(
                cl = cluster,
                X = rep(time_horizon, simulation_number),
                fun = function(x) simulate_path(
                    model = model,
                    max_time_horizon = x
                )
            )

            # stop cluster
            parallel::stopCluster(cl = cluster)

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

        # change .Random.seed in Global enviroment
        seed <- parallel::nextRNGStream(.Random.seed)
        for(i in seq_len(simulation_number))
            seed <- parallel::nextRNGStream(seed)

        assign(x = ".Random.seed",
               value = seed,
               envir = .GlobalEnv)


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

    rval <- list()

    rval[["ruin_probability"]] <- c(
        lower_bound = p - z * std / sqrt(simulation_number),
        estimate = p,
        upper_bound = p + z * std / sqrt(simulation_number)
    )

    if(return_paths)
        rval[["simulated_paths"]] <- processes

    return(rval)

}
