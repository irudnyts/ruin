#' @include AllGeneric.R
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

    # validate arguments

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

setClass(Class = "PathCramerLundbergExtended",
         slots = list(
             model = "CramerLundbergExtended",
             path = "matrix",
             claim_sizes = "numeric",
             claim_arrival_times = "numeric",
             capital_injection_sizes = "numeric",
             capital_injection_arrival_times = "numeric",
             jumps_numer = "numeric",
             time_horizon = "numeric",
             is_ruined = "logical",
             elapsed_time = "numeric",
             max_jumps_number = "numeric",
             max_time_horizon = "numeric",
             max_simulation_time = "numeric",
             seed = "integer"
         ))

setMethod(
    f = "simulate_path",
    signature = c(model = "CramerLundbergExtended"),
    definition = function(model,
                          max_jumps_number = NULL,
                          max_time_horizon = NULL,
                          max_simulation_time = NULL,
                          seed = NULL) {

        # set default arguments
        #-----------------------------------------------------------------------

        if(is.null(seed)) {
            seed <- .Random.seed
        } else {
            .Random.seed <- seed
        }

        # validate arguments
        #-----------------------------------------------------------------------

        # define aliases
        #-----------------------------------------------------------------------

        u <- model@initial_capital
        pr <- model@premium_rate
        lambda_p <- model@capital_injection_poisson_rate
        f_p <- model@capital_injection_distribution
        param_p <- model@capital_injection_parameters
        lambda_n <- model@claim_poisson_arrival_rate
        eps <- model@claim_size_mixing_parameter
        f_n1 <- model@claim_size_light_tail_distribution
        param_n1 <- model@claim_size_light_tail_parameters
        f_n2 <- model@claim_size_heavy_tail_distribution
        param_n2 <- model@claim_size_heavy_tail_parameters

        # simulate process
        #-----------------------------------------------------------------------

        # add n = 1 to all distributions parameters in order to generate only
        # one r.v.
        param_p[["n"]] <- 1
        param_n1[["n"]] <- 1
        param_n2[["n"]] <- 1

        # initialize process
        path <- matrix(NA, nrow = 1, ncol = 2)
        colnames(path) <- c("time", "X")
        path[1, ] <- c(0, u)

        # auxiliary function for adding jumps to a path
        add_jump_to_path <- function(path, arrival, size) {

            path <- rbind(
                path,
                c(arrival,
                  path[nrow(path), 2] + (arrival - path[nrow(path), 1]) * pr)
            )

            path <- rbind(
                path,
                c(arrival,
                  path[nrow(path), 2] + size)
            )
            path
        }

        s_pos <- numeric() # positive jumps' sizes
        s_neg <- numeric() # positive jumps' sizes

        a_pos <- numeric() # arrival times of positive jumps
        a_neg <- numeric() # arrival times of negative jumps

        ca_pos <- rexp(1, lambda_p) # current arrival time of a positive jump
        ca_neg <- rexp(1, lambda_n) # current arrival time of a negative jump

        nj <- 0 # number of jumps

        start_time <- Sys.time() # set a timer

        is_ruined <- FALSE

        repeat{

            if((ca_pos < max_time | ca_neg < max_time) & nj < max_jumps_number) {

                if(ca_pos > ca_neg) {

                    # current negative jump's size
                    cs_neg <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                                     yes = do.call(f_n2, param_n2),
                                     no = do.call(f_n1, param_n1))

                    path <- add_jump_to_path(path, ca_neg, -cs_neg)

                    s_neg <- c(s_neg, cs_neg)
                    a_neg <- c(a_neg, ca_neg)

                    nj <- nj + 1

                    if(path[nrow(path), 2] < 0) {
                        is_ruined <- TRUE
                        break
                    }

                    ca_neg <- ca_neg + rexp(1, lambda_n)


                } else if(ca_pos == ca_neg) {

                    # current positive jump's size
                    cs_pos <- do.call(f_p, param_p)
                    # current negative jump's size
                    cs_neg <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                                     yes = do.call(f_n2, param_n2),
                                     no = do.call(f_n1, param_n1))

                    path <- add_jump_to_path(path, ca_pos, cs_pos - cs_neg)

                    s_pos <- c(s_pos, cs_pos)
                    s_neg <- c(s_neg, cs_neg)

                    a_pos <- c(a_pos, ca_pos)
                    a_neg <- c(a_neg, ca_neg)

                    nj <- nj + 1

                    if(path[nrow(path), 2] < 0) {
                        is_ruined <- TRUE
                        break
                    }

                    ca_pos <- ca_pos + rexp(1, lambda_p)
                    ca_neg <- ca_neg + rexp(1, lambda_n)

                } else if(ca_pos < ca_neg) {

                    # current positive jump's size
                    cs_pos <- do.call(f_p, param_p)

                    path <- add_jump_to_path(path, ca_pos, cs_pos)

                    s_pos <- c(s_pos, cs_pos)
                    a_pos <- c(a_pos, ca_pos)

                    nj <- nj + 1

                    ca_pos <- ca_pos + rexp(1, lambda_p)

                }

            } else {

                break

            }

        }


        # add max_time to a path, if the path is not ruined
        if(path[nrow(path), 2] >= 0)
            path <- rbind(
                path,
                c(max_time,
                  path[nrow(path), 2] + (max_time - path[nrow(path), 1]) * pr)
            )

        # generate a returning value
        process <- new(
            Class = "PathCramerLundbergExtended",
            model = model,
            path = path,
            claim_sizes = s_neg,
            claim_arrival_times = a_neg,
            capital_injection_sizes = s_pos,
            capital_injection_arrival_times = a_pos,
            jumps_numer = nj,
            time_horizon = path[nrow(path), 1],
            is_ruined = is_ruined,
            # elapsed_time = ,
            max_jumps_number = max_jumps_number,
            max_time_horizon = max_time_horizon,
            max_simulation_time = max_simulation_time,
            seed = seed
        )

        return(process)

    }
)


