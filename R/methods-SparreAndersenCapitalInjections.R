#' @include AllClass.R
#' @include AllGeneric.R
NULL

setValidity(
  Class = "SparreAndersenCapitalInjections",
  method = function(object) {

    # define aliases
    #---------------------------------------------------------------------------

    capital_injection_interarrival_generator <- object@capital_injection_interarrival_generator
    capital_injection_interarrival_parameters <- object@capital_injection_interarrival_parameters
    capital_injection_size_parameters <- object@capital_injection_size_parameters
    capital_injection_size_generator <- object@capital_injection_size_generator

    errors <- character(0)

    # check formal arguments of capital_injection_interarrival_generator and
    # capital_injection_interarrival_parameters
    #---------------------------------------------------------------------------

    if(
      isFALSE(
        all(
          names(capital_injection_interarrival_parameters) %in%
          names(formals(capital_injection_interarrival_generator))
        )
      )
    )
      errors <- c(errors,
                  paste0("capital_injection_interarrival_parameters must have",
                         " the same names as formal argument of",
                         " capital_injection_interarrival_generator"))

    # check formal arguments of capital_injection_size_generator and
    # capital_injection_size_parameters
    #---------------------------------------------------------------------------

    if(
      isFALSE(
        all(
          names(capital_injection_size_parameters) %in%
          names(formals(capital_injection_size_generator))
        )
      )
    )
      errors <- c(errors,
                  paste0("capital_injection_size_parameters must have the same",
                         " names as formal argument of",
                         " capital_injection_size_generator"))

    # return TRUE if slots are valid, otherwise errors messeges
    #---------------------------------------------------------------------------

    if(length(errors) == 0) {
      TRUE
    } else {
      errors
    }

  }

)

#' @export
SparreAndersenCapitalInjections <- function(
  initial_capital = NULL,
  premium_rate = NULL,
  claim_interarrival_generator = NULL,
  claim_interarrival_parameters = NULL,
  claim_size_generator = NULL,
  claim_size_parameters = NULL,
  capital_injection_interarrival_generator = NULL,
  capital_injection_interarrival_parameters = NULL,
  capital_injection_size_generator = NULL,
  capital_injection_size_parameters = NULL
) {

  # set default arguments
  #-----------------------------------------------------------------------------

  if(is.null(initial_capital))
    initial_capital <- 0

  if(is.null(premium_rate))
    premium_rate <- 1

  if(is.null(claim_interarrival_generator))
    claim_interarrival_generator <- rexp

  if(is.null(claim_interarrival_parameters))
    claim_interarrival_parameters <- list(rate = 1)

  if(is.null(claim_size_generator))
    claim_size_generator <- rexp

  if(is.null(claim_size_parameters))
    claim_size_parameters <- list(rate = 1)

  if(is.null(capital_injection_interarrival_generator))
    capital_injection_interarrival_generator <- rexp

  if(is.null(capital_injection_interarrival_parameters))
    capital_injection_interarrival_parameters <- list(rate = 1)

  if(is.null(capital_injection_size_generator))
    capital_injection_size_generator <- rexp

  if(is.null(capital_injection_size_parameters))
    capital_injection_size_parameters <- list(rate = 1)

  # generate an object and return it
  #-----------------------------------------------------------------------------

  model <- new(
    Class = "SparreAndersenCapitalInjections",
    initial_capital = initial_capital,
    premium_rate = premium_rate,
    claim_interarrival_generator = claim_interarrival_generator,
    claim_interarrival_parameters = claim_interarrival_parameters,
    claim_size_generator = claim_size_generator,
    claim_size_parameters = claim_size_parameters,
    capital_injection_interarrival_generator = capital_injection_interarrival_generator,
    capital_injection_interarrival_parameters = capital_injection_interarrival_parameters,
    capital_injection_size_generator = capital_injection_size_generator,
    capital_injection_size_parameters = capital_injection_size_parameters
  )

  return(model)

}

setMethod(
  f = "simulate_path",
  signature = c(model = "SparreAndersenCapitalInjections"),
  definition = function(model,
                        max_time_horizon = NULL,
                        max_simulation_time = NULL,
                        seed = NULL) {

    # set default arguments
    #---------------------------------------------------------------------------

    if(is.null(max_time_horizon))
      max_time_horizon <- Inf

    if(is.null(max_simulation_time))
      max_simulation_time <- Inf

    if(is.null(seed)) {
      seed <- .Random.seed
    } else {
      # .Random.seed <<- seed
      assign(x = ".Random.seed", value = seed, envir = .GlobalEnv)
    }

    # validate arguments
    #---------------------------------------------------------------------------

    stopifnot(

      is.numeric(max_time_horizon) &&
        length(max_time_horizon) == 1 &&
        isFALSE(is.na(max_time_horizon)) &&
        max_time_horizon > 0,

      is.numeric(max_simulation_time) &&
        length(max_simulation_time) == 1 &&
        isFALSE(is.na(max_simulation_time)) &&
        max_simulation_time > 0,

      is.numeric(seed)

    )

    if(is.infinite(max_time_horizon) && is.infinite(max_simulation_time))
      warning(paste0("Setting both max_time_horizon and max_simulation_time",
                     "to Inf might result in an infinite loop."))

    # define aliases
    #---------------------------------------------------------------------------

    u <- model@initial_capital
    pr <- model@premium_rate


    f_pa <- model@capital_injection_interarrival_generator
    param_pa <- model@capital_injection_interarrival_parameters
    f_ps <- model@capital_injection_size_generator
    param_ps <- model@capital_injection_size_parameters

    f_na <- model@claim_interarrival_generator
    param_na <- model@claim_interarrival_parameters
    f_ns <- model@claim_size_generator
    param_ns <- model@claim_size_parameters

    # simulate process
    #-----------------------------------------------------------------------

    # add n = 1 to all distributions parameters in order to generate only
    # one r.v.
    param_pa[["n"]] <- 1
    param_ps[["n"]] <- 1
    param_na[["n"]] <- 1
    param_ns[["n"]] <- 1

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

    ca_pos <- do.call(what = f_pa, args = param_pa) # current arrival time of a
                                                     # positive jump
    ca_neg <- do.call(what = f_na, args = param_na) # current arrival time of a
                                                     # negative jump

    is_ruined <- FALSE

    start_time <- Sys.time() # set a timer

    repeat{

      if(as.numeric(difftime(time1 = Sys.time(),
                             time2 = start_time,
                             units = "secs")) < max_simulation_time) {

        if(ca_pos < max_time_horizon || ca_neg < max_time_horizon) {

          if(ca_pos > ca_neg) {

            # current negative jump's size
            cs_neg <- do.call(what = f_ns, args = param_ns)

            path <- add_jump_to_path(path, ca_neg, -cs_neg)

            s_neg <- c(s_neg, cs_neg)
            a_neg <- c(a_neg, ca_neg)

            if(path[nrow(path), 2] < 0) {
              is_ruined <- TRUE
              break
            }

            ca_neg <- ca_neg + do.call(what = f_na, args = param_na)


          } else if(ca_pos == ca_neg) {

            # current positive jump's size
            cs_pos <- do.call(what = f_ps, args = param_ps)
            # current negative jump's size
            cs_neg <- do.call(what = f_ns, args = param_ns)

            path <- add_jump_to_path(path, ca_pos, cs_pos - cs_neg)

            s_pos <- c(s_pos, cs_pos)
            s_neg <- c(s_neg, cs_neg)

            a_pos <- c(a_pos, ca_pos)
            a_neg <- c(a_neg, ca_neg)

            if(path[nrow(path), 2] < 0) {
              is_ruined <- TRUE
              break
            }

            ca_pos <- ca_pos + do.call(what = f_pa, args = param_pa)
            ca_neg <- ca_neg + do.call(what = f_na, args = param_na)

          } else if(ca_pos < ca_neg) {

            # current positive jump's size
            cs_pos <- do.call(what = f_ps, args = param_ps)

            path <- add_jump_to_path(path, ca_pos, cs_pos)

            s_pos <- c(s_pos, cs_pos)
            a_pos <- c(a_pos, ca_pos)

            ca_pos <- ca_pos + do.call(what = f_pa, args = param_pa)

          }

        } else {

          # add max_time_horizon to a path
          path <- rbind(
            path,
            c(max_time_horizon,
              path[nrow(path), 2] + (max_time_horizon - path[nrow(path), 1]) * pr)
          )

          break

        }

      } else {

        break

      }

    }

    end_time <- Sys.time()

    elapsed_time <- as.numeric(difftime(time1 = end_time,
                                        time2 = start_time,
                                        units = "secs"))

    # generate a returning object
    process <- new(
      Class = "PathSparreAndersenCapitalInjections",
      model = model,
      path = path,
      claim_sizes = s_neg,
      claim_arrival_times = a_neg,
      capital_injection_sizes = s_pos,
      capital_injection_arrival_times = a_pos,
      time_horizon = path[nrow(path), 1],
      is_ruined = is_ruined,
      elapsed_time = elapsed_time,
      max_time_horizon = max_time_horizon,
      max_simulation_time = max_simulation_time,
      seed = seed
    )

    return(process)

  }
)
