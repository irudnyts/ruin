#' @include AllClass.R
#' @include AllGeneric.R
NULL


setValidity(
  Class = "SparreAndersen",
  method = function(object) {

    # define aliases
    #---------------------------------------------------------------------------

    initial_capital <- object@initial_capital
    premium_rate <- object@premium_rate
    claim_interarrival_generator <- object@claim_interarrival_generator
    claim_interarrival_parameters <- object@claim_interarrival_parameters
    claim_size_parameters <- object@claim_size_parameters
    claim_size_generator <- object@claim_size_generator

    errors <- character(0)

    # check initial_capital
    #---------------------------------------------------------------------------

    if(isFALSE(length(initial_capital) == 1) ||
       is.na(initial_capital) ||
       initial_capital < 0)
      errors <- c(errors,
                  paste0("initial_capital must be a numeric length 1",
                         " non-negative vector containing no missing values."))

    # check premium_rate
    #---------------------------------------------------------------------------

    if(isFALSE(length(premium_rate) == 1) ||
       is.na(premium_rate) ||
       premium_rate < 0)
      errors <- c(errors,
                  paste0("premium_rate must be a numeric length 1",
                         " non-negative vector containing no missing values."))


    # check formal arguments of claim_interarrival_generator and
    # claim_interarrival_parameters
    #---------------------------------------------------------------------------

    if(
      isFALSE(
        all(
          names(claim_interarrival_parameters) %in%
          names(formals(claim_interarrival_generator))
        )
      )
    )
      errors <- c(errors,
                  paste0("claim_interarrival_parameters must have the same",
                         " names as formal argument of",
                         " claim_interarrival_generator"))

    # check formal arguments of claim_size_generator and claim_size_parameters
    #---------------------------------------------------------------------------

    if(
      isFALSE(
        all(
          names(claim_size_parameters) %in% names(formals(claim_size_generator))
        )
      )
    )
      errors <- c(errors,
                  paste0("claim_size_parameters must have the same names as",
                         " formal argument of claim_size_generator."))

    # return TRUE if slots are valid, otherwise errors messeges
    #---------------------------------------------------------------------------

    if(length(errors) == 0) {
      TRUE
    } else {
      errors
    }

  }

)


#' Constructs an object of SparreAndersen S4 class
#'
#' \code{SparreAndersen()} constructs an object of \code{SparreAndersen} S4
#' class.
#'
#' The function constructs an object of a formal S4 class
#' \code{SparreAndersen}, a representation of an extension of
#' Cramer-Lundberg model that allows for non-exponential interarrival times and
#' defined as follows:
#' \deqn{X(t) = u + ct - \sum_{i=1}^{N(t)} Y_i,}
#' where \eqn{u} is the initial capital (\code{initial_capital}), \eqn{c} is the
#' premium rate (\code{premium_rate}), \eqn{N(t)} is the renewal process defined
#' by distribution of interarrival times (\code{claim_interarrival_generator}
#' and \code{claim_interarrival_parameters}), \eqn{Y_i} are iid claim sizes
#' (\code{claim_size_generator} and \code{claim_size_parameters}).
#'
#' @param initial_capital a length one numeric non-negative vector specifying an
#' initial capital. Default: \code{0}.
#' @param premium_rate a length one numeric non-negative vector specifying a
#' premium rate. Default: \code{1}.
#' @param claim_interarrival_generator a function indicating the random
#' generator of claims' interarrival times. Default: \code{rexp}.
#' @param claim_interarrival_parameters a named list containing parameters for
#' the random generator of claims' interarrival times. Default:
#' \code{list(rate = 1)}.
#' @param claim_size_generator a function indicating the random generator of
#' claims' sizes. Default: \code{rexp}.
#' @param claim_size_parameters a named list containing parameters for the
#' random generator of claims' sizes. Default: \code{list(rate = 1)}.
#'
#' @return An object of \linkS4class{SparreAndersen} class.
#'
#' @seealso \code{\link{CramerLundberg}},
#' \code{\link{CramerLundbergCapitalInjections}},
#' \code{link{SparreAndersenCapitalInjections}}.
#'
#' @references \itemize{
#' \item Andersen, E. Sparre. \emph{On the collective theory of risk in case of
#' contagion between claims}. Transactions of the XVth International Congress
#' of Actuaries, 2(6), 1957.
#' \item Thorin O. \emph{Some Comments on the Sparre Andersen Model in the Risk
#' Theory}. ASTIN Bulletin: The Journal of the IAA, 8(1):104-125, 1974.
#' }
#'
#' @examples
#' model <- SparreAndersen(
#'   initial_capital = 10,
#'   premium_rate = 1,
#'   claim_interarrival_generator = rexp,
#'   claim_interarrival_parameters = list(rate = 1),
#'   claim_size_generator = rexp,
#'   claim_size_parameters = list(rate = 1)
#' )
#'
#' @export
SparreAndersen <- function(initial_capital = NULL,
                           premium_rate = NULL,
                           claim_interarrival_generator = NULL,
                           claim_interarrival_parameters = NULL,
                           claim_size_generator = NULL,
                           claim_size_parameters = NULL) {

  # set default arguments
  #-----------------------------------------------------------------------------

  if(is.null(initial_capital))
    initial_capital <- 0

  if(is.null(premium_rate))
    premium_rate <- 1

  if(is.null(claim_interarrival_generator))
    claim_interarrival_generator <- stats::rexp

  if(is.null(claim_interarrival_parameters))
    claim_interarrival_parameters <- list(rate = 1)

  if(is.null(claim_size_generator))
    claim_size_generator <- stats::rexp

  if(is.null(claim_size_parameters))
    claim_size_parameters <- list(rate = 1)

  # generate an object and return it
  #-----------------------------------------------------------------------------

  model <- methods::new(
    Class = "SparreAndersen",
    initial_capital = initial_capital,
    premium_rate = premium_rate,
    claim_interarrival_generator = claim_interarrival_generator,
    claim_interarrival_parameters = claim_interarrival_parameters,
    claim_size_generator = claim_size_generator,
    claim_size_parameters = claim_size_parameters
  )

  return(model)

}


#' Simulates a path of a Sparre Andersen model
#'
#' \code{simulate_path()} simulates a path of \linkS4class{SparreAndersen}
#' model until one of the following conditions is met: (1) the process is
#' ruined, (2) \code{max_time_horizon} is achieved, (3) the elapsed time of the
#' simulation is greater than \code{max_simulation_time}.
#'
#' @param model an S4 object of \linkS4class{SparreAndersen} class.
#' @param max_time_horizon a length one numeric vector specifying the maximum
#' time horizon, until with the process will be simulated. Default: \code{Inf}.
#' @param max_simulation_time a length one numeric vector indicating the maximum
#' allowed time of simulation. The value should be specified in seconds.
#' Default: \code{Inf}.
#' @param seed an optional arbitrary length numeric vector specifying the seed.
#' If provided, the \code{.Random.seed} in \code{.GlobalEnv} is set to its
#' value.
#'
#' @return \linkS4class{PathSparreAndersen}
#'
#' @section Warning:
#' Setting both \code{max_time_horizon} and \code{max_simulation_time} to
#' \code{Inf} might be dangerous. In this case, the only stopping condition is a
#' ruin of the process, which might not happen.
#'
#' @examples
#' model <- SparreAndersen(initial_capital = 10,
#'                         premium_rate = 1,
#'                         claim_interarrival_generator = rexp,
#'                         claim_interarrival_parameters = list(rate = 2),
#'                         claim_size_generator = rexp,
#'                         claim_size_parameters = list(rate = 1))
#'
#' path <- simulate_path(model = model, max_time_horizon = 10)
setMethod(
  f = "simulate_path",
  signature = c(model = "SparreAndersen"),
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

      isS4(model),

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
    f_a <- model@claim_interarrival_generator
    param_a <- model@claim_interarrival_parameters
    f_s <- model@claim_size_generator
    param_s <- model@claim_size_parameters

    # simulate process
    #-----------------------------------------------------------------------

    # add n = 1 to distribution's parameters in order to generate only
    # one r.v.
    param_a[["n"]] <- 1
    param_s[["n"]] <- 1

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

    s_neg <- numeric() # positive jumps' sizes

    a_neg <- numeric() # arrival times of negative jumps

    ca_neg <- do.call(what = f_a, args = param_a) # itinital arrival time of a
                                                  # jump

    is_ruined <- FALSE

    start_time <- Sys.time() # set a timer

    repeat{

      if(as.numeric(difftime(time1 = Sys.time(),
                             time2 = start_time,
                             units = "secs")) < max_simulation_time) {

        if(ca_neg < max_time_horizon) {

          # current negative jump's size
          cs_neg <- do.call(what = f_s, args = param_s)

          path <- add_jump_to_path(path, ca_neg, -cs_neg)

          s_neg <- c(s_neg, cs_neg)
          a_neg <- c(a_neg, ca_neg)

          if(path[nrow(path), 2] < 0) {
            is_ruined <- TRUE
            break
          }

          ca_neg <- ca_neg + do.call(what = f_a, args = param_a)

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
    process <- methods::new(
      Class = "PathSparreAndersen",
      model = model,
      path = path,
      claim_sizes = s_neg,
      claim_arrival_times = a_neg,
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
