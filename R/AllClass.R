#-------------------------------------------------------------------------------
# CramerLundberg classes

#' A formal S4 class CramerLundberg
#'
#' A formal S4 class representation of classical Cramer-Lundberg model.
#'
#' The model is defined as follows:
#' \deqn{X(t) = u + ct - \sum_{i=1}^{N(t)} Y_i,}
#' where \eqn{u} is the initial capital (\code{initial_capital}), \eqn{c} is the
#' premium rate (\code{premium_rate}), \eqn{N(t)} is the Poisson process with
#' intencity \eqn{\lambda} (\code{claim_poisson_arrival_rate}), \eqn{Y_i} are
#' iid claim sizes (\code{claim_size_generator} and \code{claim_size_parameters}
#' ).
#'
#' Objects of class can be created only by using the constructor
#' \code{\link{CramerLundberg}}.
#'
#' @slot initial_capital a length one numeric non-negative vector specifying an
#' initial capital.
#' @slot premium_rate a length one numeric non-negative vector specifying a
#' premium rate.
#' @slot claim_poisson_arrival_rate a length one numeric positive vector
#' specifying the rate of the Poisson process of claims' arrivals.
#' @slot claim_size_generator a function indicating the random generator of
#' claims' sizes.
#' @slot claim_size_parameters a named list containing parameters for the
#' random generator of claims' sizes.
#'
#' @seealso \code{\link{CramerLundberg}}
#'
#' @references
#' Albrecher H., Asmussen A. \emph{Ruin Probabilities}. World Scientific, 2010.
setClass(
  Class = "CramerLundberg",
  slots = list(
    initial_capital = "numeric",
    premium_rate = "numeric",
    claim_poisson_arrival_rate = "numeric",
    claim_size_generator = "function",
    claim_size_parameters = "list"
  )
)

setClass(
  Class = "PathCramerLundberg",
  slots = list(
    model = "CramerLundberg",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)

#-------------------------------------------------------------------------------
# CramerLundbergCapitalInjections classes

setClass(
  Class = "CramerLundbergCapitalInjections",
  contains = "CramerLundberg",
  slots = list(
    capital_injection_poisson_rate = "numeric",
    capital_injection_size_generator = "function",
    capital_injection_size_parameters = "list"
  )
)

setClass(
  Class = "PathCramerLundbergCapitalInjections",
  slots = list(
    model = "CramerLundbergCapitalInjections",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    capital_injection_sizes = "numeric",
    capital_injection_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)

#-------------------------------------------------------------------------------
# SparreAndersen classes

setClass(
  Class = "SparreAndersen",
  slots = list(
    initial_capital = "numeric",
    premium_rate = "numeric",
    claim_interarrival_generator = "function",
    claim_interarrival_parameters = "list",
    claim_size_generator = "function",
    claim_size_parameters = "list"
  )
)

setClass(
  Class = "PathSparreAndersen",
  slots = list(
    model = "SparreAndersen",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)

#-------------------------------------------------------------------------------
# SparreAndersenCapitalInjections classes


setClass(
  Class = "SparreAndersenCapitalInjections",
  contains = "SparreAndersen",
  slots = list(
    capital_injection_interarrival_generator = "function",
    capital_injection_interarrival_parameters = "list",
    capital_injection_size_generator = "function",
    capital_injection_size_parameters = "list"
  )
)

setClass(
  Class = "PathSparreAndersenCapitalInjections",
  slots = list(
    model = "SparreAndersenCapitalInjections",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    capital_injection_sizes = "numeric",
    capital_injection_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)
