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

setClass(
  Class = "CramerLundbergCapitalInjections",
  slots = list(
    initial_capital = "numeric",
    premium_rate = "numeric",
    claim_poisson_arrival_rate = "numeric",
    claim_size_mixing_parameter = "numeric",
    claim_size_light_tail_generator = "function",
    claim_size_light_tail_parameters = "list",
    claim_size_heavy_tail_generator = "function",
    claim_size_heavy_tail_parameters = "list",
    capital_injection_poisson_rate = "numeric",
    capital_injection_generator = "function",
    capital_injection_parameters = "list"
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
