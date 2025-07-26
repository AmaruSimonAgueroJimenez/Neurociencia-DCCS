pacman::p_load(
    tidyverse,
    RWiener
)
setwd(this.path::here())

# likelihood function ----

ddm_log_likelihood <- function(params, data) {
    # set of paramaters
    # boundary separation
    alpha <- params[1]
    # non-decision time
    tau <- params[2]
    # starting point
    beta <- params[3]
    # drift rate
    delta <- params[4]

    # ground truth paramaters
    # actual rt for the subject in a particular trial
    rt_truth <- data$rt
    # actual response
    response_truth <- data$response

    # check for ground truth coherence
    if (params[2] >= min(data$rt) ||
        params[1] <= 0 ||
        params[3] <= 0 ||
        params[3] >= 1) {
        return(1e6)
    }

    # compute log-likelihood
    raw_log_lik <- dwiener(
        rt_truth,
        alpha = alpha,
        tau = tau,
        beta = beta,
        delta = delta,
        resp = response_truth
    )

    log_lik <- log(raw_log_lik)

    # avoid -Inf :c
    if (any(is.infinite(log_lik)) || any(is.nan(log_lik))) {
        return(1e6) # huge number, super sad
    }

    # return the sum
    return(-sum(log_lik))
}

# example data ----

set.seed(420)
sample_data <- tibble(
    rt = c(0.75, 0.89, 0.62, 1.1, 0.95),
    response = c("upper", "lower", "upper", "upper", "lower")
)

samp_params <- c(
    alpha = 1.5,
    tau = 0.3,
    beta = 0.5,
    delta = 2.0
)

neg_log_lik <- ddm_log_likelihood(
    params = samp_params,
    data = sample_data
)

print(neg_log_lik)

# parameter recovery ----

true_params <- c(
    alpha = 1.6,
    tau = 0.25,
    beta = 0.4,
    delta = 2.5
)

# generate fake data
n_trials <- 600
set.seed(421)

# generate n_trials of fake data :)
fake_data <- rwiener(
    n = n_trials,
    alpha = true_params["alpha"],
    tau = true_params["tau"],
    beta = true_params["beta"],
    delta = true_params["delta"]
) %>%
    as_tibble() %>%
    rename(
        rt = q,
        response = resp
    )

# lets see if we recover the parameters
start_params <- c(
    alpha = 1.0,
    tau = 0,
    beta = 0.5,
    delta = 1.0
)

param_recovery_fit <- optim(
    par = start_params,
    fn = ddm_log_likelihood,
    data = fake_data,
    lower = c(0.1, 0.05, 0.01, -Inf),
    upper = c(5, 1.0, 0.99, Inf),
    method = "L-BFGS-B",
    control = list(maxit = 500)
)
param_recovery_fit

recovered_params <- param_recovery_fit$par

# set actual subject data for the fitting procedure ----

data_fast <- read_rds("../data/data_clean_fast.rds") %>%
    mutate(
        task = "fast"
    )
data_slow <- read_rds("../data/data_clean_slow.rds") %>%
    mutate(
        task = "slow"
    )

complete_data <- bind_rows(data_fast, data_slow) %>%
    select(
        Subject,
        correct_answer,
        RT_ms,
        trial_type,
        task,
        Trial
    ) %>%
    rename(
        response = correct_answer,
        rt = RT_ms,
        id = Subject
    ) %>%
    mutate(
        response = if_else(response == 1, "upper", "lower"),
        rt = rt / 1000
    ) %>%
    group_by(id, trial_type, task) %>%
    group_split()
complete_data

write_rds(x = complete_data, file = "../data/complete_data.rds")

# fit all subjects x task conditions

start_params <- c(
    alpha = 1.0,
    tau = 0,
    beta = 0.5,
    delta = 1.0
)

set.seed(666)
fit_list <- complete_data %>%
    map_dfr(., function(X) {
        final_fit <- optim(
            par = start_params,
            fn = ddm_log_likelihood,
            data = X,
            lower = c(0.1, 0.05, 0.01, -Inf),
            upper = c(5, 1.0, 0.99, Inf),
            method = "L-BFGS-B",
            control = list(maxit = 500)
        )
        return_param <- final_fit$par %>%
            enframe() %>%
            pivot_wider() %>%
            mutate(
                id = unique(X$id),
                trial_type = unique(X$trial_type),
                task = unique(X$task)
            )
        return(return_param)
    })

write_rds(x = fit_list, file = "../data/parameter_fits.rds")
