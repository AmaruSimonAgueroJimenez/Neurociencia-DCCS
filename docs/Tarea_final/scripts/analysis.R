# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    ggthemes,
    ggokabeito,
    ggsignif,
    brms,
    furrr
)
setwd(this.path::here())
plan(multisession, workers = availableCores())

# load data ----
data <- read_rds(file = "../data/complete_data.rds") %>%
    bind_rows() %>%
    mutate(
        id = replace_na(id, "NA"), # there's an actual participant
        log_rt = log(rt),
        response = as.factor(response)
    ) %>%
    ungroup() %>%
    group_by(id, task) %>%
    arrange(Trial, .by_group = TRUE)

param_fit <- read_rds(file = "../data/parameter_fits.rds")

# analysis I: flanker effect ----

# difference in reaction time between incongruent and congruent trials

flanker_effect_mdl <- lme4::lmer(
    data = data,
    log(rt) ~ trial_type * task + (1 | id)
)

flanker_effect_emm <- emmeans::emmeans(
    flanker_effect_mdl,
    pairwise ~ trial_type | task,
    type = "response"
)
flanker_effect_emm

flanker_did <- pairs(
    pairs(flanker_effect_emm$emmeans, type = "link"),
    by = NULL
)
flanker_did

# analysis II: error rate ----

error_rate_mdl <- lme4::glmer(
    response ~ trial_type * task + (1 + trial_type | id),
    data = data,
    family = binomial(link = "logit")
)
summary(error_rate_mdl)

error_rate_emm <- emmeans::emmeans(
    error_rate_mdl,
    pairwise ~ trial_type | task,
    type = "response"
)
error_rate_emm

error_rate_did <- pairs(
    pairs(error_rate_emm$emmeans, type = "link"),
    by = NULL
)
error_rate_did

# analysis III: trial-by-trial adjustments ----

lagged_trial_data <- data %>%
    group_by(
        id, task
    ) %>%
    mutate(
        n_1_lag_rt = lag(rt, n = 1),
        prev_trial_type = lag(trial_type, n = 1)
    )
lagged_trial_data

lag_mdl <- lmerTest::lmer(
    data = lagged_trial_data,
    log(rt) ~ n_1_lag_rt * trial_type * task + prev_trial_type + (1 + trial_type | id)
)
summary(lag_mdl)

lag_emm <- emmeans::emtrends(
    lag_mdl,
    pairwise ~ trial_type | task + prev_trial_type,
    var = "n_1_lag_rt",
    type = "response"
) %>% emmeans::test()
lag_emm

# analysis IV: ddm parameters ----

param_names <- c("alpha", "beta", "delta", "tau")

param_fit_data <- param_fit %>%
    pivot_longer(
        cols = alpha:delta,
        names_to = "parameter",
        values_to = "values"
    ) %>%
    group_by(
        parameter
    ) %>%
    group_split()
param_fit_data

ddm_mdl <- param_fit_data %>%
    map(
        ., function(param) {
            mdl <- lme4::lmer(
                data = param,
                values ~ trial_type * task + (1 | id)
            )
            return(mdl)
        }
    )

ddm_emm <- ddm_mdl %>%
    imap(
        ., function(mdl_param, idx) {
            emm <- emmeans::emmeans(
                mdl_param,
                pairwise ~ trial_type | task,
                type = "response"
            )
            return(list(
                emm = emm,
                tidy_emm = broom.mixed::tidy(emm$contrasts, conf.int = TRUE) %>%
                    mutate(param = param_names[idx])
            ))
        }
    )
ddm_emm

ddm_emm[[1]]$emm
ddm_emm[[2]]$emm
ddm_emm[[3]]$emm
ddm_emm[[4]]$emm

# drift rate comparisons

drift_rate_emm <- emmeans::emmeans(
    ddm_mdl[[3]],
    pairwise ~ task | trial_type,
    type = "response"
)
drift_rate_emm

# analysis V: mechanism of inhibitory strategy ----

surprise_data <- data %>%
    group_by(id, task) %>%
    mutate(
        trialIndex = row_number()
    ) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(
        n_task = length(unique(task)),
        n_trial_type = length(unique(trial_type)),
        trialType = trial_type
    ) %>%
    # we require that participant have both tasks
    filter(n_task == 2, n_trial_type == 2) %>%
    do(broom::augment(lm(rt ~ trialIndex * trialType * task + response, data = .))) %>%
    ungroup() %>%
    mutate(
        rtSurprise = abs(.std.resid)
    )

optim_data <- surprise_data %>%
    mutate(
        isIncongruent = if_else(trialType == "incongruent", 1, 0)
    ) %>%
    arrange(id, task, trialIndex) %>%
    group_by(id, task) %>%
    mutate(
        rtSurprisePrev = lag(rtSurprise, n = 1),
        task = if_else(task == "slow", 0, 1),
        trialType = if_else(trialType == "congruent", 1, 0),
        response = as.character(response)
    ) %>%
    ungroup() %>%
    group_by(id) %>%
    group_split()

# likelihood function
full_model_nll <- function(params, data) {
    # ewma parameters
    logit_learningRate <- params[1]
    # to bound between 0 and 1
    learningRate <- plogis(logit_learningRate)

    # drift rate parameters
    delta_intercept <- params[2]
    delta_surprise <- params[3]
    delta_task <- params[4]
    delta_trialType <- params[5]

    # boundary separation parameters
    bs_intercept <- params[6]
    bs_ewma <- params[7]
    bs_trialType <- params[8]
    bs_task <- params[9]
    bs_ewma_trialType <- params[10]

    # non-decision time parameters
    ndt_intercept <- params[11]
    ndt_task <- params[12]

    # bias parameters
    bias_intercept <- params[13]

    # trial-by-trial predictors
    n_trials <- nrow(data)
    ewmaPred <- numeric(n_trials)
    predictionError <- numeric(n_trials)

    delta_t <- numeric(n_trials)
    bs_t <- numeric(n_trials)
    ndt_t <- numeric(n_trials)
    bias_t <- numeric(n_trials)

    # first ewma set to neutral
    last_ewma <- 0.5
    # we assume no error in first trials
    last_pred_error <- 0

    # loop through each trial
    for (t in 1:n_trials) {
        # reset when task is changed
        # equations for each parameter
        delta_t[t] <- delta_intercept +
            delta_surprise *
                data$rtSurprisePrev[t] +
            delta_task *
                data$task[t] +
            delta_trialType *
                data$trialType[t]
        bs_t[t] <- exp(
            bs_intercept +
                bs_ewma *
                    last_pred_error +
                bs_task *
                    data$task[t] +
                bs_trialType *
                    data$trialType[t] +
                bs_ewma_trialType *
                    (last_pred_error * data$trialType[t])
        )
        ndt_t[t] <- exp(
            ndt_intercept +
                ndt_task *
                    data$task[t]
        )
        bias_t[t] <- plogis(
            bias_intercept
        )

        # this is the prediction error for the trial type
        predictionError[t] <- data$isIncongruent[t] - ewmaPred[t]
        last_pred_error <- predictionError[t]

        last_ewma <- learningRate *
            data$isIncongruent[t] +
            (1 - learningRate) * last_ewma
    }

    # check for ground truth coherence
    if (params[11] >= min(data$rt) ||
        params[6] <= 0 ||
        params[13] <= 0 ||
        params[13] >= 1) {
        return(1e6)
    }

    likelihoods <- dwiener(
        data$rt,
        alpha = bs_t,
        tau = ndt_t,
        beta = bias_t,
        delta = delta_t,
        resp = data$response
    )

    if (any(likelihoods <= 0 | is.na(likelihoods))) {
        likelihoods[likelihoods <= 0 | is.na(likelihoods)] <- 1e-9
    }

    nll <- -sum(log(likelihoods))

    if (!is.finite(nll)) {
        return(1e9)
    } else {
        return(nll)
    }
}


restricted_model_nll <- function(params, data) {
    # ewma parameters
    logit_learningRate <- params[1]
    # to bound between 0 and 1
    learningRate <- plogis(logit_learningRate)

    # drift rate parameters
    delta_intercept <- params[2]
    delta_surprise <- params[3]
    delta_task <- params[4]
    delta_trialType <- params[5]

    # boundary separation parameters
    bs_intercept <- params[6]
    bs_ewma <- params[7]
    bs_trialType <- params[8]
    bs_task <- params[9]
    bs_ewma_trialType <- params[10]

    # non-decision time parameters
    ndt_intercept <- params[11]
    ndt_task <- params[12]

    # bias parameters
    bias_intercept <- params[13]

    # trial-by-trial predictors
    n_trials <- nrow(data)

    delta_t <- numeric(n_trials)
    bs_t <- numeric(n_trials)
    ndt_t <- numeric(n_trials)
    bias_t <- numeric(n_trials)

    # first ewma set to neutral
    last_ewma <- 0.5
    # we assume no error in first trials
    last_pred_error <- 0

    # loop through each trial
    for (t in 1:n_trials) {
        # equations for each parameter
        delta_t[t] <- delta_intercept
        bs_t[t] <- exp(
            bs_intercept
        )
        ndt_t[t] <- exp(
            ndt_intercept
        )
        bias_t[t] <- plogis(
            bias_intercept
        )
    }

    # check for ground truth coherence
    if (params[11] >= min(data$rt) ||
        params[6] <= 0 ||
        params[13] <= 0 ||
        params[13] >= 1) {
        return(1e6)
    }

    likelihoods <- dwiener(
        data$rt,
        alpha = bs_t,
        tau = ndt_t,
        beta = bias_t,
        delta = delta_t,
        resp = data$response
    )

    if (any(likelihoods <= 0 | is.na(likelihoods))) {
        likelihoods[likelihoods <= 0 | is.na(likelihoods)] <- 1e-9
    }

    nll <- -sum(log(likelihoods))

    if (!is.finite(nll)) {
        return(1e9)
    } else {
        return(nll)
    }
}

# start parameters
start_params <- c(
    # for ewma
    learningRate = 0.5,
    # drif rate intercept
    delta_intercept = 3,
    # drift rate slope for surprise
    delta_surprise = 0,
    # drift rate slope for task
    delta_task = 0,
    # drift rate slope for trialType
    delta_trialType = 0,
    # bs intercept
    bs_intercept = log(1.5),
    # bs slope for ewma
    bs_ewma = 0,
    # bs slope for task
    bs_task = 0,
    # bs slope for trialType
    bs_trialType = 0,
    # bs interaction term
    bs_ewma_trialType = 0,
    # non-decision time intercept
    ndt_intercept = log(0.1),
    # non-decision time slope for task
    ndt_task = 0,
    # bias intercept
    bias_intercept = 0
)

lower_bounds <- c(0.001, rep(-Inf, 12))
upper_bounds <- c(0.999, rep(Inf, 12))


## restricted model optimization ----
optim_results_restricted <- optim_data %>%
    imap(
        ., function(participant_data, idx) {
            optim_results <- optim(
                par = start_params,
                fn = restricted_model_nll,
                data = participant_data,
                method = "L-BFGS-B",
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = 1000, trace = 1)
            )
            vals <- optim_results$par
            nams <- names(vals)
            return(tibble(
                parameter_name = nams,
                parameter_value = vals,
            ) %>%
                mutate(
                    id = unique(participant_data$id),
                    nl = optim_results$value
                ))
        }
    )

## full model optimization ----
optim_results <- optim_data %>%
    future_imap(
        ., function(participant_data, idx) {
            optim_results <- optim(
                par = start_params,
                fn = full_model_nll,
                data = participant_data,
                method = "L-BFGS-B",
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = 1000, trace = 1)
            )
            vals <- optim_results$par
            nams <- names(vals)
            return(tibble(
                parameter_name = nams,
                parameter_value = vals,
            ) %>%
                mutate(
                    id = unique(participant_data$id),
                    nl = optim_results$value
                ))
        }
    )

write_rds(x = optim_results, file = "../data/full_model_params.rds")
write_rds(x = optim_results_restricted, file = "../data/restricted_model_params.rds")
optim_results <- read_rds("../data/full_model_params.rds")
optim_results_restricted <- read_rds("../data/restricted_model_params.rds")

## model comparisons ----

nl_full <- bind_rows(optim_results) %>%
    select(id, nl) %>%
    group_by(id) %>%
    slice_head() %>%
    rename(full_nl = nl)
nl_full

nl_restricted <- bind_rows(optim_results_restricted) %>%
    select(id, nl) %>%
    group_by(id) %>%
    slice_head() %>%
    rename(restricted_nl = nl)
nl_restricted

nl_comp <- left_join(nl_full, nl_restricted, by = c("id")) %>%
    mutate(
        D = 2 * (restricted_nl - full_nl),
        pval = pchisq(D, df = (13 - 4), lower.tail = FALSE)
    )
nl_comp

# mdl p values
pvals <- nl_comp$pval
k <- length(pvals)
fisher_chi_sq <- -2 * sum(log(pvals))
final_p_value <- pchisq(fisher_chi_sq, df = 2 * k, lower.tail = FALSE)
final_p_value

nl_comp %>%
    ggplot(aes(
        D,
        fill = "1"
    )) +
    geom_density() +
    geom_vline(
        xintercept = 0, linetype = "dashed"
    ) +
    theme_par() +
    theme(legend.position = "none") +
    scale_fill_okabe_ito() +
    xlab("Deviation statistic (restricted - full)") +
    ylab("Density")

## bootstrap statistics ----

boot_n <- 5000
boot_stat <- tibble(
    parameter_name = character(),
    mean_param_val = numeric(),
    iteration = numeric()
)
ids <- 1:length(optim_results)
for (i in 1:boot_n) {
    id_resample <- sample(ids, replace = TRUE, size = max(ids))
    id_list <- bind_rows(optim_results[id_resample])
    tmp <- id_list %>%
        group_by(parameter_name) %>%
        summarise(
            mean_param_val = mean(parameter_value)
        ) %>%
        mutate(
            iteration = i
        )
    boot_stat <- add_row(boot_stat, tmp)
}

boot_coefs <- boot_stat %>%
    group_by(parameter_name) %>%
    summarise(
        mean = mean(mean_param_val),
        median = median(mean_param_val),
        sd = sd(mean_param_val),
        lb = quantile(mean_param_val, probs = c(0.025)),
        ub = quantile(mean_param_val, probs = c(0.975)),
        sig = if_else((lb * ub) <= 0, "NS.", "SIG.")
    )
boot_coefs

## model predictions ----
# compare model prediction  (RMSE) between ITI conditions
# lower RMSE would support more strategy switching or an overall
# less defined strategy

predict_rt <- function(params,
                       data,
                       n_sims) {
    # parameter unpack

    # ewma parameters
    logit_learningRate <- params[1]
    # to bound between 0 and 1
    learningRate <- plogis(logit_learningRate)

    # drift rate parameters
    delta_intercept <- params[2]
    delta_surprise <- params[3]
    delta_task <- params[4]
    delta_trialType <- params[5]

    # boundary separation parameters
    bs_intercept <- params[6]
    bs_ewma <- params[7]
    bs_trialType <- params[8]
    bs_task <- params[9]
    bs_ewma_trialType <- params[10]

    # non-decision time parameters
    ndt_intercept <- params[11]
    ndt_task <- params[12]

    # bias parameters
    bias_intercept <- params[13]

    # linear combination of parameters

    # first ewma set to neutral
    last_ewma <- 0.5
    # we assume no error in first trials
    last_pred_error <- 0

    n_trials <- nrow(data)
    delta_t <- numeric(n_trials)
    bs_t <- numeric(n_trials)
    ndt_t <- numeric(n_trials)
    bias_t <- numeric(n_trials)
    ewmaPred <- numeric(n_trials)
    predictionError <- numeric(n_trials)
    simulated_rts <- numeric(n_trials)

    epsilon <- 1e-9
    # loop through each trial
    for (t in 1:n_trials) {
        current_rt_surprise_prev <- if (is.na(data$rtSurprisePrev[t])) {
            0 # A neutral, non-NA starting value
        } else {
            data$rtSurprisePrev[t]
        }
        ewmaPred[t] <- last_ewma

        # equations for each parameter
        delta_t[t] <- delta_intercept +
            delta_surprise *
                current_rt_surprise_prev +
            delta_task *
                data$task[t] +
            delta_trialType *
                data$trialType[t]
        bs_t[t] <- exp(
            bs_intercept +
                bs_ewma *
                    last_pred_error +
                bs_task *
                    data$task[t] +
                bs_trialType *
                    data$trialType[t] +
                bs_ewma_trialType *
                    (last_pred_error * data$trialType[t])
        )
        ndt_t[t] <- exp(
            ndt_intercept +
                ndt_task *
                    data$task[t]
        )
        bias_t[t] <- plogis(
            bias_intercept
        )

        # simulate trial data based on parameters
        if (any(is.na(c(bs_t, ndt_t, bias_t, delta_t)))) {
            simulated_rts[t] <- NA_real_
        } else {
            sim_data <- rwiener(
                n = 100,
                alpha = bs_t[t] + epsilon,
                tau = ndt_t[t] + epsilon,
                beta = bias_t[t] + epsilon,
                delta = delta_t[t] + epsilon
            )
            simulated_rts[t] <- mean(sim_data$q)
        }


        # this is the prediction error for the trial type
        predictionError[t] <- data$isIncongruent[t] - ewmaPred[t]
        last_pred_error <- predictionError[t]

        last_ewma <- learningRate *
            data$isIncongruent[t] +
            (1 - learningRate) * last_ewma
    }

    return(simulated_rts)
}

# data for predictions

mdl_preds <- optim_data %>%
    imap(
        ., function(participant_data, idx) {
            task <- participant_data$task
            id <- unique(participant_data$id)
            rt_pred <- predict_rt(
                optim_results[[idx]]$parameter_value,
                participant_data,
                1
            )
            rt_truth <- participant_data$rt
            mean_rt <- mean(participant_data$rt)
            return(
                tibble(
                    rt_pred = rt_pred,
                    rt_truth = rt_truth,
                ) %>%
                    mutate(
                        id = id,
                        task = task
                    ) %>%
                    rowwise() %>%
                    mutate(
                        rmse = sqrt((rt_truth - rt_pred)^2) / mean_rt
                    ) %>%
                    ungroup()
            )
        }
    )
mdl_preds

## statistical comparison ----
rt_task_mdl <- lmerTest::lmer(
    data = bind_rows(mdl_preds) %>%
        mutate(task = if_else(task == 1, "fast", "slow")),
    log(rmse) ~ task + (1 | id)
)
summary(rt_task_mdl)

rt_task_emm <- emmeans::emmeans(
    rt_task_mdl,
    pairwise ~ task,
    type = "response"
)
rt_task_emm

# figures ----

# to make correct plot boundaries
overall_rt_mean <- mean(data$rt)
filter_rt_max <- quantile(data$rt, probs = 0.75) + (IQR(data$rt) * 1.5)

figure_1 <- data %>%
    filter(rt <= filter_rt_max) %>%
    mutate(
        trial_type = if_else(trial_type == "congruent", "Congruent", "Incongruent"),
        response = if_else(response == "upper", "Correct", "Wrong")
    ) %>%
    ggplot(aes(
        rt,
        fill = task
    )) +
    geom_density(alpha = 0.5, bounds = c(-Inf, Inf)) +
    facet_wrap(~ trial_type * response) +
    geom_vline(
        xintercept = overall_rt_mean,
        linetype = "dashed"
    ) +
    scale_x_continuous(
        transform = "log",
        labels = scales::label_number(accuracy = 0.01),
        breaks = seq(0, 1, 0.25)
    ) +
    scale_fill_okabe_ito() +
    theme_par() +
    xlab("Reaction time (sec.)") +
    ylab("Density") +
    theme(
        legend.position = "right",
        text = element_text(size = 30)
    ) +
    annotation_logticks(
        sides = "b"
    ) +
    labs(
        fill = ""
    )
figure_1

ggsave(
    filename = "../figures/figure_1.png",
    plot = figure_1,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)

figure_2 <- flanker_effect_emm$emmeans %>%
    broom.mixed::tidy(., conf.int = TRUE) %>%
    ggplot(aes(
        trial_type, response,
        color = task
    )) +
    geom_pointrange(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        size = 1.5,
        linewidth = 1.5
    ) +
    geom_line(aes(group = task), alpha = 0.5, linewidth = 1.5) +
    geom_text(aes(label = "*", x = 1.5, y = response[4]), size = 10, color = "black") +
    geom_text(aes(label = "*", x = 1.5, y = response[2]), size = 10, color = "black") +
    scale_x_discrete(labels = c("Congruent", "Incongruent")) +
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(0.2, 1, 0.1),
        limits = c(0.4, 0.8)
    ) +
    labs(
        color = ""
    ) +
    theme_par() +
    theme(
        legend.position = "right",
        text = element_text(size = 30)
    ) +
    scale_color_okabe_ito() +
    xlab("") +
    ylab("Reaction time (sec.)") +
    coord_fixed(ratio = 4)
figure_2

ggsave(
    filename = "../figures/figure_2.png",
    plot = figure_2,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)


figure_3 <- error_rate_emm$emmeans %>%
    broom.mixed::tidy(., conf.int = TRUE) %>%
    ggplot(aes(
        trial_type, prob,
        color = task
    )) +
    geom_pointrange(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        size = 1.5,
        linewidth = 1.5,
        position = position_dodge(0.25)
    ) +
    geom_line(aes(group = task),
        alpha = 0.5,
        linewidth = 1.5,
        position = position_dodge(0.25)
    ) +
    geom_text(aes(label = "*", x = 1.5, y = 0.9), size = 10, color = "black") +
    geom_text(aes(label = "*", x = 1.5, y = 0.8), size = 10, color = "black") +
    scale_x_discrete(labels = c("Congruent", "Incongruent")) +
    labs(
        color = ""
    ) +
    theme_par() +
    theme(
        legend.position = "right",
        text = element_text(size = 30)
    ) +
    scale_color_okabe_ito() +
    xlab("") +
    ylab("Pr(Correct)") +
    coord_fixed(ratio = 4)
figure_3

ggsave(
    filename = "../figures/figure_3.png",
    plot = figure_3,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)

figure_4 <- lag_emm$emtrends %>%
    broom.mixed::tidy(., conf.int = TRUE) %>%
    ggplot(aes(
        trial_type, n_1_lag_rt.trend,
        color = task
    )) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(
        aes(
            ymin = n_1_lag_rt.trend - std.error,
            ymax = n_1_lag_rt.trend + std.error
        ),
        size = 1.5,
        linewidth = 1.5,
        position = position_dodge(0.25)
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(-0.2, 0.5, 0.1),
        limits = c(-0.2, 0.5)
    ) +
    theme_par() +
    theme(
        legend.position = "right",
        text = element_text(size = 30)
    ) +
    scale_color_okabe_ito() +
    xlab("") +
    labs(
        color = ""
    ) +
    ylab("Prev. trial estimate") +
    coord_fixed(ratio = 4)
figure_4

ggsave(
    filename = "../figures/figure_4.png",
    plot = figure_4,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)

figure_5 <- bind_rows(
    ddm_emm[[1]]$tidy_emm,
    ddm_emm[[2]]$tidy_emm,
    ddm_emm[[3]]$tidy_emm,
    ddm_emm[[4]]$tidy_emm
) %>%
    ggplot(aes(
        task, estimate,
        color = param
    )) +
    geom_pointrange(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        size = 1.5,
        linewidth = 1.5,
        position = position_dodge(0.25)
    ) +
    geom_hline(
        yintercept = 0,
        linetype = "dashed"
    ) +
    facet_wrap(~param, scales = "free") +
    theme_par() +
    theme(
        legend.position = "none",
        text = element_text(size = 30)
    ) +
    ylab("Congruent - Incongruent") +
    xlab("")
figure_5

ggsave(
    filename = "../figures/figure_5.png",
    plot = figure_5,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)

figure_6 <- boot_coefs %>%
    mutate(
        parameter_name = c(
            "INTERCEPT",
            "EWMA",
            "EWMA x TRIALTYPE",
            "Intercept",
            "TASK",
            "TRIALTYPE",
            "INTERCEPT",
            "PREDICTION ERROR",
            "TASK",
            "TRIALTYPE",
            "LEARNINGRATE",
            "INTERCEPT",
            "TASK"
        ),
        parameter_class = c(
            "Bias",
            "Boundary separation",
            "Boundary separation",
            "Boundary separation",
            "Boundary separation",
            "Drift rate",
            "Drift rate",
            "Drift rate",
            "Drift rate",
            "Learning rate",
            "Non-decision time",
            "Non-decision time",
            "Non-decision time"
        )
    ) %>%
    ggplot(aes(
        parameter_name, mean
    )) +
    geom_pointrange(aes(
        ymin = lb,
        ymax = ub
    )) +
    geom_hline(
        yintercept = 0,
        linetype = "dashed"
    ) +
    theme_par() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    facet_wrap(~parameter_class, scales = "free") +
    xlab("") +
    ylab("Bootstrapped estimates")
figure_6
