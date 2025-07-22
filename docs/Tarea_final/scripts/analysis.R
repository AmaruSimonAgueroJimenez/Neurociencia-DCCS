# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    ggthemes,
    ggokabeito,
    ggsignif
)
setwd(this.path::here())

# load data ----
data <- read_rds(file = "../data/complete_data.rds") %>%
    bind_rows() %>%
    mutate(
        id = replace_na(id, "NA"), # there's an actual participant
        log_rt = log(rt)
    )

# descriptive and positive controls ----

## flanker effect ----

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


# figures ----

overall_rt_mean <- mean(data$rt)
filter_rt_max <- quantile(data$rt, probs = 0.75) + (IQR(data$rt) * 1.5)

correct_incorrect_p1 <- data %>%
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
correct_incorrect_p1

ggsave(
    filename = "../figures/correct_incorrect_p1.png",
    plot = correct_incorrect_p1,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)

flanker_effect_p1 <- flanker_effect_emm$emmeans %>%
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
    geom_text(aes(label = "Fast", x = 2.2, y = response[4], color = task[4]), size = 10) +
    geom_text(aes(label = "Slow", x = 2.2, y = response[2], color = task[2]), size = 10) +
    scale_x_discrete(labels = c("Congruent", "Incongruent")) +
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(0.2, 1, 0.1),
        limits = c(0.4, 0.8)
    ) +
    theme_par() +
    theme(
        legend.position = "none",
        text = element_text(size = 30)
    ) +
    scale_color_okabe_ito() +
    xlab("") +
    ylab("Reaction time (sec.)") +
    coord_fixed(ratio = 4)
flanker_effect_p1

ggsave(
    filename = "../figures/flanker_effect_p1.png",
    plot = flanker_effect_p1,
    width = 5000,
    height = 5000,
    dpi = 300,
    units = c("px")
)
