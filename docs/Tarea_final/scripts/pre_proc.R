# libs ----
pacman::p_load(
    tidyverse,
    ggplot2
)
setwd(this.path::here())

# load data ----

all_data <- list.files(
    path = "../data",
    full.names = TRUE,
    recursive = TRUE
) %>%
    as_tibble()

# flanker ----
ft_rapido <- all_data %>%
    filter(
        grepl(pattern = "Ericsen_FT_rapidp.log", x = value)
    )
ft_rapido

ft_lento <- all_data %>%
    filter(
        grepl(pattern = "Ericsen_FT_lentp.log", x = value)
    )
ft_lento

ft_rapido_read <- ft_rapido %>%
    group_by(row_number()) %>%
    group_split() %>%
    map_dfr(., function(X) {
        dat <- X$value
        log_data <- read_delim(
            X$value,
            delim = "\t",
            skip = 3,
            skip_empty_rows = TRUE
        ) %>%
            filter(
                !Subject %in% c("Picture", "Event Type")
                # also filter picture response
            )
        return(log_data)
    })

ft_lento_read <- ft_lento %>%
    group_by(row_number()) %>%
    group_split() %>%
    map_dfr(., function(X) {
        dat <- X$value
        log_data <- read_delim(
            X$value,
            delim = "\t",
            skip = 3,
            skip_empty_rows = TRUE
        ) %>%
            filter(
                !Subject %in% c("Picture", "Event Type")
            )
        return(log_data)
    })

# write data ----
write_rds(x = ft_rapido_read, file = "../data/ft_rapido.rds")
write_rds(x = ft_lento_read, file = "../data/ft_lento.rds")

# data ----
ft_data_rapido <- ft_rapido_read %>%
    group_by(Subject) %>%
    mutate(
        group_trial = cumsum(if_else(`Event Type` == "Picture", 1, 0))
    ) %>%
    ungroup() %>%
    group_by(Subject, group_trial) %>%
    mutate(
        Code = as.numeric(Code),
        valid_trial = if_else(n() == 2, 1, 0),
        arrow = as.numeric(substr(Code, 1, 1)),
        correct_answer = as.numeric(arrow[1] == arrow)
    ) %>%
    select(
        Subject,
        Trial,
        `Event Type`,
        Code,
        Time,
        group_trial:correct_answer
    ) %>%
    ungroup() %>%
    group_by(Subject, group_trial) %>%
    mutate(
        Time = as.numeric(Time),
        RT_ms = (Time - Time[1]) / 10,
        trial_type = if_else(head(Code, n = 1) %in% c(20, 10), "incongruent", "congruent")
    )

ft_data_lento <- ft_lento_read %>%
    group_by(Subject) %>%
    mutate(
        group_trial = cumsum(if_else(`Event Type` == "Picture", 1, 0))
    ) %>%
    ungroup() %>%
    group_by(Subject, group_trial) %>%
    mutate(
        Code = as.numeric(Code),
        valid_trial = if_else(n() == 2, 1, 0),
        arrow = as.numeric(substr(Code, 1, 1)),
        correct_answer = as.numeric(arrow[1] == arrow)
    ) %>%
    select(
        Subject,
        Trial,
        `Event Type`,
        Code,
        Time,
        group_trial:correct_answer
    ) %>%
    ungroup() %>%
    group_by(Subject, group_trial) %>%
    mutate(
        Time = as.numeric(Time),
        RT_ms = (Time - Time[1]) / 10,
        trial_type = if_else(head(Code, n = 1) %in% c(20, 10), "incongruent", "congruent")
    )

write_rds(x = ft_data_rapido, file = "../data/data_dirty_fast.rds")
write_rds(x = ft_data_lento, file = "../data/data_dirty_slow.rds")

final_fast <- ft_data_rapido %>%
    mutate(
        action = if_else(arrow == 2, "R", "L"),
        reward = correct_answer
    ) %>%
    filter(
        `Event Type` != "Picture"
    ) %>%
    ungroup() %>%
    group_by(Subject, group_trial) %>%
    filter(
        RT_ms >= 150 # do something better
    ) %>%
    slice_min(order_by = Time, n = 1) %>%
    ungroup() %>%
    filter(
        `Event Type` != "Quit"
    )

final_slow <- ft_data_lento %>%
    mutate(
        action = if_else(arrow == 2, "R", "L"),
        reward = correct_answer
    ) %>%
    filter(
        `Event Type` != "Picture"
    ) %>%
    ungroup() %>%
    group_by(Subject, group_trial) %>%
    filter(
        RT_ms >= 150 # do something better
    ) %>%
    slice_min(order_by = Time, n = 1) %>%
    ungroup() %>%
    filter(
        `Event Type` != "Quit"
    )

write_rds(x = final_fast, file = "../data/data_clean_fast.rds")
write_rds(x = final_slow, file = "../data/data_clean_slow.rds")
