# nolint start
# Load Libraries ----
# Uncomment below to load libraries (avoids renv)
# library(REDCapTidieR)
# library(purrr)
# library(dplyr)
# library(lubridate)
# library(tidyr)
# library(ggplot2)
# library(stringr)
devtools::load_all(".")

# Set Up CGTTrialsReporter Fnctns ----
db_list_select <- function(db, tbls) {
  tbls <- tidyselect::eval_select(data = db, expr = enquo(tbls))
  db[tbls]
}


join_data <- function(db_tbl_list,
                      join_type = left_join,
                      ...) {
  # First: compile all names related to tables
  # Second: Identify names that exist in multiple tables (not infseq_id)
  # Third: Append identified names with name of the table they belong to

  duplicate_colnames <- db_tbl_list |>
    map(names) |>
    unlist()

  duplicate_colnames <- tibble(name = duplicate_colnames) |>
    count(name) |>
    # don't append table name to pk: infseq_id
    filter(n > 1 & name != "infseq_id") |>
    pull(name) |>
    unname()

  if (length(duplicate_colnames) > 0) {
    db_tbl_list <- map2(
      db_tbl_list,
      names(db_tbl_list),
      .f = function(df, df_name) {
        # [duplicate_col] -> [duplicate_col].[table_name]
        if (any(duplicate_colnames %in% names(df))) {
          rename_with(
            df,
            .cols = any_of(duplicate_colnames),
            .fn = function(col) paste0(col, ".", df_name)
          )
        } else {
          df
        }
      }
    )
  }

  # Multi-join using reduce
  reduce(
    db_tbl_list,
    # left_join if non-Complete data should be kept, inner_join if only complete
    join_type,
    by = "infseq_id",
    ...
  )
}

# Load Data ----
# Uncomment below to load libraries (avoids renv)
db <- read_redcap(
  Sys.getenv("REDCAP_URI"),
  Sys.getenv("PRODIGY_REDCAP_API")
)

db <- db |>
  extract_tibbles()

db_tbls <- db |>
  db_list_select(tbls = c(infusion_sequence, infusion, disease_assessment, end_of_study)) |>
  join_data()

prodigy <- db_tbls |>
  select(
    infseq_id,
    # Infusion Vars
    infseq_number,
    infusion_admin,
    infusion_date,
    # Disease Assessment Vars
    dasmt_date,
    dasmt_bcell_status, # BCR
    dasmt_overall, # CR, CRi
    # End of Study Vars
    end_study_date,
    end_study_reason # Death
  )

# Randomize Data ----
# Define a function to shuffle dates within each group
shuffle_dates <- function(x) {
  if (is.Date(x)) {
    return(sample(x))
  } else {
    return(x)
  }
}

# Define a function to shift dates within each group while keeping the order
shift_dates_within_group <- function(x) {
  if (is.Date(x)) {
    shift_amount <- sample(-max_shift:max_shift, size = 1)
    return(x + shift_amount)
  } else {
    return(x)
  }
}

# Define max_shift parameter
max_shift <- 10  # You can adjust this value as needed

# Apply transformations to each column
prodigy_randomized <- prodigy %>%
  group_by(infseq_id) %>%
  mutate(across(where(is.Date), shuffle_dates)) %>%
  ungroup() %>%
  mutate(infusion_date = as.Date(infusion_date),
         end_study_date = as.Date(end_study_date)) %>%
  group_by(infseq_id) %>%
  mutate(across(c(infusion_date, end_study_date), shift_dates_within_group)) %>%
  ungroup()

# Manipulate Data ----
patient_data <- prodigy_randomized |>
  filter(infusion_admin) |>
  # Shuffle IDs
  mutate(
    # Remove study ID
    infseq_id = str_extract(infseq_id, "(?<=-).*$"),
    infseq_id = case_when(
      infseq_id == "01.0" ~ "03.0",
      infseq_id == "01.1" ~ "03.1",
      infseq_id == "02.0" ~ "05.0",
      infseq_id == "02.1" ~ "05.1",
      infseq_id == "03.0" ~ "08.0",
      infseq_id == "03.1" ~ "08.1",
      infseq_id == "04.0" ~ "06.0",
      infseq_id == "05.0" ~ "12.0",
      infseq_id == "06.0" ~ "09.0",
      infseq_id == "07.0" ~ "01.0",
      infseq_id == "08.0" ~ "04.0",
      infseq_id == "09.0" ~ "02.0",
      infseq_id == "12.0" ~ "07.0"
    )
  ) |>
  mutate(
    # Add patient ID to consolidate infusion/reinfusion
    pt_id = str_extract(infseq_id, "^\\d+")
  ) |>
  mutate(
    .by = pt_id,
    .keep = "none",
    infseq_id,
    infusion_type = if_else(infseq_number > 0, "Reinfusion", "Infusion"),
    infusion_date,
    initial_infusion_date = if_else(infusion_type == "Infusion", infusion_date, first(infusion_date, na_rm = TRUE)),
    # Data for arrow neck length
    today = interval(initial_infusion_date, Sys.Date()) %/% days(1),
    # Zero out infusion time
    infusion_event = if_else(infusion_type == "Infusion", 0, interval(initial_infusion_date, infusion_date) %/% days(1)),
    # Set dasmt interval from zero time
    dasmt_date,
    disease_assessment = interval(initial_infusion_date, dasmt_date) %/% days(1),
    dasmt_bcell_status,
    # Extract abbrev from ()
    dasmt_overall = str_extract(dasmt_overall, "(?<=\\()[^\\)]+"),
    # Set end study time interval from zero time
    end_study_date,
    end_study_event = interval(initial_infusion_date, end_study_date) %/% days(1),
    end_study_reason
  ) |>
  mutate(
    .by = infseq_id,
    # Remove duplicated end_study_events
    end_study_event = ifelse(row_number() == n(), end_study_event, NA)
  ) |>
  pivot_longer(
    cols = c(disease_assessment, infusion_event, end_study_event),
    names_to = "event_marker",
    values_to = "delta_t0"
  ) |>
  # Change time to months
  mutate(
    delta_t0_months = round(delta_t0 / 30.417, digit = 0), # average days in a month
    delta_today = round(today / 30.417, digit = 0)
  ) |>
  filter(!is.na(delta_t0)) |>
  # Temporary method for handling reinfusions with no disease assessment data
  mutate(
    dasmt_date = case_when(
      is.na(dasmt_date) & infusion_type != "Infusion" ~ lag(dasmt_date),
      TRUE ~ dasmt_date
    ),
    dasmt_bcell_status = case_when(
      is.na(dasmt_bcell_status) & infusion_type != "Infusion" ~ lag(dasmt_bcell_status),
      TRUE ~ dasmt_bcell_status
    ),
    dasmt_overall = case_when(
      is.na(dasmt_overall) & infusion_type != "Infusion" ~ lag(dasmt_overall),
      TRUE ~ dasmt_overall
    )
  ) |>
  # Fill end study reasons for reinfusions to help arrow_status
  group_by(pt_id) |>
  fill(end_study_reason) |>
  mutate(
    arrow_status = is.na(end_study_reason)
  ) |>
  arrange(pt_id) |>
  ungroup()

end_study_events <- patient_data |>
  mutate(
    end_study_label = case_when(
      end_study_reason == "Completed study follow-up" ~ "✅",
      end_study_reason == "Death" ~ "❌",
      is.na(end_study_reason) ~ NA,
      TRUE ~ "⚠️"
    ),
    end_study_name = case_when(
      end_study_reason == "Completed study follow-up" ~ "Completed Study Follow-Up",
      end_study_reason == "Death" ~ "Deceased",
      is.na(end_study_reason) ~ NA,
      TRUE ~ "Other End Study Reason"
    )
  ) |>
  filter(event_marker == "end_study_event" & !is.na(end_study_label))

infusion_events <- patient_data |>
  select(pt_id, infusion_type, event_marker, delta_t0, delta_t0_months) |>
  mutate(
    .keep = "none",
    .by = pt_id,
    pt_id,
    infusion_type,
    infusion_delta_t0 = case_when(
      infusion_type == "Infusion" ~ 0,
      event_marker == "infusion_event" ~ delta_t0,
      TRUE ~ NA
    ),
    infusion_delta_t0_months = case_when(
      infusion_type == "Infusion" ~ 0,
      event_marker == "infusion_event" ~ delta_t0_months,
      TRUE ~ NA
    )
  ) |>
  filter(!is.na(infusion_delta_t0)) |>
  unique()

# Clean Data for Display ----
patient_data <- patient_data |>
  select(-c(
    contains("_date"),
    today, infseq_id, end_study_reason
  )) |>
  relocate(pt_id, .before = dasmt_bcell_status) |>
  rename(
    "bcell_status" = dasmt_bcell_status,
    disease_assessment_status = dasmt_overall
  ) |>
  unique()

end_study_events <- end_study_events |>
  select(pt_id, delta_t0, delta_t0_months, end_study_label, end_study_name)

# Save data ----
usethis::use_data(patient_data, overwrite = TRUE)
usethis::use_data(infusion_events, overwrite = TRUE)
usethis::use_data(end_study_events, overwrite = TRUE)

# ggswim ----
# Uncomment for testing
# ggswim(
#   patient_data |> dplyr::rename("Status Markers" = bcell_status),
#   mapping = aes(x = delta_t0_months, y = pt_id, fill = disease_assessment_status),
#   arrow = arrow_status,
#   arrow_head_length = unit(.25, "inches"),
#   arrow_neck_length = delta_today,
#   width = 0.25
# ) +
#   add_marker(
#     aes(x = delta_t0_months, y = pt_id, color = `Status Markers`, shape = `Status Markers`),
#     size = 5, position = "identity", alpha = 1
#   ) +
#   add_marker(
#     data = end_study_events,
#     aes(x = delta_t0_months, y = pt_id, label = end_study_label, color = end_study_name),
#     label.size = NA, fill = NA, size = 5
#   ) +
#   add_marker(
#     data = infusion_events,
#     aes(x = infusion_delta_t0, y = pt_id, color = infusion_type, shape = infusion_type),
#     size = 5, position = "identity", alpha = 1
#   ) +
#   scale_colour_manual(
#     values = c("firebrick", "#F5EB0A", "gray50", NA, NA, NA, "#25DA6D", "#25DA6D")
#   ) +
#   scale_shape_manual(
#     values = c(19, 19, 15, 17, 18)
#   ) +
#   scale_fill_manual(
#     name = "Overall Disease Assessment",
#     values = c("#6394F3", "#F3C363", "#EB792F")
#   ) +
#   labs(title = "Prodigy Swimmer Plot") +
#   xlab("Time (Months)") + ylab("Patient ID") +
#   theme_ggswim()

# nolint end
