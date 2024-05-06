# nolint start
# Load Libraries ----
# Uncomment below to load libraries (avoids renv)
library(REDCapTidieR)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)
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
  ) |>
  # Randomize
  mutate(
    infseq_id = case_when(
      infseq_id == "22CT011-01.0" ~ "22CT011-03.0",
      infseq_id == "22CT011-01.1" ~ "22CT011-03.1",
      infseq_id == "22CT011-02.0" ~ "22CT011-05.0",
      infseq_id == "22CT011-02.1" ~ "22CT011-05.1",
      infseq_id == "22CT011-03.0" ~ "22CT011-08.0",
      infseq_id == "22CT011-03.1" ~ "22CT011-08.1",
      infseq_id == "22CT011-04.0" ~ "22CT011-06.0",
      infseq_id == "22CT011-05.0" ~ "22CT011-12.0",
      infseq_id == "22CT011-06.0" ~ "22CT011-09.0",
      infseq_id == "22CT011-07.0" ~ "22CT011-01.0",
      infseq_id == "22CT011-08.0" ~ "22CT011-04.0",
      infseq_id == "22CT011-09.0" ~ "22CT011-02.0",
      infseq_id == "22CT011-10.0" ~ "22CT011-07.0",
      infseq_id == "22CT011-11.0" ~ "22CT011-20.0",
      infseq_id == "22CT011-12.0" ~ "22CT011-19.0",
      infseq_id == "22CT011-13.0" ~ "22CT011-18.0",
      infseq_id == "22CT011-14.0" ~ "22CT011-16.0",
      infseq_id == "22CT011-15.0" ~ "22CT011-17.0",
      infseq_id == "22CT011-16.0" ~ "22CT011-11.0",
      infseq_id == "22CT011-17.0" ~ "22CT011-13.0",
      infseq_id == "22CT011-18.0" ~ "22CT011-13.0",
      infseq_id == "22CT011-19.0" ~ "22CT011-15.0",
      infseq_id == "22CT011-20.0" ~ "22CT011-14.0"
    )
  ) |>
  arrange(infseq_id)

# patient_data ----
patient_data <- prodigy |>
  arrange(infseq_id, infusion_date) %>%
  filter(infusion_admin) |>
  mutate(
    # Remove study ID
    infseq_id = str_extract(infseq_id, "(?<=-).*$"),
    # Add patient ID to consolidate infusion/reinfusion
    pt_id = str_extract(infseq_id, "^\\d+"),
    dasmt_overall = str_extract(dasmt_overall, "(?<=\\()[^\\)]+"),
    # Combine b-cell and dasmt overall
    disease_assessment = case_when(
      (dasmt_overall == "CR" | dasmt_overall == "CRi") &
        dasmt_bcell_status == "B-cell Aplasia" ~ "CR/CRi + B Cell Aplasia",
      (dasmt_overall == "CR" | dasmt_overall == "CRi") &
        dasmt_bcell_status == "B-cell Recovery" ~ "CR/CRi + B Cell Recovery",
      dasmt_overall == "RD" ~ "RD",
      TRUE ~ dasmt_overall
    )
  ) |>
  mutate(
    # Establish an initial infusion time so that reinfusions can also be mapped chronologically
    # Group by pt_id
    .by = pt_id,
    # Initial infusion date used for some downstream logic
    initial_infusion_date = if_else(infseq_number == 0, infusion_date, first(infusion_date, na_rm = TRUE)),
    # For cases where there has been a reinfusion and no disease assessment yet
    # Use reinfusion date as placeholder with previous disease assessment
    dasmt_date = case_when(
      is.na(dasmt_date) & is.na(disease_assessment) ~ infusion_date,
      TRUE ~ dasmt_date
    ),
    disease_assessment = case_when(
      is.na(disease_assessment) & dasmt_date == infusion_date ~ lag(disease_assessment),
      TRUE ~ disease_assessment
    )
  ) |>
  mutate(
    .by = infseq_id,
    time_from_initial_infusion = interval(initial_infusion_date, dasmt_date) %/% days(1),
    today = interval(infusion_date, Sys.Date()) %/% days(1),
    start_time = time_from_initial_infusion,
    end_time = case_when(
      # If end study reason and last value, tdiff between initial and end study date
      !is.na(end_study_reason) & row_number() == n() ~
        interval(initial_infusion_date, end_study_date) %/% days(1),
      TRUE ~ lead(time_from_initial_infusion)
    )
  ) |>
  # Debatable: removing rows that have no timespan because there is no end date
  # to the status. Only status changes or end study markers can give a range end
  dplyr::filter(!is.na(end_time)) |>
  select(-c(
    infseq_id, infseq_number, dasmt_bcell_status, dasmt_overall, infusion_admin,
    contains("end_study"), today,
    initial_infusion_date, time_from_initial_infusion, contains("date")
  )) |>
  dplyr::relocate(pt_id, .before = everything()) |>
  # Convert to months
  mutate(
    start_time = round(start_time / 30.417, digit = 1), # average days in a month.
    end_time = round(end_time / 30.417, digit = 1)
  )

# infusion_events ----

infusion_events <- prodigy |>
  mutate(
    # Remove study ID
    infseq_id = str_extract(infseq_id, "(?<=-).*$"),
    # Add patient ID to consolidate infusion/reinfusion
    pt_id = str_extract(infseq_id, "^\\d+"),
    infseq_number,
    infusion_date
  ) |>
  mutate(
    .by = pt_id,
    pt_id,
    initial_infusion_date = if_else(infseq_number == 0, infusion_date, first(infusion_date, na_rm = TRUE)),
    time_from_initial_infusion = case_when(
      infseq_number == 0 ~ 0,
      TRUE ~ interval(initial_infusion_date, infusion_date) %/% days(1)
    ),
    # Convert to months
    time_from_initial_infusion = round(time_from_initial_infusion / 30.417, digit = 1) # average days in a month
  ) |>
  dplyr::filter(pt_id %in% patient_data$pt_id &
    !is.na(initial_infusion_date)) |>
  select(pt_id, time_from_initial_infusion) |>
  unique() |>
  mutate(infusion_type = dplyr::if_else(time_from_initial_infusion == 0, "Initial Infusion", "Reinfusion"))

# end_study_events ----

end_study_events <- prodigy |>
  mutate(
    .keep = "none",
    # Remove study ID
    infseq_id = str_extract(infseq_id, "(?<=-).*$"),
    # Add patient ID to consolidate infusion/reinfusion
    pt_id = str_extract(infseq_id, "^\\d+"),
    initial_infusion_date = if_else(infseq_number == 0, infusion_date, first(infusion_date, na_rm = TRUE)),
    time_from_initial_infusion = interval(initial_infusion_date, end_study_date) %/% days(1),
    # Convert to months
    time_from_initial_infusion = round(time_from_initial_infusion / 30.417, digit = 1), # average days in a month
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
  filter(!is.na(end_study_label) &
    !is.na(initial_infusion_date)) |> # Remove Screen Fail
  unique() |>
  dplyr::filter(pt_id %in% patient_data$pt_id) |>
  select(pt_id, time_from_initial_infusion, end_study_label, end_study_name)

# Save data ----
usethis::use_data(patient_data, overwrite = TRUE)
usethis::use_data(infusion_events, overwrite = TRUE)
usethis::use_data(end_study_events, overwrite = TRUE)

# Uncomment below for proofing and testing
# POC: geom_segment ----
# Cant work with legend because shares layer type with color, not fill
# p <- patient_data |>
#   # ggswim(aes(x = final_time, y = pt_id, fill = disease_assessment), position = "identity")
#   ggplot() +
#   geom_segment(aes(x = start_time, xend = end_time,  y = pt_id, colour = disease_assessment), linewidth = 10) +
#   geom_point(data = infusion_events,
#              aes(x = time_from_initial_infusion, y = pt_id)) +
#   geom_label(data = end_study_events,
#              aes(x = time_from_initial_infusion, y = pt_id, label = end_study_label), show.legend = TRUE)
#
# p

# ggswim ----
# Uncomment for testing
# ggswim(
#   patient_data |> arrange(desc(delta_t0)),
#   mapping = aes(x = delta_t0_months, y = pt_id, fill = disease_assessment),
#   arrow = arrow_status,
#   arrow_head_length = unit(.25, "inches"),
#   arrow_neck_length = delta_today,
#   width = 0.25, position = "identity"
# ) +
#   add_marker(
#     data = end_study_events |> dplyr::rename("Status Markers" = end_study_name),
#     aes(x = delta_t0_months, y = pt_id, label_vals = end_study_label, label_names = `Status Markers`),
#     label.size = NA, fill = NA, size = 5
#   )  +
#   add_marker(
#     data = infusion_events,
#     aes(x = delta_t0_months, y = pt_id, name = "Infusion"), color = "#25DA6D",
#     size = 5, position = "identity", alpha = .75
#   ) +
#   scale_fill_manual(
#     name = "Overall Disease Assessment",
#     values = c("#6394F3", "#F3C363", "#EB792F")
#   ) +
#   labs(title = "Prodigy Swimmer Plot", subtitle = "Test", caption = "Test") +
#   xlab("Time Since Infusion (Months)") + ylab("Patient ID") +
#   theme_ggswim()

# nolint end
