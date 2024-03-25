# Load Libraries ----
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
db <- read_redcap(Sys.getenv("REDCAP_URI"),
                  Sys.getenv("PRODIGY_REDCAP_API"))

db <- db |>
  extract_tibbles()

db_tbls <- db |>
  db_list_select(tbls = c(infusion_sequence, infusion, disease_assessment, end_of_study)) |>
  join_data()

prodigy <- db_tbls |>
  select(infseq_id,
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

# Manipulate Data ----
patient_data <- prodigy |>
  filter(infusion_admin) |>
  mutate(
    # Add patient ID to consolidate infusion/reinfusion
    pt_id = str_extract(infseq_id, "(?<=-)\\d{2}")
  ) |>
  mutate(
    .by = pt_id,
    keep = "none",
    infusion_type = if_else(infseq_number > 0, "Reinfusion", "Infusion"),
    # Remove study ID
    infseq_id = str_extract(infseq_id, "(?<=-).*$"),
    infusion_date,
    initial_infusion_date = if_else(infusion_type == "Infusion", infusion_date, first(infusion_date, na_rm = TRUE)),
    # Data for arrow neck length
    today = interval(initial_infusion_date, Sys.Date()) %/% days(1),
    # Zero out infusion time
    infusion_time = if_else(infusion_type == "Infusion", 0, interval(initial_infusion_date, infusion_date) %/% days(1)),
    # Set dasmt interval from zero time
    dasmt_date,
    dasmt_time = interval(initial_infusion_date, dasmt_date) %/% days(1),
    dasmt_bcell_status,
    # Extract abbrev from ()
    dasmt_overall = str_extract(dasmt_overall, "(?<=\\()[^\\)]+"),
    # Set end study time interval from zero time
    end_study_date,
    end_study_time = interval(initial_infusion_date, end_study_date) %/% days(1),
    end_study_reason
  ) |>
  mutate(
    .by = infseq_id,
    # Remove duplicated end_study_times
    end_study_time = ifelse(row_number() == n(), end_study_time, NA)
  ) |>
  pivot_longer(
    cols = c(ends_with("_time")),
    names_to = "time_marker",
    values_to = "time_value"
  ) |>
  # Change time to months
  mutate(
    time_value_months = round(time_value/30.417, digit=0), # average days in a month
    time_from_today = round(today/30.417, digit=0)
  ) |>
  filter(!is.na(time_value)) |>
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
  ungroup() |>
  # Remove unnecessary columns for display
  select(-contains("_date"), today)

end_study_status <- patient_data |>
  mutate(
    end_study_label = case_when(
     end_study_reason == "Completed study follow-up" ~ "✅" ,
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
  filter(time_marker == "end_study_time" & !is.na(end_study_label))

infusion_events <- patient_data |>
  select(pt_id, infusion_type, time_marker, time_value_months) |>
  mutate(
    .keep = "none",
    .by = pt_id,
    pt_id,
    infusion_type,
    infusion_time_value = case_when(
      infusion_type == "Infusion" ~ 0,
      time_marker == "infusion_time" ~ time_value_months,
      TRUE ~ NA
    )
  ) |>
  filter(!is.na(infusion_time_value)) |>
  unique()

# Save data ----
usethis::use_data(patient_data, overwrite = TRUE)

# ggswim ----
# Uncomment for testing
ggswim(
  patient_data,
  mapping = aes(x = time_value_months, y = pt_id, fill = dasmt_overall),
  arrow = arrow_status,
  arrow_head_length = unit(.25, "inches"),
  arrow_neck_length = time_from_today,
  width = 0.25
) +
  add_marker(
    patient_data |> rename("Status Markers" = dasmt_bcell_status),
    aes(x = time_value_months, y = pt_id, color = `Status Markers`, shape = `Status Markers`),
    size = 5, position = "identity", alpha = 1
  ) +
  add_marker(
    end_study_status,
    aes(x = time_value_months, y = pt_id, label = end_study_label, color = end_study_name),
    label.size = NA, fill = NA, size = 5
  ) +
  add_marker(
    infusion_events,
    aes(x = infusion_time_value, y = pt_id, color = infusion_type, shape = infusion_type),
    size = 5, position = "identity", alpha = 1
  ) +
  scale_colour_manual(
    values = c("firebrick", "#F5EB0A", "gray50", NA, NA, NA, "#25DA6D", "#25DA6D")
  ) +
  scale_shape_manual(
    values = c(19, 19, 15, 17, 18)
  ) +
  scale_fill_manual(
    name = "Overall Disease Assessment",
    values = c("#6394F3", "#F3C363", "#EB792F")
  ) +
  labs(title = "Prodigy Swimmer Plot") +
  xlab("Time (Months)") + ylab("Patient ID") +
  theme_ggswim()
