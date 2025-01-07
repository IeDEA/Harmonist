# Users can create scripts unique to the needs of their research group and 
# add them to this file.

# createSpecificReportContent should return a LIST of tables formatted for RMarkdown reports.
# The tables returned by this function will be included in the Toolkit reports
# in the Dataset Summary section. 
# This function is called automatically from the generateDatasetSummary function
# in downloadReports.R when a user clicks Generate Report in Step 3 of the Toolkit
# 
createSpecificReportContent <- function(tableData, reportFormat){
  # tableData is the list of tables already formatted to the data model; coded fields 
  # are already factors, using the labels. If they are blank, they have the value 
  # specified in the variable missingCode in specificDefinitions.R (e.g.,"Missing")

  
  ### OUTCOMES TABLE
  outcomesTableTitle <- "Outcomes"
  counts <- table(tableData$ADMIN_OUTCOME$patient_status)
  percent <- round(counts/sum(counts)*100, 2)
  outcomes_table <- merge(counts, percent, by = "Var1")
  names(outcomes_table) <- c("Category", "Count", "Percent")
  
  ### HBV TABLE
  hbvTableTitle <- "HBV"
  tableData$ADMIN_OUTCOME <- tableData$ADMIN_OUTCOME %>%
    mutate(
      hbv_dx_dt = as.Date(hbv_dx_dt),
      enroll_dt = as.Date(enroll_dt)
    )
  
  total_records <- nrow(tableData$ADMIN_OUTCOME)
  ever_started_hbv_total <- sum(!is.na(tableData$ADMIN_OUTCOME$hbv_dx_dt))
  before_registration_total <- sum(tableData$ADMIN_OUTCOME$hbv_dx_dt < tableData$ADMIN_OUTCOME$enroll_dt, na.rm = TRUE)
  at_and_after_registration_total <- sum(tableData$ADMIN_OUTCOME$hbv_dx_dt >= tableData$ADMIN_OUTCOME$enroll_dt, na.rm = TRUE)
  
  percent_ever_started_hbv <- (ever_started_hbv_total / total_records) * 100
  percent_before_registration <- (before_registration_total / ever_started_hbv_total) * 100
  percent_at_and_after_registration <- (at_and_after_registration_total / ever_started_hbv_total) * 100
  percent_on_hbv_active_medication <- (before_registration_total + at_and_after_registration_total) / ever_started_hbv_total * 100
  
  hbv_table <- data.frame(
    Category = c("Ever started HBV",
                 # "On HBV active medication", 
                 "On HBV before registration", 
                 "On HBV at and after registration"),
    Total = c(
      ever_started_hbv_total,
      # before_registration_total + at_and_after_registration_total,
      before_registration_total,
      at_and_after_registration_total
    ),
    Percentage = c(
      sprintf("%d/%d (%.1f%%)", ever_started_hbv_total, total_records, percent_ever_started_hbv),
      # sprintf("%d/%d (%.1f%%)", before_registration_total + at_and_after_registration_total, ever_started_hbv_total, percent_before_registration),
      sprintf("%d/%d (%.1f%%)", before_registration_total, ever_started_hbv_total, percent_before_registration),
      sprintf("%d/%d (%.1f%%)", at_and_after_registration_total, ever_started_hbv_total, percent_at_and_after_registration)
    ),
    stringsAsFactors = FALSE
  )
  
  if (reportFormat == "html"){
    outcomes_table <- kbl(outcomes_table, format = "html", caption = outcomesTableTitle) %>% 
      kable_styling("bordered", full_width = FALSE, position = "left")
    
    hbv_table <- kbl(hbv_table, format = "html", caption = hbvTableTitle) %>% 
      kable_styling("bordered", full_width = FALSE, position = "left") %>%
      pack_rows("On HBV active medication", 2, 3)
    
  }
  else {
    outcomes_table <- kbl(outcomes_table, format = "latex", longtable = T, booktabs = T, caption = outcomesTableTitle)
    hbv_table <- kbl(hbv_table, format = "latex", longtable = T, booktabs = T, caption = hbvTableTitle)
  }
  # this function should return NULL if no custom report tables were created:
  # return(NULL)
  # otherwise it should return a named LIST of tables
  return(
    list(outcomes = outcomes_table, 
         hbv = hbv_table)
  )
}

createSpecificReportPlots <- function(tableData, rawTableData, reportFormat){
  # tableData is the list of tables already formatted to the data model; coded fields 
  # are already factors, using the labels. If they are blank, they have the value 
  # specified in the variable missingCode in specificDefinitions.R (e.g.,"Missing")

  ### PERSON TIME DISTRIBUTION HISTOGRAM
  person_time_table <- tableData$ADMIN_OUTCOME %>%
    mutate(
      enroll_dt = as.Date(enroll_dt),
      last_activity_dt = as.Date(last_activity_dt),
      person_time_years = as.numeric(difftime(last_activity_dt, enroll_dt, units = "days")) / 365.25,
      person_time_months = as.numeric(difftime(last_activity_dt, enroll_dt, units = "days")) / 30
    )
  
  # filter out person_time_years greater than 10 and < 0
  person_time_table <- subset(person_time_table, person_time_years <= 10 & person_time_years > 0)
  
  person_time_summary_stats <- person_time_table %>%
    summarise(
      total_records = n(),
      # 0
      exactly_0 = sum(as.integer(person_time_months) == 0, na.rm = TRUE),
      # > 0 less than 3 months
      zero_to_3_months = sum(as.integer(person_time_months) > 0 & as.integer(person_time_months) < 3),
      # 3 to 6 months
      three_to_6_months = sum(as.integer(person_time_months) > 3 & as.integer(person_time_months) < 6),
      # 6 months to 12 months (6 months to 1 year)
      six_to_12_months = sum(as.integer(person_time_months) > 6 & as.integer(person_time_months) < 12),
      # 12 months to 48 months (1 year to 4 years)
      twelve_to_4_years = sum(as.integer(person_time_months) > 12 & as.integer(person_time_months) < 48),
      # > 48 months (greater than 4 years)
      greater_than_4_years = sum(as.integer(person_time_months) > 48)

    ) %>%
    mutate(
      percent_exactly_0 = (exactly_0 / total_records) * 100,
      percent_0_to_3_months = (zero_to_3_months / total_records) * 100,
      percent_3_to_6_months = (three_to_6_months / total_records) * 100,
      percent_6_to_12_months = (six_to_12_months / total_records) * 100,
      percent_12_to_4_years = (twelve_to_4_years / total_records) * 100,
      percent_greater_than_4_years = (greater_than_4_years / total_records) * 100
    )
  
  person_time_histogram_plot <- ggplot(person_time_table, aes(x = person_time_years)) +
    geom_histogram(binwidth = 0.5, color = "black") +
    labs(title = "Person Time", x = "Person Time (Years)", y = "Number of Persons") +
    theme_minimal()
  
  person_time_summary_table <- paste(
    "Exactly 0: ", person_time_summary_stats$exactly_0, " (", round(person_time_summary_stats$percent_exactly_0, 2), "%)", "\n",
    "0 to 3 Months: ", person_time_summary_stats$zero_to_3_months, " (", round(person_time_summary_stats$percent_0_to_3_months, 2), "%)", "\n",
    "3 to 6 Months: ", person_time_summary_stats$three_to_6_months, " (", round(person_time_summary_stats$percent_3_to_6_months, 2), "%)", "\n",
    "6 Months to 1 Year: ", person_time_summary_stats$six_to_12_months, " (", round(person_time_summary_stats$percent_6_to_12_months, 2), "%)", "\n",
    "1 to 4 Years: ", person_time_summary_stats$twelve_to_4_years, " (", round(person_time_summary_stats$percent_12_to_4_years, 2), "%)", "\n",
    "> 4 Years: ", person_time_summary_stats$greater_than_4_years, " (", round(person_time_summary_stats$percent_greater_than_4_years, 2), "%)"
  )
  
  person_time_histogram <- person_time_histogram_plot +
    annotate("text", x = Inf, y = Inf, label = person_time_summary_table, hjust = 1.1, vjust = 1.1, size = 4, color = "black", fontface = "italic", lineheight = 0.9) +
    xlim(0, NA) + 
    theme(plot.margin = margin(1, 1, 3, 1, "cm"))
  
  ### ALT HISTOGRAM
  alt_aggregated_data <- tableData$ACTIVITY %>%
    group_by(record_id) %>%
    summarise(num_alt_entries = n()) %>%
    ungroup()
  
  total_records_alt <- nrow(alt_aggregated_data)
  records_with_2_or_more_alt <- sum(alt_aggregated_data$num_alt_entries >= 2, na.rm = TRUE)
  percent_2_or_more_alt <- (records_with_2_or_more_alt / total_records_alt) * 100
  
  alt_summary_stats <- alt_aggregated_data %>% summarise(median_alt = median(num_alt_entries, na.rm = TRUE), mean_alt = mean(num_alt_entries, na.rm = TRUE))
  
  alt_histogram <- ggplot(alt_aggregated_data, aes(x = num_alt_entries)) +
    geom_histogram(binwidth = 1, fill = "#FF9999", color = "black") +
    labs(title = "ALT",
         x = "Number of ALT results",
         y = "Number of Persons") +
    theme_minimal() +
    xlim(0, 30) +
    annotate("text", x = Inf, y = Inf,
             label = paste("Median:", round(alt_summary_stats$median_alt, 2), "\n",
                           "Mean:", round(alt_summary_stats$mean_alt, 2), "\n",
                           sprintf("Participants with 2 or more ALT: %d/%d (%.2f%%)", records_with_2_or_more_alt, total_records_alt, percent_2_or_more_alt)),
             hjust = 1.1, vjust = 1.1, size = 4, color = "black", 
             fontface = "italic", 
             lineheight = 0.9)
  
  ### OUTCOMES BAR CHART
  outcomes_filtered_data <- subset(tableData$ADMIN_OUTCOME, patient_status %in% c(
    "Currently active", 
    "Died", 
    "Lost to follow-up (no new data [visits, labs, etc] for at least the past 1 year) BUT known to be alive",
    "Transferred out (to another clinic/program or patient moved away)",
    "Patient withdrew from cohort/clinical care"
  ))
  outcomes_filtered_data$patient_status <- as.character(outcomes_filtered_data$patient_status)
  outcomes_filtered_data$patient_status[outcomes_filtered_data$patient_status == "Currently active"] <- "Active"
  outcomes_filtered_data$patient_status[outcomes_filtered_data$patient_status == "Lost to follow-up (no new data [visits, labs, etc] for at least the past 1 year) BUT known to be alive"] <- "LTFU > 12m"
  outcomes_filtered_data$patient_status[outcomes_filtered_data$patient_status == "Transferred out (to another clinic/program or patient moved away)"] <- "Transferred"
  outcomes_filtered_data$patient_status[outcomes_filtered_data$patient_status == "Patient withdrew from cohort/clinical care"] <- "Withdrawn"
  
  outcomes_bar_chart <- ggplot(outcomes_filtered_data, aes(x = patient_status, fill = patient_status)) +
    geom_bar() +
    labs(title = "Outcomes", x = "", y = "Number of Records", fill = "Outcome") +
    theme_minimal()
  
  #### LOSS TO FOLLOW UP PATTERNS BAR CHART
  ltfu_filtered_data <- tableData$ADMIN_OUTCOME %>% filter(!is.na(ltfu_lastcontact_dt))
  ltfu_filtered_data <- ltfu_filtered_data %>% mutate(years_ago = as.numeric(difftime(Sys.Date(), ltfu_lastcontact_dt, units = "days")) / 365.25)
  
  ltfu_filtered_data <- ltfu_filtered_data %>%
    mutate(ltfu_category = case_when(
      years_ago >= 1 & years_ago < 2 ~ "1-2 years ago",
      years_ago >= 2 & years_ago < 4 ~ "2-4 years ago",
      years_ago >= 5 ~ "5 or more years ago",
      TRUE ~ NA_character_  
    )) %>%
    filter(!is.na(ltfu_category))
  
  ltfu_bar_chart <- ggplot(ltfu_filtered_data, aes(x = ltfu_category)) + geom_bar() +
    labs(title = "Loss to Follow Up Patterns", x = "", y = "Number of Persons") +
    theme_minimal()
  
  # this function should return NULL if no custom report plots were created:
  # return(NULL)
  # otherwise it should return a named LIST of plots
  return(
    list(person_time = person_time_histogram, 
         alt = alt_histogram, 
         outcomes = outcomes_bar_chart, 
         ltfu = ltfu_bar_chart)
  )
}