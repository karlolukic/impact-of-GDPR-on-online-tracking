# ------------------------------------------------------------------------------
# Title: Reproducible Code for "The Impact of the General Data Protection Regulation (GDPR) on Online Tracking"
# Authors: Klaus M. Miller, Karlo Lukic, Bernd Skiera
# Script Author: Karlo Lukic
# Date: January 10, 2025
# ------------------------------------------------------------------------------

# Description:
# This R script reproduces all tables and figures included in the main text and web appendix of the paper
# titled "The Impact of the General Data Protection Regulation (GDPR) on Online Tracking."
# The code processes the underlying data, applies econometric models, and outputs the final results as 
# presented in the paper. It covers Difference-in-Differences (DiD) and Synthetic Control analyses.

# Repository:
# GitHub: https://github.com/karlolukic/impact-of-GDPR-on-online-tracking-2025-01-10
# TODO: Posit (RStudio Cloud): https://rstudio.cloud/project/<project-id>

# Data Sets:
# - Public Data: The WhoTracks.me data used in this analysis is publicly available at:
#   https://github.com/whotracksme/whotracks.me
# - Proprietary Data: Filtered version of proprietary SimilarWeb data set is used to ensure reproducibility.

# Licensing:
# This script is released under the MIT License, allowing open access and modification.
# Please cite our paper if you use this script in your work.

# Dependencies:
# See the script:./02_r_scripts/packages_and_functions-2025-01-10-KL.R

# Acknowledgments:
# The authors thank the WhoTracks.me team for providing the data for this research project.
# This script uses data from public and proprietary sources as described in the paper. 
# Proprietary data is not included in this script.

# Contact:
# Karlo Lukic (karlo.lukic@protonmail.com)
# ------------------------------------------------------------------------------



#################################### SETUP #####################################

# load packages and functions --------------------------------------------------

source("./02_r_scripts/packages_and_functions-2025-01-10-KL.R")


# set flextable package settings ----------------------------------------------

set_flextable_defaults(
  font.family = "Times", font.size = 10, font.color = "black", # default font size = 10
  text.align = "left", table.layout = "autofit",
  theme_fun = "theme_booktabs", 
  digits = 3,
  decimal.mark = ".", big.mark = ",", na_str = "",
  line_spacing = 1,
  padding = 0
)


# set fixest package settings -------------------------------------------------

setFixest_dict(c(
  n_tracker_providers = "Number of Tracker Providers",
  n_trackers = "Number of Trackers",
  n_trackers_broadly_essential = "Number of Essential Trackers",
  n_trackers_broadly_non_essential = "Number of Non-Essential Trackers",
  n_trackers_essential = "Tag Managers, Error Reports and Performance",
  n_trackers_consent = "Consent",
  n_trackers_advertising = "Advertising",
  n_trackers_site_analytics = "Analytics",
  n_trackers_privacy_friendly_site_analytics = "Privacy-Friendly Analytics",
  n_trackers_social_media = "Social Media",
  n_trackers_comments = "Comments",
  n_trackers_hosting = "Hosting",
  n_trackers_cdn = "CDN",
  n_trackers_audio_video_player = "Audio Video",
  n_trackers_misc = "Misc",
  n_trackers_customer_interaction = "Customer Interaction",
  n_trackers_unknown = "Unknown",
  n_trackers_pii_collect_status_round2_Yes = "PII Collect: Yes",
  n_trackers_pii_collect_status_round2_No = "PII Collect: No",
  n_trackers_pii_collect_status_round2_Undisclosed = "PII Collect: Unknown",
  n_trackers_pii_collect_status_round2_NA = "PII Collect: No Match",
  n_trackers_pii_share_status_round2_Yes = "PII Share: Yes",
  n_trackers_pii_share_status_round2_No = "PII Share: No",
  n_trackers_pii_share_status_round2_Undisclosed  = "PII Share: Unknown",
  n_trackers_pii_share_status_round2_NA = "PII Share: No Match",
  n_trackers_do_not_collect_or_share_pii = "Not Collecting PII",
  n_trackers_collect_does_not_share_pii = "Collecting PII",
  n_trackers_collect_and_share_pii = "Collecting and Sharing PII",
  n_trackers_unknown_pii = "Unknown (Undisclosed or No Match)",
  n_companies_market_share_level_High = "Number of Tracker Providers with High Market Share", 
  n_companies_market_share_level_Low = "Number of Tracker Providers with Low Market Share",
  n_trackers_from_high_market_share_providers_pre_post_GDPR = "Number of Trackers from High Market Share Tracker Providers",
  n_trackers_from_low_market_share_providers_pre_post_GDPR = "Number of Trackers from Low Market Share Tracker Providers",
  `(Intercept)` = "Constant",
  site_category = "Type of Publisher:",
  publisher_industry = "Type of Publisher:",
  post_GDPR1 = "PostGDPR",
  post_GDPR = "PostGDPR",
  post_GDPR_fine1 = "PostFine",
  post_GDPR_fine = "PostFine",
  is_treated_target_audience = "Treatment",
  is_treated_cb = "Treatment",
  is_treated_server = "Treatment",
  is_treated_TLD = "Treatment",
  period_id = "Month ID",
  site_instance_id = "Publisher Instance ID",
  site_id = "Publisher ID",
  fake_post_GDPR_one = "PostPretend",
  fake_post_GDPR_two = "PostPretend",
  fake_post_GDPR_three = "PostPretend",
  fake_post_GDPR_four = "PostPretend",
  fake_post_GDPR_five = "PostPretend",
  fake_post_GDPR_six = "PostPretend",
  fake_post_GDPR_seven = "PostPretend",
  fake_post_GDPR_eight = "PostPretend",
  fake_post_GDPR_nine = "PostPretend",
  fake_post_GDPR_ten = "PostPretend",
  fake_post_GDPR_eleven = "PostPretend",
  number_of_users = "Number of Users",
  after_GDPR = "PostGDPR",
  n_days_from_2018_04_01 = "Time",
  linear_trend = "Time"
))
setFixest_notes(FALSE) # remove reporting of removing coefficients due to collinearity


# read data sets ----------------------------------------------------------

# main panel data set (= long)
global_sites_trackers_panel_DT <- read_rds(file = "./01_data/global_sites_trackers_panel_balanced.rds")

# main non-panel data set (= wide)
global_sites_trackers_DT <- read_rds(file = "./01_data/global_sites_trackers_balanced.rds")

# panel data set of EU/US users (= long)
eu_us_sites_trackers_panel_DT <- read_rds(file = "01_data/eu_us_sites_trackers_panel.rds")

# internet archive data of Ghostery users (used in web appendix)
ia_tbl <- read_rds(file = "01_data/ia_tbl.rds")

# publicly available SimilarWeb data set (used in web appendix)
sw_public_DT <- read_rds("01_data/merged_similar_web_data.rds")

# proprietary SimilarWeb data set (used in web appendix)
# NOTE: Given that this data set is proprietary, we cannot provide the data set in the repository.
# Instead, we use the filtered version of the data set to reproduce the results.
sw_proprietary_filtered_DT <- read_rds(file = "01_data/sw_proprietary_filtered.rds")

# data set for generalized synthetic control method (used in web appendix)
synth_global_sites_trackers_DT <- read_rds("01_data/global_sites_trackers_panel.rds")

# unbalanced panel data set (used in web appendix)
unbalanced_global_sites_trackers_DT <- read_rds("./01_data/global_sites_trackers_panel_unbalanced.rds")


################## REPRODUCE TABLES AND FIGURES IN MAIN TEXT ###################

# Figure 1: Main Actors Involved in Online Tracking --------------

pdf_render_page("03_results/figure_01.pdf", dpi = 300) %>% 
  magick::image_read() %>% 
  plot()

# Figure 2: Tracker Providers Collecting Data Across Users and Publishers --------------

pdf_render_page("03_results/figure_02.pdf", dpi = 300) %>% 
  magick::image_read() %>% 
  plot()

# Table 1: Categorization of Online Trackers by Purpose and Necessity --------------

t <- read_excel(here::here("01_data", "lookup_tables", 
                           "tracker_category_description-2025-01-09.xlsx"), 
                col_names = T
) %>% 
  flextable() %>% 
  set_header_labels(purpose = "Purpose",
                    purpose_description = "Description of Purpose",
                    examples_of_trackers = "Examples of Trackers",
                    defined_by = "Defined By", 
                    necessity = "Necessity", 
                    necessity_description = "Description of Necessity"
                    ) %>% 
  autofit() %>% 
  merge_v(j = c("necessity", "necessity_description"))  %>%
  fix_border_issues() %>% 
  vline(j = "defined_by", border = std_border, part = "body") %>% 
  hline(i = 5, border = std_border) %>%
  flextable::align(align = "left", part = "all") %>%
  flextable::add_footer_lines("") %>%  # add empty line
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 8, part = "all") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_01.docx")


# Table 2: Description of the Data Sets --------------

t <- read_excel(here::here("01_data", "lookup_tables",
                           "description_of_data_sources-2025-01-09.xlsx"), 
           col_names = T
) %>% 
  flextable() %>% 
  merge_v(j = c("Data Set", "Source")) %>% 
  fix_border_issues() %>%
  hline(i = c(6, 8, 11), border = std_border) %>%
  flextable::align(align = "left", part = "all") %>%
  flextable::add_footer_lines("") %>%  # add empty line
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 9, part = "all") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_02.docx")


# referenced numbers in Table 2 for Evidon data set

tracker_summary <- data.table(
  `Summary Statistic` = 
    c(
      # "Total Number of Unique Trackers", 
      "Number of Unique Trackers that Disclose Personal Data Collection and Sharing", 
      "Number of Unique Trackers that Do Not Disclose Personal Data Collection Only",
      "Number of Unique Trackers that Do Not Disclose Personal Data Sharing Only",
      "Number of Unique Trackers that Do Not Disclose Either",
      "Number of Unique Trackers with No Match"),
  Count = c(
    # n_distinct(global_sites_trackers_DT$tracker),
    n_distinct(global_sites_trackers_DT$tracker[global_sites_trackers_DT$pii_collect_status_round2 %in% 
                                                  c("Yes", "No") &
                                                  global_sites_trackers_DT$pii_share_status_round2 %in% 
                                                  c("Yes", "No")]),
    n_distinct(global_sites_trackers_DT$tracker[global_sites_trackers_DT$pii_collect_status_round2 %in% 
                                                  c("Undisclosed") & global_sites_trackers_DT$pii_share_status_round2 != 
                                                  c("Undisclosed")]),
    n_distinct(global_sites_trackers_DT$tracker[global_sites_trackers_DT$pii_share_status_round2 %in% 
                                                  c("Undisclosed") & global_sites_trackers_DT$pii_collect_status_round2 != 
                                                  c("Undisclosed")]),
    n_distinct(global_sites_trackers_DT$tracker[global_sites_trackers_DT$pii_collect_status_round2 %in% 
                                                  c("Undisclosed") &
                                                  global_sites_trackers_DT$pii_share_status_round2 %in% 
                                                  c("Undisclosed")]),                                                        
    n_distinct(global_sites_trackers_DT$tracker[is.na(global_sites_trackers_DT$pii_collect_status_round2) &
                                                  is.na(global_sites_trackers_DT$pii_share_status_round2)]))
)
tracker_summary %>% 
  mutate(Percentage = round((Count / n_distinct(global_sites_trackers_DT$tracker)) * 100, 2)) %>% 
  add_row(`Summary Statistic` = "∑",
          Count = sum(.$Count),
          Percentage = sum(.$Percentage)) %>% 
  flextable() %>% 
  set_header_labels(Percentage = "(%)") %>% 
  flextable::colformat_double(j = c(3), prefix = "(", suffix = "%)", digits = 1) %>% 
  flextable::hline(i = c(1, 5)) %>% 
  flextable::autofit() %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines(values = "Notes: Values in cells add up to 949 unique trackers we observe in our sample across all months. We cannot match 225 (23.7%) trackers in our WhoTracks.me sample with the ones in the Evidon database.")

two_by_two_table <- global_sites_trackers_DT %>%
  filter(pii_collect_status_round2 %in% c("Yes", "No"), 
         pii_share_status_round2 %in% c("Yes", "No")) %>%
  distinct(tracker, pii_collect_status_round2, pii_share_status_round2) %>%
  count(pii_collect_status_round2, pii_share_status_round2) %>%
  spread(key = pii_share_status_round2, value = n) %>%
  rename("PII Collect" = pii_collect_status_round2)
two_by_two_table[is.na(two_by_two_table)] <- 0
two_by_two_table$No_Percentage <- round((two_by_two_table$No / 546) * 100, 2)
two_by_two_table$Yes_Percentage <- round((two_by_two_table$Yes / 546) * 100, 2)
two_by_two_table <- two_by_two_table %>%
  mutate(Total = No + Yes,
         Total_Percentage = No_Percentage + Yes_Percentage)
two_by_two_table %>% 
  select(`PII Collect`, No, No_Percentage, Yes, Yes_Percentage, Total, Total_Percentage) %>% 
  add_row(`PII Collect` = "∑",
          No = sum(.$No),
          Yes = sum(.$Yes),
          No_Percentage = sum(.$No_Percentage),
          Yes_Percentage = sum(.$Yes_Percentage),
          Total = sum(.$Total),
          Total_Percentage = sum(.$Total_Percentage)) %>% 
  flextable() %>% 
  flextable::colformat_double(j = c(3, 5, 7), prefix = "(", suffix = "%)", digits = 1) %>% 
  flextable::add_header_row(top = T, values = c("", "PII Share", "PII Share", "PII Share", "PII Share", "∑", "∑")) %>% 
  set_header_labels(No_Percentage = "(%)", Yes_Percentage = "(%)", Total_Percentage = "(%)", Total = "∑") %>% 
  flextable::hline(i = 2) %>%
  flextable::vline(j = c(1, 5)) %>%
  flextable::merge_h(part = "header") %>% 
  flextable::align(part = "header", align = "center") %>% 
  flextable::autofit() %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines(values = "Notes: Values in cells add up to 546 unique trackers \n that disclose data collection and sharing.")


# Table 3: Distribution of Observations (Monthly Publishers) Across Publisher Designation --------------

rounding_pct_scales_percent <- 0.01

n_sites_global <- global_sites_trackers_panel_DT[, uniqueN(site_id)]
n_sites_adjusted_global <- global_sites_trackers_panel_DT[, uniqueN(site_id)]
t_periods_adjusted_global <- global_sites_trackers_panel_DT[, uniqueN(period_id)]
N_obs_adjusted_global <- global_sites_trackers_panel_DT[, .N]

summarized_DT <- global_sites_trackers_panel_DT[, .(n_obs = .N),
                                                         by = .(target_audience, is_treated_target_audience)
] %>%
  mutate(prop = n_obs / sum(n_obs)) %>% 
  mutate(target_audience = case_when(target_audience == "EU" ~ "EU publisher",
                                     target_audience == "Non-EU" ~ "Non-EU publisher", 
                                     TRUE ~ target_audience))

summarized_wide_DT <-
  summarized_DT %>%
  add_row(
    target_audience = "∑",
    n_obs = sum(.$n_obs),
    prop = sum(.$prop)
  ) %>%
  mutate(n_obs_fmt = big_mark(n_obs)) %>%
  mutate(prop_fmt = paste0("(", scales::percent(prop, rounding_pct_scales_percent), ")")) %>%
  mutate(n_obs_prop_fmt = paste(n_obs_fmt, prop_fmt, sep = " ")) %>%
  select(
    target_audience,
    n_obs_prop_fmt
  )

total_n_obs <- summarized_DT %>%
  summarize(sum(n_obs)) %>%
  pull() %>%
  big_mark()
ctrl_group_obs <- summarized_DT %>%
  filter(is_treated_target_audience == 0) %>%
  select(n_obs) %>%
  pull() %>%
  big_mark()
treat_group_obs <- summarized_DT %>%
  filter(is_treated_target_audience == 1) %>%
  summarize(sum_n_obs = sum(n_obs)) %>%
  select(sum_n_obs) %>%
  pull() %>%
  big_mark()

t <- summarized_wide_DT %>%
  flextable() %>%
  set_header_labels(
    target_audience = "Publisher Designation",
    n_obs_prop_fmt = "Number and Percentage of Observations"
  ) %>%
  flextable::align(align = "center", part = "all") %>%
  vline(j = 1, border = std_border, part = "header") %>%
  vline(j = 1, border = std_border, part = "body") %>%
  hline(i = 2, border = thick_border, part = "body") %>%
  bg(i = 1, j = 2, part = "body", bg = "white") %>%
  bg(i = 2, j = 2, part = "body", bg = "lightgray") %>%
  footnote(
    i = 1, j = 1,
    value = as_paragraph('A publisher is designated as an “EU publisher” if (1) the publisher uses an EU top-level domain (e.g., .de) or (2) the publisher receives more traffic from EU than non-EU users.'),
    ref_symbols = c("1"), part = "body", inline = T, sep = " "
  ) %>% 
  footnote(
    i = 2, j = 1,
    value = as_paragraph('A publisher is designated as a “non-EU publisher” if (1) the publisher uses a non-EU top-level domain (e.g., .com) and (2) the publisher receives more traffic from non-EU users than EU users.'),
    ref_symbols = c("2"), part = "body", inline = T, sep = " "
  ) %>% 
  add_footer_lines(
    paste0(
      "Notes: The cells in this table show the number and percentage of observations in our sample corresponding to each case. The cell belonging to the control group—where GDPR does not apply—is colored gray, and the cell belonging to the treatment group—where GDPR applies—is not colored. In total, 23% (N observations = ", treat_group_obs, ") of all observations (N observations = ", total_n_obs, ") belong to the treatment group and 77% (N observations = ", ctrl_group_obs, ") to the control group."
    )
  ) %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/table_03.docx")


# Figure 3: Distribution of the Average Number of Trackers per Publisher ------

figure_annotation_size <- 5
rounding_decimal <- 3

n_sites <- uniqueN(global_sites_trackers_panel_DT$site)
t_periods <- uniqueN(global_sites_trackers_panel_DT$period_id)
N_obs <- nrow(global_sites_trackers_panel_DT)
mean_n_trackers <- mean(global_sites_trackers_panel_DT$n_trackers, na.rm = T)
sd_n_trackers <- sd(global_sites_trackers_panel_DT$n_trackers)
min_n_trackers <- min(global_sites_trackers_panel_DT$n_trackers)
max_n_trackers <- max(global_sites_trackers_panel_DT$n_trackers)

ggplot(global_sites_trackers_panel_DT, aes(x = n_trackers)) +
  geom_histogram(binwidth = 1) +  # Set bin width to 1 for the finest granularity
  scale_x_continuous(breaks = seq(0, 120, by=5)) +
  coord_cartesian(ylim = c(0, 500)) +
  geom_vline(xintercept = mean_n_trackers, linetype = 1, col = "black", linewidth = 1) +
  geom_vline(xintercept = mean_n_trackers + sd_n_trackers, linetype = 9, col = "darkgray", , linewidth = 0.8) +
  geom_vline(xintercept = mean_n_trackers - sd_n_trackers, linetype = 9, col = "darkgray", , linewidth = 0.8) +
  annotate("text",
           x = 90, y = 500,
           label = paste0("N publishers = ", big_mark(n_sites)),
           size = figure_annotation_size, family = "Times"
  ) +
  annotate("text",
           x = 90, y = 450,
           label = paste0("T = ", t_periods, " months"),
           size = figure_annotation_size, family = "Times"
  ) +
  annotate("text",
           x = 90, y = 400,
           label = paste0("N observations = ", big_mark(N_obs)),
           size = figure_annotation_size, family = "Times"
  ) +
  annotate("text",
           x = 90, y = 350,
           label = paste0("mean = ", round(mean_n_trackers, rounding_decimal)),
           size = figure_annotation_size, family = "Times", col = "black"
  ) +
  annotate("text",
           x = 90, y = 300,
           label = paste0("SD = ", round(sd_n_trackers, rounding_decimal)),
           size = figure_annotation_size, family = "Times", col = "black"
  ) +
  annotate("text",
           x = 90, y = 250,
           label = paste0("min = ", min_n_trackers),
           size = figure_annotation_size, family = "Times"
  ) +
  annotate("text",
           x = 90, y = 200,
           label = paste0("max = ", max_n_trackers),
           size = figure_annotation_size, family = "Times"
  ) +
  labs(x = "Number of Trackers per Publisher Across All Months",
       y = "Number of Observations") +
  labs(
    caption = str_wrap("\nNotes: Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408). The black vertical line indicates the mean number of trackers per publisher, while the gray lines represent ± one standard deviation from the mean.", width = 120
    )
  ) +
  theme_classic(base_family = "Times") +
  theme(
    plot.caption = element_text(hjust = 0, size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
    panel.grid.minor = element_blank()
  )

ggsave("03_results/figure_03.pdf", width=8, height=6, font="Times", dpi=300)


# Table 4: Distribution of the Average Number of Trackers per Publisher By Categorizations of Trackers ---------
# (broken down in four separate tables, then appended to a single table)

# Table 4a: Categorization of Trackers by Purpose and Necessity
tracker_summary_wide <- global_sites_trackers_panel_DT[, .(
  # I. broadly essential
  broadly_essential_mean = mean(n_trackers_broadly_essential, na.rm = TRUE),
  broadly_essential_SD = sd(n_trackers_broadly_essential, na.rm = TRUE),
  broadly_essential_min = min(n_trackers_broadly_essential, na.rm = TRUE),
  broadly_essential_max = max(n_trackers_broadly_essential, na.rm = TRUE),
  # I. privacy-friendly site analytics
  privacy_friendly_site_analytics_mean = mean(n_trackers_privacy_friendly_site_analytics, na.rm = TRUE),
  privacy_friendly_site_analytics_SD = sd(n_trackers_privacy_friendly_site_analytics, na.rm = TRUE),
  privacy_friendly_site_analytics_min = min(n_trackers_privacy_friendly_site_analytics, na.rm = TRUE),
  privacy_friendly_site_analytics_max = max(n_trackers_privacy_friendly_site_analytics, na.rm = TRUE),
  # I. essential
  essential_mean = mean(n_trackers_essential, na.rm = TRUE),
  essential_SD = sd(n_trackers_essential, na.rm = TRUE),
  essential_min = min(n_trackers_essential, na.rm = TRUE),
  essential_max = max(n_trackers_essential, na.rm = TRUE),
  # I. consent
  consent_mean = mean(n_trackers_consent, na.rm = TRUE),
  consent_SD = sd(n_trackers_consent, na.rm = TRUE),
  consent_min = min(n_trackers_consent, na.rm = TRUE),
  consent_max = max(n_trackers_consent, na.rm = TRUE),
  # I. CDN
  cdn_mean = mean(n_trackers_cdn, na.rm = TRUE),
  cdn_SD = sd(n_trackers_cdn, na.rm = TRUE),
  cdn_min = min(n_trackers_cdn, na.rm = TRUE),
  cdn_max = max(n_trackers_cdn, na.rm = TRUE),
  # I. hosting
  hosting_mean = mean(n_trackers_hosting, na.rm = TRUE),
  hosting_SD = sd(n_trackers_hosting, na.rm = TRUE),
  hosting_min = min(n_trackers_hosting, na.rm = TRUE),
  hosting_max = max(n_trackers_hosting, na.rm = TRUE),
  # II. broadly non-essential
  broadly_non_essential_mean = mean(n_trackers_broadly_non_essential, na.rm = TRUE),
  broadly_non_essential_SD = sd(n_trackers_broadly_non_essential, na.rm = TRUE),
  broadly_non_essential_min = min(n_trackers_broadly_non_essential, na.rm = TRUE),
  broadly_non_essential_max = max(n_trackers_broadly_non_essential, na.rm = TRUE),
  # II. advertising
  advertising_mean = mean(n_trackers_advertising, na.rm = TRUE),
  advertising_SD = sd(n_trackers_advertising, na.rm = TRUE),
  advertising_min = min(n_trackers_advertising, na.rm = TRUE),
  advertising_max = max(n_trackers_advertising, na.rm = TRUE),
  # II. analytics
  analytics_mean = mean(n_trackers_analytics, na.rm = TRUE),
  analytics_SD = sd(n_trackers_analytics, na.rm = TRUE),
  analytics_min = min(n_trackers_analytics, na.rm = TRUE),
  analytics_max = max(n_trackers_analytics, na.rm = TRUE),
  # II. social_media
  social_media_mean = mean(n_trackers_social_media, na.rm = TRUE),
  social_media_SD = sd(n_trackers_social_media, na.rm = TRUE),
  social_media_min = min(n_trackers_social_media, na.rm = TRUE),
  social_media_max = max(n_trackers_social_media, na.rm = TRUE),
  # II. comments
  comments_mean = mean(n_trackers_comments, na.rm = TRUE),
  comments_SD = sd(n_trackers_comments, na.rm = TRUE),
  comments_min = min(n_trackers_comments, na.rm = TRUE),
  comments_max = max(n_trackers_comments, na.rm = TRUE),
  # II. audio video player
  audio_video_player_mean = mean(n_trackers_audio_video_player, na.rm = TRUE),
  audio_video_player_SD = sd(n_trackers_audio_video_player, na.rm = TRUE),
  audio_video_player_min = min(n_trackers_audio_video_player, na.rm = TRUE),
  audio_video_player_max = max(n_trackers_audio_video_player, na.rm = TRUE),
  # II. misc
  misc_mean = mean(n_trackers_misc, na.rm = TRUE),
  misc_SD = sd(n_trackers_misc, na.rm = TRUE),
  misc_min = min(n_trackers_misc, na.rm = TRUE),
  misc_max = max(n_trackers_misc, na.rm = TRUE),
  # II. customer interaction
  customer_interaction_mean = mean(n_trackers_customer_interaction, na.rm = TRUE),
  customer_interaction_SD = sd(n_trackers_customer_interaction, na.rm = TRUE),
  customer_interaction_min = min(n_trackers_customer_interaction, na.rm = TRUE),
  customer_interaction_max = max(n_trackers_customer_interaction, na.rm = TRUE),
  # II. unknown
  unknown_mean = mean(n_trackers_unknown, na.rm = TRUE),
  unknown_SD = sd(n_trackers_unknown, na.rm = TRUE),
  unknown_min = min(n_trackers_unknown, na.rm = TRUE),
  unknown_max = max(n_trackers_unknown, na.rm = TRUE)
)]

tracker_summary_long_essential_non_essential <- tracker_summary_wide %>% 
  pivot_longer(cols = everything(), 
               names_to = c("Categorization of Trackers By Purpose and Necessity", ".value"), 
               values_to = c("mean", "sd", "min", "max"), 
               names_pattern = "(.*)_(.*)") %>% 
  mutate(`Categorization of Trackers By Purpose and Necessity` = case_match(`Categorization of Trackers By Purpose and Necessity`,
                                                   "broadly_essential" ~ "Essential:",
                                                "privacy_friendly_site_analytics" ~ "Privacy-Friendly Analytics",
                                                 "essential" ~ "Tag Managers, Error Reports and Performance",
                                                 "consent" ~ "Consent",
                                                 "cdn" ~ "Content Delivery Network (CDN)",
                                                 "hosting" ~ "Hosting",
                                                "broadly_non_essential" ~ "Non-Essential:",
                                                 "advertising" ~ "Advertising",
                                                 "analytics" ~ "Analytics",
                                                 "social_media" ~ "Social Media",
                                                 "comments" ~ "Comments",
                                                 "audio_video_player" ~ "Audio Video Player",
                                                 "misc" ~ "Miscellanious",
                                                 "customer_interaction" ~ "Customer Interaction",
                                                 "unknown" ~ "Unknown"
  ))

t <- tracker_summary_long_essential_non_essential %>% 
  flextable() %>% 
  add_header_row(
    values = c(
      "", 
      "Number of Trackers per Publisher Across All Months"
    ),
    top = T,
    colwidths = c(1, 4)
  ) %>% 
  flextable::border_remove() %>% 
  theme_booktabs() %>%
  hline(i = 1, j = c(2, 5), border = std_border, part = "header") %>% 
  hline(i = 1, border = std_border, part = "body") %>% 
  hline(i = 6, border = std_border, part = "body") %>%
  hline(i = 7, border = std_border, part = "body") %>%
  flextable::italic(i = c(1, 7), italic = TRUE, part = "body") %>% 
  flextable::colformat_double(j = c(2:5), digits = 3) %>% 
  # optional:
  # add_footer_lines("Notes: The table displays descriptive statistics for the number of trackers per publisher across all months, ordered by \"Essential\" and \"Non-Essential\" categories of trackers. Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>%
  flextable::align(part = "footer", align = "justify") %>%
  # flextable::add_footer_lines("") %>%
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_04a.docx")
  
# Table 4b: Categorization of Trackers by Tracking Functionality
tracker_summary_wide <- global_sites_trackers_panel_DT[, .(
  # I. Do not collect or share PII
  n_trackers_do_not_collect_or_share_pii_mean = mean(n_trackers_do_not_collect_or_share_pii, na.rm = TRUE),
  n_trackers_do_not_collect_or_share_pii_SD = sd(n_trackers_do_not_collect_or_share_pii, na.rm = TRUE),
  n_trackers_do_not_collect_or_share_pii_min = min(n_trackers_do_not_collect_or_share_pii, na.rm = TRUE),
  n_trackers_do_not_collect_or_share_pii_max = max(n_trackers_do_not_collect_or_share_pii, na.rm = TRUE),
  # II. Collect but do not share PII
  n_trackers_collect_does_not_share_pii_mean = mean(n_trackers_collect_does_not_share_pii, na.rm = TRUE),
  n_trackers_collect_does_not_share_pii_SD = sd(n_trackers_collect_does_not_share_pii, na.rm = TRUE),
  n_trackers_collect_does_not_share_pii_min = min(n_trackers_collect_does_not_share_pii, na.rm = TRUE),
  n_trackers_collect_does_not_share_pii_max = max(n_trackers_collect_does_not_share_pii, na.rm = TRUE),
  # III. Collect and share PII
  n_trackers_collect_and_share_pii_mean = mean(n_trackers_collect_and_share_pii, na.rm = TRUE),
  n_trackers_collect_and_share_pii_SD = sd(n_trackers_collect_and_share_pii, na.rm = TRUE),
  n_trackers_collect_and_share_pii_min = min(n_trackers_collect_and_share_pii, na.rm = TRUE),
  n_trackers_collect_and_share_pii_max = max(n_trackers_collect_and_share_pii, na.rm = TRUE),
  # IV. Unknown PII
  n_trackers_unknown_pii_mean = mean(n_trackers_unknown_pii, na.rm = TRUE),
  n_trackers_unknown_pii_SD = sd(n_trackers_unknown_pii, na.rm = TRUE),
  n_trackers_unknown_pii_min = min(n_trackers_unknown_pii, na.rm = TRUE),
  n_trackers_unknown_pii_max = max(n_trackers_unknown_pii, na.rm = TRUE)
)]

tracker_summary_long_pii_status <- tracker_summary_wide %>% 
  pivot_longer(cols = everything(), 
               names_to = c("Categorization of Trackers By Tracking Functionality", ".value"), 
               values_to = c("mean", "sd", "min", "max"), 
               names_pattern = "(.*)_(.*)") %>% 
  mutate(`Categorization of Trackers By Tracking Functionality` = case_match(`Categorization of Trackers By Tracking Functionality`,
                                                               "n_trackers_do_not_collect_or_share_pii" ~ "Not Collecting PII",
                                                               "n_trackers_collect_does_not_share_pii" ~ "Collecting PII",
                                                               "n_trackers_collect_and_share_pii" ~ "Collecting and Sharing PII",
                                                               "n_trackers_unknown_pii" ~ "Unknown (Undisclosed or No Match)"
  ))

t <- tracker_summary_long_pii_status %>% 
  flextable() %>% 
  add_header_row(
    values = c(
      "", 
      "Number of Trackers per Publisher Across All Months"
    ),
    top = T,
    colwidths = c(1, 4)
  ) %>% 
  flextable::border_remove() %>% 
  theme_booktabs() %>%
  hline(i = 1, j = c(2, 5), border = std_border, part = "header") %>%
  flextable::colformat_double(j = c(2:3), digits = 3) %>% 
  # optional:
  # add_footer_lines("Notes: The table displays descriptive statistics for the number of trackers per publisher across all months and the Categorization of Trackers By Tracking Functionality. Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>%
  flextable::align(part = "footer", align = "justify") %>%
  # flextable::add_footer_lines("") %>%
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_04b.docx")


# Table 4c: Categorization of Trackers by Type of Publisher
news_summary <- global_sites_trackers_panel_DT[site_category == 'News & Portals', ] %>% 
  .[, .(`mean` = mean(n_trackers),
                                    `SD` = sd(n_trackers),
                                    `min` = min(n_trackers),
                                    `max` = max(n_trackers))]

other_summary <- global_sites_trackers_panel_DT[site_category != 'News & Portals', ] %>% 
  .[, .(`mean` = mean(n_trackers),
                                      `SD` = sd(n_trackers),
                                      `min` = min(n_trackers),
                                      `max` = max(n_trackers))]

publisher_summary_long <- global_sites_trackers_panel_DT[, .(`mean` = mean(n_trackers),
                                            `SD` = sd(n_trackers),
                                            `min` = min(n_trackers),
                                            `max` = max(n_trackers)), 
                                        by = .(`Categorization of Trackers by Type of Publisher` = site_category)] %>% 
  .[order(-`mean`)]

publisher_summary_long <- publisher_summary_long %>% 
  add_row(.before = 1, `Categorization of Trackers by Type of Publisher` = "News Publishers:", mean = news_summary$mean, SD = news_summary$SD, min = news_summary$min, max = news_summary$max) %>% 
  add_row(.before = 3, `Categorization of Trackers by Type of Publisher` = "Non-News Publishers:", mean = other_summary$mean, SD = other_summary$SD, min = other_summary$min, max = other_summary$max)

t <- publisher_summary_long %>% 
  flextable() %>% 
  add_header_row(
    values = c(
      "", 
      "Number of Trackers per Publisher Across All Months"
    ),
    top = T,
    colwidths = c(1, 4)
  ) %>% 
  flextable::border_remove() %>% 
  theme_booktabs() %>%
  hline(i = 1, j = c(2, 5), border = std_border, part = "header") %>% 
  hline(i = 1, border = std_border, part = "body") %>% 
  hline(i = c(2, 3), border = std_border, part = "body") %>% 
  flextable::italic(i = 1, italic = TRUE, part = "body") %>% 
  flextable::italic(i = 3, italic = TRUE, part = "body") %>% 
  flextable::colformat_double(j = c(2:5)) %>% 
  # optional:
  # add_footer_lines("Notes: The table displays descriptive statistics for the number of trackers per publisher across all months and different types of publishers. Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>%
  flextable::align(part = "footer", align = "justify") %>%
  # flextable::add_footer_lines("") %>%
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_04c.docx")

# Table 4d: Categorization of Trackers by Size of Tracker Provider
tracker_summary_wide <- global_sites_trackers_panel_DT[, .(
  # I. essential
  market_share_level_high_mean = mean(n_trackers_from_high_market_share_providers_all_months, na.rm = TRUE),
  market_share_level_high_SD = sd(n_trackers_from_high_market_share_providers_all_months, na.rm = TRUE),
  market_share_level_high_min = min(n_trackers_from_high_market_share_providers_all_months, na.rm = TRUE),
  market_share_level_high_max = max(n_trackers_from_high_market_share_providers_all_months, na.rm = TRUE),
  # I. consent
  market_share_level_low_mean = mean(n_trackers_from_low_market_share_providers_all_months, na.rm = TRUE),
  market_share_level_low_SD = sd(n_trackers_from_low_market_share_providers_all_months, na.rm = TRUE),
  market_share_level_low_min = min(n_trackers_from_low_market_share_providers_all_months, na.rm = TRUE),
  market_share_level_low_max = max(n_trackers_from_low_market_share_providers_all_months, na.rm = TRUE)
)]

tracker_summary_long_market_share <- tracker_summary_wide %>% 
  pivot_longer(cols = everything(), 
               names_to = c("Categorization of Trackers by Size of Tracker Provider", ".value"), 
               values_to = c("mean", "sd", "min", "max"), 
               names_pattern = "(.*)_(.*)") %>% 
  mutate(`Categorization of Trackers by Size of Tracker Provider` = case_match(`Categorization of Trackers by Size of Tracker Provider`,
                                                 "market_share_level_high" ~ "Trackers of Providers with High Market Share ",
                                                 "market_share_level_low" ~ "Trackers of Providers with Low Market Share "
  ))

t <- tracker_summary_long_market_share %>% 
  flextable() %>% 
  add_header_row(
    values = c(
      "", 
      "Number of Trackers per Publisher Across All Months"
    ),
    top = T,
    colwidths = c(1, 4)
  ) %>% 
  flextable::border_remove() %>% 
  theme_booktabs() %>%
  hline(i = 1, j = c(2, 5), border = std_border, part = "header") %>%
  flextable::colformat_double(j = c(2:5), digits = 3) %>% 
  # optional:
  # add_footer_lines("Notes: The table displays descriptive statistics for the number of trackers per publisher across all months and types of tracker providers, ordered by column \"Type of Tracker Provider\". Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>%
  # alternative, add master footnote for all tables:
  add_footer_lines("Notes: This table displays descriptive statistics for the number of trackers per publisher across all months and types of tracker categorizations. Italicized labels represent grouped variables, where category descriptives (e.g., “Essential:”) are followed by descriptives for subcategories within that group (e.g., “Privacy-Friendly Analytics”). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>%
  flextable::align(part = "footer", align = "justify") %>%
  # flextable::add_footer_lines("") %>%
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_04d.docx")

# Append four separate tables into a single table
final_doc <- read_docx()
final_doc <- final_doc %>%
  body_add_docx(src = "03_results/table_04a.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_04b.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_04c.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_04d.docx")
print(final_doc, target = "03_results/table_04.docx")

# open preprocessed table 4 locally in Word?
# temp_file <- tempfile(fileext = ".docx")
# print(final_doc, target = temp_file)
# browseURL(temp_file)


# Table 5: Distribution of the Average Number of Trackers per Publisher By Categorizations of Trackers in the Treatment and Control Groups ------------
# (broken down in four separate tables, then appended to a single table)

# Table 5a: Number of Trackers per Publisher Across All Months
total_trackers_grouped <- global_sites_trackers_panel_DT[, .(
  treatment_total_trackers_mean = mean(n_trackers[is_treated_target_audience == 1], na.rm = TRUE),
  control_total_trackers_mean = mean(n_trackers[is_treated_target_audience == 0], na.rm = TRUE)
)]

total_trackers_grouped[, `Difference` := treatment_total_trackers_mean - control_total_trackers_mean]
total_trackers_grouped[, `Difference (%)` := ((treatment_total_trackers_mean - control_total_trackers_mean) / control_total_trackers_mean) * 100]

total_trackers_long <- data.table(
  `Categorization of Trackers` = "Total Trackers Across All Categories",
  `Treatment Group` = total_trackers_grouped$treatment_total_trackers_mean,
  `Control Group` = total_trackers_grouped$control_total_trackers_mean,
  Difference = total_trackers_grouped$Difference,
  `Difference (%)` = paste0(round(total_trackers_grouped$`Difference (%)`, 2), "%")
)

t <- total_trackers_long %>% 
  flextable() %>% 
  theme_booktabs() %>%
  flextable::colformat_double(j = c(2:4), digits = 3) %>% 
  flextable::colformat_double(j = 5, prefix = "(", suffix = "%)", digits = 2)
t

t %>% flextable::save_as_docx(path = "03_results/table_05a.docx")


# Table 5b: Categorization of Trackers by Purpose and Necessity
tracker_summary_grouped_essential <- global_sites_trackers_panel_DT[, .(
  # Broadly Essential
  treatment_broadly_essential_mean = mean(n_trackers_broadly_essential[is_treated_target_audience == 1], na.rm = TRUE),
  control_broadly_essential_mean = mean(n_trackers_broadly_essential[is_treated_target_audience == 0], na.rm = TRUE),
  # Privacy-Friendly Analytics
  treatment_privacy_friendly_mean = mean(n_trackers_privacy_friendly_site_analytics[is_treated_target_audience == 1], na.rm = TRUE),
  control_privacy_friendly_mean = mean(n_trackers_privacy_friendly_site_analytics[is_treated_target_audience == 0], na.rm = TRUE),
  # Essential (Tag Managers, Error Reports, and Performance)
  treatment_essential_mean = mean(n_trackers_essential[is_treated_target_audience == 1], na.rm = TRUE),
  control_essential_mean = mean(n_trackers_essential[is_treated_target_audience == 0], na.rm = TRUE),
  # Consent Trackers
  treatment_consent_mean = mean(n_trackers_consent[is_treated_target_audience == 1], na.rm = TRUE),
  control_consent_mean = mean(n_trackers_consent[is_treated_target_audience == 0], na.rm = TRUE),
  # CDN Trackers
  treatment_cdn_mean = mean(n_trackers_cdn[is_treated_target_audience == 1], na.rm = TRUE),
  control_cdn_mean = mean(n_trackers_cdn[is_treated_target_audience == 0], na.rm = TRUE),
  # Hosting Trackers
  treatment_hosting_mean = mean(n_trackers_hosting[is_treated_target_audience == 1], na.rm = TRUE),
  control_hosting_mean = mean(n_trackers_hosting[is_treated_target_audience == 0], na.rm = TRUE),
  # Broadly Non-Essential
  treatment_broadly_non_essential_mean = mean(n_trackers_broadly_non_essential[is_treated_target_audience == 1], na.rm = TRUE),
  control_broadly_non_essential_mean = mean(n_trackers_broadly_non_essential[is_treated_target_audience == 0], na.rm = TRUE),
  # Advertising Trackers
  treatment_advertising_mean = mean(n_trackers_advertising[is_treated_target_audience == 1], na.rm = TRUE),
  control_advertising_mean = mean(n_trackers_advertising[is_treated_target_audience == 0], na.rm = TRUE),
  # Analytics Trackers
  treatment_analytics_mean = mean(n_trackers_analytics[is_treated_target_audience == 1], na.rm = TRUE),
  control_analytics_mean = mean(n_trackers_analytics[is_treated_target_audience == 0], na.rm = TRUE),
  # Social Media Trackers
  treatment_social_media_mean = mean(n_trackers_social_media[is_treated_target_audience == 1], na.rm = TRUE),
  control_social_media_mean = mean(n_trackers_social_media[is_treated_target_audience == 0], na.rm = TRUE),
  # Comments Trackers
  treatment_comments_mean = mean(n_trackers_comments[is_treated_target_audience == 1], na.rm = TRUE),
  control_comments_mean = mean(n_trackers_comments[is_treated_target_audience == 0], na.rm = TRUE),
  # Audio Video Player Trackers
  treatment_audio_video_mean = mean(n_trackers_audio_video_player[is_treated_target_audience == 1], na.rm = TRUE),
  control_audio_video_mean = mean(n_trackers_audio_video_player[is_treated_target_audience == 0], na.rm = TRUE),
  # Miscellaneous Trackers
  treatment_misc_mean = mean(n_trackers_misc[is_treated_target_audience == 1], na.rm = TRUE),
  control_misc_mean = mean(n_trackers_misc[is_treated_target_audience == 0], na.rm = TRUE),
  # Customer Interaction Trackers
  treatment_customer_interaction_mean = mean(n_trackers_customer_interaction[is_treated_target_audience == 1], na.rm = TRUE),
  control_customer_interaction_mean = mean(n_trackers_customer_interaction[is_treated_target_audience == 0], na.rm = TRUE),
  # Unknown Trackers
  treatment_unknown_mean = mean(n_trackers_unknown[is_treated_target_audience == 1], na.rm = TRUE),
  control_unknown_mean = mean(n_trackers_unknown[is_treated_target_audience == 0], na.rm = TRUE)
)]

tracker_summary_grouped_essential <- tracker_summary_grouped_essential[, .(
  treatment_broadly_essential_mean, control_broadly_essential_mean, broadly_essential_diff_mean = treatment_broadly_essential_mean - control_broadly_essential_mean,
  treatment_privacy_friendly_mean, control_privacy_friendly_mean, privacy_friendly_diff_mean = treatment_privacy_friendly_mean - control_privacy_friendly_mean,
  treatment_essential_mean, control_essential_mean, essential_diff_mean = treatment_essential_mean - control_essential_mean,
  treatment_consent_mean, control_consent_mean, consent_diff_mean = treatment_consent_mean - control_consent_mean,
  treatment_cdn_mean, control_cdn_mean, cdn_diff_mean = treatment_cdn_mean - control_cdn_mean,
  treatment_hosting_mean, control_hosting_mean, hosting_diff_mean = treatment_hosting_mean - control_hosting_mean,
  treatment_broadly_non_essential_mean, control_broadly_non_essential_mean, broadly_non_essential_diff_mean = treatment_broadly_non_essential_mean - control_broadly_non_essential_mean,
  treatment_advertising_mean, control_advertising_mean, advertising_diff_mean = treatment_advertising_mean - control_advertising_mean,
  treatment_analytics_mean, control_analytics_mean, analytics_diff_mean = treatment_analytics_mean - control_analytics_mean,
  treatment_social_media_mean, control_social_media_mean, social_media_diff_mean = treatment_social_media_mean - control_social_media_mean,
  treatment_comments_mean, control_comments_mean, comments_diff_mean = treatment_comments_mean - control_comments_mean,
  treatment_audio_video_mean, control_audio_video_mean, audio_video_diff_mean = treatment_audio_video_mean - control_audio_video_mean,
  treatment_misc_mean, control_misc_mean, misc_diff_mean = treatment_misc_mean - control_misc_mean,
  treatment_customer_interaction_mean, control_customer_interaction_mean, customer_interaction_diff_mean = treatment_customer_interaction_mean - control_customer_interaction_mean,
  treatment_unknown_mean, control_unknown_mean, unknown_diff_mean = treatment_unknown_mean - control_unknown_mean
)]

tracker_summary_long_essential <- data.table(
  `Categorization of Trackers by Purpose and Necessity` = c("Essential:", "Privacy-Friendly Analytics", "Tag Managers, Error Reports and Performance", "Consent", "CDN", "Hosting", "Non-Essential:", "Advertising", "Analytics", "Social Media", "Comments", "Audio Video Player", "Miscellaneous", "Customer Interaction", "Unknown"),
  `Treatment Group` = c(tracker_summary_grouped_essential$treatment_broadly_essential_mean, tracker_summary_grouped_essential$treatment_privacy_friendly_mean, tracker_summary_grouped_essential$treatment_essential_mean, tracker_summary_grouped_essential$treatment_consent_mean, tracker_summary_grouped_essential$treatment_cdn_mean, tracker_summary_grouped_essential$treatment_hosting_mean, tracker_summary_grouped_essential$treatment_broadly_non_essential_mean, tracker_summary_grouped_essential$treatment_advertising_mean, tracker_summary_grouped_essential$treatment_analytics_mean, tracker_summary_grouped_essential$treatment_social_media_mean, tracker_summary_grouped_essential$treatment_comments_mean, tracker_summary_grouped_essential$treatment_audio_video_mean, tracker_summary_grouped_essential$treatment_misc_mean, tracker_summary_grouped_essential$treatment_customer_interaction_mean, tracker_summary_grouped_essential$treatment_unknown_mean),
  `Control Group` = c(tracker_summary_grouped_essential$control_broadly_essential_mean, tracker_summary_grouped_essential$control_privacy_friendly_mean, tracker_summary_grouped_essential$control_essential_mean, tracker_summary_grouped_essential$control_consent_mean, tracker_summary_grouped_essential$control_cdn_mean, tracker_summary_grouped_essential$control_hosting_mean, tracker_summary_grouped_essential$control_broadly_non_essential_mean, tracker_summary_grouped_essential$control_advertising_mean, tracker_summary_grouped_essential$control_analytics_mean, tracker_summary_grouped_essential$control_social_media_mean, tracker_summary_grouped_essential$control_comments_mean, tracker_summary_grouped_essential$control_audio_video_mean, tracker_summary_grouped_essential$control_misc_mean, tracker_summary_grouped_essential$control_customer_interaction_mean, tracker_summary_grouped_essential$control_unknown_mean),
  Difference = c(tracker_summary_grouped_essential$broadly_essential_diff_mean, tracker_summary_grouped_essential$privacy_friendly_diff_mean, tracker_summary_grouped_essential$essential_diff_mean, tracker_summary_grouped_essential$consent_diff_mean, tracker_summary_grouped_essential$cdn_diff_mean, tracker_summary_grouped_essential$hosting_diff_mean, tracker_summary_grouped_essential$broadly_non_essential_diff_mean, tracker_summary_grouped_essential$advertising_diff_mean, tracker_summary_grouped_essential$analytics_diff_mean, tracker_summary_grouped_essential$social_media_diff_mean, tracker_summary_grouped_essential$comments_diff_mean, tracker_summary_grouped_essential$audio_video_diff_mean, tracker_summary_grouped_essential$misc_diff_mean, tracker_summary_grouped_essential$customer_interaction_diff_mean, tracker_summary_grouped_essential$unknown_diff_mean)
)

tracker_summary_long_essential[, `Difference (%)` := ((`Treatment Group` - `Control Group`) / `Control Group`) * 100]

print(tracker_summary_long_essential)

t <- tracker_summary_long_essential %>% 
  flextable() %>% 
  theme_booktabs() %>%
  flextable::colformat_double(j = c(2:4), digits = 3) %>% 
  flextable::colformat_double(j = c(5), prefix = "(", suffix = "%)", digits = 2) %>% 
  flextable::align(part = "footer", align = "justify")
t

t %>% flextable::save_as_docx(path = "03_results/table_05b.docx")


# Table 5c: Categorization of Trackers by Tracking Functionality
tracker_summary_grouped_functionality <- global_sites_trackers_panel_DT[, .(
  # Trackers Not Collecting PII
  treatment_do_not_collect_or_share_pii_mean = mean(n_trackers_do_not_collect_or_share_pii[is_treated_target_audience == 1], na.rm = TRUE),
  control_do_not_collect_or_share_pii_mean = mean(n_trackers_do_not_collect_or_share_pii[is_treated_target_audience == 0], na.rm = TRUE),
  # Trackers Collecting PII
  treatment_collect_does_not_share_pii_mean = mean(n_trackers_collect_does_not_share_pii[is_treated_target_audience == 1], na.rm = TRUE),
  control_collect_does_not_share_pii_mean = mean(n_trackers_collect_does_not_share_pii[is_treated_target_audience == 0], na.rm = TRUE),
  # Trackers Collecting and Sharing PII
  treatment_collect_and_share_pii_mean = mean(n_trackers_collect_and_share_pii[is_treated_target_audience == 1], na.rm = TRUE),
  control_collect_and_share_pii_mean = mean(n_trackers_collect_and_share_pii[is_treated_target_audience == 0], na.rm = TRUE),
  # Trackers with Unknown PII Status
  treatment_unknown_pii_mean = mean(n_trackers_unknown_pii[is_treated_target_audience == 1], na.rm = TRUE),
  control_unknown_pii_mean = mean(n_trackers_unknown_pii[is_treated_target_audience == 0], na.rm = TRUE)
)]

tracker_summary_grouped_functionality <- tracker_summary_grouped_functionality[, .(
  treatment_do_not_collect_or_share_pii_mean, control_do_not_collect_or_share_pii_mean, do_not_collect_or_share_pii_diff_mean = treatment_do_not_collect_or_share_pii_mean - control_do_not_collect_or_share_pii_mean,
  treatment_collect_does_not_share_pii_mean, control_collect_does_not_share_pii_mean, collect_does_not_share_pii_diff_mean = treatment_collect_does_not_share_pii_mean - control_collect_does_not_share_pii_mean,
  treatment_collect_and_share_pii_mean, control_collect_and_share_pii_mean, collect_and_share_pii_diff_mean = treatment_collect_and_share_pii_mean - control_collect_and_share_pii_mean,
  treatment_unknown_pii_mean, control_unknown_pii_mean, unknown_pii_diff_mean = treatment_unknown_pii_mean - control_unknown_pii_mean
)]

tracker_summary_long_functionality <- data.table(
  `Categorization of Trackers by Tracking Functionality` = c("Not Collecting PII", "Collecting PII", "Collecting and Sharing PII", "Unknown (Undisclosed or No Match)"),
  `Treatment Group` = c(tracker_summary_grouped_functionality$treatment_do_not_collect_or_share_pii_mean, tracker_summary_grouped_functionality$treatment_collect_does_not_share_pii_mean, tracker_summary_grouped_functionality$treatment_collect_and_share_pii_mean, tracker_summary_grouped_functionality$treatment_unknown_pii_mean),
  `Control Group` = c(tracker_summary_grouped_functionality$control_do_not_collect_or_share_pii_mean, tracker_summary_grouped_functionality$control_collect_does_not_share_pii_mean, tracker_summary_grouped_functionality$control_collect_and_share_pii_mean, tracker_summary_grouped_functionality$control_unknown_pii_mean),
  Difference = c(tracker_summary_grouped_functionality$do_not_collect_or_share_pii_diff_mean, tracker_summary_grouped_functionality$collect_does_not_share_pii_diff_mean, tracker_summary_grouped_functionality$collect_and_share_pii_diff_mean, tracker_summary_grouped_functionality$unknown_pii_diff_mean)
)

tracker_summary_long_functionality[, `Difference (%)` := ((`Treatment Group` - `Control Group`) / `Control Group`) * 100]

print(tracker_summary_long_functionality)

t <- tracker_summary_long_functionality %>% 
  flextable() %>% 
  theme_booktabs() %>%
  flextable::colformat_double(j = c(2:4), digits = 3) %>% 
  flextable::colformat_double(j = c(5), prefix = "(", suffix = "%)", digits = 2) %>% 
  flextable::align(part = "footer", align = "justify")
t

t %>% flextable::save_as_docx(path = "03_results/table_05c.docx")


# Table 5d: Categorization of Trackers by Type of Publisher
tracker_summary_grouped_publisher <- global_sites_trackers_panel_DT[, .(
  # News Publishers
  treatment_news_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'News & Portals'], na.rm = TRUE),
  control_news_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'News & Portals'], na.rm = TRUE),
  # Non-News Publishers
  treatment_non_news_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category != 'News & Portals'], na.rm = TRUE),
  control_non_news_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category != 'News & Portals'], na.rm = TRUE),
  # E-Commerce
  treatment_ecommerce_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'E-Commerce'], na.rm = TRUE),
  control_ecommerce_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'E-Commerce'], na.rm = TRUE),
  # Recreation
  treatment_recreation_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'Recreation'], na.rm = TRUE),
  control_recreation_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'Recreation'], na.rm = TRUE),
  # Business
  treatment_business_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'Business'], na.rm = TRUE),
  control_business_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'Business'], na.rm = TRUE),
  # Entertainment
  treatment_entertainment_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'Entertainment'], na.rm = TRUE),
  control_entertainment_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'Entertainment'], na.rm = TRUE),
  # Reference
  treatment_reference_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'Reference'], na.rm = TRUE),
  control_reference_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'Reference'], na.rm = TRUE),
  # Adult
  treatment_adult_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'Adult'], na.rm = TRUE),
  control_adult_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'Adult'], na.rm = TRUE),
  # Government
  treatment_government_mean = mean(n_trackers[is_treated_target_audience == 1 & site_category == 'Government'], na.rm = TRUE),
  control_government_mean = mean(n_trackers[is_treated_target_audience == 0 & site_category == 'Government'], na.rm = TRUE)
)]

tracker_summary_grouped_publisher <- tracker_summary_grouped_publisher[, .(
  treatment_news_mean, control_news_mean, news_diff_mean = treatment_news_mean - control_news_mean,
  treatment_non_news_mean, control_non_news_mean, non_news_diff_mean = treatment_non_news_mean - control_non_news_mean,
  treatment_ecommerce_mean, control_ecommerce_mean, ecommerce_diff_mean = treatment_ecommerce_mean - control_ecommerce_mean,
  treatment_recreation_mean, control_recreation_mean, recreation_diff_mean = treatment_recreation_mean - control_recreation_mean,
  treatment_business_mean, control_business_mean, business_diff_mean = treatment_business_mean - control_business_mean,
  treatment_entertainment_mean, control_entertainment_mean, entertainment_diff_mean = treatment_entertainment_mean - control_entertainment_mean,
  treatment_reference_mean, control_reference_mean, reference_diff_mean = treatment_reference_mean - control_reference_mean,
  treatment_adult_mean, control_adult_mean, adult_diff_mean = treatment_adult_mean - control_adult_mean,
  treatment_government_mean, control_government_mean, government_diff_mean = treatment_government_mean - control_government_mean
)]

tracker_summary_long_publisher <- data.table(
  `Categorization of Trackers by Type of Publisher` = c("News Publishers:", "News & Portals", "Non-News Publishers:", "E-Commerce", "Recreation", "Business", "Entertainment", "Reference", "Adult", "Government"),
  `Treatment Group` = c(tracker_summary_grouped_publisher$treatment_news_mean,
                        tracker_summary_grouped_publisher$treatment_news_mean, # duplicate row
                        tracker_summary_grouped_publisher$treatment_non_news_mean, 
                        tracker_summary_grouped_publisher$treatment_ecommerce_mean, tracker_summary_grouped_publisher$treatment_recreation_mean, tracker_summary_grouped_publisher$treatment_business_mean, tracker_summary_grouped_publisher$treatment_entertainment_mean, tracker_summary_grouped_publisher$treatment_reference_mean, tracker_summary_grouped_publisher$treatment_adult_mean, tracker_summary_grouped_publisher$treatment_government_mean),
  `Control Group` = c(tracker_summary_grouped_publisher$control_news_mean,
                      tracker_summary_grouped_publisher$control_news_mean, # duplicate row
                      tracker_summary_grouped_publisher$control_non_news_mean,
                      tracker_summary_grouped_publisher$control_ecommerce_mean, tracker_summary_grouped_publisher$control_recreation_mean, tracker_summary_grouped_publisher$control_business_mean, tracker_summary_grouped_publisher$control_entertainment_mean, tracker_summary_grouped_publisher$control_reference_mean, tracker_summary_grouped_publisher$control_adult_mean, tracker_summary_grouped_publisher$control_government_mean),
  Difference = c(tracker_summary_grouped_publisher$news_diff_mean,
                 tracker_summary_grouped_publisher$news_diff_mean, # duplicate row
                 tracker_summary_grouped_publisher$non_news_diff_mean,
                 tracker_summary_grouped_publisher$ecommerce_diff_mean, tracker_summary_grouped_publisher$recreation_diff_mean, tracker_summary_grouped_publisher$business_diff_mean, tracker_summary_grouped_publisher$entertainment_diff_mean, tracker_summary_grouped_publisher$reference_diff_mean, tracker_summary_grouped_publisher$adult_diff_mean, tracker_summary_grouped_publisher$government_diff_mean)
)

tracker_summary_long_publisher[, `Difference (%)` := ((`Treatment Group` - `Control Group`) / `Control Group`) * 100]

print(tracker_summary_long_publisher)

t <- tracker_summary_long_publisher %>% 
  flextable() %>% 
  theme_booktabs() %>%
  flextable::colformat_double(j = c(2:4), digits = 3) %>% 
  flextable::colformat_double(j = c(5), prefix = "(", suffix = "%)", digits = 2) %>% 
  flextable::align(part = "footer", align = "justify")
  # optional:
  # flextable::add_footer_lines("Notes: The Government publisher has been deliberately omitted from this analysis, given that only a single publisher of this type was represented in the control group of our sample.")
t

t %>% flextable::save_as_docx(path = "03_results/table_05d.docx")


# Table 5e: Categorization of Trackers by Size of Tracker Provider
tracker_summary_grouped_provider <- global_sites_trackers_panel_DT[, .(
  # High Market Share Tracker Providers
  treatment_high_market_share_mean = mean(n_trackers_from_high_market_share_providers_all_months[is_treated_target_audience == 1], na.rm = TRUE),
  control_high_market_share_mean = mean(n_trackers_from_high_market_share_providers_all_months[is_treated_target_audience == 0], na.rm = TRUE),
  # Low Market Share Tracker Providers
  treatment_low_market_share_mean = mean(n_trackers_from_low_market_share_providers_all_months[is_treated_target_audience == 1], na.rm = TRUE),
  control_low_market_share_mean = mean(n_trackers_from_low_market_share_providers_all_months[is_treated_target_audience == 0], na.rm = TRUE)
)]

tracker_summary_grouped_provider <- tracker_summary_grouped_provider[, .(
  treatment_high_market_share_mean, control_high_market_share_mean, high_market_share_diff_mean = treatment_high_market_share_mean - control_high_market_share_mean,
  treatment_low_market_share_mean, control_low_market_share_mean, low_market_share_diff_mean = treatment_low_market_share_mean - control_low_market_share_mean
)]

tracker_summary_long_provider <- data.table(
  `Categorization of Trackers by Size of Tracker Provider` = c("Trackers of Providers with High Market Share", "Trackers of Providers with Low Market Share"),
  `Treatment Group` = c(tracker_summary_grouped_provider$treatment_high_market_share_mean, tracker_summary_grouped_provider$treatment_low_market_share_mean),
  `Control Group` = c(tracker_summary_grouped_provider$control_high_market_share_mean, tracker_summary_grouped_provider$control_low_market_share_mean),
  Difference = c(tracker_summary_grouped_provider$high_market_share_diff_mean, tracker_summary_grouped_provider$low_market_share_diff_mean)
)

tracker_summary_long_provider[, `Difference (%)` := ((`Treatment Group` - `Control Group`) / `Control Group`) * 100]

print(tracker_summary_long_provider)

t <- tracker_summary_long_provider %>% 
  flextable() %>% 
  theme_booktabs() %>%
  flextable::colformat_double(j = c(2:4), digits = 3) %>% 
  flextable::colformat_double(j = c(5), prefix = "(", suffix = "%)", digits = 2) %>% 
  flextable::align(part = "footer", align = "justify")
t

t %>% flextable::save_as_docx(path = "03_results/table_05e.docx")

# Table 5f: Publisher Characteristics
website_summary_grouped <- global_sites_trackers_panel_DT[, .(
  # Share of Traffic from EEA
  treatment_traffic_EEA_mean = mean(sw_share_EEA_traffic_top_five_countries[is_treated_target_audience == 1], na.rm = TRUE),
  control_traffic_EEA_mean = mean(sw_share_EEA_traffic_top_five_countries[is_treated_target_audience == 0], na.rm = TRUE),
  # Share of Traffic from Non-EEA
  treatment_traffic_nonEEA_mean = mean(sw_share_non_EEA_traffic_top_five_countries[is_treated_target_audience == 1], na.rm = TRUE),
  control_traffic_nonEEA_mean = mean(sw_share_non_EEA_traffic_top_five_countries[is_treated_target_audience == 0], na.rm = TRUE)
)]

website_summary_grouped[, `traffic_EEA_diff_mean` := (treatment_traffic_EEA_mean - control_traffic_EEA_mean) * 100]  # In percentage points
website_summary_grouped[, `traffic_nonEEA_diff_mean` := (treatment_traffic_nonEEA_mean - control_traffic_nonEEA_mean) * 100]  # In percentage points

tld_summary <- global_sites_trackers_panel_DT[, .(
  treatment_top_tlds = paste(names(sort(table(TLD[is_treated_target_audience == 1]), decreasing = TRUE)[1:5]), collapse = ", "),
  control_top_tlds = paste(names(sort(table(TLD[is_treated_target_audience == 0]), decreasing = TRUE)[1:5]), collapse = ", ")
)]

website_summary_long <- data.table(
  `Publisher Characteristics` = c(
    "Share of Traffic from EU Users",
    "Share of Traffic from Non-EU Users",
    "5 Most Common TLDs"
  ),
  `Treatment Group` = c(
    paste0(format(website_summary_grouped$treatment_traffic_EEA_mean * 100, digits = 4), "%"),
    paste0(format(website_summary_grouped$treatment_traffic_nonEEA_mean * 100, digits = 4), "%"),
    tld_summary$treatment_top_tlds
  ),
  `Control Group` = c(
    paste0(format(website_summary_grouped$control_traffic_EEA_mean * 100, digits = 4), "%"),
    paste0(format(website_summary_grouped$control_traffic_nonEEA_mean * 100, digits = 4), "%"),
    tld_summary$control_top_tlds
  ),
  `Difference` = c(
    NA,
    NA,
    NA
  ),
  `Difference (%)` = c(
    paste0("(", format(website_summary_grouped$traffic_EEA_diff_mean, digits = 4), " pp)"),
    paste0("(", format(website_summary_grouped$traffic_nonEEA_diff_mean, digits = 4), " pp)"),
    NA  # No difference for TLDs
  )
)

t <- website_summary_long %>% 
  flextable() %>% 
  theme_booktabs() %>%
  flextable::colformat_double(j = c(2:3), digits = 2) %>%  # Format Traffic Percentages
  flextable::align(part = "footer", align = "justify") %>% 
  # optional:
  # flextable::add_footer_lines("Notes: The table displays publisher characteristics for treatment and control groups. Percent differences are displayed as percentage points (pp) for shares of traffic from (non)-EU users.") %>% 
  # alternative, footnote for the entire table:
  flextable::add_footer_lines("Notes: This table shows the average number of trackers for the treatment and control groups across all months and types of tracker categorizations. Italicized labels represent grouped variables, where broad category descriptives (e.g., “Essential:”) are followed by descriptives for subcategories within that group (e.g., “Privacy-Friendly Analytics”). The table also shows the average share of traffic from (non)-EU users and the five most common TLDs for treatment and control groups. Percent differences are displayed as percentage points (pp) for shares of traffic from (non)-EU users. The Government publisher has been deliberately omitted from this analysis, given that only a single publisher of this type was present in the control group of our sample.")
t

t %>% flextable::save_as_docx(path = "03_results/table_05f.docx")

# Append separate tables into a single table
final_doc <- read_docx()
final_doc <- final_doc %>%
  body_add_docx(src = "03_results/table_05a.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_05b.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_05c.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_05d.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_05e.docx") %>%
  body_add_par("") %>%
  body_add_docx(src = "03_results/table_05f.docx")
print(final_doc, target = "03_results/table_05.docx")

# open preprocessed table locally in Word?
# temp_file <- tempfile(fileext = ".docx")
# print(final_doc, target = temp_file)
# browseURL(temp_file)


# Figure 4: Comparison of the Average Number of Trackers in the Treatment and Control Groups Before and After the GDPR’s Enactment ----------

set_group_color_fill <- scale_fill_manual(
  values = c(
    "1" = "white",
    "0" = "lightgray"
  ),
  name = "Group:",
  breaks = c("1", "0"),
  labels = c("Treatment", "Control")
)
set_group_names_on_x_axis <- scale_x_discrete(
  breaks = c(1, 0),
  labels = c("Treatment", "Control")
)
gdpr_periods_labeller <- as_labeller(c(`0` = "Before GDPR's Enactment", `1` = "After GDPR's Enactment"))

t_test_n_trackers <- global_sites_trackers_panel_DT %>%
  group_by(post_GDPR) %>%
  t_test(n_trackers ~ is_treated_target_audience, ref.group = "1", detailed = T) %>%
  add_significance()

delta_df <- t_test_n_trackers %>%
  select(post_GDPR,
         delta = estimate
  ) %>%
  mutate(
    delta = round(delta, rounding_decimal),
    is_treated_target_audience = c(1L, 0L)
  )

ggplot(
  global_sites_trackers_panel_DT,
  aes(
    x = factor(is_treated_target_audience, levels = c(1, 0)),
    y = n_trackers,
    fill = factor(is_treated_target_audience, levels = c(1, 0))
  )
) +
  stat_summary(
    geom = "bar", fun = mean, position = "dodge",
    color = "black", show.legend = F
  ) +
  set_group_color_fill +
  set_group_names_on_x_axis +
  coord_cartesian(ylim = c(NA, 36)) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    x = "", y = "Average Number of Trackers",
    caption = paste0(
      "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001. \n", str_wrap(
        "Notes: Error bars = +/- 1 SEs. This figure shows independent t-test comparisons between group averages in periods before (May 2017-April 2018) and after (May 2018-December 2019) the GDPR's enactment using the number of trackers as a dependent variable.",
        width = 100
      )
    )
  ) +
  facet_wrap(~ factor(post_GDPR), labeller = gdpr_periods_labeller) +
  stat_summary(
    geom = "errorbar", fun.data = mean_se, # add errorbars
    position = "dodge", width = .2, show.legend = F
  ) +
  stat_compare_means(
    method = "t.test", # add 2 unpaired t-test comparisons
    comparisons = list(c("0", "1")),
    paired = F,
    label = "p.value",
    symnum.args = list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("***", "**", "*", "")
    ),
    label.y = 25,
    size = 4,
    family = "Times"
  ) +
  geom_text(
    x = 1.5, y = 29,
    # aes(label = paste0("∆ = ", delta)),
    aes(label = paste0("Delta == ", delta)),
    parse = T,
    size = 4, family = "Times",
    data = delta_df
  ) +
  theme_classic(base_family = "Times") +
  theme(plot.caption = element_text(hjust = 0, size = 11),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank()
        )

ggsave("03_results/figure_04.pdf", width=8, height=6, font="Times", dpi=300)


# Figure 5: Development of the Average Monthly Number of Trackers in the Treatment and Control Groups ----------

data_summary <- global_sites_trackers_panel_DT %>%
  group_by(ymd, is_treated_target_audience_facet) %>%
  summarize(mean_n_trackers = mean(n_trackers, na.rm = TRUE), .groups = "keep")

data_summary$is_treated_target_audience_facet <- factor(data_summary$is_treated_target_audience_facet, levels = c(1, 0))

ggplot(data_summary, aes(x = ymd, y = mean_n_trackers, linetype = factor(is_treated_target_audience_facet))) +
  geom_point(size = 4, aes(shape = factor(is_treated_target_audience_facet))) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +
  labs(y = "Average Number of Trackers", x = "", linetype = "Group:", shape = "Group:") +
  coord_cartesian(ylim = c(1, 35)) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("1" = "solid", "0" = "dashed"),
                        labels = c("Treatment", "Control")) +
  scale_shape_manual(values = c(16, 3),
                     labels = c("Treatment", "Control")) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank()
        )

ggsave("03_results/figure_05.pdf", width=8, height=6, font="Times", dpi=300)


# Table 6: Cross-Table for the Average (Monthly) Number of Trackers in the Treatment and Control Groups Before and After the GDPR’s Enactment --------

t <- get_did_table(global_sites_trackers_panel_DT, 
                   dep_var = "n_trackers", 
                   period_var = "post_GDPR", 
                   treat_var = "is_treated_target_audience")
t <- t %>% 
  flextable::add_footer_lines(
    "This table shows the average (monthly) number of trackers for the treatment and control groups in periods before (May 2017-April 2018) and after (May 2018-December 2019) GDPR’s enactment and the differences in the average (monthly) number of trackers between groups and periods. We use unrounded values to derive the differences. The values in parentheses represent the percent changes for each group from the period before to the period after the GDPR’s enactment. The Difference-in-Differences (DiD) as a percentage is calculated by comparing the observed value in the treatment group after GDPR (22.765) with the expected value if the GDPR had not been enacted. The expected value is calculated by adding the pre-GDPR difference between groups (7.347) to the post-GDPR control group value (19.366), which equals 26.714. The percent decrease is then derived from the ratio of the difference between these two values to the expected value: DiD (%) = [(26.714 - 22.765) / 26.714] × 100 ≈ 14.79%.") %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% 
  flextable::keep_with_next() %>% 
  flextable::save_as_docx(path = "03_results/table_06.docx")


# Table 7: Result of Difference-in-Differences (DiD) Analysis for the Number of Trackers -------------

m2 <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_id + period_id,
      cluster = ~site_id + period_id,
      data = global_sites_trackers_panel_DT
)

etable(
    m2, view = F, digits = "r3", 
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

t <- etable(m2,
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 5, 6, 9)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:", 
                    "m2" = "(1)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 5, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regression. We assign treatment to each publisher according to the publisher’s designation (EU or non-EU). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/table_07.docx")


# Figure 6: Distribution of the GDPR’s Impact Across Categorizations of Trackers ---------------

variable_name_mapping <- c(
  "n_trackers_broadly_essential" = "*Essential:*",
  "n_trackers_broadly_non_essential" = "*Non-Essential:*",
  "n_trackers_privacy_friendly_site_analytics" = "Privacy-Friendly Analytics",
  "n_trackers_essential" = "Tag Managers, Error Reports, and Performance",
  "n_trackers_consent" = "Consent",
  "n_trackers_hosting" = "Hosting",
  "n_trackers_cdn" = "Content Delivery Network (CDN)",
  "n_trackers_advertising" = "Advertising",
  "n_trackers_site_analytics" = "Analytics",
  "n_trackers_social_media" = "Social Media",
  "n_trackers_comments" = "Comments",
  "n_trackers_audio_video_player" = "Audio Video Player",
  "n_trackers_misc" = "Miscellaneous",
  "n_trackers_customer_interaction" = "Customer Interaction",
  "n_trackers_unknown" = "Unknown",
  "n_trackers_do_not_collect_or_share_pii" = "Not Collecting PII",
  "n_trackers_collect_does_not_share_pii" = "Collecting PII",
  "n_trackers_collect_and_share_pii" = "Collecting and Sharing PII",
  "n_trackers_unknown_pii" = "Unknown (Undisclosed or No Match)",
  "n_trackers_from_high_market_share_providers_pre_post_GDPR" = "Trackers of Providers with High Market Share",
  "n_trackers_from_low_market_share_providers_pre_post_GDPR" = "Trackers of Providers with Low Market Share",
  "sample.var: site_category; sample: News & Portals" = "News & Portals",
  "sample.var: site_category; sample: E-Commerce" = "E-Commerce",
  "sample.var: site_category; sample: Recreation" = "Recreation",
  "sample.var: site_category; sample: Business" = "Business",
  "sample.var: site_category; sample: Entertainment" = "Entertainment",
  "sample.var: site_category; sample: Reference" = "Reference",
  "sample.var: site_category; sample: Adult" = "Adult",
  "sample.var: site_category; sample: Government" = "Government",
  "sample.var: publisher_industry; sample: News Publishers" = "*News Publishers:*",
  "sample.var: publisher_industry; sample: Non-News Publishers" = "*Non-News Publishers:*"
)

desired_order <- c(
  "*Essential:*",
  "Privacy-Friendly Analytics",
  "Tag Managers, Error Reports, and Performance",
  "Consent",
  "Content Delivery Network (CDN)",
  "Hosting",
  "*Non-Essential:*",
  "Advertising",
  "Analytics",
  "Social Media",
  "Comments",
  "Audio Video Player",
  "Miscellaneous",
  "Customer Interaction",
  "Unknown",
  "Not Collecting PII",
  "Collecting PII",
  "Collecting and Sharing PII",
  "Unknown (Undisclosed or No Match)",
  "*News Publishers:*",
  "News & Portals",
  "*Non-News Publishers:*",
  "E-Commerce",
  "Recreation",
  "Business",
  "Entertainment",
  "Reference",
  "Adult",
  # "Government",
  "Trackers of Providers with High Market Share",
  "Trackers of Providers with Low Market Share"
)

# Model for Categorization of Trackers by Purpose and Necessity
model_purpose_necessity <- feols(c(
  n_trackers_broadly_essential,
  n_trackers_broadly_non_essential,
  n_trackers_privacy_friendly_site_analytics,
  n_trackers_essential,
  n_trackers_consent,
  n_trackers_hosting,
  n_trackers_cdn,
  n_trackers_advertising,
  n_trackers_site_analytics,
  n_trackers_social_media,
  n_trackers_comments,
  n_trackers_audio_video_player,
  n_trackers_misc,
  n_trackers_customer_interaction,
  n_trackers_unknown
) ~ is_treated_target_audience * post_GDPR | site_id + period_id,
cluster = ~site_id + period_id,
data = global_sites_trackers_panel_DT)

# Model for Categorization of Trackers By Tracking Functionality
model_tracking_functionality <- feols(c(
  n_trackers_do_not_collect_or_share_pii,
  n_trackers_collect_does_not_share_pii,
  n_trackers_collect_and_share_pii,
  n_trackers_unknown_pii
) ~ is_treated_target_audience * post_GDPR | site_id + period_id,
cluster = ~site_id + period_id,
data = global_sites_trackers_panel_DT)

# Model for Site Category
model_site_category <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_id + period_id,
                             cluster = ~site_id + period_id,
                             data = global_sites_trackers_panel_DT,
                             fsplit = c("site_category")) %>% .[c(-1, -6)]

# Model for Publisher Industry
model_publisher_industry <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_id + period_id,
                                  cluster = ~site_id + period_id,
                                  data = global_sites_trackers_panel_DT,
                                  fsplit = c("publisher_industry")) %>% .[-1]

# Model for Categorization of Trackers by Size of Tracker Provider
model_market_share <- feols(c(
  n_trackers_from_high_market_share_providers_pre_post_GDPR,
  n_trackers_from_low_market_share_providers_pre_post_GDPR) ~ is_treated_target_audience * post_GDPR | site_id + period_id,
  cluster = ~site_id + period_id,
  data = global_sites_trackers_panel_DT)

# Extract coefficients for each model
coefs_purpose_necessity <- extract_coefs(model_purpose_necessity, "Across Categorization of Trackers by Purpose and Necessity")
coefs_tracking_functionality <- extract_coefs(model_tracking_functionality, "Across Categorization of Trackers by Tracking Functionality")
coefs_site_category <- extract_coefs(model_site_category, "Across Categorization of Trackers by Type of Publisher", is_sample = TRUE)
coefs_publisher_industry <- extract_coefs(model_publisher_industry, "Across Categorization of Trackers by Type of Publisher", is_sample = TRUE)
coefs_market_share <- extract_coefs(model_market_share, "Across Categorization of Trackers by Size of Tracker Provider")

coefs <- bind_rows(coefs_purpose_necessity, coefs_tracking_functionality,
                   coefs_site_category, coefs_publisher_industry, coefs_market_share)

coefs$dep_var <- coefs$dep_var %>%
  recode(!!!variable_name_mapping)

coefs$dep_var <- factor(coefs$dep_var, levels = desired_order)

custom_theme <- theme_classic(base_family = "Times") +
  theme(plot.caption = element_text(hjust = 0),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.text.y = element_markdown())

num_coefs_purpose_necessity <- length(unique(coefs_purpose_necessity$dep_var))
num_coefs_tracking_functionality <- length(unique(coefs_tracking_functionality$dep_var))
num_coefs_type_publisher <- length(unique(c(coefs_site_category$dep_var, coefs_publisher_industry$dep_var)))
num_coefs_market_share <- length(unique(coefs_market_share$dep_var))

plot_purpose_necessity <- plot_coefs(coefs, "Across Categorization of Trackers by Purpose and Necessity") +
  theme(plot.margin = margin(t = 10, b = 10, unit = "pt")) +
  labs(y = "DiD Coefficient and 95% Confidence Interval",
       x = "Distribution of the Effect of GDPR's Enactment")
plot_tracking_functionality <- plot_coefs(coefs, "Across Categorization of Trackers by Tracking Functionality") +
  theme(plot.margin = margin(t = 10, b = 10, unit = "pt")) + 
  labs(y = "DiD Coefficient and 95% Confidence Interval",
       x = "Distribution of the Effect of GDPR's Enactment")
plot_type_publisher <- plot_coefs(coefs, "Across Categorization of Trackers by Type of Publisher") +
  theme(plot.margin = margin(t = 10, b = 10, unit = "pt")) +
  labs(y = "DiD Coefficient and 95% Confidence Interval",
       x = "Distribution of the Effect of GDPR's Enactment")
plot_market_share <- plot_coefs(coefs, "Across Categorization of Trackers by Size of Tracker Provider") +
  theme(plot.margin = margin(t = 10, b = 10, unit = "pt")) +
  labs(y = "DiD Coefficient and 95% Confidence Interval",
       x = "Distribution of the Effect of GDPR's Enactment")

notes <- str_wrap("Notes: This figure shows the difference-in-differences coefficients (Treatment x PostGDPR) from OLS regressions and 95% confidence intervals, with the dependent variables being the number of trackers across different categorizations of trackers. Italicized labels represent grouped variables, where a broad category estimation (e.g., \"Essential:\") is followed by estimations for subcategories within that group (e.g., \"Privacy-Friendly Analytics\"). Except for the categorization by Type of Publisher, all models have N = 9,408 observations (294 publishers * 32 months). For the categorization by Type of Publisher, the number of observations varies: News Publishers have N = 928 (29 publishers * 32 months), Non-News Publishers have N = 6,328 (198 publishers * 32 months), and specific types of publishers are as follows: News & Portals (N = 928), E-Commerce (N = 480), Recreation (N = 224), Business (N = 2,048), Entertainment (N = 2,432), Reference (N = 736), and Adult (N = 2,528). The Governmental publisher type is excluded due to having only one publisher. All models include website instance and month fixed effects. Two-way standard errors are clustered at the publisher and month levels.", width = 123)

combined_plot <- (plot_purpose_necessity) /
  plot_tracking_functionality /
  plot_type_publisher /
  plot_market_share +
  plot_layout(ncol = 1, heights = c(
    num_coefs_purpose_necessity / 2,
    num_coefs_tracking_functionality / 2,
    num_coefs_type_publisher / 2,
    num_coefs_market_share / 2
  ), guides = "collect", axis_titles = "collect") &
  theme(axis.title.y = element_text(angle = 90, vjust = 2), 
        axis.title.x = element_text(vjust = -0.5))

combined_plot <- combined_plot +
  plot_annotation(
    caption = notes,
    theme = theme(
      plot.caption = element_text(size = 10.5, 
                                  hjust = 0, vjust = 1, family = "Times")
    )
  )
combined_plot

ggsave("03_results/figure_06.pdf", combined_plot, width = 8, height = 12, dpi = 300)


# Table 8: Summary of Robustness Tests ---------------

t <- read_excel(here::here("01_data", 
                           "lookup_tables", 
                           "summary_of_robustness_tests-2025-01-09.xlsx"), 
           col_names = T
) %>% 
  flextable() %>% 
  autofit() %>%
  flextable::align(j = "Web Appendix", align = "right") %>%
  hline(border = std_border, part = "body") %>%
  flextable::add_footer_lines("") %>%  # add empty line
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 10, part = "all") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_08.docx")


# Table 9: Summary of Empirical Findings on Description of Online Trackers and Their Conclusions --------------

t <- read_excel(here::here("01_data", 
                           "lookup_tables", 
                           "summary_of_empirical_findings_on_description_of_online_trackers-2025-01-09.xlsx"), 
                col_names = T
) %>% 
  flextable() %>% 
  merge_at(i = 3, j = 1:3, part = "body") %>% 
  flextable::italic(i = 3, j = 1, part = "body", italic = T) %>%
  flextable::align(i = 3, j = 1, align = "center", part = "body") %>% 
  fix_border_issues() %>%
  hline(i = c(2, 3, 5, 15, 19, 21), border = std_border) %>%
  flextable::add_footer_lines("") %>%  # add empty line
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 9, part = "all") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_09.docx")


# Table 10: Summary of Empirical Findings on Description of Online Trackers and Their Conclusions --------------

t <- read_excel(here::here("01_data", 
                           "lookup_tables", 
                           "summary_of_empirical_findings_on_impact_of_GDPR-2025-01-09.xlsx"), 
                col_names = T
) %>% 
  flextable() %>% 
  merge_at(i = 3, j = 1:3, part = "body") %>% 
  flextable::italic(i = 3, j = 1, part = "body", italic = TRUE) %>%
  flextable::align(i = 3, j = 1, align = "center", part = "body") %>% 
  fix_border_issues() %>%
  hline(i = c(2, 3, 5, 15, 19, 25), border = std_border) %>%
  flextable::add_footer_lines("") %>%  # add empty line
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 9, part = "all") %>% 
  compose(part = "header", j = 2, value = as_paragraph("Summary of Findings", flextable::as_sup("A"))) %>% 
  add_footer_lines(values = "Notes: A) The summary of findings refers to the average reduction of trackers per EU publisher.") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/table_10.docx")


################# REPRODUCE TABLES AND FIGURES IN WEB APPENDICES ###############


# Table 11: Distribution of Observations (Monthly Publishers) Across Publisher Designation for the Sample with a Server Location Treatment Assignment ---------------

n_sites_global <- global_sites_trackers_panel_DT[, uniqueN(site_id)]
n_sites_adjusted_global <- global_sites_trackers_panel_DT[, uniqueN(site_id)]
t_periods_adjusted_global <- global_sites_trackers_panel_DT[, uniqueN(period_id)]
N_obs_adjusted_global <- global_sites_trackers_panel_DT[, .N]

summarized_DT <- global_sites_trackers_panel_DT[, .(n_obs = .N),
                                                by = .(server_region, is_treated_server)
] %>%
  mutate(prop = n_obs / sum(n_obs)) %>% 
  mutate(server_region = case_when(server_region == "EU" ~ "EU publisher",
                                   server_region == "Non-EU" ~ "Non-EU publisher", 
                                     TRUE ~ server_region))

summarized_wide_DT <-
  summarized_DT %>%
  add_row(
    server_region = "∑",
    n_obs = sum(.$n_obs),
    prop = sum(.$prop)
  ) %>%
  mutate(n_obs_fmt = big_mark(n_obs)) %>%
  mutate(prop_fmt = paste0("(", scales::percent(prop, rounding_pct_scales_percent), ")")) %>%
  mutate(n_obs_prop_fmt = paste(n_obs_fmt, prop_fmt, sep = " ")) %>%
  select(
    server_region,
    n_obs_prop_fmt
  )

total_n_obs <- summarized_DT %>%
  summarize(sum(n_obs)) %>%
  pull() %>%
  big_mark()
ctrl_group_obs <- summarized_DT %>%
  filter(is_treated_server == 0) %>%
  select(n_obs) %>%
  pull() %>%
  big_mark()
treat_group_obs <- summarized_DT %>%
  filter(is_treated_server == 1) %>%
  summarize(sum_n_obs = sum(n_obs)) %>%
  select(sum_n_obs) %>%
  pull() %>%
  big_mark()

t <- summarized_wide_DT %>%
  flextable() %>%
  set_header_labels(
    server_region = "Publisher's Designation",
    n_obs_prop_fmt = "Number and Percentage of Observations"
  ) %>%
  flextable::align(align = "center", part = "all") %>%
  vline(j = 1, border = std_border, part = "header") %>%
  vline(j = 1, border = std_border, part = "body") %>%
  hline(i = 2, border = thick_border, part = "body") %>%
  bg(i = 1, j = 2, part = "body", bg = "white") %>%
  bg(i = 2, j = 2, part = "body", bg = "lightgray") %>%
  footnote(
    i = 1, j = 1,
    value = as_paragraph("A publisher is designated as an “EU publisher” if the publisher's server is located inside the EU."),
    ref_symbols = c("1"), part = "body", inline = T, sep = " "
  ) %>% 
  footnote(
    i = 2, j = 1,
    value = as_paragraph("A publisher is designated as a “non-EU publisher” if the publisher's server is located outside the EU."),
    ref_symbols = c("2"), part = "body", inline = T, sep = " "
  ) %>% 
  add_footer_lines(
    paste0(
      "Notes: The cells in this table show the number and percentage of observations in our sample corresponding to each case. The cell belonging to the control group—where GDPR does not apply—is colored gray, and the cell belonging to the treatment group—where GDPR applies—is not colored. In total, 37% (N observations = ", treat_group_obs, ") of all observations (N observations = ", total_n_obs, ") belong to the treatment group and 63% (N observations = ", ctrl_group_obs, ") to the control group."
    )
  ) %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_11.docx")


# Figure 7: Development of the Average Monthly Number of Trackers with Treatment Assignment Based on Server Location --------------

data_summary <- global_sites_trackers_panel_DT %>%
  group_by(ymd, is_treated_server) %>%
  summarize(mean_n_trackers = mean(n_trackers, na.rm = TRUE), .groups = "keep")

data_summary$is_treated_server <- factor(data_summary$is_treated_server, levels = c(1, 0))

ggplot(data_summary, aes(x = ymd, y = mean_n_trackers, linetype = factor(is_treated_server))) +
  geom_point(size = 4, aes(shape = factor(is_treated_server))) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +
  labs(y = "Average Number of Trackers", x = "", linetype = "Group:", shape = "Group:") +
  coord_cartesian(ylim = c(1, 35)) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("1" = "solid", "0" = "dashed"),
                        labels = c("Treatment", "Control")) +
  scale_shape_manual(values = c(16, 3),
                     labels = c("Treatment", "Control")) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank()
  )

ggsave("03_results/web_appendix/figure_07.pdf", width=8, height=6, font="Times", dpi=300)


# Table 12: Result of Difference-in-Differences Analysis for the Sample with a Server Location Treatment Assignment ------------------

m_server <- feols(n_trackers ~ is_treated_server * post_GDPR | site_id + period_id,
      cluster = ~site_id + period_id,
      data = global_sites_trackers_panel_DT
)

t <- etable(m_server,
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 5, 6, 9)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:", 
                    "m_server" = "(1)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 5, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regression. We assign treatment to each publisher according to the publisher’s server location (within EU or outside EU). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_12.docx")


# Table 13: Distribution of Observations (Monthly Publishers) Across Publisher Designation and User Location (EU vs. US) --------

sites_in_main_sample <- global_sites_trackers_panel_DT[, unique(site)]
eu_us_sites_trackers_panel_DT <- eu_us_sites_trackers_panel_DT[site %in% sites_in_main_sample]

uniqueN(eu_us_sites_trackers_panel_DT$site_id)
uniqueN(eu_us_sites_trackers_panel_DT$period_id)

summarized_DT <- eu_us_sites_trackers_panel_DT[, .(n_obs = .N),
                                               by = .(country, target_audience, is_treated_target_audience)
] %>%
  mutate(prop = n_obs / sum(n_obs)) %>%
  arrange(country) %>%
  mutate(target_audience = ifelse(target_audience == "Non-EU", "Non-EU publisher", "EU publisher"))

summarized_wide_DT <- dcast(summarized_DT,
                            formula = target_audience ~ country,
                            value.var = c("n_obs", "prop")
) %>%
  arrange(target_audience) %>%
  add_row(
    target_audience = "∑", n_obs_eu = sum(.$n_obs_eu), n_obs_us = sum(.$n_obs_us),
    prop_eu = sum(.$prop_eu), prop_us = sum(.$prop_us)
  )

summarized_wide_DT <-  summarized_wide_DT %>%
  group_by(target_audience) %>%
  mutate(
    n_obs_total = n_obs_eu + n_obs_us,
    prop_total = n_obs_total / N_obs
  ) %>%
  ungroup() %>%
  mutate(
    n_obs_eu_fmt = scales::comma(n_obs_eu),
    n_obs_us_fmt = scales::comma(n_obs_us),
    n_obs_total_fmt = scales::comma(n_obs_total),
    prop_eu_fmt = paste0("(", scales::percent(prop_eu, rounding_pct_scales_percent), ")"),
    prop_us_fmt = paste0("(", scales::percent(prop_us, rounding_pct_scales_percent), ")"),
    prop_total_fmt = paste0("(", scales::percent(prop_total, rounding_pct_scales_percent), ")"),
    n_prop_obs_eu = paste(n_obs_eu_fmt, prop_eu_fmt, sep = " "),
    n_prop_obs_us = paste(n_obs_us_fmt, prop_us_fmt, sep = " "),
    n_prop_obs_total = paste(n_obs_total_fmt, prop_total_fmt, sep = " ")
  ) %>%
  select(
    target_audience,
    n_prop_obs_eu,
    n_prop_obs_us,
    n_prop_obs_total
  )

col_header <- tribble(
  ~col_keys, ~line2, ~line3,
  "n_prop_obs_eu", "User Location", "EU",
  "n_prop_obs_us", "User Location", "US",
  "target_audience", "", "Publisher Designation",
  "n_prop_obs_total", "", "∑"
)

total_n_obs <- summarized_DT %>%
  summarize(sum(n_obs)) %>%
  pull() %>%
  big_mark()
ctrl_group_obs <- summarized_DT %>%
  filter(is_treated_target_audience == 0) %>%
  select(n_obs) %>%
  pull() %>%
  big_mark()
treat_group_obs <- summarized_DT %>%
  filter(is_treated_target_audience == 1) %>%
  summarize(sum_n_obs = sum(n_obs)) %>%
  select(sum_n_obs) %>%
  pull() %>%
  big_mark()

t <- summarized_wide_DT %>%
  flextable() %>%
  set_header_df(mapping = col_header, key = "col_keys") %>%
  merge_h(part = "header") %>%
  flextable::align(align = "center", part = "all") %>%
  hline_top(border = std_border, part = "header") %>% 
  hline(i = 1, border = std_border, part = "header") %>%
  hline(i = 2, border = std_border, part = "header") %>%
  hline(i = 2, border = thick_border, part = "body") %>%
  vline(j = 1, border = std_border) %>%
  vline(j = 3, border = thick_border) %>%
  bg(i = 2, j = 3, part = "body", bg = "lightgray") %>%
  footnote(
    i = 1, j = 1,
    value = as_paragraph('A publisher is designated as an “EU publisher” if (1) the publisher uses an EU top-level domain (e.g., .de) or (2) the publisher receives more traffic from EU than non-EU users.'),
    ref_symbols = c("1"), part = "body", inline = T, sep = " "
  ) %>% 
  footnote(
    i = 2, j = 1,
    value = as_paragraph('A publisher is designated as a “non-EU publisher” if (1) the publisher uses a non-EU top-level domain (e.g., .com) and (2) the publisher receives more traffic from non-EU users than EU users.'),
    ref_symbols = c("2"), part = "body", inline = T, sep = " "
  ) %>% 
  add_footer_lines(
    paste0(
      "Notes: The cells in this table show the number and the percentage of observations (corresponding to monthly publisher instances) in our sample corresponding to each case. The cell belonging to the control group—where GDPR does not apply—is colored gray, and the cells belonging to the treatment group—where GDPR applies—are not colored. In total, 61% (N observations = ", treat_group_obs, ") of all observations (N observations = ", total_n_obs, ") belong to the treatment group, and 39% (N observations = ", ctrl_group_obs, ") belong to the control group."
    )
  ) %>%
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_13.docx")


# Figure 8: Development of the Average Monthly Number of Trackers in the Secondary Sample with Treatment Assignment Based on Publisher Designation and User Location (EU vs. US) ----------
  
data_summary <- eu_us_sites_trackers_panel_DT %>%
  group_by(ymd, is_treated_target_audience_facet) %>%
  summarize(mean_n_trackers = mean(n_trackers, na.rm = TRUE), .groups = "keep")

data_summary$is_treated_target_audience_facet <- factor(data_summary$is_treated_target_audience_facet, levels = c(1, 0))

ggplot(data_summary, aes(x = ymd, y = mean_n_trackers, linetype = factor(is_treated_target_audience_facet))) +
  geom_point(size = 4, aes(shape = factor(is_treated_target_audience_facet))) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +
  labs(y = "Average Number of Trackers", x = "", linetype = "Group:", shape = "Group:") +
  coord_cartesian(ylim = c(1, 35)) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("1" = "solid", "0" = "dashed"),
                        labels = c("Treatment", "Control")) +
  scale_shape_manual(values = c(16, 3),
                     labels = c("Treatment", "Control")) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank()
        )

ggsave("03_results/web_appendix/figure_08.pdf", width=8, height=6, font="Times", dpi=300)


# Table 14: Result of Difference-in-Differences Analysis for the Number of Trackers of a Secondary Sample with Treatment Assignment Based on Publisher Designation and User Location (EU vs. US) ----------

uniqueN(global_sites_trackers_panel_DT$period_id)
global_sites_trackers_panel_DT[, uniqueN(period_id), by = ymd]
global_sites_trackers_panel_DT[, .(site_id, ymd, period_id, is_treated_target_audience, post_GDPR, n_trackers)]

data_before_may_2018 <- global_sites_trackers_panel_DT[ymd <= "2018-04-01"]

avg_trackers <- data_before_may_2018[, .(avg_n_trackers = mean(n_trackers)), by = site_id]

data_april_2018 <- data_before_may_2018[ymd == "2018-04-01"]
data_april_2018[, n_trackers := avg_trackers$avg_n_trackers]

data_from_may_2018 <- global_sites_trackers_panel_DT[ymd > "2018-04-01"]

adjusted_global_sites_trackers_panel_DT <- rbind(data_april_2018, data_from_may_2018)
adjusted_global_sites_trackers_panel_DT[, period_id := .GRP, by = ymd]
adjusted_global_sites_trackers_panel_DT[, .(site_id, ymd, period_id, is_treated_target_audience, post_GDPR, n_trackers)]

uniqueN(adjusted_global_sites_trackers_panel_DT$site_id)
uniqueN(adjusted_global_sites_trackers_panel_DT$period_id)

m3 <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_id + period_id,
            cluster = ~site_id + period_id,
            data = adjusted_global_sites_trackers_panel_DT
)

etable(
  m3, view = F, digits = "r3",
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))


m4 <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_instance_id + period_id,
            cluster = ~site_instance_id + period_id,
            data = eu_us_sites_trackers_panel_DT
)

etable(
  m4, view = F, digits = "r3", 
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

t <- etable(m4, m3,
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 6, 7, 10)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  slice(c(1, 2, 4, 3, 5, 6)) %>%  # reorder rows
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:", 
                    "m4" = "(1)",
                    "m3" = "(2)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher Instance and Month",
                            "Number of Trackers per Publisher and Month"
                            )) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 5, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 6, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher instance and month levels in model (1); Two-way standard errors are clustered at the publisher and month levels in model (2); 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regressions. In model (1), we assign treatment to each publisher instance according to the publisher’s designation (EU or non-EU) and the users who visited it (EU vs. US). Multiplying the number of publishers (N publishers = 294), the number of publisher instances (N publisher instances per publisher = 2) and the number of months (T = 21 months) yields the number of observations (N observations = 12,348) in model (1). In model (2), we assign treatment to each publisher according to the publisher’s designation (EU or non-EU). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 21 months) yields the number of observations (N observations = 6,174) in model (2).") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_14.docx")


# Figure 9: Development of the Monthly Difference-in-Differences Coefficients for the Number of Trackers -------------------------------

interaction_did <- feols(n_trackers ~ i(period_id, is_treated_target_audience, 
                                        12) #  (immediately prior to treatment that starts in period 13)
                         | site_id + period_id,
                         cluster = ~site_id + period_id, data = global_sites_trackers_panel_DT)

months_DT <- global_sites_trackers_panel_DT %>% 
  select(period_id, ymd, ym) %>% 
  distinct()

interaction_did_DT <- tidy(interaction_did, conf.int = T) %>%  # add nice formatting of periods
  separate(col = "term", into = c("period_var", "empty_string", "period_id", "treat_var"), sep = ":") %>%
  mutate(period_id = as.integer(period_id)) %>% 
  merge(., months_DT, on = "period_id", all.y = T) %>% 
  mutate(estimate = replace_na(estimate, value = 0)) %>% 
  arrange(period_id)

caption_text <- "Notes: This figure shows the monthly difference-in-differences coefficients (Treatment x PostGDPR) from the OLS regression. We assign treatment to each publisher according to the publisher's designation (EU or non-EU). The model includes publisher and month fixed effects. Two-way standard errors are clustered at the publisher and month levels. The first month (April 2018) before GDPR's enactment serves as a reference month in the estimation, so its coefficient is zero."
caption_text <- str_replace_all(caption_text, "’", "'")

ggplot(interaction_did_DT, aes(x = as.Date(ymd), 
                               y = estimate, 
                               group = 1)) +
  geom_point(size = 2, shape = 16) + 
  geom_line(linewidth = 0.3) +
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = 0.5) +
  scale_y_continuous(n.breaks = 12) +
  geom_hline(yintercept = 0, linetype = 1) + 
  geom_vline(xintercept = as.Date("2018-04-01"), linetype = 2, size = 0.5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  labs(y = "DiD Coefficient and 95% Confidence Interval",
       x = "",
       caption = str_wrap(caption_text, width = 110)) + 
  theme_classic(base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
    plot.caption = element_text(hjust = 0, size = 12),
    panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
    panel.grid.minor = element_blank()
    )

ggsave("03_results/web_appendix/figure_09.pdf", width=8, height=6, font="Times", dpi=300)


# Table 15: Result of Placebo Difference-in-Differences Analysis for the Number of Trackers ----

adjusted_global_sites_trackers_panel_before_GDPR_DT <- global_sites_trackers_panel_DT[period_id <= 12]

adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_one := ifelse(period_id >= 2, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_two := ifelse(period_id >= 3, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_three := ifelse(period_id >= 4, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_four := ifelse(period_id >= 5, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_five := ifelse(period_id >= 6, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_six := ifelse(period_id >= 7, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_seven := ifelse(period_id >= 8, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_eight := ifelse(period_id >= 9, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_nine := ifelse(period_id >= 10, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_ten := ifelse(period_id >= 11, 1L, 0L)]
adjusted_global_sites_trackers_panel_before_GDPR_DT <- adjusted_global_sites_trackers_panel_before_GDPR_DT[, fake_post_GDPR_eleven := ifelse(period_id >= 12, 1L, 0L)]

placebo_model_one <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_one | site_id + period_id,
                           cluster = ~site_id + period_id,
                           data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_two <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_two | site_id + period_id,
                           cluster = ~site_id + period_id,
                           data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_three <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_three | site_id + period_id,
                             cluster = ~site_id + period_id,
                             data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_four <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_four | site_id + period_id,
                            cluster = ~site_id + period_id,
                            data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_five <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_five | site_id + period_id,
                            cluster = ~site_id + period_id,
                            data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_six <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_six | site_id + period_id,
                           cluster = ~site_id + period_id,
                           data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_seven <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_seven | site_id + period_id,
                             cluster = ~site_id + period_id,
                             data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_eight <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_eight | site_id + period_id,
                             cluster = ~site_id + period_id,
                             data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_nine <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_nine | site_id + period_id,
                            cluster = ~site_id + period_id,
                            data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_ten <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_ten | site_id + period_id,
                           cluster = ~site_id + period_id,
                           data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)
placebo_model_eleven <- feols(n_trackers ~ is_treated_target_audience * fake_post_GDPR_eleven | site_id + period_id,
                              cluster = ~site_id + period_id,
                              data = adjusted_global_sites_trackers_panel_before_GDPR_DT
)

t <- etable(
  placebo_model_one,
  placebo_model_two,
  placebo_model_three,
  placebo_model_four,
  placebo_model_five,
  placebo_model_six,
  placebo_model_seven,
  placebo_model_eight,
  placebo_model_nine,
  placebo_model_ten,
  placebo_model_eleven,
  coefstat = "confint",
  # headers = c("Test", "Test2"),
  depvar = F,
  digits = "r3",
  digits.stats = 3,
  style.df = style.df
  (
    depvar.title = "Dependent Variable:",
    fixef.line = "_",
    fixef.suffix = " Fixed Effects",
    yesNo = c("✓", "✘"),
    stats.title = "Fit Statistics:",
  )
) %>%
  as_tibble(.name_repair = "unique") %>%
  filter(!row_number() %in% c(2, 5, 6, 9)) %>% # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>%
  border_remove() %>%
  set_header_labels(
    "...1" = "Placebo GDPR Treatment in:",
    "placebo_model_one" = "2017-06",
    "placebo_model_two" = "2017-07",
    "placebo_model_three" = "2017-08",
    "placebo_model_four" = "2017-09",
    "placebo_model_five" = "2017-10",
    "placebo_model_six" = "2017-11",
    "placebo_model_seven" = "2017-12",
    "placebo_model_eight" = "2018-01",
    "placebo_model_nine" = "2018-02",
    "placebo_model_ten" = "2018-03",
    "placebo_model_eleven" = "2018-04"
  ) %>%
  add_header_row(values = c(
    "Model:",
    "(1)",
    "(2)",
    "(3)",
    "(4)",
    "(5)",
    "(6)",
    "(7)",
    "(8)",
    "(9)",
    "(10)",
    "(11)"
  )) %>%
  add_header_row(values = c(
    "Dependent Variable:",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month",
    "Number of Trackers per Publisher and Month"
  )) %>%
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>%
  hline_bottom(border = thick_border) %>%
  hline(i = 2, border = std_border, part = "header") %>%
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>%
  compose(
    i = 5, j = 1,
    value = as_paragraph(
      "R",
      as_chunk("2", props = fp_text(vertical.align = "superscript"))
    )
  ) %>%
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>%
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>%
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostPretend) from each OLS regression used for the placebo test. We use observations before GDPR's enactment (May 2017-April 2018) and assign treatment to each publisher using the publisher’s designation (EU or non-EU). Multiplying the number of publisher (N publishers = 294), and the number of months before GDPR's enactment (T = 12 months) yields the number of observations (N observations = 3,528).") %>%
  flextable::align(part = "footer", align = "justify") %>%
  flextable::add_footer_lines("") %>% # add empty line
  flextable::keep_with_next() %>% 
  flextable::fontsize(size = 9, part = "all") # fit to landscape Word page
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_15.docx")


# Table 16: Result of Difference-in-Differences Analysis for the Number of Trackers Between Different Definitions of the Treatment and Control Groups --------------

cell_one_DT <- eu_us_sites_trackers_panel_DT[country == "eu" & target_audience == "EU"]
cell_two_DT <- eu_us_sites_trackers_panel_DT[country == "us" & target_audience == "EU"]
cell_three_DT <- eu_us_sites_trackers_panel_DT[country == "eu" & target_audience == "Non-EU"]
cell_four_DT <- eu_us_sites_trackers_panel_DT[country == "us" & target_audience == "Non-EU"] # control group 

cell_one_vs_cell_four_DT <- rbind(cell_one_DT, cell_four_DT) %>% 
  arrange(site_id)

rc_1_did_estimate_n_trackers <- feols(n_trackers ~ # DV
                                        is_treated_target_audience * post_GDPR # DiD estimate
                                      | site_instance_id + period_id, # FEs
                                      cluster = ~ site_instance_id + period_id, 
                                      data = cell_one_vs_cell_four_DT)

etable(rc_1_did_estimate_n_trackers, coefstat = "confint")

cell_three_vs_cell_four_DT <- rbind(cell_three_DT, cell_four_DT) %>% 
  arrange(site_id)

rc_2_did_estimate_n_trackers <- feols(n_trackers ~ # DV
                                        is_treated_target_audience * post_GDPR # DiD estimate
                                      | site_instance_id + period_id, # FEs
                                      cluster = ~ site_instance_id + period_id, 
                                      data = cell_three_vs_cell_four_DT)

etable(rc_2_did_estimate_n_trackers, coefstat = "confint")

cell_two_vs_cell_four_DT <- rbind(cell_two_DT, cell_four_DT) %>% 
  arrange(site_id)

rc_3_did_estimate_n_trackers <- feols(n_trackers ~ # DV
                                        is_treated_target_audience * post_GDPR # DiD estimate
                                      | site_instance_id + period_id, # FEs
                                      cluster = ~ site_instance_id + period_id, 
                                      data = cell_two_vs_cell_four_DT)

etable(rc_3_did_estimate_n_trackers, coefstat = "confint")

t <- etable(rc_1_did_estimate_n_trackers,
            rc_2_did_estimate_n_trackers,
            rc_3_did_estimate_n_trackers,
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 5, 6, 9)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model: ", 
                    "rc_1_did_estimate_n_trac.." = "(1) \n\n Cell 1 vs. Cell 4", 
                    "rc_2_did_estimate_n_trac.." = "(2) \n\n Cell 3 vs. Cell 4", 
                    "rc_3_did_estimate_n.." = "(3) \n\n Cell 2 vs. Cell 4") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher Instance and Month", 
                            "Number of Trackers per Publisher Instance and Month", 
                            "Number of Trackers per Publisher Instance and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>% 
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 5, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  # compose(i = 6, j = 1, 
  #         value = as_paragraph("Within R", 
  #                              as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher instance and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficients (Treatment x PostGDPR) from the OLS regressions between different cells in our treatment assignment framework. We assign treatment to each publisher instance according to the publisher’s designation (EU or non-EU) and the users who visited it (EU vs. US). Multiplying the number of publisher instances (N publisher instances) per cell(s) in our sample and the number of months (T = 21 months) yields the number of observations (N observations) for each model.") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_16.docx")


# Figure 10: Development of the Number of Ghostery Extension Users for Chrome and Firefox Browsers per Wayback Machine Capture Date -----------------

ghostery_chrome_tbl <- ia_tbl %>%
  filter(
    data_for_whotracksme_data_source == "ghostery_chrome_browser_addon",
    date_of_capture >= "2018-02-08", date_of_capture <= "2020-01-01" # observation period used in our study
  ) %>% 
  select(
    data_source, data_for_whotracksme_data_source, name_for_whotracksme_data_source,
    date_of_capture, number_of_users
  ) %>%
  mutate(
    after_GDPR = ifelse(date_of_capture >= "2018-05-25", 1L, 0L),
    period = ifelse(after_GDPR == 0L, "before GDPR", "after GDPR"),
    period = as_factor(period)
  ) %>%
  mutate(
    year = lubridate::year(date_of_capture),
    month = lubridate::month(date_of_capture),
    day = lubridate::day(date_of_capture),
    n_days_from_2018_04_01 = as.numeric(date_of_capture) - as.numeric(as.Date("2018-04-01"))
  )

ghostery_firefox_tbl <- ia_tbl %>% 
  filter(data_for_whotracksme_data_source == "ghostery_firefox_browser_addon",
         date_of_capture >= "2017-05-12",
         date_of_capture <= "2020-01-01") %>% 
  select(data_source, data_for_whotracksme_data_source, name_for_whotracksme_data_source, 
         date_of_capture, number_of_users) %>%
  mutate(after_GDPR = ifelse(date_of_capture >= "2018-05-25", 1L, 0L),
         period = ifelse(after_GDPR == 0L, "before GDPR", "after GDPR"),
         period = as_factor(period)) %>% 
  mutate(
    year = lubridate::year(date_of_capture),
    month = lubridate::month(date_of_capture),
    day = lubridate::day(date_of_capture),
    n_days_from_2018_04_01 = as.numeric(date_of_capture) - as.numeric(as.Date("2018-04-01")))

my_theme <- theme_classic(base_family = "Times")
scale_x_date_monthly <- scale_x_date(date_breaks = "2 month", labels = scales::date_format("%Y-%m")) 
scale_x_date_monthly <- scale_x_date(breaks = seq(as.Date("2018-05-01"), max(ghostery_chrome_tbl$date_of_capture), by = "2 month"), labels = scales::date_format("%Y-%m"))
theme_x_axis_label <- theme(axis.text.x = element_text(angle = 60, hjust = 1))
add_linear_trend_line <- geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black", linewidth = 0.6, alpha = 0.1)

fig_1 <- ghostery_chrome_tbl %>% 
  ggplot(., aes(x = date_of_capture, y = number_of_users)) + 
  geom_point(color = "gray") +
  scale_y_continuous(limits = c(0, 4000000), labels = scales::label_comma()) + 
  scale_x_date(breaks = seq(as.Date("2018-02-08"), max(ghostery_chrome_tbl$date_of_capture), by = "2 month"), labels = scales::date_format("%Y-%m")) +
  labs(x = "", 
       y = "",
       subtitle = "Ghostery Extension for the Chrome Browser"
  ) +
  geom_vline(xintercept = as.Date("2018-05-25"), linetype = 2, size = 0.3) +
  my_theme +
  theme_x_axis_label +
  add_linear_trend_line + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 0.9))

fig_2 <- ghostery_firefox_tbl %>% 
  ggplot(., aes(x = date_of_capture, y = number_of_users)) + 
  geom_point(color = "gray") +
  scale_y_continuous(limits = c(0, 4000000), labels = scales::label_comma()) + 
  scale_x_date(breaks = seq(as.Date("2017-04-12"), max(ghostery_chrome_tbl$date_of_capture), by = "2 month"), labels = scales::date_format("%Y-%m")) +
  labs(x = "", 
       y = "",
       subtitle = "Ghostery Extension for the Firefox Browser"
  ) +
  geom_vline(xintercept = as.Date("2018-05-25"), linetype = 2, size = 0.3) +
  my_theme +
  theme_x_axis_label +
  add_linear_trend_line +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        axis.title.y = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 0.9))

figure <- ggarrange(fig_1, fig_2, nrow = 2)

figure <- annotate_figure(figure,
                          left = text_grob("Number of Users", 
                                           family = "Times", rot = 90, 
                                           size = 11, 
                                           hjust = -0.1), 
                          bottom = text_grob("Wayback Machine Capture Date", 
                                             family = "Times", 
                                             size = 11))
figure

ggsave("03_results/web_appendix/figure_10.pdf", width=6, height=6, font="Times", dpi=300)


# Table 17: Result of Regressions for the Number of Ghostery Extension Users ----------------

lm_1 <- feols(number_of_users ~ after_GDPR, ghostery_chrome_tbl)
lm_2 <- feols(number_of_users ~ n_days_from_2018_04_01 + after_GDPR, ghostery_chrome_tbl)
lm_3 <- feols(number_of_users ~ after_GDPR, ghostery_firefox_tbl)
lm_4 <- feols(number_of_users ~ n_days_from_2018_04_01 + after_GDPR, ghostery_firefox_tbl)

t <- etable("(1)" = lm_1, "(2)" = lm_2, "(3)" = lm_3, "(4)" = lm_4,
            headers = c("Ghostery Extension for Chrome Browser", 
                        "Ghostery Extension for Chrome Browser", 
                        "Ghostery Extension for Firefox Browser", 
                        "Ghostery Extension for Firefox Browser"),
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(6, 7)) %>%  # remove "VCOV..."
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:") %>% 
  merge_h(part = "body", 1) %>%
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Users per Wayback Machine Capture Date",
                            "Number of Users per Wayback Machine Capture Date",
                            "Number of Users per Wayback Machine Capture Date",
                            "Number of Users per Wayback Machine Capture Date")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  hline(i = 1, border = std_border, part = "body") %>% 
  compose(i = 6, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 7, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  compose(i = 8, j = 1, 
          value = as_paragraph("Adjusted R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table displays the PostGDPR coefficients obtained from OLS regressions. PostGDPR coefficients represent the difference in the number of users of the Ghostery browser extension after the GDPR's enactment. Models (1) and (2) have 49 observations based on weekly captures of Google Chrome's Ghostery product listing page from the Internet Archive's Wayback Machine. Models (3) and (4) have 83 observations, using weekly captures from Firefox's Ghostery product listing page.") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_17.docx")


# Table 18: Steps Taken to Prepare the Sample of 294 Publishers -----------------

t <- read_excel(here::here("01_data", 
                           "lookup_tables",
                           "description_of_sample_preparation-2025-01-09.xlsx"), 
                col_names = T
) %>% 
  mutate("Percent Change" = (`Percent Change` * 100)) %>% 
  flextable() %>% 
  autofit() %>% 
  colformat_double(j = "Percent Change", digits = 2, suffix = "%") %>% 
  hline(i = c(2, 4, 5), border = std_border) %>%
  flextable::add_footer_lines("") %>%  # add empty line
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 10, part = "all") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_18.docx")


# Table 19: Description of Publisher Industries and Types  --------------------------

t <- read_excel(here::here("01_data", 
                           "lookup_tables", 
                           "site_category_description-2025-01-09.xlsx"), 
                col_names = T
) %>% 
  flextable() %>% 
  set_header_labels(publisher_industry = "Publisher Industry", 
                    category_id = "", 
                    site_category = "Publisher Type",
                    description_of_site_category = "Description of Publisher Type",
                    examples_of_sites = "Examples of Publishers") %>% 
  merge_v(j = "publisher_industry") %>% 
  fix_border_issues() %>% 
  hline(i = 1, border = std_border) %>%
  flextable::align(align = "center", part = "all") %>%  # align text in all cells "center"
  flextable::fontsize(size = 10.0, part = "all") %>% # fit to Word page
  flextable::add_footer_lines("Notes: We adapt this table from Karaj et al. (2018)") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_19.docx")


# Table 20: Result of Difference-in-Differences Analysis without March, April, May and June 2018 -------------------------

global_sites_trackers_panel_DT %>% select(ymd, period_id) %>% unique()
compliance_DT <- global_sites_trackers_panel_DT[!period_id %in% c(11, 12, 13, 14)]
compliance_DT %>% select(ymd, period_id) %>% unique()

est_obedience <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_id + period_id,
                       cluster = ~site_id + period_id,
                       data = compliance_DT
)

t <- etable(est_obedience,
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 5, 6, 9)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model: ", 
                    "est_obedience" = "(1)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 5, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regression. We remove the months of March, April, May and June 2018 from our sample and assign treatment to each publisher according to the publisher’s designation (EU or non-EU). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 28 months) yields the number of observations (N observations = 8,232).") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_20.docx")


# Figure 11: Development of the Log of Average Monthly Number of Trackers in the Treatment and Control Groups ----------

data_summary <- global_sites_trackers_panel_DT %>%
  group_by(ymd, is_treated_target_audience_facet) %>%
  summarize(mean_n_trackers = mean(n_trackers, na.rm = TRUE), .groups = "keep")

data_summary$is_treated_target_audience_facet <- factor(data_summary$is_treated_target_audience_facet, levels = c(1, 0))

ggplot(data_summary, aes(x = ymd, y = log(mean_n_trackers), linetype = factor(is_treated_target_audience_facet))) +
  geom_point(size = 4, aes(shape = factor(is_treated_target_audience_facet))) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +
  labs(y = "Log of Average Number of Trackers", x = "", linetype = "Group:", shape = "Group:") +
  coord_cartesian(ylim = c(1, 4)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("1" = "solid", "0" = "dashed"),
                        labels = c("Treatment", "Control")) +
  scale_shape_manual(values = c(16, 3),
                     labels = c("Treatment", "Control")) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank()
  )

ggsave("03_results/web_appendix/figure_11.pdf", width=8, height=6, font="Times", dpi=300)


# Table 21: Result of Difference-in-Differences Analysis with Log Transformation of the Dependent Variable  ----------

mlog <- feols(log(n_trackers) ~ is_treated_target_audience * post_GDPR | site_id + period_id,
            cluster = ~site_id + period_id,
            data = global_sites_trackers_panel_DT
)

etable(
  mlog, view = F, digits = "r3", 
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

t <- etable(mlog,
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 5, 6, 9)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:", 
                    "mlog" = "(1)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Logarithm of the Number of Trackers per Publisher and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 5, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regression. We assign treatment to each publisher according to the publisher’s designation (EU or non-EU). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_21.docx")


# Figure 12: Estimated Fixed Effects Coefficients for Publisher ID and Month ID ------

global_sites_trackers_panel_DT[, site_id := .GRP, by = site]

m2 <- feols(n_trackers ~ is_treated_target_audience * post_GDPR | site_id + period_id,
            cluster = ~site_id + period_id,
            data = global_sites_trackers_panel_DT
)

etable(
  m2, view = F, digits = "r3", 
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

fe_m2 <- fixef(m2)

fe_site <- data.frame(site_id = names(fe_m2$site_id), fe_value = fe_m2$site_id)
fe_period <- data.frame(period_id = names(fe_m2$period_id), fe_value = fe_m2$period_id)

fe_combined <- bind_rows(
  fe_site %>% mutate(type = "Publisher ID", id = as.numeric(site_id)) %>% select(id, fe_value, type),
  fe_period %>% mutate(type = "Month ID", id = as.numeric(period_id)) %>% select(id, fe_value, type)
)

fe_site_plot <- fe_combined %>% filter(type == "Publisher ID")
fe_period_plot <- fe_combined %>% filter(type == "Month ID")

plot_site <- ggplot(fe_site_plot, aes(x = id, y = fe_value)) +
  geom_point(size = 0.7) +
  scale_x_continuous(breaks = pretty(fe_site_plot$id, n = 10)) +  # Show a subset of x-axis labels
  scale_y_continuous(limits = c(-10, 60), n.breaks = 10) +
  labs(
    x = "Publisher ID",
    y = ""
  ) +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.x = element_text(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
    axis.title.y = element_text(size = 10)
  )

plot_period <- ggplot(fe_period_plot, aes(x = id, y = fe_value)) +
  geom_point(size = 0.7) +
  scale_x_continuous(breaks = seq(1, 32, by = 1)) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    x = "Month ID",
    y = ""
  ) +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.x = element_text(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
    axis.title.y = element_text(size = 10)
  )

figure <- ggarrange(plot_site, plot_period, ncol = 1)

figure <- annotate_figure(figure,
                          left = text_grob("Centered Fixed Effects Coefficient", 
                                           family = "Times", rot = 90, 
                                           size = 11, hjust = 0.4))
figure

ggsave("03_results/web_appendix/figure_12.pdf", width=6, height=6, font="Times", dpi=300)


# Table 22: Result of Difference-in-Differences Analysis for the Number of Trackers with a Linear Time Trend -----

global_sites_trackers_panel_DT[, linear_trend := period_id]

m2_with_linear_trend <- feols(n_trackers ~ is_treated_target_audience * post_GDPR + linear_trend | site_id,
                              cluster = ~site_id + period_id,
                              data = global_sites_trackers_panel_DT)

etable(
  m2_with_linear_trend, view = F, digits = "r3", 
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

t <- etable(m2_with_linear_trend,
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(4, 6, 7, 10)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:", 
                    "m2_with_linear_trend" = "(1)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 5, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 6, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regression. We assign treatment to each publisher according to the publisher’s designation (EU or non-EU). Multiplying the number of publishers (N publishers = 294) and the number of months (T = 32 months) yields the number of observations (N observations = 9,408).") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_22.docx")


# Table 23: Detailed Estimated Fixed Effects Coefficients for Publisher ID and Month ID ----

fe_site <- data.frame(site_id = names(fe_m2$site_id), fe_site_id = fe_m2$site_id)
fe_period <- data.frame(period_id = names(fe_m2$period_id), fe_period_id = fe_m2$period_id)

fe_combined <- bind_rows(
  fe_site %>% mutate(type = "Publisher ID"),
  fe_period %>% mutate(type = "Month ID")
) %>% 
  relocate(type)

t <- fe_combined %>% 
  flextable() %>% 
  flextable::set_header_labels(type = "Type of FE",
                               site_id = "Publisher ID",
                               fe_site_id = "Publisher ID Centered FE Coefficient",
                               period_id = "Month ID", 
                               fe_period_id = "Month ID Centered FE Coefficient") %>% 
  flextable::colformat_double(j = c("fe_site_id", "fe_period_id"), digits = 3) %>% 
  flextable::add_footer_lines(c(
    "Notes: We abbreviate \"Fixed Effect\" to \"FE\". Summary of Fixed Effects Coefficients:",
    "Publisher ID Centered FE Coefficients: Mean = 9.231, SD = 10.774, N = 294;",
    "Month ID Centered FE Coefficients: Mean = 8.020, SD = 5.423, N = 32."
  )) %>%
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 8, part = "all") %>% 
  # flextable::keep_with_next() %>% 
  flextable::padding(padding = 2) %>% 
  flextable::padding(padding = 6, part = "header") %>% 
  flextable::autofit()
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_23.docx")


# Table 24: Summary of EU Traffic Share Differences Between Public and Proprietary SimilarWeb Data Sets ------------------------

sites_in_main_sample <- global_sites_trackers_panel_DT[, unique(site)]

sw_public_filtered_DT <- sw_public_DT %>%
  filter(site %in% sites_in_main_sample) %>%
  mutate(eea_share_public = share_EEA_traffic_top_five_countries,
         non_eea_share_public = share_non_EEA_traffic_top_five_countries) %>%
  select(site, eea_share_public, non_eea_share_public)
sw_public_filtered_DT

sw_proprietary_filtered_DT

comparison_data <- sw_proprietary_filtered_DT %>%
  left_join(sw_public_filtered_DT, by = "site") %>%
  mutate(eea_share_difference = abs(eea_share_proprietary - eea_share_public))

summary_comparison <- comparison_data %>%
  summarise(
    mean_eea_share_difference = mean(eea_share_difference, na.rm = TRUE)
  )
setDT(summary_comparison)

cat("Summary of EEA Share Differences Between Proprietary and Public Data:\n")
summary_comparison

overall_summary <- summary_comparison %>%
  summarise(
    mean_difference = mean(mean_eea_share_difference, na.rm = TRUE),
    max_difference = max(mean_eea_share_difference, na.rm = TRUE),
    min_difference = min(mean_eea_share_difference, na.rm = TRUE),
    sd_difference = sd(mean_eea_share_difference, na.rm = TRUE)
  )

cat("Summary of EEA Share Differences Between Public and Proprietary SimilarWeb Data:\n")
overall_summary

overall_summary_df <- data.frame(
  Statistic = c("Average EU Traffic Share Difference", 
                "Max EU Traffic Share Difference", 
                "Min EU Traffic Share Difference", 
                "SD EU Traffic Share Difference"),
  Value = c(overall_summary$mean_difference,
            overall_summary$max_difference,
            overall_summary$min_difference,
            overall_summary$sd_difference)
) %>% 
  mutate(Value = Value * 100)  # Keep the values in percentage format

t <- flextable(overall_summary_df) %>% 
  set_header_labels(Statistic = "Statistic",
                    Value = "Value (pp)") %>%  # Updated to reflect percentage points (pp)
  autofit() %>% 
  fix_border_issues() %>% 
  colformat_double(j = "Value", digits = 2, suffix = " pp") %>%  # Updated to show "pp" instead of "%"
  flextable::add_footer_lines(paste0("Notes: This table summarizes the differences between the EU traffic shares in public and proprietary SimilarWeb data sets. The proprietary SimilarWeb data set spans multiple months pre- and post-GDPR (January 2018 - December 2019; T = 24 months). For this analysis, we aggregate the post-GDPR months in the proprietary SimilarWeb data set (T = 19 months) into a single post-GDPR period. The public SimilarWeb data set was collected in September 2021 (i.e., in the post-GDPR period; T = 1 month). We calculate the differences by comparing the aggregated post-GDPR EU traffic shares from proprietary SimilarWeb data set with the EU traffic shares in the public SimilarWeb data set for publishers that appear in both datasets (N publishers = 200). The average (mean) difference represents the average discrepancy in EU traffic shares between the two data sets across 200 publishers. The standard deviation indicates the variability of these differences. The total number of publishers (i.e., observations) included in the analysis is 200. All differences are expressed in percentage points (pp).")) %>%  # Updated footer text
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::fontsize(size = 10, part = "all") %>% 
  flextable::keep_with_next()
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_24.docx")


# Figure 13: Development of the Average Monthly Number of Trackers in the Observed (Treatment) and Predicted Counterfactual (Control) Groups -------------

data <- synth_global_sites_trackers_DT %>% 
  select(period_id, ymd, post_GDPR, site_id, site, 
         is_treated_target_audience, 
         n_trackers) %>% 
  mutate(
    id = site_id,
    time = period_id,
    Y = n_trackers,
    D = post_GDPR) %>% 
  mutate(D = case_when(
    is_treated_target_audience == 0 & post_GDPR == 1 ~ 0,
    is_treated_target_audience == 1 & post_GDPR == 0 ~ 0,
    is_treated_target_audience == 1 & post_GDPR == 1 ~ 1,
    .default = as.integer(is_treated_target_audience)
  ))
data

# NOTE: One needs to execute the following line of code
# using "Run Selected Line" option of RStudio
# (i.e., not as "Source [with Echo]" or "Run All").
# Otherwise, the execution might come to a halt.
# Proper execution shows the progress of the EM algorithm
# (e.g., Parallel computing ..., Cross-validating ..., Bootstrapping ...).
em_out <- gsynth(Y ~ D, data = data, 
                 index = c("id","time"), 
                 EM = TRUE, 
                 force = "two-way", 
                 CV = TRUE, r = c(0, 5), se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = TRUE)

ymd_seq <- seq(as.Date("2017-05-01"), as.Date("2019-12-01"), by = "month")

plot_data <- data.frame(
  ymd = ymd_seq,
  Observed = em_out$Y.bar[, "Y.tr.bar"],
  Counterfactual = em_out$Y.bar[, "Y.ct.bar"]
)

plot_data_long <- plot_data %>%
  pivot_longer(cols = c("Observed", "Counterfactual"), names_to = "Group", values_to = "mean_n_trackers") %>%
  mutate(Group = factor(
    Group, 
    levels = c("Observed", "Counterfactual"), labels = c("Observed", "Counterfactual")
  ))

caption_text <- "Notes: This figure shows the observed (treatment group) and predicted counterfactual (control group) number of trackers over time. We use the generalized synthetic control (GSC) method and the expectation-maximization (EM) algorithm to construct the predicted counterfactual."

ggplot(plot_data_long, aes(x = ymd, y = mean_n_trackers, linetype = Group)) +
  geom_point(size = 4, aes(shape = Group)) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +  # If you have this line in your environment
  labs(y = "Average Number of Trackers", 
       x = "", 
       linetype = "Group:", 
       shape = "Group:", 
       caption = str_wrap(caption_text, width = 109)) +
  coord_cartesian(ylim = c(1, 35)) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("Observed" = "solid", "Counterfactual" = "dashed"),
                        labels = c("Observed (Treatment)", "Predicted Counterfactual (Control)")) +
  scale_shape_manual(values = c(16, 3),
                     labels = c("Observed (Treatment)", "Predicted Counterfactual (Control)")) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        plot.caption = element_text(hjust = 0, size = 12),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank())

ggsave("03_results/web_appendix/figure_13.pdf", width=8, height=6, font="Times", dpi=300)

# Table 25: Result of Generalized Synthetic Control Method Analysis for the Number of Trackers -------

combined_results <- data.frame(
  Model = c("Average Treatment Effect on the Treated (ATT) Across All Months", 
            "ATT by Month:", 
            rownames(em_out$est.att)), 
  Estimate = c(sprintf("%.3f", em_out$est.avg[1]),
               rep("", 1),  # leave empty for the header row
               sprintf("%.3f", em_out$est.att[, "ATT"])),
  CI = c(paste0("[", sprintf("%.3f", em_out$est.avg[3]), "; ", 
                sprintf("%.3f", em_out$est.avg[4]), "]"),
         rep("", 1), 
         paste0("[", sprintf("%.3f", em_out$est.att[, "CI.lower"]), "; ", 
                sprintf("%.3f", em_out$est.att[, "CI.upper"]), "]")),
  "p.value" = c(sprintf("%.3f", em_out$est.avg[5]),
                rep("", 1), 
                sprintf("%.3f", em_out$est.att[, "p.value"])),
  "n.Treated" = c("", "", em_out$est.att[, "n.Treated"]),
  "Significance" = c(add_stars(em_out$est.avg[5]),  # Stars for ATT avg
                     rep("", 1),  # leave empty for the header row
                     sapply(em_out$est.att[, "p.value"], add_stars)),
  stringsAsFactors = FALSE
)

combined_results$Estimate <- paste0(combined_results$Estimate, combined_results$Significance)

combined_results <- combined_results %>% 
  select(Model, Estimate, CI)
combined_results

lookup_table <- data.frame(
  period = c(-11:20),
  period_name = c("2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10", 
                  "2017-11", "2017-12", "2018-01", "2018-02", "2018-03", "2018-04", 
                  "2018-05", "2018-06", "2018-07", "2018-08", "2018-09", "2018-10", 
                  "2018-11", "2018-12", "2019-01", "2019-02", "2019-03", "2019-04", 
                  "2019-05", "2019-06", "2019-07", "2019-08", "2019-09", "2019-10", 
                  "2019-11", "2019-12")
)

model_data <- combined_results[-c(1, 2), ]
model_data$Model <- as.numeric(model_data$Model)
model_data <- left_join(model_data, lookup_table, by = c("Model" = "period"))
model_data$Model <- model_data$period_name
model_data <- model_data[, -which(names(model_data) == "period_name")]
combined_results <- rbind(combined_results[1:2, ], model_data)

n_obs <- nrow(data) %>% format(big.mark = ",")
n_treated <- data[is_treated_target_audience == 1, uniqueN(site)]
n_control <- data[is_treated_target_audience == 0, uniqueN(site)]
n_pubs <- n_treated + n_control  # Total number of publishers (including both treatment and control)
n_periods <- uniqueN(data$period_id)  # Total number of months

mspe <- em_out$MSPE
selected_factors <- em_out$r.cv
ic <- em_out$IC
pc <- em_out$PC
sigma2 <- em_out$sigma2
niter <- em_out$niter

notes <- paste0(
  "Notes: This table presents results from the generalized synthetic control (GSC) method using the expectation-maximization (EM) algorithm. ",
  "The EM algorithm leverages information from the treatment group during the pre-treatment period to improve the precision of estimates. ",
  "The model includes two-way fixed effects (i.e., publisher and month fixed effects). We use cross-validation to select the optimal number of latent factors and choose ",
  selected_factors, " latent factors. We perform parametric inference and use 1,000 bootstrap replications to estimate the confidence intervals. ",
  "The model achieved a mean squared prediction error (MSPE) of ", round(mspe, 2), ", with an information criterion (IC) of ", round(ic, 2), 
  " and a predictive criterion (PC) of ", round(pc, 2), ". The residual variance was estimated at ", round(sigma2, 1), 
  " and the EM algorithm converged after ", niter, " iterations. ",
  "We assign treatment to each publisher according to the publisher’s designation (EU or non-EU). ",
  "Multiplying the number of publishers (N publishers = ", n_pubs, ") and the number of months (T = ", n_periods, ") yields the number of observations (N observations = ", n_obs, ")."
)

t <- flextable(combined_results) %>%
  set_header_labels(
    Model = "Model:",
    Estimate = "Estimate",
    CI = "95% CI"
  ) %>%
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher and Month"), 
                 colwidths = c(1, 2)) %>%
  add_footer_lines(values = c(
    paste0("Number of Observations: ", n_obs, "."),
    paste0("Number of Publishers in Treatment Group: ", n_treated, "; Number of Publishers in Control Group: ", n_control, "."),
    "95% confidence intervals are reported in brackets.",
    "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.",
    notes
  )) %>%
  flextable::align(part = "footer", align = "justify") %>%
  border_remove() %>%
  hline_top(border = thick_border, part = "header") %>%
  hline_bottom(border = thick_border) %>%
  hline(i = 2, border = std_border, part = "header") %>% 
  hline(i = c(1, 2), border = std_border, part = "body") %>% 
  flextable::italic(i = c(2))
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_25.docx")


# Figure 14: Development of the Monthly Average Treatment Effect (ATT) Coefficients for the Number of Trackers  -------

att_data <- data.frame(
  time = em_out$time,  # Time periods (e.g., months)
  ATT = em_out$att,    # ATT by month
  S.E. = em_out$est.att[, "S.E."],  # Standard errors for ATT
  CI.lower = em_out$est.att[, "CI.lower"],  # Lower bound of confidence interval
  CI.upper = em_out$est.att[, "CI.upper"]   # Upper bound of confidence interval
)

att_data$time <- seq(as.Date("2017-05-01"), as.Date("2019-12-01"), by = "month")

caption_text_scm <- "Notes: This figure shows the monthly average treatment effect (ATT) coefficients from the generalized synthetic control (GSC) method with the expectation-maximization (EM) algorithm. We assign treatment to each publisher according to the publisher's designation (EU or non-EU). The model leverages pre-treatment information from the treatment group to improve estimate precision and includes publisher and month fixed effects. Confidence intervals are based on parametric inference with 1,000 bootstrap replications."

ggplot(att_data, aes(x = time, y = ATT)) +
  geom_line(size = 0.3, color = "black") +  # Line for ATT
  geom_point(size = 2, color = "black") +  # Points for ATT
  geom_errorbar(aes(ymax = CI.upper, ymin = CI.lower), size = 0.5) +  # Error bars for CIs
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at ATT = 0
  labs(y = "ATT Coefficient and 95% Confidence Interval", 
       x = "", 
       caption = str_wrap(caption_text_scm, width = 110)) +  # Add caption with wrapped text
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  # X-axis date formatting
  scale_y_continuous(breaks = seq(-10, 2, by = 1)) +  # Y-axis limits and breaks
  geom_vline_gdpr +  # Add vertical line for GDPR
  theme_classic(base_family = "Times") +  # Use classic theme with Times font
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        plot.caption = element_text(hjust = 0, size = 12),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank())  # Adjust theme for caption and grid

ggsave("03_results/web_appendix/figure_14.pdf", width=8, height=6, font="Times", dpi=300)


# Figure 15: Development of the Average Monthly Number of Trackers in the Treatment and Control Groups of the Unbalanced Panel ------------------------

unbalanced_global_sites_trackers_DT[, .(n_sites = uniqueN(site)), by = month]

unbalanced_global_sites_trackers_DT[, is_treated_TLD := fifelse(TLD_region == "EU", 1L, 0L)]

unbalanced_global_sites_trackers_DT[, is_treated_server := fifelse(server_region == "EU", 1L, 0L)]

data_summary_TLD <- unbalanced_global_sites_trackers_DT %>%
  group_by(ymd, is_treated_TLD) %>%
  summarize(mean_n_trackers = mean(n_trackers, na.rm = TRUE), .groups = "keep")
data_summary_TLD$is_treated_TLD <- factor(data_summary_TLD$is_treated_TLD, levels = c(1, 0))
fig_TLD <- ggplot(data_summary_TLD, aes(x = ymd, y = mean_n_trackers, linetype = factor(is_treated_TLD))) +
  geom_point(size = 4, aes(shape = factor(is_treated_TLD))) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +
  labs(
    y = "",
    title = "Treatment Assignment Based on Publisher's Top-Level Domain",
    x = "", linetype = "Group:", shape = "Group:") +
  coord_cartesian(ylim = c(1, 35)) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("1" = "solid", "0" = "dashed"),
                        labels = c("Treatment", "Control")) +
  scale_shape_manual(values = c(16, 3),
                     labels = c("Treatment", "Control")) +
  theme_classic(base_family = "Times") +
  theme(
    # axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
    axis.text.x = element_blank(),
    panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 10)
  )

data_summary_server <- unbalanced_global_sites_trackers_DT %>%
  group_by(ymd, is_treated_server) %>%
  summarize(mean_n_trackers = mean(n_trackers, na.rm = TRUE), .groups = "keep")
data_summary_server$is_treated_server <- factor(data_summary_server$is_treated_server, levels = c(1, 0))
fig_server <- ggplot(data_summary_server, aes(x = ymd, y = mean_n_trackers, linetype = factor(is_treated_server))) +
  geom_point(size = 4, aes(shape = factor(is_treated_server))) +
  geom_line(linewidth = 0.5) +
  geom_vline_gdpr +
  labs(
    y = "",
    title = "Treatment Assignment Based on Publisher's Server Location",
    x = "", linetype = "Group:", shape = "Group:") +
  coord_cartesian(ylim = c(1, 35)) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_linetype_manual(values = c("1" = "solid", "0" = "dashed"), labels = c("Treatment", "Control")) +
  scale_shape_manual(values = c(16, 3), labels = c("Treatment", "Control")) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7),
        panel.grid.major = element_line(colour = "lightgray", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 10))

combined_fig <- ggarrange(fig_TLD, fig_server, nrow = 2)

combined_fig <- annotate_figure(combined_fig,
                                left = text_grob("Average Number of Trackers", 
                                                 family = "Times", rot = 90, size = 11, hjust = -0.1)
)
combined_fig

ggsave("03_results/web_appendix/figure_15.pdf", combined_fig, width=9, height=9, font="Times", dpi=300)


# Table 26: Result of Difference-in-Differences Analysis for the Number of Trackers in the Unbalanced Panel --------------

m4_unbalanced_TLD <- feols(
  n_trackers ~ is_treated_TLD * post_GDPR | site_id + period_id,
  cluster = ~ site_id + period_id,
  data = unbalanced_global_sites_trackers_DT
)

m4_unbalanced_server <- feols(
  n_trackers ~ is_treated_server * post_GDPR | site_id + period_id,
  cluster = ~ site_id + period_id,
  data = unbalanced_global_sites_trackers_DT
)

t <- etable(m4_unbalanced_TLD,
            m4_unbalanced_server,
            signif.code = c("*" = 0.05,"**" = 0.01, "***" = 0.001),
            coefstat = "confint",
            depvar = F,
            digits = "r3",
            digits.stats = 3,
            style.df = style.df
            (depvar.title = "Dependent Variable:",
              fixef.line = "_",
              fixef.suffix = " Fixed Effects",
              yesNo = c("✓", "✘"),
              stats.title = "Fit Statistics:",
            )
) %>% 
  as_tibble(.name_repair = "unique") %>% 
  filter(!row_number() %in% c(2, 5, 6, 9)) %>%  # remove rows "Fixed-Effects:", "Fit Statistics:", "VCOV: Clustered" and "Within R2"
  flextable() %>% 
  border_remove() %>% 
  set_header_labels("...1" = "Model:", 
                    "m4_unbalanced_TLD" = "(1)",
                    "m4_unbalanced_server" = "(2)") %>% 
  add_header_row(values = c("Dependent Variable:", 
                            "Number of Trackers per Publisher and Month",
                            "Number of Trackers per Publisher and Month")) %>% 
  merge_h(part = "header") %>%
  flextable::align(part = "all", align = "center") %>%
  hline_top(border = thick_border, part = "header") %>% 
  hline_bottom(border = thick_border) %>% 
  hline(i = 2, border = std_border, part = "header") %>% 
  compose(i = 4, j = 1, value = as_paragraph("N Observations")) %>% 
  compose(i = 5, j = 1, 
          value = as_paragraph("R", 
                               as_chunk("2", props = fp_text(vertical.align = "superscript")))) %>% 
  add_footer_lines(values = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  add_footer_lines(values = "Two-way standard errors are clustered at the publisher and month levels; 95% confidence intervals are reported in brackets.") %>% 
  add_footer_lines(values = "Notes: This table shows the difference-in-differences coefficient (Treatment x PostGDPR) from the OLS regressions. In model (1), we assign treatment to each publisher according to the publisher’s top-level domain (EU or non-EU). In model (2), we assign treatment according to the publisher’s server location (EU or non-EU). The panel includes 29,735 unique publishers (N publishers = 29,735) observed from May 2017 until December 2019 (T = 32 months), which would yield 951,520 observations in a balanced panel. However, because we use an unbalanced panel, the actual number of observations is lower (N observations = 256,595), reflecting the specific publisher-month combinations with available data.") %>% 
  flextable::align(part = "footer", align = "justify") %>% 
  flextable::keep_with_next() %>% 
  flextable::add_footer_lines("") # add empty line
t

t %>% flextable::save_as_docx(path = "03_results/web_appendix/table_26.docx")
