# Title: 
# Functions 
# Author: 
# Karlo Lukic
# Date: 
# 12.11.2020
# Description: 
# This script contains collections of 
# functions for the project: "The Impact of GDPR on the amount of online tracking"

# all packages used for the project ----
library(data.table) # data manipulation
library(tidyverse) # data manipulation
library(forcats) # nice ordering in ggplot
library(ggtext) # italicized text in ggplot
library(lubridate) # data manipulation
library(scales) # data visualization
library(readxl) # data ingestion
library(flextable) # reporting tables
library(ggpubr) # data visualization
library(fixest) # OLS regression estimations
library(gsynth) # SCM method
library(pdftools) # display prepared PDFs
library(magick) # display prepared PDFs
library(GGally) # data visualization
library(officedown) # to produce Word file
library(sjmisc) # big_mark() function
library(usethis) # ui messages
library(huxtable) # reporting tables
library(officer) # reporting tables
library(patchwork) # multiple plots
library(glue) # alternative to paste0
library(cowplot) # data visualization
library(infer) # statistical tests
library(broom) # statistical tests
library(rstatix) # statistical tests
library(gridExtra) # multiple plots


# set defaults
theme_set(theme_gray(base_family = "Arial"))

## ----flextable-settings-------------------------------------------------------------
set_flextable_defaults(
  font.family = "Times", font.size = 10, font.color = "black", # default font size = 10
  text.align = "left", table.layout = "autofit",
  theme_fun = "theme_booktabs", 
  digits = 3,
  decimal.mark = ".", big.mark = ",", na_str = "",
  line_spacing = 1,
  padding = 0
)


# ggplot shortcuts -----
geom_vline_gdpr <- geom_vline(xintercept = as.numeric(ymd("2018-05-01")),
                              linetype = "dashed", color = "black", linewidth = .3)

highlight_interest_periods <- annotate(geom = "rect", 
                                       xmin = min(as.Date("2018-12-01")), # interest periods
                                       xmax = max(as.Date("2019-04-01")), 
                                       ymin = 0, ymax = Inf, fill = "gray", alpha = 0.5)

theme_my_ggplot2_theme <- theme_bw(base_family = "Times New Roman", 
                                   base_size = 8) # default = 8 (increase to 16 for PPTs)

scale_x_date_monthly <- scale_x_date(date_breaks = "1 months", date_labels = "%Y-%m")
theme_angle_x_axis_45_degrees <- theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
scale_color_groups <- scale_color_manual(values = c("0" = "#56B4E9", # color-blind-friendly blue
                                                    "1" = "#E69F00"), # color-blind-friendly orange
                                         name = "Group:",
                                         breaks = c("1", "0"),
                                         labels = c("Treatment", "Control"))


# flextable shortcuts ----
std_border <- fp_border(color = "black", width = 1)
dashed_border <- fp_border(color = "black", width = 1, style = "dashed")
thick_border <- fp_border(color = "black", width = 2)
no_border <- fp_border(color = "white", width = 1)

# utility functions -----
silent_ggsave <- function(filename) {
  suppressMessages(ggsave(filename))
}

multmerge <- function(path) {
  filenames <- list.files(path = path, full.names = TRUE)
  usethis::ui_todo('merging WhoTracks.me .csv files, please wait...')
  DT <- rbindlist(lapply(filenames, fread), fill = T)
  usethis::ui_done('WhoTracks.me .csv files merged!')
  setorder(DT, "month")
  return(DT)
}

filter_sites <- function(DT) {
  # find unique N of sites by month: eu_sites
  eu_sites <- DT[, .(unique_sites = uniqueN(site)), by = .(ym)]
  # find min. N of above sites: min_unique_sites
  min_unique_sites <- min(eu_sites$unique_sites)
  # find corresponding ym where the min is: min_unique_sites_ym
  min_unique_sites_ym <- eu_sites[unique_sites %in% min_unique_sites, ym]
  # take only 1 ym: min_unique_sites_ym
  min_unique_sites_ym <- head(unique(min_unique_sites_ym), 1)
  # pass ym to DT, grab these sites: tracked sites
  tracked_sites <- DT[ym == min_unique_sites_ym, site]
  # track only these XYZ sites over time: DT_filtered
  DT_filtered <- DT[site %in% tracked_sites]
  # return
  return(DT_filtered)
}

filter_sites_n_times <- function(DT, n) {
  if (n == 0) {
    return(DT)
  }
  Recall(filter_sites(DT), n - 1)
}

get_ipv4 <- function(domain) {
  res <- nslookup(domain, ipv4_only = T, multiple = F, error = F)
  ifelse(length(res) == 0, # needed to handle exceptions, like 'youtube-mp3.org' that have NULL length
         return("NA"),
         return(res)
  )
}

get_single_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_mean_n_trackers_for_group_in_period <- function(panel_data, 
                                                    post_GDPR_period_indicator, is_treated_target_audience_indicator) {
  eu_us_sites_trackers_panel_DT %>% 
    filter(post_GDPR == post_GDPR_period_indicator) %>% 
    filter(is_treated_target_audience == is_treated_target_audience_indicator) %>% 
    summarize(mean_n_trackers = mean(n_trackers)) %>% 
    pull()
}


get_did_table <- function(panel_data_DT, dep_var, period_var, treat_var, rounding_decimal = 3, rounding_decimal_pct = 2, event_name = "GDPR's Enactment") {
  # 1. summarize data
  summarized_did_DT <- panel_data_DT[, .(mean_dep_var = round(mean(get(dep_var), na.rm=T), 4))
                                     , by=.(period_var = get(period_var), 
                                            treat_var = get(treat_var))]
  # 2. reshape data
  reshaped_DT <- dcast(summarized_did_DT,  treat_var ~ period_var, value.var = 'mean_dep_var') %>% 
    .[order(- treat_var), .(Group = treat_var, pre = `0`, post = `1`)] %>% 
    mutate(Group = recode(Group, `0` = 'Control', `1` = 'Treatment')) %>% 
    mutate(Difference = `post` - `pre`, 
           Percent_Change = ((`post` - `pre`) / `pre`) * 100) 
  # 3. Calculate did rows
  calculated_did_DT <- reshaped_DT %>%  
    add_row(Group = 'Difference',
            pre = summarized_did_DT[period_var==0 & treat_var==1, mean_dep_var] -
              summarized_did_DT[period_var==0 & treat_var==0, mean_dep_var],
            post = summarized_did_DT[period_var==1 & treat_var==1, mean_dep_var] -
              summarized_did_DT[period_var==1 & treat_var==0, mean_dep_var],
            Difference = `post` - `pre`,
            Percent_Change = NaN)
  # 4. prepare table for display 
  displayed_did_DT <<- calculated_did_DT %>% 
    mutate(Difference = round(Difference, rounding_decimal)) %>%  
    mutate(pre = round(pre, rounding_decimal)) %>% 
    mutate(post = round(post, rounding_decimal)) %>% 
    mutate(Percent_Change = round(Percent_Change, rounding_decimal_pct)) %>% 
    mutate(Difference = ifelse(!is.na(Percent_Change), paste0(Difference, " (", Percent_Change, "%)"), Difference)) %>% 
    select(Group, pre, post, Difference)
  # 5. to flextable
  flex_t <- flextable(displayed_did_DT) %>% 
    # flextable::colformat_double(digits = rounding_decimal) %>% # important: round numbers
    set_header_labels(pre = paste0("Before ", event_name), 
                      post = paste0("After ", event_name)) %>% 
    hline(i = 2, border = std_border) %>% 
    vline(j = 3, border = std_border) %>% 
    # flextable::bold(part = "header") %>%  # bold header
    # flextable::bold(i = 3, j = 1) %>%  # bold difference cell
    flextable::align(align = "right", part = "all") # %>%  # align text in all
  # flextable::add_footer_lines("") # add empty line
  return(flex_t)
}

get_did_table_share <- function(panel_data_DT, dep_var, period_var, treat_var, rounding_decimal = 3, event_name = "GDPR Enactment") {
  # 1. summarize data
  summarized_did_DT <- panel_data_DT[, .(mean_dep_var = mean(get(dep_var), na.rm = T)),
                                     by = .(
                                       period_var = get(period_var),
                                       treat_var = get(treat_var)
                                     )
  ]
  # 2. reshape data
  reshaped_DT <- dcast(summarized_did_DT, treat_var ~ period_var, value.var = "mean_dep_var") %>%
    .[order(-treat_var), .(Group = treat_var, pre = `0`, post = `1`)] %>%
    mutate(Group = recode(Group, `0` = "Control", `1` = "Treatment")) %>%
    mutate(Difference = `post` - `pre`)
  # 3. calculate did rows
  calculated_did_DT <- reshaped_DT %>%
    add_row(
      Group = "Difference",
      pre = summarized_did_DT[period_var == 0 & treat_var == 1, mean_dep_var] -
        summarized_did_DT[period_var == 0 & treat_var == 0, mean_dep_var],
      post = summarized_did_DT[period_var == 1 & treat_var == 1, mean_dep_var] -
        summarized_did_DT[period_var == 1 & treat_var == 0, mean_dep_var],
      Difference = `post` - `pre`
    )
  # 4. prepare table for display
  displayed_did_DT <<- calculated_did_DT %>% 
    mutate(Difference = Difference * 100,
           Difference = round(Difference, rounding_decimal),
           Difference = paste0(Difference, " pp")) %>% 
    mutate(pre, pre = case_when(
      Group == "Difference" ~ paste0(sprintf("%0.3f", pre * 100), " pp"),
      Group != "Difference" ~ paste0(round(pre * 100, rounding_decimal), "%")
    )) %>% 
    mutate(post, post = case_when(
      Group == "Difference" ~ paste0(sprintf("%0.3f", post * 100), " pp"),
      Group != "Difference" ~ paste0(round(post * 100, rounding_decimal), "%")
    ))
  
  # 5.
  flex_t <- flextable(displayed_did_DT) %>%
    set_header_labels(pre = paste0("Before ", event_name), 
                      post = paste0("After ", event_name)) %>%
    hline(i = 2, border = std_border) %>%
    vline(j = 3, border = std_border) %>%
    flextable::bold(part = "header") %>% # bold header
    flextable::bold(i = 3, j = 1) %>% # bold difference cell
    flextable::align(align = "right", part = "all") # %>% # align text in all
    # flextable::add_footer_lines("") # add empty line
  return(flex_t)
}

get_did_table_content <- function(panel_data_DT, dep_var, period_var, treat_var, rounding_decimal = 3, event_name = "GDPR Enactment") {
  # 1. summarize data
  summarized_did_DT <- panel_data_DT[, .(mean_dep_var = (mean(get(dep_var), na.rm = T)), rounding_decimal),
                                     by = .(
                                       period_var = get(period_var),
                                       treat_var = get(treat_var)
                                     )
  ]
  # 2. reshape data
  reshaped_DT <- dcast(summarized_did_DT, treat_var ~ period_var, value.var = "mean_dep_var") %>%
    .[order(-treat_var), .(Group = treat_var, pre = `0`, post = `1`)] %>%
    mutate(Group = recode(Group, `0` = "Control", `1` = "Treatment")) %>%
    mutate(Difference = `post` - `pre`)
  # 3. calculate did rows
  calculated_did_DT <- reshaped_DT %>%
    add_row(
      Group = "Difference",
      pre = summarized_did_DT[period_var == 0 & treat_var == 1, mean_dep_var] -
        summarized_did_DT[period_var == 0 & treat_var == 0, mean_dep_var],
      post = summarized_did_DT[period_var == 1 & treat_var == 1, mean_dep_var] -
        summarized_did_DT[period_var == 1 & treat_var == 0, mean_dep_var],
      Difference = `post` - `pre`
    )
  # 4. prepare table for display
  displayed_did_DT <<- calculated_did_DT %>% 
    mutate(Difference = Difference,
           Difference = round(Difference, rounding_decimal),
           Difference = paste0(Difference, " MB")) %>% 
    mutate(pre, pre = case_when(
      Group == "Difference" ~ paste0(round(pre, rounding_decimal), " MB"),
      Group != "Difference" ~ paste0(round(pre, rounding_decimal), " MB")
    )) %>% 
    mutate(post, post = case_when(
      Group == "Difference" ~ paste0(round(post, rounding_decimal), " MB"),
      Group != "Difference" ~ paste0(round(post, rounding_decimal), " MB")
    ))
  
  # 5.
  flex_t <- flextable(displayed_did_DT) %>%
    set_header_labels(pre = paste0("Before ", event_name), 
                      post = paste0("After ", event_name)) %>%
    hline(i = 2, border = std_border) %>%
    vline(j = 3, border = std_border) %>%
    flextable::bold(part = "header") %>% # bold header
    flextable::bold(i = 3, j = 1) %>% # bold difference cell
    flextable::align(align = "right", part = "all") # %>% # align text in all
    # flextable::add_footer_lines("") # add empty line
  return(flex_t)
}

tabulate_ten_most_common_trackers_on_site <- function(eu_us_sites_trackers_data = eu_us_sites_trackers_DT,
                                                      site_name) {
  eu_us_sites_trackers_data %>% 
    filter(site %in% site_name) %>% 
    select(tracker_name, tracker_category_name_bucket, site_proportion) %>%
    group_by(tracker_name, tracker_category_name_bucket) %>%
    summarize(mean_site_proportion = mean(site_proportion)) %>%
    mutate(mean_site_proportion = scales::percent(mean_site_proportion, accuracy = 2)) %>%
    arrange(desc(mean_site_proportion)) %>%
    head(10) %>%
    flextable() %>%
    set_header_labels(
      tracker_name = "tracker name",
      tracker_category_name_bucket = "tracker category",
      mean_site_proportion = "mean proportion of webpages containing tracker"
    ) %>%
    flextable::set_caption(caption = glue("10 most common trackers on {site_name} (April 2018 - December 2019)")) %>%
    autofit(part = "all") %>% 
    flextable::align(align = "center", part = "all") %>% 
    flextable::font(fontname = "Times", part = "all")
}

tabulate_ten_trackers_that_track_the_most_on_site <- function(eu_us_sites_trackers_data = eu_us_sites_trackers_DT,
                                                              site_name) {
  eu_us_sites_trackers_data %>%
    filter(site %in% site_name) %>% 
    select(tracker_name, tracker_category_name_bucket, tracked) %>%
    group_by(tracker_name, tracker_category_name_bucket) %>%
    summarize(mean_tracked = mean(tracked)) %>%
    mutate(mean_tracked = scales::percent(mean_tracked, accuracy = 2)) %>%
    arrange(desc(mean_tracked)) %>%
    head(10) %>%
    flextable() %>%
    set_header_labels(
      tracker_name = "tracker name",
      tracker_category_name_bucket = "tracker category",
      mean_tracked = "mean proportion of webpages on which tracker tracked users"
    ) %>%
    flextable::set_caption(caption = glue("10 trackers that track the most on {site_name}")) %>%
    add_header_lines(values = "Across observation period: April 2018 - December 2019") %>% 
    footnote(
      j = "mean_tracked", ref_symbols = "1", part = "header",
      value = as_paragraph("Via cookies or fingerprinting tracking methods.")
    ) %>% 
    autofit(part = "all") %>%
    flextable::align(align = "center", part = "all") %>% 
    flextable::font(fontname = "Times", part = "all")
}


plot_dev_n_trackers_per_user_group_on_site <- function(eu_us_sites_trackers_panel_data = eu_us_sites_trackers_panel_DT, 
                                                       site_name, 
                                                       ylim = c(0, 80)) {
  eu_us_sites_trackers_panel_data %>%
    filter(site %in% site_name) %>%
    select(ymd, site, country, n_trackers) %>%
    ggplot(., aes(ymd, n_trackers, color = country)) +
    geom_point() +
    geom_line() +
    geom_vline_gdpr + 
    highlight_interest_periods +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = "number of trackers",
         title = glue("Development of number of trackers on {site_name}"),
         subtitle = "Between 2 user groups",
         color = "user group")
}

plot_dev_mean_n_trackers_per_webpage_by_user_group_on_site <- function(eu_us_sites_panel_data = eu_us_sites_panel_DT,
                                                                       site_name, 
                                                                       ylim = c(0, 20)) {
  eu_us_sites_panel_data %>% 
    filter(site %in% site_name) %>% 
    select(ymd, country, trackers) %>% 
    ggplot(aes(ymd, trackers, color = country)) + 
    geom_point() +
    geom_line() + 
    geom_vline_gdpr + 
    coord_cartesian(ylim = c(0, 30)) +
    labs(x = "", y = "mean number of trackers (per webpage)",
         title = glue("Development in mean number of trackers (per webpage) on {site_name}"),
         subtitle = "Between 2 user groups") +
    highlight_interest_periods
}

plot_dev_mean_n_trackers_per_webpage_by_user_group_on_all_sites <- function(eu_us_sites_panel_data = eu_us_sites_panel_DT,
                                                                            ylim = c(0, 20)) {
  n <- uniqueN(eu_us_sites_panel_data$site) %>% 
    big_mark()
  eu_us_sites_panel_data %>% 
    select(ymd, country, trackers) %>% 
    group_by(ymd, country) %>% 
    summarize(mean_n_trackers = mean(trackers)) %>% 
    ggplot(aes(ymd, mean_n_trackers, color = country)) + 
    geom_point() +
    geom_line() + 
    geom_vline_gdpr + 
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = "mean number of trackers (per webpage)",
         title = glue("Development in mean number of trackers (per webpage) across all websites (n = {n})"),
         subtitle = "Between 2 user groups") +
    highlight_interest_periods
}


plot_dev_n_trackers_per_user_groups_and_tracker_category_on_site <- function(eu_us_sites_trackers_panel_data = eu_us_sites_trackers_panel_DT, 
                                                                             site_name, 
                                                                             ylim = c(0, 80)) {
  eu_us_sites_trackers_panel_data %>% 
    filter(site %in% site_name) %>%
    select(ymd, country, 
           # + 6 tracker categories
           n_trackers_advertising, n_trackers_analytics, n_trackers_social_media, 
           n_trackers_comments, n_trackers_essential, n_trackers_other) %>% 
    pivot_longer(cols = starts_with("n_trackers_"), names_to = "n_trackers_per_category") %>% 
    mutate(n_trackers_per_category = str_remove_all(n_trackers_per_category, "n_trackers_"),
           n_trackers_per_category = str_replace_all(n_trackers_per_category, "_", " ")) %>% 
    ggplot(aes(ymd, value, color = n_trackers_per_category)) + 
    geom_point() +
    geom_line() + 
    facet_wrap(vars(country)) +
    geom_vline_gdpr + 
    highlight_interest_periods +
    scale_y_continuous(n.breaks = 10) +
    scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m") +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = "number of trackers", color = "tracker category",
         title = glue("Development of number of trackers on {site_name}"),
         subtitle = "Between 2 user groups per tracker category")
}

plot_dev_relative_popularity_of_site_by_user_group <- function(eu_us_sites_panel_data = eu_us_sites_panel_DT, 
                                                               site_name,
                                                               ylim) {
  eu_us_sites_panel_DT %>% 
    filter(site %in% site_name) %>% 
    select(ymd, country, popularity) %>% 
    ggplot(aes(ymd, popularity, color = country)) + 
    geom_point() + 
    geom_line() + 
    scale_y_continuous(labels = scales::label_percent()) +
    coord_cartesian(ylim = ylim) +
    labs(y = "relative popularity", x = "", 
         title = glue("Development of {site_name}'s relative popularity"),
         subtitle = "Between 2 user groups",
         caption = ("Notes: Relative popularity = share of traffic (%) compared to most popular site (google.com).")) + 
    highlight_interest_periods + 
    geom_vline_gdpr
}


plot_trackers_presence_on_site_by_user_group_and_tracker_category <- function(eu_us_sites_trackers_data = eu_us_sites_trackers_DT,
                                                                              site_name,
                                                                              country_name,
                                                                              tracker_category_bucket_name) {
  n_trackers <- eu_us_sites_trackers_data %>% 
    filter(site == site_name) %>% 
    filter(country == country_name) %>%
    filter(tracker_category_bucket == tracker_category_bucket_name) %>% 
    select(tracker) %>% 
    uniqueN() %>% 
    big_mark()
  eu_us_sites_trackers_data %>% 
    filter(site == site_name) %>% 
    filter(country == country_name) %>%
    filter(tracker_category_bucket == tracker_category_bucket_name) %>% 
    select(ymd, site, country, tracker) %>% 
    mutate(tracker_present = 1L) %>% 
    complete(tracker, nesting(ymd, site, country), 
             fill = list(tracker_present = 0)) %>% 
    select(ymd, everything()) %>% 
    arrange(ymd, tracker, tracker_present) %>% 
    mutate(tracker_present_fct = factor(tracker_present)) %>% 
    ggplot(aes(ymd, reorder(tracker, -tracker_present), fill = tracker_present_fct)) +
    geom_tile() +
    scale_fill_brewer(label = as_labeller(c(`0` = "No", `1` = "Yes"))) +
    scale_x_date_monthly +
    theme_angle_x_axis_45_degrees +
    labs(x = "", y = glue("{tracker_category_bucket_name} tracker"), 
         title = glue("Development of {tracker_category_bucket_name} trackers' presence on {site_name}"),
         subtitle = glue("For US group of users visiting {site_name}"),
         fill = "present",
         caption = glue("Notes: Trackers ordered from lowest to highest presence on site.
                  {n_trackers} {tracker_category_bucket_name} trackers were recorded at {site_name} over observation period (T = 21 months).")) + 
    geom_vline_gdpr + 
    highlight_interest_periods + 
    facet_wrap(vars(country))
}


plot_trackers_presence_on_site_for_two_user_groups_and_tracker_category <- function(eu_us_sites_trackers_data = eu_us_sites_trackers_DT,
                                                                                    site_name,
                                                                                    tracker_category_bucket_name) {
  n_trackers <- eu_us_sites_trackers_data %>% 
    filter(site == site_name) %>% 
    filter(tracker_category_bucket == tracker_category_bucket_name) %>% 
    select(tracker) %>% 
    uniqueN() %>% 
    big_mark()
  eu_us_sites_trackers_data %>% 
    filter(site == site_name) %>% 
    filter(tracker_category_bucket == tracker_category_bucket_name) %>% 
    select(ymd, site, country, tracker) %>% 
    mutate(tracker_present = 1L) %>% 
    complete(tracker, nesting(ymd, site, country), 
             fill = list(tracker_present = 0)) %>% 
    select(ymd, everything()) %>% 
    arrange(ymd, tracker, tracker_present) %>% 
    mutate(tracker_present_fct = factor(tracker_present)) %>% 
    ggplot(aes(ymd, reorder(tracker, -tracker_present), fill = tracker_present_fct)) +
    geom_tile() +
    scale_fill_brewer(label = as_labeller(c(`0` = "No", `1` = "Yes"))) +
    scale_x_date_monthly +
    theme_angle_x_axis_45_degrees +
    labs(x = "", y = glue("{tracker_category_bucket_name} tracker"), 
         title = glue("Development of {tracker_category_bucket_name} trackers' presence on {site_name}"),
         subtitle = glue("For EU and US group of users visiting {site_name}"),
         fill = "present",
         caption = glue("Notes: Trackers ordered from lowest to highest presence on site.
                  {n_trackers} {tracker_category_bucket_name} trackers were recorded at {site_name} over observation period (T = 21 months).")) + 
    geom_vline_gdpr + 
    highlight_interest_periods + 
    facet_wrap(vars(country))
}

plot_dev_mean_n_trackers_for_two_user_groups_across_all_sites <-
  function(eu_us_sites_trackers_panel_data = eu_us_sites_trackers_panel_DT,
           ylim = c(0, 40)) {
    n_sites <- uniqueN(eu_us_sites_trackers_panel_data$site)
    eu_us_sites_trackers_panel_data %>%
      group_by(ymd, country) %>%
      summarise(mean_n_trackers = mean(n_trackers)) %>%
      ggplot(aes(ymd, mean_n_trackers, color = country)) +
      geom_point() +
      geom_line() +
      coord_cartesian(ylim = ylim) +
      scale_x_date_monthly +
      theme_angle_x_axis_45_degrees +
      labs(
        x = "",
        y = "mean number of trackers",
        title = "Development in mean number of trackers",
        subtitle = glue(
          "Between 2 user groups across all websites in sample (n = {n_sites} sites)"
        )
      ) +
      highlight_interest_periods +
      geom_vline_gdpr
  }

plot_dev_mean_n_trackers_for_two_user_groups_across_all_sites_per_tracker_category <- function(eu_us_sites_trackers_panel_data = eu_us_sites_trackers_panel_DT) {
  n_sites <- uniqueN(eu_us_sites_trackers_panel_data$site)
  eu_us_sites_trackers_panel_data %>% 
    select(ymd, site, country, # + 6 tracker categories
           n_trackers_advertising, n_trackers_analytics, n_trackers_social_media, 
           n_trackers_comments, n_trackers_essential, n_trackers_other) %>% 
    pivot_longer(cols = starts_with("n_trackers_"), names_to = "n_trackers_per_category") %>% 
    mutate(n_trackers_per_category = str_remove_all(n_trackers_per_category, "n_trackers_"),
           n_trackers_per_category = str_replace_all(n_trackers_per_category, "_", " ")) %>% 
    group_by(ymd, country, n_trackers_per_category) %>% 
    summarise(mean_n_trackers_per_category = mean(value)) %>% 
    ggplot(aes(ymd, mean_n_trackers_per_category, color = n_trackers_per_category)) +
    geom_point() + 
    geom_line() + 
    facet_wrap(vars(country)) + 
    labs(x = "", y = "mean number of trackers", color = "tracker category",
         title = "Development in mean number of trackers per tracker category",
         subtitle = glue("Between 2 user groups across all websites in sample (n = {n_sites} sites)")) +
    highlight_interest_periods +
    coord_cartesian(ylim = c(0, 15)) + 
    geom_vline_gdpr
  
}

extract_coefs <- function(model, group_name, is_sample = FALSE) {
  coefs_list <- lapply(1:length(model), function(i) {
    tidy(model[[i]]) %>%
      filter(term == "is_treated_target_audience:post_GDPR") %>%
      mutate(conf.low = estimate - 1.96 * std.error,
             conf.high = estimate + 1.96 * std.error,
             significance = case_when(
               p.value < 0.001 ~ "***",
               p.value < 0.01 ~ "**",
               p.value < 0.05 ~ "*",
               TRUE ~ ""
             ),
             group = group_name,
             dep_var = ifelse(is_sample, names(model)[i], gsub("^lhs: ", "", names(model)[i])))
  })
  
  bind_rows(coefs_list)
}

plot_coefs <- function(coefs, group_name) {
  ggplot(
    coefs %>% filter(group == !!group_name),
    aes(
      x = fct_rev(dep_var),
      y = estimate,
      ymin = conf.low,
      ymax = conf.high
    )
  ) +
    geom_pointrange() +
    geom_errorbar() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(title = group_name, x = NULL, y = NULL) +
    custom_theme
}

add_stars <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}
