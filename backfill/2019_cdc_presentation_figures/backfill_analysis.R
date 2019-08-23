library(tidyverse)
library(MMWRweek)
library(grid)

flu <- readRDS("flu_data_with_backfill.rds")
flu <- flu %>%
  mutate(
    year = substr(epiweek, 1, 4) %>% as.numeric(),
    week = substr(epiweek, 5, 6) %>% as.numeric(),
    report_year = substr(issue, 1, 4) %>% as.numeric(),
    report_week = substr(issue, 5, 6) %>% as.numeric(),
    epi_week_date = MMWRweek::MMWRweek2Date(
      MMWRyear = year,
      MMWRweek = week
    ),
    report_week_date = MMWRweek::MMWRweek2Date(
      MMWRyear = report_year,
      MMWRweek = report_week
    )
  )

flu$season <- ifelse(
  flu$week <= 30,
  paste0(flu$year - 1, "/", flu$year),
  paste0(flu$year, "/", flu$year + 1)
)

## Season week column: week number within season
## weeks after week 30 get season_week = week - 30
## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
## This computation relies on the start_date function in package MMWRweek,
## which is not exported from that package's namespace!!!
flu$season_week <- ifelse(
  flu$week <= 30,
  flu$week + MMWRweek(MMWRweek:::start_date(flu$year) - 1)$MMWRweek - 30,
  flu$week - 30
)

flu <- flu %>%
  ungroup() %>%
  as.data.frame() %>%
  group_by(region, epi_week_date) %>%
  do(
    mutate(
      .,
      final_num_ili = num_ili[lag == max(lag)],
      final_ili = ili[lag == max(lag)],
      final_wili = wili[lag == max(lag)],
      diff_log_curr_final_num_ili = log(num_ili) - log(final_num_ili)
    )
  ) %>%
  ungroup()


reduced_data <- flu %>%
  as.data.frame() %>%
  filter(region == "hhs9", season == "2011/2012", season_week == 10) %>%
  mutate(
    ratio_current_final_num_ili = num_ili / final_num_ili,
    ratio_current_final_ili = ili / final_ili,
    ratio_current_final_wili = wili / final_wili
  ) %>%
  select(
    num_ili, num_patients, num_providers, wili, ili,
    ratio_current_final_num_ili, ratio_current_final_ili, ratio_current_final_wili,
    report_year, report_week, report_week_date) %>%
  gather(
    "measure", "value", num_ili, num_patients, num_providers, wili, ili,
    ratio_current_final_num_ili, ratio_current_final_ili, ratio_current_final_wili
  ) %>%
  mutate(
    measure = ifelse(measure == "num_ili", "Reported Number of Patients with ILI",
              ifelse(measure == "num_patients", "Reported Number of Patients",
              ifelse(measure == "num_providers", "Reported Number of Providers",
              ifelse(measure == "wili", "Reported Weighted ILI",
              ifelse(measure == "ili", "Reported ILI",
              ifelse(measure == "ratio_current_final_num_ili", "Current/Final Num. Patients with ILI",
              ifelse(measure == "ratio_current_final_ili", "Current/Final ILI",
              ifelse(measure == "ratio_current_final_wili", "Current/Final weighted ILI", measure))))))))
  )

ggplot() +
  geom_point(aes(x = report_week_date, y = value), data = reduced_data) +
  facet_wrap(~ measure, scales = "free_y") +
#  ylab("(reported number of cases at lag x) /\n(reported number of cases at lag 51)") +
  ggtitle("Reporting for Epidemic Week 201140, HHS Region __") +
  theme_bw()



pdf("reporting_overview.pdf", width = 12, height = 8)
ggplot() +
  geom_point(aes(x = report_week_date, y = value),
    data = reduced_data %>%
      filter(measure %in% c("Reported Number of Providers", "Reported Number of Patients", "Reported Number of Patients with ILI")) %>%
      mutate(
        measure = factor(measure, levels = c("Reported Number of Providers", "Reported Number of Patients with ILI", "Reported Number of Patients"))
      )
    ) +
  facet_wrap(~ measure, scales = "free_y", ncol = 1) +
#  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week") +
#  scale_x_date(date_breaks = "1 month") +
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  ylab("") +
  xlab("Report Time") +
  #  ylab("(reported number of cases at lag x) /\n(reported number of cases at lag 51)") +
  ggtitle("Reporting for Epidemic Week 201140, HHS Region __") +
  theme_bw(base_size = 20)
dev.off()


pdf("reporting_overview_ili_wili.pdf", width = 12, height = 9)
ggplot() +
  geom_point(aes(x = report_week_date, y = value),
             data = reduced_data %>%
               filter(measure %in% c("Reported Number of Patients", "Reported Number of Patients with ILI", "Reported ILI", "Reported Weighted ILI")) %>%
               mutate(
                 measure = factor(measure, levels = c("Reported Number of Patients with ILI", "Reported Number of Patients", "Reported ILI", "Reported Weighted ILI"))
               )
  ) +
  facet_wrap(~ measure, scales = "free_y", ncol = 1) +
  #  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week") +
  #  scale_x_date(date_breaks = "1 month") +
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  ylab("") +
  xlab("Report Time") +
  #  ylab("(reported number of cases at lag x) /\n(reported number of cases at lag 51)") +
  ggtitle("Reporting for Epidemic Week 201140, HHS Region __") +
  theme_bw(base_size = 20)
dev.off()





reduced_data <- flu %>% filter(lag == 1) %>%
  mutate(
    initial_error = wili - final_wili,
    initial_error_ratio = wili / final_wili
  )

pdf("reporting_overview_magnitude_histogram.pdf", width = 12, height = 6)
p1 <- ggplot() +
  geom_histogram(aes(x = initial_error), data = reduced_data %>% filter(region != "nat"),
    binwidth = 0.1, center = 0) +
  #facet_wrap(~ measure, scales = "free_y", ncol = 1) +
  #  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week") +
  #  scale_x_date(date_breaks = "1 month") +
  #scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  #ylab("") +
  #xlab("Initial Reported ILI - Final Reported ILI") +
  xlim(c(-2.25, 1.75)) +
  xlab("") +
  #  ylab("(reported number of cases at lag x) /\n(reported number of cases at lag 51)") +
  ggtitle("Distributon of Initial Reporting Errors\nAll HHS Regions, 2003/2004 through 2018/2019") +
  theme_bw(base_size = 20) +
  theme(
    axis.text.x = element_blank()
  )

p2 <- ggplot() +
  geom_boxplot(aes(y = initial_error), data = reduced_data %>% filter(region != "nat")) +
  scale_x_continuous(breaks = NULL, minor_breaks = NULL) +
  ylab("First Reported minus Final wILI") +
  #  ylab("(reported number of cases at lag x) /\n(reported number of cases at lag 51)") +
  #ggtitle("Distributon of Initial Reporting Errors\nAll HHS Regions, 2003/2004 through 2018/2019") +
  coord_flip() +
  theme_bw(base_size = 20) +
  ylim(c(-2.25, 1.75)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0.35, 0, 2.8), "cm")
  )

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, heights = unit(c(1, 0.2), c("null", "null")))))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

dev.off()

reduced_data %>% summarize(
  mean(abs(initial_error) <= 0.1),
  mean(abs(initial_error) <= 0.5),
  mean(abs(initial_error) <= 1)
)



pdf("reporting_overview_magnitude_season.pdf", width = 12, height = 9)
ggplot() +
  geom_boxplot(aes(y = initial_error, x = season), data = reduced_data %>% filter(region != "nat")) +
  xlab("Season") +
  ylab("First Reported minus Final wILI") +
  ggtitle("Distributon of Initial Reporting Errors\nAll HHS Regions") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


pdf("reporting_overview_boxplots_season_week.pdf", width = 12, height = 9)
ggplot() +
geom_boxplot(aes(y = initial_error, x = factor(season_week)), data = reduced_data %>% filter(region != "nat")) +
xlab("Season Week (0 corresponds to EW 30)") +
ylab("First Reported minus Final wILI") +
ggtitle("Distributon of Initial Reporting Errors (Difference) \nAll HHS Regions, 2003/2004 through 2015/2016") +
theme_bw(base_size = 20) +
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
dev.off()


pdf("reporting_overview_ratio_boxplots_season_week.pdf", width = 12, height = 9)
ggplot() +
  geom_boxplot(aes(y = initial_error_ratio, x = factor(season_week)), data = reduced_data %>% filter(region != "nat")) +
  xlab("Season Week (0 corresponds to EW 30)") +
  ylab("First Reported wILI / Final Reported wILI") +
  ggtitle("Distributon of Initial Reporting Errors (Ratio) \nAll HHS Regions, 2003/2004 through 2015/2016") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
dev.off()


error_summaries <- reduced_data %>%
  dplyr::filter(region != "nat", season %in% c("2016/2017", "2017/2018", "2018/2019")) %>%
  dplyr::group_by(season, season_week) %>%
  dplyr::summarize(
    mean_initial_error = mean(initial_error),
    median_initial_error = median(initial_error)
  ) %>%
  ungroup()

ggplot() +
  geom_line(aes(y = mean_initial_error, x = season_week, color = season),
    data = error_summaries
  ) +
#  geom_line(aes(y = median_initial_error, x = season_week), color = "red",
#            data = error_summaries
#  ) +
  xlab("Season Week (0 corresponds to EW 30)") +
  ylab("First Reported minus Final wILI") +
  ggtitle("Distributon of Initial Reporting Errors\nAll HHS Regions, 2003/2004 through 2015/2016") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




error_summaries <- reduced_data %>%
  dplyr::filter(region != "nat") %>%
  dplyr::group_by(season, season_week) %>%
  dplyr::summarize(
    mean_initial_error = mean(initial_error),
    median_initial_error = median(initial_error)
  ) %>%
  ungroup()

reduced_data <- reduced_data %>%
  mutate(
    season_group = case_when(
      season %in% paste0(2003:2005, "/", 2004:2006) ~ "03/04 - 05/06",
      season %in% paste0(2006:2008, "/", 2007:2009) ~ "06/07 - 08/09",
      season %in% paste0(2009:2011, "/", 2010:2012) ~ "09/10 - 11/12",
      season %in% paste0(2012:2014, "/", 2013:2015) ~ "12/13 - 14/15",
      season %in% paste0(2015:2017, "/", 2016:2018) ~ "15/16 - 17/18",
      season %in% paste0(2018, "/", 2019) ~ "18/19",
      TRUE ~ "NOT HANDLED"
    )
  )


pdf("reporting_overview_ratio_bias_season_season_week.pdf", width = 12, height = 9)
ggplot() +
  geom_smooth(aes(y = initial_error, x = season_week, color = season_group, size = season_group),
            data = reduced_data,
            method = "loess", se = FALSE
  ) +
  scale_size_manual("Season Group", values = c(rep(0.4, 4), 1, 1.5)) +
  scale_color_viridis_d("Season Group", option = "C", end = 0.8) +
  #  geom_line(aes(y = median_initial_error, x = season_week), color = "red",
  #            data = error_summaries
  #  ) +
  xlab("Season Week (0 corresponds to EW 30)") +
  ylab("First Reported minus Final wILI") +
  ggtitle("Smoothed Estimates of Bias of Initial Report, by Season Week") +
  theme_bw(base_size = 20)
dev.off()

pdf("corrected_wili.pdf", width = 9, height = 4.5)
ggplot(
  data = flu %>% filter(season == "2018/2019", report_week %in% c(2, 20), region == "hhs7", season_week <= 23)
) +
  scale_color_viridis_d("", labels = c("Observed as of\nWeek 23", "Estimated\nFinal wILI"), option = "C", begin = 0.2, end = 0.8) +
  geom_line(mapping = aes(x = season_week, color = factor(report_week), y = wili), size = 1) +
  xlim(c(0, 30)) +
  xlab("Season Week (0 corresponds to EW 30)") +
  theme_bw(base_size = 20)
dev.off()


fake_forecast <- flu %>%
  filter(season == "2018/2019", report_week == 20, region == "hhs7", season_week == 23) %>%
  mutate(pi_upper = wili, pi_lower = wili)

forecast_mean_diffs  <- c(0, 0.4, 0.7, 0.4)
forecast_half_widths <- c(0.3, 0.5, 1.0, 1.5)
fake_forecast <- rbind(
  fake_forecast %>% select(season_week, report_week, wili, pi_lower, pi_upper),
  data.frame(
    season_week = fake_forecast$season_week + 1:4,
    report_week = fake_forecast$report_week,
    wili = fake_forecast$wili + forecast_mean_diffs,
    pi_lower = fake_forecast$wili + forecast_mean_diffs - forecast_half_widths,
    pi_upper = fake_forecast$wili + forecast_mean_diffs + forecast_half_widths
  )
)


pdf("corrected_wili_forecast.pdf", width = 9, height = 4.5)
ggplot(
  data = flu %>% filter(season == "2018/2019", report_week %in% c(2, 20), region == "hhs7", season_week <= 23)
) +
  geom_line(mapping = aes(x = season_week, color = factor(report_week), y = wili), size = 1) +
  geom_line(data = fake_forecast, mapping = aes(x = season_week, color = factor(report_week), y = wili), linetype = 2, size = 0.5) +
  geom_point(data = fake_forecast, mapping = aes(x = season_week, color = factor(report_week), y = wili), size = 0.5) +
  geom_ribbon(data = fake_forecast, mapping = aes(x = season_week, color = factor(report_week), fill = factor(report_week), ymin = pi_lower, ymax = pi_upper), alpha = 0.4, show.legend = FALSE) +
  scale_color_viridis_d("", labels = c("Observed as of\nWeek 23\n", "Estimated or\nPredicted\nFinal wILI"), option = "C", begin = 0.2, end = 0.8) +
  scale_fill_viridis_d("", labels = c("Estimated or\nPredicted\nFinal wILI"), option = "C", begin = 0.8, end = 0.8) +
  xlim(c(0, 30)) +
  xlab("Season Week (0 corresponds to EW 30)") +
  theme_bw(base_size = 20)
dev.off()




get_onset_baseline <- function(region,season="2015/2016"){
  Influenza_onset_baselines <- read.csv("/Users/gcgibson/Downloads/2018-2019-cdc-flu-contest-master/data-raw/flu_onset_baselines.csv")
  levels(Influenza_onset_baselines$region) <- c("nat","hhs1","hhs10",paste0("hhs",2:9))
  idx <- which(Influenza_onset_baselines$region == region&
                 Influenza_onset_baselines$season==season)
  return (Influenza_onset_baselines[idx,"baseline"])
}

get_onset_week <- function(incidence_trajectory,
                           baseline,
                           onset_length,
                           first_season_week = 31,
                           weeks_in_first_season_year) {
  exceeded_threshold <- sapply(
    seq_len(length(incidence_trajectory) - onset_length),
    function(start_ind) {
      above_baseline <- incidence_trajectory[seq(from = start_ind, length = onset_length)] >= baseline
      length(above_baseline)>0 &&
        all(above_baseline) &&
        !all(is.na(incidence_trajectory))
    }
  )
  if(any(exceeded_threshold, na.rm = TRUE)) {
    season_week <- min(which(exceeded_threshold))
    return(season_week)
  } else {
    return("none")
  }
}

data <- flu

test_week <- 201606
test_region <- "hhs4"
hhs_data <- data[data$region == test_region,]
library(ggplot2)
library(dplyr)
fully <- hhs_data %>% group_by(epiweek) %>% filter(lag==max(lag)) %>% arrange(epiweek)
fully_2015 <- fully[fully$epiweek >= 201540 & fully$epiweek <= test_week,]
available <- hhs_data[hhs_data$issue <= test_week,] %>% group_by(epiweek) %>% filter(lag==max(lag)) %>% arrange(epiweek)
available_2015 <- available[available$epiweek >= 201540 & available$epiweek <= test_week,]
baseline <- cdcfluutils::get_onset_baseline(gsub("hhs", "Region", test_region),"2015/2016")
print(get_onset_week(available_2015$wili,baseline = baseline,3,31,52))
print(get_onset_week(fully_2015$wili,baseline = baseline,3,31,52))
library(ggplot2)


pdf("effect_seasonal_target.pdf", width = 12, height = 9)
plot_df <- data.frame(x=as.factor(available_2015$epiweek), available=available_2015$wili,final=fully_2015$wili[1:(length(fully_2015$wili)-1)])
ggplot(plot_df,aes(x=x,y=available,group=0,col='Available', shape = 'Available')) +
  geom_hline(yintercept = baseline,linetype=2) +
  geom_point(size = 4) +
  geom_point(aes(x=x,y=final,col='Final', shape = 'Final'), size = 4) +
  scale_color_discrete("") +
  scale_shape("") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Epidemic Week") +
  ylab("wILI") +
  ggtitle("Determining Season Onset\nEpidemic Week 2016-05, HHS Region 4")
dev.off()


