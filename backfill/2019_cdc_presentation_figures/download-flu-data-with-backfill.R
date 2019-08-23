library(plyr) # for rbind.fill
library(dplyr)
source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")

region_val <- "hhs2"
lag_val <- 1L
obs_one_lag <- Epidata$fluview(
  regions = list(region_val),
  epiweeks = list(Epidata$range(199740, 201552)),
  lag = list(lag_val))

results_df <- lapply(obs_one_lag$epidata,
       function(x) {
         x[sapply(x, function(comp) is.null(comp))] <- NA
         return(as.data.frame(x))
       }) %>%
  rbind.fill()

# Fetch data
all_obs <- lapply(c("nat", paste0("hhs", 1:10)),
  function(region_val) {
    lapply(1:51,
      function(lag_val) {
        obs_one_lag <- Epidata$fluview(
          regions = list(region_val),
          epiweeks = list(Epidata$range(199740, 201930)),
          lag = list(lag_val))

        lapply(obs_one_lag$epidata,
          function(x) {
            x[sapply(x, function(comp) is.null(comp))] <- NA
            return(as.data.frame(x))
          }) %>%
          rbind.fill()
      }) %>%
      rbind.fill()
  }) %>%
  rbind.fill()

saveRDS(all_obs,
  file = "flu_data_with_backfill.rds")
  

backfill_data <- all_obs
lag_df <- data.frame()
epiweeks <- unique(backfill_data$epiweek)
regions <- unique(backfill_data$region)

region <- "hhs2"
#for (region in regions){
  #for (epiweek in c(seq(201540,201552),seq(201601,201620))){#{
   all_epiweeks <- unique(backfill_data[backfill_data$region==region,]$epiweek)
  #all_epiweeks <- "201040"
  for (epiweek in all_epiweeks){
    print(epiweek)
    tmp <- rep(NA,51)
    #all_lags <- 0:50
    all_lags <- 1
    for (lag in all_lags){
      print(max(backfill_data[backfill_data$epiweek == epiweek & backfill_data$region==region,]$lag))
      lag_used <- max(backfill_data[backfill_data$epiweek == epiweek & backfill_data$region==region,]$lag)
      current <- backfill_data[backfill_data$region==region & backfill_data$epiweek == epiweek & backfill_data$lag ==lag,]$wili
      finally_reported <- backfill_data[backfill_data$region==region & backfill_data$epiweek == epiweek & backfill_data$lag ==max(backfill_data[backfill_data$epiweek == epiweek & backfill_data$region==region,]$lag),]$wili
      if (length(current) > 0 & length(finally_reported) > 0){
        tmp[lag+1] <- current/finally_reported
      }
    }
    lag_df <- rbind(lag_df,c(match(region,regions),epiweek,as.integer(substr(epiweek,5,6)), lag_used,tmp))
  }
}


lag_df <- lag_df[, c(1, 2, 3, 4, 6)]
colnames(lag_df) <- c("region_index", "epiweek", "week", "lag_used", "lag1_ratio")
lag_df <- lag_df %>%
  mutate(
    year = substr(epiweek, 1, 4)
  )

library(ggplot2)
ggplot(data = lag_df, mapping = aes(x = lag1_ratio)) +
  geom_histogram() +
  facet_wrap(~lag_used)

lag_df %>%
  group_by(lag_used) %>%
  summarize(
    mean_ratio = mean(lag1_ratio, na.rm = TRUE)
  )

