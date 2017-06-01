library(plyr) # for rbind.fill
library(dplyr)
source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")

# Fetch data
all_obs <- lapply(c("nat", paste0("hhs", 1:10)),
  function(region_val) {
    lapply(1:51,
      function(lag_val) {
        obs_one_lag <- Epidata$fluview(
          regions = list(region_val),
          epiweeks = list(Epidata$range(199740, 201552)),
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
  file = "data/flu_data_with_backfill.rds")
  
