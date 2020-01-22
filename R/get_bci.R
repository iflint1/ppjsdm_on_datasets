get_bci <- function(prevalent = 2, jitter = .1) {
  load("../data/bci.tree8.rdata")
  raw_data <- bci.tree8
  
  raw_data <- raw_data[!is.na(raw_data$gx) & !is.na(raw_data$gy), ]
  raw_data <- raw_data[raw_data$status == "A", ]

  sp_prevalence <- rev(sort(table(raw_data$sp)))
  sp_keep <- names(sp_prevalence)[1:prevalent]
  raw_data <- raw_data[raw_data$sp %in% sp_keep, ]

  number_locations <- length(raw_data$gx)
  jiggle <- rnorm(2 * number_locations, 0, jitter)
  raw_data$gx <- raw_data$gx + jiggle[1:number_locations]
  raw_data$gy <- raw_data$gy + jiggle[-(1:number_locations)]

  # Truncate everything to the observation region (it could have moved outside because of the jitter).
  raw_data$gx[raw_data$gx > 1000] <- 1000
  raw_data$gx[raw_data$gx < 0] <- 0
  raw_data$gy[raw_data$gy > 500] <- 500
  raw_data$gy[raw_data$gy < 0] <- 0

  sp_factor <- factor(raw_data$sp)
  list(configuration = Configuration(raw_data$gx, raw_data$gy, sp_factor), window = Rectangle_window(c(0, 1000), c(0, 500)))
}
