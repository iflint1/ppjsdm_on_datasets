get_qld <- function(index = 15, year = 2013, prevalent = 10, jitter = 0.3) {
  stopifnot(index >= 1, index <= 20, year >= 1971, year <= 2013)
  raw_qld_trees <- ppjsdm::raw_qld_trees

  index_epNumber <- unique(raw_qld_trees$epNumber)[index]
  message("The chosen index corresponds to ", paste(index_epNumber, collapse = ", "), ".")
  modified_qld_trees <- raw_qld_trees[raw_qld_trees$epNumber %in% index_epNumber, ]

  used_year <- year
  repeat {
    if(year < 1971) {
      stop("There was no data for any year less than or equal to the requested year.")
    }
    condition <- modified_qld_trees$year == used_year
    if(any(condition)) {
      modified_qld_trees <- modified_qld_trees[condition, ]
      break
    } else {
      used_year <- used_year - 1
    }
  }

  if(used_year != year) {
    warning("There was no data for the requested year ", year, ", using year ", used_year, " instead.")
  }


  sp_prevalence <- rev(sort(table(modified_qld_trees$taxon)))
  sp_keep <- names(sp_prevalence)[1:prevalent]
  modified_qld_trees <- modified_qld_trees[modified_qld_trees$taxon %in% sp_keep, ]

  number_locations <- length(modified_qld_trees$coordinates_x_metres)
  jiggle <- rnorm(2 * number_locations, 0, jitter)
  modified_qld_trees$coordinates_x_metres <- modified_qld_trees$coordinates_x_metres + jiggle[1:number_locations]
  modified_qld_trees$coordinates_y_metres <- modified_qld_trees$coordinates_y_metres + jiggle[-(1:number_locations)]

  # Truncate everything to the observation region (it could have moved outside because of the jitter).
  modified_qld_trees$coordinates_x_metres[modified_qld_trees$coordinates_x_metres > 100] <- 100
  modified_qld_trees$coordinates_x_metres[modified_qld_trees$coordinates_x_metres < 0] <- 0
  modified_qld_trees$coordinates_y_metres[modified_qld_trees$coordinates_y_metres > 50] <- 50
  modified_qld_trees$coordinates_y_metres[modified_qld_trees$coordinates_y_metres < 0] <- 0

  taxon_factor <- factor(modified_qld_trees$taxon)
  configuration <- Configuration(modified_qld_trees$coordinates_x_metres, modified_qld_trees$coordinates_y_metres, taxon_factor)
  window <- Rectangle_window(c(0, 100), c(0, 50))
  list(configuration = configuration, window = window)
}
