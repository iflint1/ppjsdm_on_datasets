get_nsw <- function(prevalent = 2) {
  vars <- list.files("../data/grids", pattern = ".tif$", full.names = TRUE)
  ibra <- st_read("../data/ibraone.shp", crs = 4283, quiet = TRUE)
  covariates_raster <- stack(vars)
    crs(covariates_raster) <- CRS("+init=epsg:4283")
  
  covariates <- lapply(as.list(covariates_raster), 
                         function(element) maptools::as.im.RasterLayer(element))
  names(covariates) <- names(covariates_raster)
  
  window <- covariates[[1]]
  
  load("../data/moddat.RData")
  species <- list(angobake = angobake_moddat, 
                  coryexim = coryexim_moddat,
                  corymacu = corymacu_moddat,
                  eucaaggl = eucaaggl_moddat,
                  eucaaggr = eucaaggr_moddat,
                  eucablax = eucablax_moddat,
                  eucacine = eucacine_moddat,
                  eucacype = eucacype_moddat,
                  eucadalh = eucadalh_moddat,
                  eucadalr = eucadalr_moddat,
                  eucadean = eucadean_moddat,
                  eucadeld = eucadeld_moddat,
                  eucadive = eucadive_moddat,
                  eucafast = eucafast_moddat,
                  eucafrax = eucafrax_moddat,
                  eucagreg = eucagreg_moddat,
                  eucalueh = eucalueh_moddat,
                  eucamolu = eucamolu_moddat,
                  eucaniph = eucaniph_moddat,
                  eucaobli = eucaobli_moddat,
                  eucaobst = eucaobst_moddat,
                  eucaorea = eucaorea_moddat,
                  eucaovat = eucaovat_moddat,
                  eucaparr = eucaparr_moddat,
                  eucaparv = eucaparv_moddat,
                  eucapauc = eucapauc_moddat,
                  eucapilu = eucapilu_moddat,
                  eucapipe = eucapipe_moddat,
                  eucapunc = eucapunc_moddat,
                  eucaquad = eucaquad_moddat,
                  eucarobu = eucarobu_moddat,
                  eucaross = eucaross_moddat,
                  eucasieb = eucasieb_moddat,
                  eucasqua = eucasqua_moddat,
                  eucasten = eucasten_moddat,
                  eucatric = eucatric_moddat)
  for(i in seq_len(length(species))) {
    species[[i]]$sp <- names(species)[i]
  }
  all_species <- Reduce(function(a, b) rbind(a, b), species)
  
  sp_prevalence <- rev(sort(table(all_species$sp)))
  sp_keep <- names(sp_prevalence)[1:prevalent]
  all_species <- all_species[all_species$sp %in% sp_keep, ]
  
  list(configuration = Configuration(all_species$long, all_species$lat, factor(all_species$sp)), window = window, covariates = covariates)
}
