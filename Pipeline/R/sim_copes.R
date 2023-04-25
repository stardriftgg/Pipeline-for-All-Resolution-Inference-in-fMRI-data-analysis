sim_copes <- function(simulations = 10, signal = 5, noise = 5){
  copes <- array <- list()
  # generate copes equal to the specified number of simulations
  for (i in 1:simulations){
    # create spatially correlated noise where every sub is represented by one large array
    array[[i]] <- spatialnoise(dim = c(91, 109, 91),
                               sigma = noise * sqrt(simulations), # sigma equals standard deviation
                               nscan = 1, # since copes are simulated via outer loop
                               method = "gaussRF", # Gaussian random field
                               #rho = 0)
                               FWHM = 3) # three times the size of "simulating" a 1mm voxel brain
    array[[i]] <- array[[i]][,,,1] * noise * sqrt(simulations) # remove the 4th (time) dimension from the noise
                                           # scale noise when smoothed
    
    # create a signal for our simplistic purposes a square in the brain
    array[[i]][40:50,49:59,52:62] <- array[[i]][40:50,49:59,52:62] + signal
    copes[[i]] <- asNifti(array[[i]], reference = "stats/newstandard.nii.gz")
  }
  # store each cope in a nifti file
  path <- c()
  ids <- c(1:length(copes))
  for (i in 1:length(copes)){
    path[i] <- paste0("data/simcope-0", ids[i], ".nii.gz")
    RNifti::writeNifti(copes[[i]], file = path[i])
  }
  return(copes)
}