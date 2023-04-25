statmap <- function(copes, alternative = "two.sided", path = getwd(), 
                    name = "map", Pmap = FALSE, mask = NULL, dimensions = dimensions){
  
  if(!is.list(copes)){stop("Please insert the list of copes as list class object")}
  if(is.character(mask)){mask = RNifti::readNifti(mask)}
  
  x <- dimensions[1]
  y <- dimensions[2]
  z <- dimensions[3]
  img <- array(NA, c(dimensions, length(copes)))
  
  for (i in 1:length(copes)){
    img[, , , i] <- copes[[i]]
  }
  
  scores <- matrix(img, nrow = (x*y*z), ncol = length(copes))
  scores[!mask, ] = NA
  resO <- oneSamplePar(X = scores, alternative = alternative)
  if(Pmap){
    pv = array(data = resO$p, dim = c(x, y, z))
    RNifti::writeNifti(pv, file = paste0(path, "/P", name, ".nii.gz"), template = mask)
  }
  
  t = array(data = resO$T, dim = c(x, y, z))
  RNifti::writeNifti(t, file = paste0(path, "/Stat", name, ".nii.gz"), template = mask)
  
  #p.values <- as.matrix.data.frame(pv)
  #return(p.values)
}