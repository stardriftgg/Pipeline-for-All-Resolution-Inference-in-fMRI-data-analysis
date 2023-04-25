map_TDP <- function(ARIout, path = getwd(), name = "tdp", mask){
  
  if(is.character(mask)){mask = RNifti::readNifti(mask)}
  # check ARIout$out == matrix, else select ARIout$out[4]
  if(is.matrix(pARI$out)){
    cl = as.numeric(gsub("cl", "", names(ARIout$out[, 4])))
    tdp = as.numeric(ARIout$out[, 4])
  }else{
    cl = as.numeric(gsub("cl", "", names(ARIout$out[4])))
    tdp = as.numeric(ARIout$out[4])
  }
  tdp[length(cl)] = 0
  
  map = ARIout$clusters
  
  if(!is.array(map)){ map <- array(map, dim(map))}
  for(i in 1:length(cl)){
    map[map==cl[i]]=tdp[i]
  }
  
  RNifti::writeNifti(map, file = paste0(path, "/", name, ".nii"), template = mask)
}