#install_github('wdweeda/niftiR6')
library(niftiR6)

dat <- niftiR6::readNifti('~/Downloads/Statmap(1).nii.gz')
dat2 <- dat$clone()
dat2$data <- out$clusters
dat2$changeName('pARI_clusters')
niftiR6::writeNifti(dat2)
