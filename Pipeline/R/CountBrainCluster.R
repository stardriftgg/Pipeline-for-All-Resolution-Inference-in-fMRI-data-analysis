CountBrainCluster <- function(Pmap = Pmap, clusters = clusters, alpha = alpha, Statmap = Statmap){

  cluster.ids <- sort(unique(as.vector(clusters)))
  cluster.pvalues <- cluster.zvalues <- dims <- list()
  discoveries <- Size <- Stat <- ActiveProp <- TrueNull <- FalseNull <- x <- y <- z <- c()
  
  for(i in 1:length(cluster.ids)){
    # p and z-values per cluster
    cluster.pvalues[[i]] <- na.omit(Pmap[which(clusters==cluster.ids[i], arr.ind = T)])
    cluster.zvalues[[i]] <- na.omit(Statmap[which(clusters==cluster.ids[i], arr.ind = T)])
    # output statistics
    Size[i] <- length(cluster.pvalues[[i]])
    Stat[i] <- max(cluster.zvalues[[i]])
    dims[[i]] <- which(Statmap==Stat[i], arr.ind = T)
    x[i] <- dims[[i]][1]
    y[i] <- dims[[i]][2]
    z[i] <- dims[[i]][3]
    # discoveries within each cluster
    discoveries[i] <- 0
    for(k in 1:length(cluster.pvalues[[i]])){
      if (cluster.pvalues[[i]][k] < alpha){ # FDP
        discoveries[i] <- discoveries[i] + 1
        }
    }
    # discovery proportions
    ActiveProp[i] <- discoveries[i] / Size[i] # TDP
    TrueNull[i] <- Size[i] - discoveries[i]
    FalseNull[i] <- Size[i] - TrueNull[i]
  }
  
  # create output table
  out <- matrix(c(Size = Size,
                  FalseNull = FalseNull,
                  TrueNull = TrueNull,
                  ActiveProp = ActiveProp,
                  dim1 = x,
                  dim2 = y,
                  dim3 = z,
                  Stat = Stat), ncol = 8, byrow = F)
  # fill col and row names
  colnames(out) = c('Size','FalseNull','TrueNull','ActiveProp','dim1','dim2','dim3','Stat')
  if(!is.null(dim(out))){
    rownames(out) = paste("cl", sep="", cluster.ids)
  }
  out <- out[dim(out)[1]:1, ]
  print(out)
  return(list(out = out, clusters = clusters))
}

