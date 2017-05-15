#-----------------------------------------------------#
# Title:  fractus.R - helper functions                #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   05.15.17                                    #
#-----------------------------------------------------#

## Return data frame
.chaos.df <- function(n, r, shape) 
{
  message('Generating index...')
  
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3, width = 40)
  s <- c(0.5, 0)
  
  ls1 <- list() 
  for (i in 1:n) {
    if (shape == 'triangle') {
      ls1[[i]] <- .dice.tern()
    }
    if (shape == 'square') {
      ls1[[i]] <- .dice.four()
    }
    if (shape == 'pentagon') {
      ls1[[i]] <- .dice.five()
    }
  }
  
  ls2 <- list()
  for (i in 2:n) {
    ls2[[1]] <- data.frame(x = r * sum(c(s[[1]], ls1[[1]][1, 1])), 
                           y = r * sum(c(s[[2]], ls1[[1]][1, 2])))
    
    ls2[[i]] <- data.frame(x = r * sum(c(ls1[[i]][1, 1], ls2[[i - 1]][1, 1])),
                           y = r * sum(c(ls1[[i]][1, 2], ls2[[i - 1]][1, 2])))
    utils::setTxtProgressBar(pb, i)
  }
  
  message('Wrapping things up...')
  
  tmp <- data.table::rbindlist(ls2)
  
  message('Done!\n')
  return(tmp)
}



## 'Dice'
.dice.tern <- function() 
{
  alpha <- c(0, 0)
  beta  <- c(0.5, sqrt(3)/2)
  gamma <- c(1, 0)
  
  tmp <- sample(1:3, 1)
  
  if(tmp == 1) {
    return(data.frame(x = alpha[1], y = alpha[2]))
  } 
  
  if(tmp == 2) {
    return(data.frame(x = beta[1], y = beta[2]))
  }
  
  if(tmp == 3) {
    return(data.frame(x = gamma[1], y = gamma[2]))
  }
}


.dice.four <- function() 
{
  a <- c(0, 0)
  b <- c(0, 1)
  c <- c(1, 1)
  d <- c(1, 0)
  
  tmp <- sample(1:4, 1)
  
  if(tmp == 1) {
    return(data.frame(x = a[1], y = a[2]))
  } 
  
  if(tmp == 2) {
    return(data.frame(x = b[1], y = b[2]))
  }
  
  if(tmp == 3) {
    return(data.frame(x = c[1], y = c[2]))
  }
  
  if(tmp == 4) {
    return(data.frame(x = d[1], y = d[2]))
  }
}


.dice.tern <- function() 
{
  alpha <- c(0, 0)
  beta  <- c(0.5, sqrt(3)/2)
  gamma <- c(1, 0)
  
  tmp <- sample(1:3, 1)
  
  if(tmp == 1) {
    return(data.frame(x = alpha[1], y = alpha[2]))
  } 
  
  if(tmp == 2) {
    return(data.frame(x = beta[1], y = beta[2]))
  }
  
  if(tmp == 3) {
    return(data.frame(x = gamma[1], y = gamma[2]))
  }
}