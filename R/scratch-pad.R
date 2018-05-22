#   _____                _       _       _____          _ 
#  / ____|              | |     | |     |  __ \        | |
# | (___   ___ _ __ __ _| |_ ___| |__   | |__) |_ _  __| |
#  \___ \ / __| '__/ _` | __/ __| '_ \  |  ___/ _` |/ _` |
#  ____) | (__| | | (_| | || (__| | | | | |  | (_| | (_| |
# |_____/ \___|_|  \__,_|\__\___|_| |_| |_|   \__,_|\__,_|



# New dice
.dice.tern <- function(n) 
{
  a <- c(0, 0)
  b <- c(0.5, sqrt(3)/2)
  c <- c(1, 0)
  
  ls1 <- sample(list(a, b, c), n, replace = TRUE)
  data.frame(t(matrix(unlist(ls1), nrow = 2)))
}


.dice.four <- function(n) 
{
  a <- c(0, 0)
  b <- c(0, 1)
  c <- c(1, 1)
  d <- c(1, 0)
  
  ls1 <- sample(list(a, b, c, d), n, replace = TRUE)
  data.frame(t(matrix(unlist(ls1), nrow = 2)))
}


.dice.five <- function(n) 
{
  a <- c(0.5, 0)
  b <- c(1.5, 0)
  c <- c(1.8122, 0.951)
  d <- c(1, 1.539)
  e <- c(0.1878, 0.951)
  
  ls1 <- sample(list(a, b, c, d, e), n, replace = TRUE)
  data.frame(t(matrix(unlist(ls1), nrow = 2)))
}

.seed <- function()
{
  a <- seq(0, 0.5, 0.001)
  sample(a, 2, replace = TRUE)
}

.iterate <- function(r, s)
{
  x <- r * sum(c(s[1], tmp[1][[1, 1]]))
  y <- r * sum(c(s[2], tmp[1][[1, 2]]))
  list(x, y)
}

s <- .seed()



.chaos.df <- function(n, r, shape, c.vert = TRUE) 
{
  
  s <- .seed()
  
  if (shape == 'triangle') {
    ls1 <- .dice.tern(n)
  } else if (shape == 'square') {
    ls1 <- .dice.four(n)
  } else if (shape == 'pentagon') {
    ls1 <- .dice.five(n)
  }
  
  tmp <- data.table::rbindlist(ls1)
  
  if (isTRUE(c.vert)) {
    tmp <- tmp
  } else {
    tmp <- tmp[with(tmp, c(TRUE, diff(as.numeric(interaction(x, y))) != 0)), ]
  }
  
  pb <- utils::txtProgressBar(min = 0, max = nrow(tmp), style = 3, width = 40)
  
  ls2 <- list()
  for (i in 2:nrow(tmp)) {
    ls2[[1]] <- data.frame(x = r * sum(c(s[[1]], tmp[1][[1, 1]])), 
                           y = r * sum(c(s[[2]], tmp[1][[1, 2]])))
    
    ls2[[i]] <- data.frame(x = r * sum(c(tmp[i][[1, 1]], ls2[[i - 1]][1, 1])),
                           y = r * sum(c(tmp[i][[1, 2]], ls2[[i - 1]][1, 2])))
    utils::setTxtProgressBar(pb, i)
  }
  
  message('Wrapping things up...')
  
  tmp <- data.table::rbindlist(ls2)
  
  message('Done!\n')
  return(tmp)
}