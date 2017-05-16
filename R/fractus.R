#-----------------------------------------------------#
# Title:  fractus.R                                   #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   05.15.17                                    #
#-----------------------------------------------------#

fractus <- function(n, r, shape, point = NULL, c.vert = TRUE, title = TRUE) 
{
  if(is.null(point)) {
    point = 0.5
  }
    
  if(isTRUE(title)) {
    m.label <- paste0('n = ', n, ', r = ', r)
  } 
  else {
    m.label <- NULL
  }
  
  graphics::plot(.chaos.df(n, r, shape, c.vert), bty = 'n', xaxt = 'n', 
                 yaxt = 'n', pch = 16, xlab = '', ylab = '', cex = point, 
                 main = m.label)
}