#------------------------------------------------------------------------------
# Title:         fractus.R                                   
# Author:        Brandon Monier (brandon.monier@sdstate.edu) 
# Created:       2017-05-17 00:00:01 CDT 
# Last Modified: 2018-05-21 19:12:28 CDT                                
#------------------------------------------------------------------------------

fractus <- function(n, r, shape, point = NULL, c.vert = TRUE, title = TRUE) {
	if(is.null(point)) {
		point = 0.5
	}
		
	if(isTRUE(title)) {
		m.label <- paste0("n = ", n, ", r = ", r)
	} 
	else {
		m.label <- NULL
	}
	
	graphics::par(mar = c(0, 0, 4, 0))
	graphics::plot(
		.chaos.df(n, r, shape, c.vert), 
		bty = "n", 
		xaxt = "n", 
		yaxt = "n", 
		pch = 16, 
		xlab = "", 
		ylab = "", 
		cex = point, 
		main = m.label
	)
}
