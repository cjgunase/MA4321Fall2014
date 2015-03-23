library(mlbench)
data(Glass)

glass_red <- Glass[,1:3]
pairs(glass_red,upper.panel=panel.cor,diag.panel=panel.hist)

glass_red <- Glass[,4:6]
pairs(glass_red,upper.panel=panel.cor,diag.panel=panel.hist)

glass_red <- Glass[,7:9]
pairs(glass_red,upper.panel=panel.cor,diag.panel=panel.hist)








############# Functions######################
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}