hdr.den <- function(x=NULL, prob=c(50,95,99), den=NULL, h=hdrbw(BoxCox(x,lambda),mean(prob)), lambda=1, plot.lines=TRUE, xlab=NULL, ylab="Density",...)
{
  if(missing(den))
    den <- tdensity(x,bw=h,lambda=lambda)
  else if(missing(x))
    x <- sample(den$x, 500, replace=TRUE, prob=den$y)
  
  hdr <- hdr(x,prob,den,h)
  maxden <- max(den$y)
  
  stepy <- maxden*0.02
  ylim=c(-length(prob)*stepy, max(den$y))
  starty <- ylim[1]
  newpoint <- starty+stepy
  
  plot(den,type="l", ylim=ylim, xlab=xlab,ylab=ylab,...)
  cols <- rep(c(4, 2, 3, 0), 3)
  
  for (i in 1:length(prob)) {
    j <- 1
    while(!is.na(hdr$hdr[[i,j]])) {
      xvalue<-c(hdr$hdr[[i,j]], hdr$hdr[[i,j]], hdr$hdr[[i,j+1]], hdr$hdr[[i,j+1]])
      yvalue<-c(starty, newpoint, newpoint, starty)
      polygon(xvalue,yvalue,c=cols[i], density=-1)
      
      if (plot.lines) {
        kdey <- den$y[den$x<=hdr$hdr[[i,j]]]
        kdey <- den$y[length(kdey)]
        lines(x=c(hdr$hdr[[i,j]], hdr$hdr[[i,j]]), y=c(newpoint, kdey), col=cols[i], type="l")
        kdey<-den$y[den$x<=hdr$hdr[[i,j]]]
        kdey <- den$y[length(kdey)]
        lines(x=c(hdr$hdr[[i,j+1]], hdr$hdr[[i,j+1]]), y=c(newpoint, kdey), col=cols[i], type="l")
      }
      
      if((j <- j+2)>dim(hdr$hdr)[[2]]) {break}
    }
    starty<-newpoint
    newpoint<-starty+stepy
    lines(x=c(-500,2e04), y=c(hdr$falpha[[i]], hdr$falpha[[i]]), col=colors()[74], type="l", lty=2)
  }
  return(hdr)
}
