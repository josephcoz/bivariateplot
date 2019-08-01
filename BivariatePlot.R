# Bivariate Normal joint density
biv.norm<-function(x,y,mu.x,mu.y,sigma2.x,sigma2.y,rho){
  (
    (1/(2*pi*sqrt((1-rho^2)*sigma2.x*sigma2.y)))
    *exp( -(1/(2*(1-rho^2)))*( (x-mu.x)^2/sigma2.x
                               - (2*rho*(x-mu.x)*(y-mu.y)/sqrt(sigma2.x*sigma2.y))
                               + (y-mu.y)^2/sigma2.y ) )
  )
}
# range of (X,Y) values for plot
x<-seq(0,1,length=50)
y<-seq(0,1,length=50)
# a contour plot
contour(x,y,outer(x,y,biv.norm,mu.x=5,mu.y=3,sigma2.x=6,sigma2.y=2,rho=0.9),
        drawlabels=FALSE)

