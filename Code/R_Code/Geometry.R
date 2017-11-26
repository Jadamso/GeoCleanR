
vector3D=function(start=c(0,0,0), end, mycol='green', cone.length=0.1, ... ){
	# ... cylinder as basis-vector
	c=cylinder3d(rbind( c(start), c(0,0,1)), # end
         radius = 0.4,
         e1=cbind(0, 0, 1),
         e2=cbind(1, 0, 0),
         sides=10)

	# ... rotate cylinder horizontally and scale it
	len=sqrt(sum(abs(end)*abs(end)))
	c=scale3d(c,0.1,0.1,len-cone.length)
	
	# ... Duncan Murdoch's code snipe
	rotation <-  rgl:::GramSchmidt(end-start+c(1,0,0),end-start+c(0,1,0), end-start, order=c(3,1,2))
	c <- rotate3d(c, matrix=rotation)
	shade3d(addNormals(c), col=mycol)

	# ... cone3d from rgl-demos
	q1 <- cone3d(qmesh=T,trans=diag(4))  # height=1,radius=1, base at (0,0,0)
	q1=translate3d(scale3d(q1,0.15,0.15,1.4),0,0,len-cone.length)
	q1=rotate3d(q1, matrix=rotation) # hinlegen ... negative rotation == zeigerrrichtung
	shade3d(q1,col=mycol) # verschachtelt aufrufen
}


cone3d <- function(base=c(0,0,0),tip=c(0,0,1),rad=1,n=30,draw.base=TRUE,qmesh=FALSE, trans = par3d("userMatrix"), ...) {
	ax <- tip-base
	if (missing(trans) && !rgl.cur()) trans <- diag(4)
	### is there a better way?
	if (ax[1]!=0) {
		p1 <- c(-ax[2]/ax[1],1,0)
		p1 <- p1/sqrt(sum(p1^2))
		if (p1[1]!=0) {
			p2 <- c(-p1[2]/p1[1],1,0)
			p2[3] <- -sum(p2*ax)
			p2 <- p2/sqrt(sum(p2^2))
		} else {
			p2 <- c(0,0,1)
		}
	} else if (ax[2]!=0) {
	p1 <- c(0,-ax[3]/ax[2],1)
	p1 <- p1/sqrt(sum(p1^2))
	if (p1[1]!=0) {
		p2 <- c(0,-p1[3]/p1[2],1)
		p2[3] <- -sum(p2*ax)
		p2 <- p2/sqrt(sum(p2^2))
	} else {
		p2 <- c(1,0,0)
	}
	} else {
p1 <- c(0,1,0); p2 <- c(1,0,0)
}
degvec <- seq(0,2*pi,length=n+1)[-1]
ecoord2 <- function(theta) {
base+rad*(cos(theta)*p1+sin(theta)*p2)
}

i <- rbind(1:n,c(2:n,1),rep(n+1,n))
v <- cbind(sapply(degvec,ecoord2),tip)
if (qmesh)
## minor kluge for quads -- draw tip twice
i <- rbind(i,rep(n+1,n))
if (draw.base) {
v <- cbind(v,base)
i.x <- rbind(c(2:n,1),1:n,rep(n+2,n))
if (qmesh) ## add base twice
i.x <- rbind(i.x,rep(n+2,n))
i <- cbind(i,i.x)
}
if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
if (!qmesh)
triangles3d(v[1,i],v[2,i],v[3,i],...)
else
return(rotate3d(qmesh3d(v,i,material=...), matrix=trans))
}


ellipsoid3d <- function(rx=1,ry=1,rz=1,n=30,ctr=c(0,0,0),
qmesh=FALSE,
    trans = par3d("userMatrix"),...)
{
    if (missing(trans) && !rgl.cur()) trans <- diag(4) 
    degvec <- seq(0,pi,length=n)
    ecoord2 <- function(p) {
    c(rx*cos(p[1])*sin(p[2]),ry*sin(p[1])*sin(p[2]),rz*cos(p[2])) }
    v <- apply(expand.grid(2*degvec,degvec),1,ecoord2)
    
    if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
    e <- expand.grid(1:(n-1),1:n)
    i1 <- apply(e,1,function(z)z[1]+n*(z[2]-1))
    i2 <- i1+1
    i3 <- (i1+n-1) %% n^2 + 1
    i4 <- (i2+n-1) %% n^2 + 1
    i <- rbind(i1,i2,i4,i3)
    
    if (!qmesh) {
        quads3d(v[1,i],v[2,i],v[3,i],...)
    } else {
        return( rotate3d(qmesh3d(v,i,material=list(...)),matrix=trans))
    }
}
