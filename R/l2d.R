l2d <- function(x1, x2, method="gaussiand", check=FALSE, varw1=NULL, varw2=NULL)
{
  # L2-inner product between two distributions
  #
  # x1, x2: the data.
  # method: string. The method used for the estimation of the
  #         densities. It can be:
  #         - "gaussiand": gaussian densities; the parameters
  #                        (mean vectors and variance matrices) are
  #                        estimated.
  #         - "kern":      the densities are estimated using the
  #                        kernel method. For the moment, only the
  #                        Gaussian kernel is available.

  if (!(method %in% c("gaussiand", "kern")))
    stop("method must be either 'gaussiand' or 'kern'.")

  p <- NCOL(x1)
  if (NCOL(x2) != p)
    stop("x1 and x2 must be two vectors, or have the same number of columns.")

  if (method == "gaussiand")
    choix <- "g."
  if (method == "kern")
    {
     if ((!is.null(varw1)) & (!is.null(varw2)))
       choix <- "kw" else
       choix <- "k."
    }

  if (p == 1)
    choix <- paste0(choix, "u") else
    choix <- paste0(choix, "p")

  switch(choix,
    g.u = { # Univariate Gaussian densities:
            var1<-var(x1);
            var2<-var(x2);
            if(check)
              {if(var1<.Machine$double.eps |var2<.Machine$double.eps)
                {stop("At least one variance is zero")
                }
              }
            m1<-mean(x1);
          	m2<-mean(x2);
          	return(l2dpar(m1,var1,m2,var2))
          },
    g.p = { # Multivariate Gaussian densities:
            var1<-var(x1);
            var2<-var(x2);
            if(check)
             {if(abs(det(var1))<.Machine$double.eps | abs(det(var2))<.Machine$double.eps )
               {stop("One of the sample variances is degenerate")
               }
             }
            p<-ncol(x1);
            m1<-colMeans(x1);
            m2<-colMeans(x2);
            return(as.numeric(l2dpar(m1,var1,m2,var2)))
          },
    kwu = { # Univariate, Gaussian kernel method, smoothing window given:
            if(check)
              {if(abs(varw1)<.Machine$double.eps | abs(varw2)<.Machine$double.eps )
                {stop("One of the bandwidths is zero")
                }
              }
            n1=length(x1)
            n2=length(x2)
           	p<-1;
           	expo<-matrix(0,ncol=n2,nrow=n1);
           	varsom<-varw1+varw2;
           	varinv<-1/varsom;
           	x1<-as.vector(x1);
           	x2<-as.vector(x2);
         	  for (i1 in 1:n1)
          	{for (i2 in 1:n2)
          		{expo[i1,i2]=exp((-1/2)*(x1[i1]-x2[i2])*varinv*(x1[i1]-x2[i2])) }};
          	return((1/(n1*n2))*(1/(2*pi)^(p/2))*(1/varsom^(1/2))*sum(expo))
          },
    kwp = { # Multivariate, Gaussian kernel method, smoothing window given:
            if(check)
            {if(abs(det(varw1))<.Machine$double.eps | abs(det(varw2))<.Machine$double.eps )
              {stop("One of the bandwidth matrices is degenerate")
              }
            }
            n1=nrow(x1)
            n2=nrow(x2)
            p<-ncol(x1);
        	  expo<-matrix(0,ncol=n2,nrow=n1);
            varsom<-varw1+varw2;
            varinv<-solve(varsom);
            x1<-as.matrix(x1);
            x2<-as.matrix(x2);
            for (i1 in 1:n1)
           	 {for (i2 in 1:n2)
          	 		{expo[i1,i2]=exp((-1/2)*(x1[i1,]-x2[i2,])%*%varinv%*%(x1[i1,]-x2[i2,])) }};
         	  return((1/(n1*n2))*(1/(2*pi)^(p/2))*(1/det(varsom)^(1/2))*sum(expo))
          },
    k.u = { # Univariate, Gaussian kernel method, AMISE window:
        	  var1<-var(x1);
          	var2<-var(x2);
            if(check)
              {if(var1<.Machine$double.eps |var2<.Machine$double.eps)
                {stop("At least one variance is zero")
                }
              }
          	n1<-length(x1);
          	n2<-length(x2);
          	expo<-matrix(0,ncol=n2,nrow=n1);
          	w1<-bandwidth.parameter(1,n1);
          	w2<-bandwidth.parameter(1,n2);
          	vars<-w1^2*var1+w2^2*var2;
          	x1<-as.vector(x1);
          	x2<-as.vector(x2);
          	for (i1 in 1:n1)
          		{for (i2 in 1:n2)
          			{expo[i1,i2]=exp((-1/2)*(x1[i1]-x2[i2])^2/vars) }};
          	return(sum(expo)/(sqrt(2*pi*vars)*n1*n2))
          },
    k.p = { # Multivariate, Gaussian kernel method, AMISE window:
          	var1<-var(x1);
          	var2<-var(x2);
            if(check)
             {if(abs(det(var1))<.Machine$double.eps | abs(det(var2))<.Machine$double.eps)
              {stop("One of the sample variances is degenerate")
              }
             }
          	p<-ncol(x1);
          	n1<-nrow(x1);
          	n2<-nrow(x2);
          	expo<-matrix(0,ncol=n2,nrow=n1);
            w1<-bandwidth.parameter(p,n1);
          	w2<-bandwidth.parameter(p,n2);
          	vars<-w1^2*var1+w2^2*var2;
          	varinv<-solve(vars);
          	x1<-as.matrix(x1);
          	x2<-as.matrix(x2);
          	for (i1 in 1:n1)
          		{for (i2 in 1:n2)
          			{expo[i1,i2]=exp((-1/2)*(x1[i1,]-x2[i2,])%*%varinv%*%(x1[i1,]-x2[i2,]))
                }
              }
          	return((1/(n1*n2))*(1/(2*pi)^(p/2))*(1/det(vars)^(1/2))*sum(expo))
          }
  )
}