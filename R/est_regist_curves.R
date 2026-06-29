##### Auxiliar functions for registration procedures in the Latent Deformation Model
library(dbrobust)

# Monotone transformation of an individual curve (as in Dupuy, Loubes and Maza, Statistics and Computing 2011)
monot <- function(y) { 
  # y is a vector representing a single realization of a curve
  m <- length(y)
  y.mon <- y
  if(m>1){
    for (k in 2:m){
      y.mon[k]<-y.mon[k-1]+abs(y[k]-y[k-1])
    }
  }
  return(y.mon) 
}


# Moving average smoothing of a sample of curves with adaptive window length
moving_average <- function(ymat, order,thresh=10^-5){
  cmy = colMeans(ymat) # average of the points of every curve
  ymat_ma = t(apply(ymat,1,forecast::ma,order=order)) # soften curves
  # forecast::ma: Function from the package forecast which calculates the Moving Average. 
  #               Given a vector, it softens it using the average of every point with its neighbours.
  #               The parameter 'order' defines the size of the time window. 
  #               (how many points to the right and left we are taking into account in the average)
  cmy_new = colMeans(ymat_ma)
  
  m=ncol(ymat)
  
  k = round(order/2)
  i1= k+1
  i2= m-k
  msre= mean( ((cmy-cmy_new)/cmy)[i1:i2]^2) # cuadratic difference of cmy (original average) and cmy_new (soften average)
  # Big values of msre mean the softening has deleted important information of the original curve.
  if ( msre > thresh){
    # Recalculate msre with less soft curves (using a smaller time window)
    or=order-1
    while(msre > thresh){
      ymat_ma = t(apply(ymat,1,forecast::ma,order=or))
      cmy_new = colMeans(ymat_ma)
      k = round(or/2)
      i1= k+1
      i2= m-k
      msre= mean( ((cmy-cmy_new)/cmy)[i1:i2]^2)

      or=or-1
    }
    or = or +1
  }else{
    # Recalculate msre with more soft curves (using a bigger time window)
    or=order+1
    while(msre< thresh & or<m/3){
      ymat_ma = t(apply(ymat,1,forecast::ma,order=or))
      cmy_new = colMeans(ymat_ma)
      k = round(or/2)
      i1= k+1
      i2= m-k
      msre= mean( ((cmy-cmy_new)/cmy)[i1:i2]^2)

      or=or+1
    }
    or = or -1
  }
  # Now, we have the optimal soften curves ymat_ma calculated with the optimal window size (order)
  or = ifelse(or==1,order,or)     #in case no smoothing order preserves the curves well enough, we keep the original one (m/5)
  ymat_ma = t(apply(ymat,1,mov_avg,order=or))
  
  return(ymat_ma)
}
mov_avg <- function(y,order){
  # Like the forecast::ma function, but using the maximum possible number of neighbors 
  # for calculating this first and last points 
  # (if order = 5 the first and last five points can not be calculated with forecast::ma)
  y_ma <- forecast::ma(y,order=order)
  k = round(order/2)
  or=k
  for (j in 1:k){ #we fill in values in the extremes
    j1=k-j+1
    j2=m-k+j
    y_ma[j1] = mean(c(y[1:j1],y_ma[(j1+1):(j1+or)]))
    y_ma[j2] = mean(c(y_ma[(j2-or):(j2-1)],y[j2:m])) 
    or=or-1
  }
  y_ma[1]=y[1]
  y_ma[m]=y[m]
  return(y_ma)
}

# Monotone transformation and normalization of the multivariate functions, possibly including smoothing
MonTrans <- function(y,smooth=TRUE, method="MA", bw=0.01) { 
  # y is matrix or data.frame with n rows (individuals) and m columns (time points)
  
  n <- nrow(y); m <- ncol(y) # n: number of observations (curves);  m: length of time grid

  # Smoothing
    if (smooth==TRUE & method=="MA"){ #Esto es para hacer suavizado de las curvas pero creo que es mejor quitarlo
      y = moving_average(y,order=round(m/5))
    }
  
  # Normalization of the curves
  amp <- abs(apply(y,1,max)) # vector que cada valor i es el valor máximo de la fila i de y, i.e. 
                             # el valor máximo que toma la curva y_i                                     
  amp_j_mat <- matrix(rep(amp,each=m),nrow=n, byrow=T)
  y.nor<- y/amp_j_mat

  # Monotone transformation of the curves (as in Dupuy, Loubes and Maza, Statistics and Computing 2011)
  y.mon<- matrix(NA,n,m)
  for (i in 1:n) {
     y.mon[i,] <- monot(y.nor[i,])
  }
    
  # # Normalisation of the monotonized curves
  # # min <- apply(y.mon,1,min)             #  We have to first monotonize, then normalize again
  # # min_j_mat <- matrix(rep(min,each=m),nrow=n, byrow=T)   # Otherwise the final curves might not be normalized. 
  #   
  # # amp2 <- apply(y.mon- min_j_mat,1,max)    
  # amp2 <- apply(y.mon,1,max)  
  # amp_j_mat <- matrix(rep(amp2,each=m),nrow=n, byrow=T)   # Otherwise the final curves might not be normalized. 
  # # y.mon_nor <- (y.mon- min_j_mat)/amp_j_mat +  min_j_mat
  # # y.mon_nor <- (y.mon- min_j_mat)/amp_j_mat #between 0 and 1
  # y.mon_nor <- y.mon/amp_j_mat

  return(list(mon.data.df = y.mon, mon.data.nor.df = y.mon, data.nor.df = y.nor, amp=amp, smooth=data.frame(y)))

}





# Deepest curve in the pooled monotone sample (or trimmed mean of deepest curves)

# median_overall_monot <- function(ymon,y,t,trim=0){ ?????? t is not defined!
median_overall_monot <- function(ymon,y,t,trim=0){ 
  
  # y is matrix or data.frame with n rows (individuals) and m columns (time points)
  # ymon contains the monotone sample
  
  n <- nrow(y); m <- ncol(y) # n: number of observations (curves);  m: length of time grid
  
  # Modified band depth calculation
  y_fd <- roahd::fData(t, ymon) # roahd::fDatais from the library roahd. It converts the matrix of
  #                monotone curves (ymon) into a functional object.
  #                It associates to each value i of every curve j, ymon[i,j],
  #                its point in time, t[i].
  
  mbd <- roahd::MBD(y_fd) # Calculates the Modified Band Depth of each curve in y_fd
  
  # Select the curve or curves with the higest MBD
  med <- sort(mbd,decreasing = T, index.return=T)$ix[1:(trim*n+1)] #If trim=0, it just selects the highest value of mbd
  if (length(med)>1){
    median_curve <- colMeans(y[med,]) #trimmed mean of deepest curves
  }else{
    median_curve <- y[med,]
  }
  
  return(list(median_curve=median_curve,med=med))
}




function_composition <- function(y,x,t){ #returns h(t) = x^-1 o y(t), y is a strictly monotone function
  # y is a vector of length m, an observed curve  
  # x is a vector of length m, the amplitude function. y(t) = x o h(t) => h(t) = x^-1 o y (t)
  # t is a vector of length m, with the time domain of x and y
  
  m = length(y) # num of observed points
  mi = min(x)
  ma = max(x)
   
  yout = mi+(ma-mi)*(y-min(y))/(max(y)-min(y))  #adjust to ensure yout and x have the same range, so we can do x^-1 o y
  
  #low resolution for linear interpolation to avoid problems at "almost plateaux": we sample 1 every 10 points
  idd = seq(1,m,10)   
  tt = c(min(t),t[idd],max(t) ) 
  
  #low resolution of the values in yout
  vec = c(min(yout), yout[idd], max(yout) )  
  yy = unique(vec )
  
  idy = which(!duplicated(vec)) # which indexes in vec have been retained
  tt0 = tt[idy]
  
  #low_res estimate of y^-1(x(t)) at tt0 by linear interpolation
  zz = approx(x, t , yy, rule=2)$y  # approx(x, t, yy) interpola:
                                    # Dados t valores de x, devuelve los valores de t tales que x(t) ≈ yy
                                    # Es decir, zz ≈  x^-1 o y
  
  # cubic spline monotonic interpolation of  x^-1(y(t)) at t
  z <- demography::cm.spline(tt0,zz,n=m)$y  # demography::cm.spline: uses a "Cubic Monotonic Spline". 
                                            # Creates a soft curve that is strictly incresing.
                                            # n=m: Estira el resultado de baja resolución para que vuelva a tener los m puntos originales.
  
  z[c(1,m)] <- t[c(1,m)] # We force extremes to be tied to the starting and end points of the interval 
  
  return(z)
}






# Individual warping function estimates
warp_est <- function(D, t, L_hat){ 
  # D is the output of MonTrans
  # t the vector of time points
  # L_hat the estimated common amplitude function
  
  y <- D$mon.data.nor.df   # monotone normalized sample
  
  n <- nrow(y); m <- ncol(y) # n: number of observations (curves);  m: length of time grid

  x = monot(L_hat) # monotonized version of L_hat
  h  <- matrix(0, nrow=n, ncol=m)
  for(i in 1:n){    
      
      yout = y[i,]  # i-th monotone normalized curve 
      h[i,] <- function_composition(yout,x,t)  # returns h_i(t) = x^-1(yout(t))
  }

  return(h)
}

# Registered curves
registered_curves <- function(y, h_hat, t){
  # y is matrix or data.frame with n rows (individuals) and m columns (time points)
  # h_hat is an n*m matrix with the individual warping function estimates 
  # t is a vector of length m, with the observation time points
  
  n <- nrow(y); m <- ncol(y) # n: number of observations (curves);  m: length of time grid
  
  x_registered = matrix(0, nrow = n, ncol=m)
  for(i in 1:n){  
        # y_i = x o h_i => x_registered_i = y_i o h_i^-1
        h_inv = approx(h_hat[i,], t, t,rule=2)$y    #h_i^-1
        x_registered[i,] = approx(t,y[i,],h_inv,rule=2)$y        #y_ij(h_ij^-1)

    }

  return(x_registered)
}



RMS <- function(D1, D2){
  # Related Metric Scaling for 2 different distances matrices: D1 and D2 (supposing geom var = 1)
  n <- nrow(D1)
  
  D1 <- make_euclidean(D1,w = rep(1, n))$D_euc
  D2 <- make_euclidean(D2,w = rep(1, n))$D_euc
    
  ones <- matrix(1, nrow = n, ncol = 1)
  H <- diag(n) - 1/n*ones%*%t(ones)
  
  G1 <- -1/2*H%*%D1%*%H
  G2 <- -1/2*H%*%D2%*%H
  
  G_RMS <- G1 + G2 - 1/2*(sqrt_mat(G1)%*%sqrt_mat(G2) + sqrt_mat(G2)%*%sqrt_mat(G1))
  
  g <- matrix(diag(G_RMS), nrow = 1, ncol = n)
  D_RMS <- t(g)%*%t(ones) + ones%*%g - 2*G_RMS
}



RMS4 <- function(D1, D2, D3, D4){
  # Related Metric Scaling for 4 different distances matrices (supposing geom var = 1)
  n <- nrow(D1)

  D1 <- make_euclidean(D1,w = rep(1, n))$D_euc
  D2 <- make_euclidean(D2,w = rep(1, n))$D_euc
  D3 <- make_euclidean(D3,w = rep(1, n))$D_euc
  D4 <- make_euclidean(D4,w = rep(1, n))$D_euc
  
  ones <- matrix(1, nrow = n, ncol = 1)
  H <- diag(n) - 1/n*ones%*%t(ones)
  
  G1 <- -1/2*H%*%D1%*%H
  G2 <- -1/2*H%*%D2%*%H
  G3 <- -1/2*H%*%D3%*%H
  G4 <- -1/2*H%*%D4%*%H
  
  G_RMS <- G1 + G2 + G3 + G4 - 1/4*(sqrt_mat(G1)%*%sqrt_mat(G2) + sqrt_mat(G1)%*%sqrt_mat(G3) + sqrt_mat(G1)%*%sqrt_mat(G4) + 
                                    sqrt_mat(G2)%*%sqrt_mat(G1) + sqrt_mat(G2)%*%sqrt_mat(G3) + sqrt_mat(G2)%*%sqrt_mat(G4) +
                                    sqrt_mat(G3)%*%sqrt_mat(G1) + sqrt_mat(G3)%*%sqrt_mat(G2) + sqrt_mat(G3)%*%sqrt_mat(G4) + 
                                    sqrt_mat(G4)%*%sqrt_mat(G1) + sqrt_mat(G4)%*%sqrt_mat(G2) + sqrt_mat(G4)%*%sqrt_mat(G3))
  
  g <- matrix(diag(G_RMS), nrow = 1, ncol = n)
  D_RMS <- t(g)%*%t(ones) + ones%*%g - 2*G_RMS
}



# Function to obtain M^(1/2) for any matrix
# sqrt_mat <- function(M) {
#   # Spectral decomposition
#   eig <- eigen(M, symmetric = TRUE)
#   
#   # Eigen values
#   eigen_values <- pmax(eig$values, 0) 
#   
#   return(eig$vectors %*% diag(sqrt(eigen_values)) %*% t(eig$vectors))
# }


library(RSpectra)

sqrt_mat <- function(M, k = 15) {
  # k can not be greater than the matrix dimensions 
  k <- min(k, nrow(M) - 1)
  
  # eigs_sym since the gram matrices are symmetric 
  eig <- eigs_sym(M, k = k, which = "LM")  # 'which = "LM"' obtains the greater eigen values 
  # k can not be greater than the matrix dimensions 
  k <- min(k, nrow(M) - 1)
  
  # eigs_sym since the gram matrices are symmetric 
  eig <- eigs_sym(M, k = k, which = "LM")  # 'which = "LM"' obtains the greater eigen values 
  
  # Eigen values
  eigen_values <- pmax(eig$values, 0)
  
  return(eig$vectors %*% diag(sqrt(eigen_values)) %*% t(eig$vectors))
}





# Derivatives of a set of n curves sampled in a grid of step size dt
derivative <- function(M, dt) {
  # M is a matrix n x m  where the row i is the estimation of a function m_i in the grid of step size dt
  derivatives <- diff(M) / dt
  return(c(derivatives, derivatives[length(derivatives)])) # Repetimos el último valor para volver a tamaño m
}
