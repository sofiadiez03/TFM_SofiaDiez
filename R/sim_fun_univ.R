### Functions used to generate data in the simulation study

# normalized amplitude function. As in Carroll&Muller 2023
L1 = function(t){
  Lshape1 = 20-(5)*cos(4*pi*t)+(3)*sin(pi*t^2)+15*(t)^2
  Lshape1 / max(Lshape1)
}


L2 = function(t){
  # Una funcion trimodal
  Lshape2 = 20 + 7*sin(5*pi*t) + 4*cos(2*pi*t^2) - 10*t
  Lshape2 / max(Lshape2)
}

L3 = function(t){ 
  # Funcion cuatrimodal similar a L2
  f = 25 + 7*sin(5*pi*t) + 4*cos(2*pi*t^2) - 10*t + 5*exp(-15*(t-0.3)^2)
  
  f / max(f)
}

L4 = function(t){ 
  f = 18 + 6*cos(2*pi*t) + 4*sin(pi*t^2) + 10*t^2
  
  f / max(f)
}



# normalized amplitude function for outliers
L_out1 = function(t, fun = L1){
  Lshape_out = exp(-25*(t-0.5)^2) + t  
  Lshape_out/ max(Lshape_out) *diff(range(fun(t)))+min(fun(t))
}

# random warping function generation as proposed in Dupuy, Loubes, Maza, Statistics and Computing, 2009
warps_DLM<- function(t,n, eps=0.005,N=25){  
  
  p = length(t)
  
  #Initial warping function: identity
  h <- matrix(rep(t,n),nrow=n,ncol=p,byrow=T)
  
  #Iterative procedure to generate warping functions
  for (i in 1:N) {
    u<-runif(1)*(1-20*eps)+10*eps # punto "pivote" en t
    v<-runif(n)*2*eps+(u-eps) # nuevo valor para el punto u en el eje deformado
    pu<-floor(u*(p-1))+1
    
    
    for (j in 1:n){
      # w es una función lineal a trozos (conectando el punto [0,0] con [u,v] y luego con [1,1]).
      w<-v[j]*t[1:pu]/u
      w<-append(w, ((1-v[j])/(1-u))*t[(pu+1):p]+(v[j]-u)/(1-u) )
      
      h[j,]<-approx(t, w, h[j,], method="linear",rule=2)$y   ##composition of w and h in t
      # la deformación final es el resultado de muchas pequeñas deformaciones acumuladas
    }
  }
  
  return(h)  
}

# function to generate curve with individual warping + component-based warping + aditional distorsion. As in Carroll&Muller 2023
makecurve = function(i, j, sigma_dist, hmat, t, fun = L1){
  if(sigma_dist==0){
    dist=t
  }else{
    d = rnorm(n = 1, 0, sigma_dist^2)
    dinv = (exp(t*d)-1)/(exp(d)-1)
    dist =  approx(dinv, t, t)$y
  }
  hd = approx(t,  hmat[i,], xout = dist)$y
  # PoH = approx(t, psi_true[[j]], xout = hd)$y
  fun(hd)
}


 
#### Main function for data simulation 
sim_data = function(n, sigma_amp, sigma_warp, sigma_error = 0, sigma_dist = 0, amean,  t, m,  out=F, out_prop=0.05, fun = L1){
  
  #Individual warping functions generation
  if (sigma_warp<=1){
     hinvmat = matrix(0, nrow = n, ncol = m)
     hmat = matrix(0, nrow = n, ncol = m)
     
     z = rnorm(n = n, 0, sigma_warp^2)
    for(i in 1:n){
       if (sigma_warp==0){
         hinvmat[i,] = t
       }else{
          hinvmat[i,] = (exp(t*z[i])-1)/(exp(z[i])-1)
       }
          hmat[i,] =  approx(hinvmat[i,], t, t)$y
    }
  }else{
    hmat <- warps_DLM(t,n,eps=sigma_warp*10^-3, N=2500)   #we get H st E[H]=id
    hinvmat = matrix(0, nrow = n, ncol = m)
    for (i in 1:n){
       hinvmat[i,] =  approx(hmat[i,], t, t,rule = 2)$y
    }
    hmat=hinvmat       #since we want E[H^1]=id
  }
  
  #Random amplitudes generation
  # amat = matrix(0, nrow = n , ncol = p)
  # for(j in 1:p){
  #   amat[,j] = pmax(rnorm(n, mean = ameans[j], sd = sigma_amp^2), 5)
  # }
  amat = rnorm(n, mean=amean, sd = sigma_amp^2) # Cada curva recibe un factor de escala $a_i$. 
                                                # Esto simula que, aunque la forma sea la misma, 
                                                # algunos individuos tienen una respuesta más fuerte (curvas más altas)
                                                # y otros más débil (curvas más bajas). 
                                                # Es la variabilidad vertical de los datos.
  
  
  
  ###### If outlier contamination  
  n_out=round(out_prop*n) #num of outliers
  
  fL <- function(i){  #function to create shape outliers.
    if (out==T & i> n-n_out){
      return(function(t) L_out1(t, fun = fun))
    }else{
      return(fun)
    }
  }
  
  if(out==T){
    # outs=cbind(rep((n-n_out+1):n,p), rep(1:p, each=n_out)) #indicator of outlying curve
    outs= (n-n_out+1):n
  }else{
    outs <- c()
    }
  #####
  
  #Multivariate curves generation
  simdata = matrix(0, nrow = n, ncol = m)
  for(i in 1:n){
    simdata [i, ] =  amat[i] * makecurve(i, j, sigma_dist, hmat, t, fun=fL(i)) + 
        rnorm(n = m, mean = 0,  sd = sigma_error)
  }
  # Para cada obs i:
  #   - Toma la forma (normal o outlier)
  #   - Aplica la deformación temporal (hmat)
  #   - Multiplica por la amplitud ($a_i$)
  #   - Suma ruido blanco para que los datos no sean perfectos y parezcan mediciones reales de laboratorio.
  
  

  return(list(simdata = simdata, hmat = hmat, amat = amat, outs=outs))
}



sim_data_2groups = function(n, sigma_amp, sigma_warp, sigma_error = 0, 
                           sigma_dist = 0, amean, t, m, 
                           group_ratio = 0.5, out = FALSE, out_prop = 0.05, fun1 = L1, fun2 = L2) {
  
  #Individual warping functions generation
  if (sigma_warp<=1){
    hinvmat = matrix(0, nrow = n, ncol = m)
    hmat = matrix(0, nrow = n, ncol = m)
    
    z = rnorm(n = n, 0, sigma_warp^2)
    for(i in 1:n){
      if (sigma_warp==0){
        hinvmat[i,] = t
      }else{
        hinvmat[i,] = (exp(t*z[i])-1)/(exp(z[i])-1)
      }
      hmat[i,] =  approx(hinvmat[i,], t, t)$y
    }
  }else{
    hmat <- warps_DLM(t,n,eps=sigma_warp*10^-3, N=2500)   #we get H st E[H]=id
    hinvmat = matrix(0, nrow = n, ncol = m)
    for (i in 1:n){
      hinvmat[i,] =  approx(hmat[i,], t, t,rule = 2)$y
    }
    hmat=hinvmat       #since we want E[H^1]=id
  }
  
  #Random amplitudes generation
  # amat = matrix(0, nrow = n , ncol = p)
  # for(j in 1:p){
  #   amat[,j] = pmax(rnorm(n, mean = ameans[j], sd = sigma_amp^2), 5)
  # }
  amat = rnorm(n, mean=amean, sd = sigma_amp^2) # Cada curva recibe un factor de escala $a_i$. 
  # Esto simula que, aunque la forma sea la misma, 
  # algunos individuos tienen una respuesta más fuerte (curvas más altas)
  # y otros más débil (curvas más bajas). 
  # Es la variabilidad vertical de los datos.
  

  
  
  ###### If outlier contamination  
  n_out = if(out) round(out_prop * n) else 0
  n_g1 = round(n * group_ratio)
  n_g2 = n - n_g1
  # The first n_g1 curves belong to one group and the rest to the other group
  group_labels = c(rep(1, n_g1), rep(2, n_g2))
  
  # Outliers indexes
  outs = if(out) (n - n_out + 1):n else c()
  
  
  #Multivariate curves generation
  simdata = matrix(0, nrow = n, ncol = m)
  for(i in 1:n) {
    # Decidir función base: ¿Es outlier o pertenece a G1/G2?
    if (out && i %in% outs) {
      # Si es outlier, usamos la función de contaminación L_out1
      current_fun = function(x) L_out1(x, fun = fun1) 
    } else {
      # Si no es outlier, asignamos según su grupo
      current_fun = if(group_labels[i] == 1) fun1 else fun2
    }
    
    # Generación de la curva combinando todos los efectos:
    # - amat[i]: Escala vertical
    # - makecurve: Aplica la función base con el warping (hmat) y sigma_dist
    # - rnorm: Ruido blanco final (sigma_error)
    simdata[i, ] = amat[i] * makecurve(i, j = 1, sigma_dist, hmat, t, fun = current_fun) + 
      rnorm(n = m, mean = 0, sd = sigma_error)
  }
  
  return(list(simdata = simdata, hmat = hmat, amat = amat, 
              groups = group_labels, outs = outs))
}





sim_data_3groups = function(n, sigma_amp, sigma_warp, sigma_error = 0, 
                            sigma_dist = 0, amean, t, m, 
                            group_ratio = c(0.333, 0.333), out = FALSE, out_prop = 0.05, fun1 = L1, fun2 = L2, fun3 = L4) {
  
  #Individual warping functions generation
  if (sigma_warp<=1){
    hinvmat = matrix(0, nrow = n, ncol = m)
    hmat = matrix(0, nrow = n, ncol = m)
    
    z = rnorm(n = n, 0, sigma_warp^2)
    for(i in 1:n){
      if (sigma_warp==0){
        hinvmat[i,] = t
      }else{
        hinvmat[i,] = (exp(t*z[i])-1)/(exp(z[i])-1)
      }
      hmat[i,] =  approx(hinvmat[i,], t, t)$y
    }
  }else{
    hmat <- warps_DLM(t,n,eps=sigma_warp*10^-3, N=2500)   #we get H st E[H]=id
    hinvmat = matrix(0, nrow = n, ncol = m)
    for (i in 1:n){
      hinvmat[i,] =  approx(hmat[i,], t, t,rule = 2)$y
    }
    hmat=hinvmat       #since we want E[H^1]=id
  }
  
  #Random amplitudes generation
  # amat = matrix(0, nrow = n , ncol = p)
  # for(j in 1:p){
  #   amat[,j] = pmax(rnorm(n, mean = ameans[j], sd = sigma_amp^2), 5)
  # }
  amat = rnorm(n, mean=amean, sd = sigma_amp^2) # Cada curva recibe un factor de escala $a_i$. 
  # Esto simula que, aunque la forma sea la misma, 
  # algunos individuos tienen una respuesta más fuerte (curvas más altas)
  # y otros más débil (curvas más bajas). 
  # Es la variabilidad vertical de los datos.
  
  
  
  
  ###### If outlier contamination  
  n_out = if(out) round(out_prop * n) else 0
  
  n_g1 = round(n * group_ratio[1])
  n_g2 = round(n * group_ratio[2])
  n_g3 = n - (n_g1 + n_g2)
  
  # The first n_g1 curves belong to the group generated by fun1
  # The next n_g2 curves belong to the the group generated by fun2
  # The rest of the curves belong to the group generated by fun3  
  group_labels = c(rep(1, n_g1), rep(2, n_g2), rep(3, n_g3))
  
  # Outliers indexes
  outs = if(out) (n - n_out + 1):n else c()
  
  
  #Multivariate curves generation
  simdata = matrix(0, nrow = n, ncol = m)
  for(i in 1:n) {
    # Decidir función base: ¿Es outlier o pertenece a G1/G2?
    if (out && i %in% outs) {
      # Si es outlier, usamos la función de contaminación L_out1
      current_fun = function(x) L_out1(x, fun = fun1) 
    } else {
      # Si no es outlier, asignamos según su grupo
      if(group_labels[i] == 1){
        current_fun = fun1
      } else if(group_labels[i] == 2){
        current_fun = fun2
      } else{ current_fun = fun3 }
    }
    
    # Generación de la curva combinando todos los efectos:
    # - amat[i]: Escala vertical
    # - makecurve: Aplica la función base con el warping (hmat) y sigma_dist
    # - rnorm: Ruido blanco final (sigma_error)
    simdata[i, ] = amat[i] * makecurve(i, j = 1, sigma_dist, hmat, t, fun = current_fun) + 
      rnorm(n = m, mean = 0, sd = sigma_error)
  }
  
  return(list(simdata = simdata, hmat = hmat, amat = amat, 
              groups = group_labels, outs = outs))
}









