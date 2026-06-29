##### Distance Squared Matrix computation
source("est_regist_curves.R")   # estimation of the registered curves
library(pracma)

## Distance defined for functional data in the latent deformation model
sq_dist <- function(data, t, smooth=TRUE, method = "MA", omega = 1/2, alpha = 1/2){ 
  # data: matrix or data.frame with n rows (individuals) and m columns (time points)
  # t: vector of length m with time points
  # omega: weighting parameter of the distance. High omega => More importance to the warping functions 
  # alpha: weighting parameter of the distance. High alpha => More importance to the derivatives of the registered curves and of the warping functions
  time <- proc.time()
  n = nrow(data)
  dt <- diff(t) # [t_2 - t_1, t_3 - t_2, ..., t_m - t_{m-1}]
  
  # obtain strictly monotone curves
  D <- MonTrans(data,smooth=smooth, method="MA") #smoothing with moving average
  base_time <- proc.time() - time
  
  
  time_start_delta_1 <- proc.time()
  ### Estimations using the deepest curve in the monotone global sample: 
  # Lambda estimate as the deepest curve in the monotone global sample
  L_med <- median_overall_monot(D$mon.data.nor.df,D$data.nor.df, t)
  L_hat <- L_med$median_curve
  base_time_delta_1 <- proc.time() - time_start_delta_1
  
  
  
  #### D2_M ----
  time_start_D2_M <- proc.time()
  # Individual warping function 
  h_hat <- warp_est(D, t, L_hat) # matriz n x m donde la fila i es la estimacion h_i evaluada en t_1,...,t_m 
  
  # Registered curves
  x_reg <- registered_curves(data, h_hat, t) # matriz n x m donde la fila i es la retrieved  x_i evaluada en t_1,...,t_m
  
  # Amplitude and Phase squared euclidean distances matrices and their derivatives
  D2_amp <- matrix(rep(0,n*n),nrow = n, ncol = n )
  
  D2_phase <- matrix(rep(0,n*n),nrow = n, ncol = n )
  for (i in 1:n){
    for (j in 1:n){
      X_i <- x_reg[i,]
      X_j <- x_reg[j,]
      h_i <- h_hat[i,]
      h_j <- h_hat[j,]
      
      D2_amp[i,j] <-  sum(dt*(X_i[1:length(dt)] - X_j[1:length(dt)])^2)
      
      D2_phase[i,j] <-  sum(dt*(h_i[1:length(dt)] - h_j[1:length(dt)])^2) 
    }
  }
  
  # Rescaled squared euclidean dist matrices using geometric variability 
  V_amp = (1/(2*n^2))*sum(D2_amp)
  V_phase = (1/(2*n^2))*sum(D2_phase)
  
  D2_amp = 1/V_amp*D2_amp
  D2_phase = 1/V_phase*D2_phase
  
  # D2_M computation
  D2_M = matrix(rep(0,n*n),nrow = n, ncol = n )         
  for (i in 1:n){
    for (j in 1:n){
      D_amp <- D2_amp[i,j]
      D_phase <- D2_phase[i,j]
      
      D2_M[i,j] <- (1-omega)*D_amp + omega*D_phase
    }
  }
  time_D2_M <- proc.time() - time_start_D2_M
  
  #### D2_Mejorada ----
  time <- proc.time()
  
  # Individual warping function 
  h_hat <- warp_est(D, t, L_hat) # matriz n x m donde la fila i es la estimacion h_i evaluada en t_1,...,t_m 
  h_hat_deriv <- t(apply(h_hat, 1, derivative, dt = dt[1]))
  
  # Registered curves
  x_reg <- registered_curves(data, h_hat, t) # matriz n x m donde la fila i es la retrieved  x_i evaluada en t_1,...,t_m
  x_reg_deriv <- t(apply(x_reg, 1, derivative, dt = dt[1]))
  
  # Amplitude and Phase squared euclidean distances matrices and their derivatives
  D2_amp <- matrix(rep(0,n*n),nrow = n, ncol = n )
  D2_amp_deriv <- matrix(rep(0,n*n),nrow = n, ncol = n )
  
  D2_phase <- matrix(rep(0,n*n),nrow = n, ncol = n )
  D2_phase_deriv <- matrix(rep(0,n*n),nrow = n, ncol = n )
  for (i in 1:n){
    for (j in 1:n){
      X_i <- x_reg[i,]
      X_i_deriv <- x_reg_deriv[i,]
      X_j <- x_reg[j,]
      X_j_deriv <- x_reg_deriv[j,]
      h_i <- h_hat[i,]
      h_i_deriv <- h_hat_deriv[i,]
      h_j <- h_hat[j,]
      h_j_deriv <- h_hat_deriv[j,]
      
      D2_amp[i,j] <-  sum(dt*(X_i[1:length(dt)] - X_j[1:length(dt)])^2)
      D2_amp_deriv[i,j] <-sum(dt*(X_i_deriv[1:length(dt)] - X_j_deriv[1:length(dt)])^2)
      
      D2_phase[i,j] <-  sum(dt*(h_i[1:length(dt)] - h_j[1:length(dt)])^2) 
      D2_phase_deriv[i,j] <- sum(dt*(h_i_deriv[1:length(dt)] - h_j_deriv[1:length(dt)])^2)
      
    }
  }
  
  # Rescaled squared euclidean dist matrices using geometric variability 
  V_amp = (1/(2*n^2))*sum(D2_amp)
  V_amp_deriv = (1/(2*n^2))*sum(D2_amp_deriv)
  
  V_phase = (1/(2*n^2))*sum(D2_phase)
  V_phase_deriv = (1/(2*n^2))*sum(D2_phase_deriv)
  
  D2_amp = 1/V_amp*D2_amp
  D2_amp_deriv = 1/V_amp_deriv*D2_amp_deriv
  
  D2_phase = 1/V_phase*D2_phase
  D2_phase_deriv = 1/V_phase_deriv*D2_phase_deriv
  
  time_derivative_computations <- proc.time() - time
  
  
  # D2_Mejorada computation
  time <- proc.time()
  D2_Mejorada = matrix(rep(0,n*n),nrow = n, ncol = n )  # \delta_1^*
  for (i in 1:n){
    for (j in 1:n){
      D_amp <- D2_amp[i,j]
      D_amp_deriv <- D2_amp_deriv[i,j]
      
      D_phase <- D2_phase[i,j]
      D_phase_deriv <- D2_phase_deriv[i,j]
 
      D2_Mejorada[i,j] <- (1-omega)*((1-alpha)*D_amp + alpha*D_amp_deriv) + omega*((1-alpha)*D_phase + alpha*D_phase_deriv)
    }
  }
  time_D2_Mej <- proc.time() - time
  
  
  #### D2_Mej_RMS ----
  time <- proc.time()
  
  # Related Metric Scaling 
  D2_amp_RMS = RMS(D2_amp, D2_amp_deriv)
  D2_phase_RMS = RMS(D2_phase, D2_phase_deriv)
  
  # D2_Mej_RMS computation
  D2_Mej_RMS = matrix(rep(0,n*n),nrow = n, ncol = n )   
  for (i in 1:n){
    for (j in 1:n){
      D_amp_RMS <- D2_amp_RMS[i,j]
      D_phase_RMS <- D2_phase_RMS[i,j]

      D2_Mej_RMS[i,j] <- (1-omega)*D_amp_RMS + omega*D_phase_RMS
    }
  }
  tol <- 1e-13 
  D2_Mej_RMS[abs(D2_Mej_RMS) <= tol] <- 0
  
  time_D2_Mej_RMS <- proc.time() - time
  
  
  
  
  ### \delta_2 ----
  time_start_D2_x2 <- proc.time() 
  ### Estimations using two by two comparisons
  # Amplitude and Phase squared euclidean distances matrices
  D2_amp_x2 <- matrix(rep(0,n*n),nrow = n, ncol = n )
  D2_phase_x2 <- matrix(rep(0,n*n),nrow = n, ncol = n )
  for (i in 1:n){
    for (j in 1:n){
      yi<- D$mon.data.nor.df[i,]
      yj<- D$mon.data.nor.df[j,]
      
      # Warping estimates from y_i(t) = y_j (gamma_j,i(t))  
      gamma_ij <- function_composition(yj,yi,t) # gamma_ij = y_i^(-1) o y_j
      
      # Registered curve yi_reg ~= yi (gamma_ij(t))    
      yi_reg = approx(t,yi,gamma_ij,rule=2)$y
      
      D2_amp_x2[i,j] <-  trapz(t, (yj - yi_reg)^2)
      D2_phase_x2[i,j] <- trapz(t, (gamma_ij - t)^2)
      
      # D2_amp_x2[i,j] <-  sum(dt*(yj[1:length(dt)] - yi_reg[1:length(dt)])^2)
      # D2_phase_x2[i,j] <- sum(dt*(gamma_ij[1:length(dt)] - t)^2) 
    }
  }
  
  # Make the distance matrices symmetric
  D2_amp_x2 = (D2_amp_x2 + t(D2_amp_x2))/2
  D2_phase_x2 = (D2_phase_x2 + t(D2_phase_x2))/2
  
  # Make sure the distance matrices' diagonal is equal to zero
  diag(D2_amp_x2) <- 0
  diag(D2_phase_x2) <- 0
  
  # Rescaled squared euclidean dist matrices using geometric variability 
  V_amp_x2 = (1/(2*n^2))*sum(D2_amp_x2)
  V_phase_x2 = (1/(2*n^2))*sum(D2_phase_x2)
  
  D2_amp_x2 = 1/V_amp*D2_amp_x2
  D2_phase_x2 = 1/V_phase*D2_phase_x2

  
  # Squared Distance Matrix using 2 by 2 approximation
  D2_x2 = matrix(rep(0,n*n),nrow = n, ncol = n )
  for (i in 1:n){
    for (j in 1:n){
      D_amp <- D2_amp_x2[i,j]
      D_phase <- D2_phase_x2[i,j]

      D2_x2[i,j] <-(1-omega)*D_amp + omega*D_phase
    }
  }
  time_D2_x2 <- proc.time() - time_start_D2_x2
  
  # \delta_3 ----
  time_start_D2_4RMS <- proc.time()
  D2_4RMS <- RMS4(D2_amp, D2_amp_deriv, D2_phase, D2_phase_deriv)
  tol <- 1e-13 
  D2_4RMS[abs(D2_4RMS) <= tol] <- 0
  
  time_D2_4RMS <- proc.time() - time_start_D2_4RMS
  
  
  
  
  # Times
  time_M <- base_time + base_time_delta_1 + time_D2_M
  time_M <- as.numeric(time_M["elapsed"])
  
  time_Mej <- base_time + base_time_delta_1 + time_derivative_computations + time_D2_Mej
  time_Mej <- as.numeric(time_Mej["elapsed"])
  
  time_Mej_RMS <- base_time + base_time_delta_1 + time_derivative_computations + time_D2_4RMS
  time_Mej_RMS <- as.numeric(time_Mej_RMS["elapsed"])
  
  time_delta_2 <- base_time + time_D2_x2
  time_delta_2 <- as.numeric(time_delta_2["elapsed"])
  
  time_delta_3 <- base_time + base_time_delta_1 + time_derivative_computations + time_D2_M
  time_delta_3 <- as.numeric(time_delta_3["elapsed"])

return(list(D=D,L_med=L_med, L_hat=L_hat,  h_hat = h_hat, h_hat_deriv = h_hat_deriv, x_reg = x_reg, x_reg_deriv = x_reg_deriv, a=mean(D$amp), 
            D2_M=D2_M, D2_Mejorada = D2_Mejorada, D2_Mej_RMS = D2_Mej_RMS, D2_x2 = D2_x2, D2_4RMS = D2_4RMS,
            time_M=time_M, time_Mej=time_Mej, time_Mej_RMS=time_Mej_RMS, time_delta_2=time_delta_2, time_delta_3=time_delta_3))

}
