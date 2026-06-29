rm(list = ls())

source("R/sim_fun_univ.R")   # for simulation
source("R/sq_dist.R") # for depth-based estimation main function and distance computation



# Parameters to tune for the curves generation:
n = 100 # number of curves
m = 101 # grid size in I = [0,1]
t = seq(0, 1, length.out = m)

# L1 Vs L2 ----
fun1 = L1
fun2 = L2


## 25% Vs 75% ----
prop = 0.25 # proportion of curves that belong to L1 group 


# Data simulation
num1 <- round(n*prop) # number of curves belonging to fun1 group
num2 <- n - num1      # number of curves belonging to fun2 group

simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))



# Derivative curves
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

paste0("omega_",omega*100,"_alpha_", alpha*100)

# delta_1
visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))

# delta_1 mejorada
visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))

# delta_1 mejorada RMS
visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2
visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))

# delta_RMS
visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))






### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada
visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS
visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2
visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS
visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada
visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS
visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2
visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS
visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))










## 50% Vs 50% ----
prop = 0.5 # proportion of curves that belong to L1 group 


# Data simulation
num1 <- round(n*prop) # number of curves belonging to fun1 group
num2 <- n - num1      # number of curves belonging to fun2 group

simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))



# Derivative curves
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada
visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))

# delta_1 mejorada RMS
visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2
visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS
visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))







### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada

visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada
visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS
visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2
visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS
visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))




### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada
visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS
visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2
visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS
visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))










## 75% Vs 25% ----
prop = 0.75 # proportion of curves that belong to L1 group 


# Data simulation
num1 <- round(n*prop) # number of curves belonging to fun1 group
num2 <- n - num1      # number of curves belonging to fun2 group

simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
name <- paste0("L1_", num1, "_vs_L2_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))



# Derivative curves
name <- paste0("derivatives_L1_", num1, "_vs_L2_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

paste0("omega_",omega*100,"_alpha_", alpha*100)

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))







### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))













# L2 Vs L3 ----
fun1 = L2
fun2 = L3


## 25% Vs 75% ----
prop = 0.25 # proportion of curves that belong to L1 group 


# Data simulation
num1 <- round(n*prop) # number of curves belonging to fun1 group
num2 <- n - num1      # number of curves belonging to fun2 group

simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
name <- paste0("L2_", num1, "_vs_L3_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))



# Derivative curves
name <- paste0("derivatives_L2_", num1, "_vs_L3_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

paste0("omega_",omega*100,"_alpha_", alpha*100)

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))







### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))










## 50% Vs 50% ----
prop = 0.5 # proportion of curves that belong to L1 group 


# Data simulation
num1 <- round(n*prop) # number of curves belonging to fun1 group
num2 <- n - num1      # number of curves belonging to fun2 group

simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
name <- paste0("L2_", num1, "_vs_L3_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))



# Derivative curves
name <- paste0("derivatives_L2_", num1, "_vs_L3_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

paste0("omega_",omega*100,"_alpha_", alpha*100)

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))







### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))










## 75% Vs 25% ----
prop = 0.75 # proportion of curves that belong to L1 group 


# Data simulation
num1 <- round(n*prop) # number of curves belonging to fun1 group
num2 <- n - num1      # number of curves belonging to fun2 group

simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
name <- paste0("L2_", num1, "_vs_L3_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))



# Derivative curves
name <- paste0("derivatives_L2_", num1, "_vs_L3_",num2, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

paste0("omega_",omega*100,"_alpha_", alpha*100)

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))







### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1),rep(2,num2)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1),rep(2,num2)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1),rep(2,num2)))












# L1 Vs L2 Vs L4 ----
fun1 = L1
fun2 = L2
fun3 = L4

# Parameters to tune for the curves generation:
n = 150 # number of curves
m = 101 # grid size in I = [0,1]
t = seq(0, 1, length.out = m)


## 33% Vs 33% Vs 33% ----
prop1 = 1/3 # proportion of curves that belong to L1 group 
prop2 = 1/3 # proportion of curves that belong to L2 group 


# Data simulation
num1 <- round(n*prop1)
num2 <- round(n*prop2)
num3 <- n - (num1 + num2)


simdata = sim_data_3groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = c(prop1, prop2), out=F, out_prop=0.05, fun1, fun2, fun3)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
name <- paste0("L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes



# Derivative curves
name <- paste0("derivatives_L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes 






### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

# Los puntos azules perteneces a L1, los rosas a L2 y los verdes a L4

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))










## 60% Vs 20% Vs 20% ----
prop1 = 0.6 # proportion of curves that belong to L1 group 
prop2 = 0.2 # proportion of curves that belong to L2 group 


# Data simulation
num1 <- round(n*prop1)
num2 <- round(n*prop2)
num3 <- n - (num1 + num2)


simdata = sim_data_3groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = c(prop1, prop2), out=F, out_prop=0.05, fun1, fun2, fun3)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Original curves
name <- paste0("L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes



# Derivative curves
name <- paste0("derivatives_L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes 






### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

# Los puntos azules perteneces a L1, los rosas a L2 y los verdes a L4

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))









## 20% Vs 60% Vs 20% ----
prop1 = 0.2 # proportion of curves that belong to L1 group 
prop2 = 0.6 # proportion of curves that belong to L2 group 


# Data simulation
num1 <- round(n*prop1)
num2 <- round(n*prop2)
num3 <- n - (num1 + num2)


simdata = sim_data_3groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = c(prop1, prop2), out=F, out_prop=0.05, fun1, fun2, fun3)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)





# Original curves
name <- paste0("L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes



# Derivative curves
name <- paste0("derivatives_L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes 






### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

# Los puntos azules perteneces a L1, los rosas a L2 y los verdes a L4

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))








## 20% Vs 20% Vs 60% ----
prop1 = 0.2 # proportion of curves that belong to L1 group 
prop2 = 0.2 # proportion of curves that belong to L2 group 


# Data simulation
num1 <- round(n*prop1)
num2 <- round(n*prop2)
num3 <- n - (num1 + num2)


simdata = sim_data_3groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                           group_ratio = c(prop1, prop2), out=F, out_prop=0.05, fun1, fun2, fun3)
data = simdata$simdata

# Depth-based estimation
omega = 0.1
alpha = 0.1

DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)





# Original curves
name <- paste0("L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,t(data),type="l",lty=1, main ="Observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes



# Derivative curves
name <- paste0("derivatives_L1_", num1, "_vs_L2_",num2,"_vs_L4_",num3, ".png")
png(name, width = 950, height = 400, res = 100)
par(mfrow=c(1,3))
matplot(t,apply(data, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the observed curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("deeppink", num2), rep("green", num3)))
legend("topleft", 
       legend = c("L1", "L2", "L4"), 
       col = c("blue", "deeppink", "green"), 
       lty = 1, 
       lwd = 2,      # Grosor de línea de la leyenda un poco más fino
       cex = 0.7,    # Tamaño pequeño
       bty = "n",    # Sin recuadro para que sea menos invasiva
       inset = c(0.1, 0.05)) # Pequeño margen desde el borde para que no pegue a los ejes 






### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1

# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS

# Los puntos azules perteneces a L1, los rosas a L2 y los verdes a L4

# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))








### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrices
D2_M = DBest$D2_M
D2_x2 = DBest$D2_x2
D2_Mej = DBest$D2_Mejorada
D2_Mej_RMS = DBest$D2_Mej_RMS
D2_4RMS = DBest$D2_4RMS


# delta_1
name <- paste0("d1_MDS_omega_",omega*100,".png")

visualize_distances(make_euclidean(D2_M,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada


visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_1 mejorada RMS


visualize_distances(make_euclidean(D2_Mej_RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej_RMS with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_2


visualize_distances(make_euclidean(D2_x2,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_2 with omega = ",omega),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


# delta_RMS


visualize_distances(make_euclidean(D2_4RMS,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_RMS"),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))









### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9



# Depth-based estimation
DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)


# Squared Distance Matrix
D2_Mej = DBest$D2_Mejorada



visualize_distances(make_euclidean(D2_Mej,w = rep(1, n))$D_euc, method = "mds_classic", k = 3,
                    main_title = paste0("MDS for delta_1_Mej with omega = ",omega," and alpha = ", alpha),
                    group=c(rep(1,num1), rep(2, num2), rep(3,num3)))


