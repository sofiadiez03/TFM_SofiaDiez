##### Auxiliar functions for clustering in the Latent Deformation Model
source("sim_fun_univ.R")   # for simulation
source("sq_dist.R") # for depth-based estimation main function and distance computation
library(cluster)
library(mclust)     # for computing the Adjusted Rand Index (ARI)
library(fdasrvf)    # for K-means Align Tucker clustering 



# Clustering for 2 groups ----

# Accuracy of a given classification of curves belonging to 2 groups
accuracy.2 <- function(classification_vector, num1){
  n <- length(classification_vector)
  num2 <- n - num1  # number of curves belonging to fun2 group
  
  ## Correct classification
  real_classification <- c(rep(1, num1), rep(2, num2))
  
  tab <- table(real_classification, classification_vector)
  
  accuracy <- sum(diag(tab)) / sum(tab)
  
  # In case the labels are disordered
  accuracy_perm <- sum(diag(tab[, c(2,1)])) / sum(tab)
  
  accuracy <- max(accuracy, accuracy_perm)
  return(accuracy)
}

# KNN accuracy, PAM accuracy and PAM ARI for a set of curves belonging to 2 groups
accuracies_2_groups <- function(D2, num1, k=3, real_labels){
  # D2: Squared distance matrix 
  # num1: number of curves belonging to fun1 group
  # k: number of neighbours in KNN
  n <- nrow(D2)
  
  D <- sqrt(D2)
  Ddist <- as.dist(D) # convert to dist object
  
  # PAM Algorithm Accuracy
  time <- proc.time()
  classification_pam <- pam(Ddist, 2, diss = TRUE)$clustering # we only keep the classification of each curve
  new_time <- proc.time() - time
  PAM_time <- as.numeric(new_time["elapsed"])
  
  accuracy_pam <- accuracy.2(classification_pam, num1)
  
  ### PAM Adjusted Rand Index (ARI) 
  ari_pam <- adjustedRandIndex(classification_pam, real_labels)
  
  # KNN Algorithm Accuracy
  classification_knn <- knn(D, real_labels, k = k)
  accuracy_knn <- accuracy.2(classification_knn, num1)
  
  return(list(PAM_time = PAM_time, accuracy_pam = accuracy_pam, ari_pam = ari_pam, 
              accuracy_knn = accuracy_knn))
}






# Accuracy distribution for the five distances when curves come from 2 groups
acc.distrib.2 <- function(n, m, fun1, fun2, prop, omega, alpha, N_iter, k=3){
  # n: number of curves in each generated set
  # m: size of the grid of points in the interval I = [0,1]
  # fun1: function that will generate the first group of curves
  # fun2: function that will generate the second group of curves 
  # prop: proportion of curves that are generated with fun1
  # omega: tunning parameter. High => more importance to the differences of the warping functions (h_i)
  # alpha: tunning parameter. High => more importance to the differences of X' and h'
  # N_iter: number of generated sets of curves
  # k: number of neighbours in KNN
  
  
  # Data simulation 
  t = seq(0, 1, length.out = m)
  
  # Where we will save the execution time of each generated set of curves 
  time_tucker <- rep(0, N_iter) 
  PAM_M_time <- rep(0, N_iter)
  PAM_x2_time <- rep(0, N_iter)
  PAM_Mej_time <- rep(0, N_iter)
  PAM_Mej_RMS_time <- rep(0, N_iter)
  PAM_4RMS_time <- rep(0, N_iter)
  
  
  # Where we will save the ARI of each generated set of curves for every distance
  # K-means Align Tucker clustering
  tucker_ari <- rep(0, N_iter)
  
  # ARI PAM
  ari_PAM_M <- rep(0, N_iter)
  ari_PAM_x2 <- rep(0, N_iter)
  ari_PAM_Mej <- rep(0, N_iter)
  ari_PAM_Mej_RMS <- rep(0, N_iter)
  ari_PAM_4RMS <- rep(0, N_iter)
  
  
  # KNN
  acc_KNN_M <- rep(0, N_iter)
  acc_KNN_x2 <- rep(0, N_iter)
  acc_KNN_Mej <- rep(0, N_iter)
  acc_KNN_Mej_RMS <- rep(0, N_iter)
  acc_KNN_4RMS <- rep(0, N_iter)
  
  for (i in 1:N_iter){
    # Simulation of the two groups of curves:
    num1 <- round(n*prop) # number of curves belonging to fun1 group
    num2 <- n - num1      # number of curves belonging to fun2 group
    
    simdata = sim_data_2groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                               group_ratio = prop, out=F, out_prop=0.05, fun1, fun2)
    
    # Depth-based estimation
    data = simdata$simdata
    
    #### K-means Align Tucker clustering 
    data_transpose <- t(data) 
    
    time <- proc.time()
    out_srvf <- kmeans_align(
      f = data_transpose,    
      time = t,              
      K = 2, # number of clusters 
      alignment = TRUE,      
      parallel = FALSE,      
      showplot = FALSE       
    )
    new_time <- proc.time() - time
    time_tucker[i] <- as.numeric(new_time["elapsed"])
      
    labels_pred <- out_srvf$labels
    # ARI
    labels_reales <- simdata$groups
    tucker_ari[i] <- adjustedRandIndex(labels_pred, labels_reales)
    
    
    
    
    #### Distance based clustering
    DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)

    # Squared Distance Matrices
    D2_M = DBest$D2_M             # delta_1 squared distance matrix
    D2_x2 = DBest$D2_x2           # delta_2 squared distance matrix
    D2_Mej = DBest$D2_Mejorada    # delta_1 mejorada squared distance matrix
    D2_Mej_RMS = DBest$D2_Mej_RMS # delta_1 mejorada con RMS squared distance matrix
    D2_4RMS = DBest$D2_4RMS       # RMS applied to the squared distance matrices of X, X', h and h'
    
    accuracies_2_groups_M <- accuracies_2_groups(D2_M, num1, k, real_labels = labels_reales)
    accuracies_2_groups_x2 <- accuracies_2_groups(D2_x2, num1, k, real_labels = labels_reales)
    accuracies_2_groups_Mej <- accuracies_2_groups(D2_Mej, num1, k, real_labels = labels_reales)
    accuracies_2_groups_Mej_RMS <- accuracies_2_groups(D2_Mej_RMS, num1, k, real_labels = labels_reales)
    accuracies_2_groups_4RMS <- accuracies_2_groups(D2_4RMS, num1, k, real_labels = labels_reales)
    
    # Execution times
    PAM_M_time[i] = DBest$time_M + accuracies_2_groups_M$PAM_time 
    PAM_x2_time[i] = DBest$time_delta_2 + accuracies_2_groups_x2$PAM_time 
    PAM_Mej_time[i] = DBest$time_Mej + accuracies_2_groups_Mej$PAM_time
    PAM_Mej_RMS_time[i] = DBest$time_Mej_RMS + accuracies_2_groups_Mej_RMS$PAM_time
    PAM_4RMS_time[i] = DBest$time_delta_3 + accuracies_2_groups_4RMS$PAM_time
    
    
    # ARI for PAM
    ari_PAM_M[i] <- accuracies_2_groups_M$ari_pam
    ari_PAM_x2[i] <- accuracies_2_groups_x2$ari_pam
    ari_PAM_Mej[i] <- accuracies_2_groups_Mej$ari_pam
    ari_PAM_Mej_RMS[i] <- accuracies_2_groups_Mej_RMS$ari_pam
    ari_PAM_4RMS[i] <- accuracies_2_groups_4RMS$ari_pam
    
    # KNN
    acc_KNN_M[i] <- accuracies_2_groups_M$accuracy_knn
    acc_KNN_x2[i] <- accuracies_2_groups_x2$accuracy_knn
    acc_KNN_Mej[i] <- accuracies_2_groups_Mej$accuracy_knn
    acc_KNN_Mej_RMS[i] <- accuracies_2_groups_Mej_RMS$accuracy_knn
    acc_KNN_4RMS[i] <- accuracies_2_groups_4RMS$accuracy_knn
  }
  
  return(list(time_tucker=time_tucker, 
              PAM_M_time = PAM_M_time, PAM_x2_time = PAM_x2_time, PAM_Mej_time = PAM_Mej_time, PAM_Mej_RMS_time = PAM_Mej_RMS_time, PAM_4RMS_time = PAM_4RMS_time,
              tucker_ari=tucker_ari, 
              ari_PAM_M=ari_PAM_M, ari_PAM_x2=ari_PAM_x2, ari_PAM_Mej=ari_PAM_Mej, ari_PAM_Mej_RMS=ari_PAM_Mej_RMS, ari_PAM_4RMS=ari_PAM_4RMS,
              acc_KNN_M=acc_KNN_M, acc_KNN_x2=acc_KNN_x2, acc_KNN_Mej=acc_KNN_Mej, acc_KNN_Mej_RMS=acc_KNN_Mej_RMS, acc_KNN_4RMS=acc_KNN_4RMS
              )
)}






# Accuracy distribution for the five distances when curves come from 2 groups with univariate distrib
acc.distrib.univ.distrib.2 <- function(n, m, prop, omega, alpha, N_iter, k=3, lambda = 0.5 ){
  # n: number of curves in each generated set
  # m: size of the grid of points in the interval I = [0,1]
  # prop: proportion of curves that belong to class 1
  # omega: tunning parameter. High => more importance to the differences of the warping functions (h_i)
  # alpha: tunning parameter. High => more importance to the differences of X' and h'
  # N_iter: number of generated sets of curves
  # k: number of neighbours in KNN
  # lambda: global fixing parameter
  
  
  # Data simulation 
  t = seq(0, 1, length.out = m)
  
  # Where we will save the accuracy of each generated set of curves for every distance
  acc_PAM_M <- rep(0, N_iter)
  acc_PAM_x2 <- rep(0, N_iter)
  acc_PAM_Mej <- rep(0, N_iter)
  acc_PAM_Mej_RMS <- rep(0, N_iter)
  acc_PAM_4RMS <- rep(0, N_iter)
  
  acc_KNN_M <- rep(0, N_iter)
  acc_KNN_x2 <- rep(0, N_iter)
  acc_KNN_Mej <- rep(0, N_iter)
  acc_KNN_Mej_RMS <- rep(0, N_iter)
  acc_KNN_4RMS <- rep(0, N_iter)
  
  # Simulation of the two groups of curves:
  num1 <- round(n*prop) # number of curves belonging to fun1 group
  num2 <- n - num1      # number of curves belonging to fun2 group
  
  for (i in 1:N_iter){
    # Density functions generation
    params <- generate_dist_params(num1, num2)
    a <- params$a
    b <- params$b
    c <- params$c
    d <- params$d
    
    x_grid <- seq(0, 1000, length.out = m)
    
    g1 <- matrix(0, num1, length(x_grid))
    g2 <- matrix(0, num1, length(x_grid))
    
    for(j in 1:num1){
      # Class 1
      g1[j, ] <- dunif(x_grid, a[j], b[j])
      # Class 2
      g2[j, ] <- lambda * dunif(x_grid, a[j], b[j]) + (1 - lambda) * dunif(x_grid, c[j], d[j])
    }
    
    data <- rbind(g1, g2)
    
    
    
    # Depth-based estimation
    DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)
    
    
    # Squared Distance Matrices
    D2_M = DBest$D2_M             # delta_1 squared distance matrix
    D2_x2 = DBest$D2_x2           # delta_2 squared distance matrix
    D2_Mej = DBest$D2_Mejorada    # delta_1 mejorada squared distance matrix
    D2_Mej_RMS = DBest$D2_Mej_RMS # delta_1 mejorada con RMS squared distance matrix
    D2_4RMS = DBest$D2_4RMS       # RMS applied to the squared distance matrices of X, X', h and h'
    
    # Accuracy with PAM algorithm of this set of curves for every distance
    labels_reales <- c(rep(1, num1), rep(2, num2))
    accuracies_2_groups_M <- accuracies_2_groups(D2_M, num1, k, real_labels = labels_reales)
    accuracies_2_groups_x2 <- accuracies_2_groups(D2_x2, num1, k, real_labels = labels_reales)
    accuracies_2_groups_Mej <- accuracies_2_groups(D2_Mej, num1, k, real_labels = labels_reales)
    accuracies_2_groups_Mej_RMS <- accuracies_2_groups(D2_Mej_RMS, num1, k, real_labels = labels_reales)
    accuracies_2_groups_4RMS <- accuracies_2_groups(D2_4RMS, num1, k, real_labels = labels_reales)
    
    acc_PAM_M[i] <- accuracies_2_groups_M$accuracy_pam
    acc_PAM_x2[i] <- accuracies_2_groups_x2$accuracy_pam
    acc_PAM_Mej[i] <- accuracies_2_groups_Mej$accuracy_pam
    acc_PAM_Mej_RMS[i] <- accuracies_2_groups_Mej_RMS$accuracy_pam
    acc_PAM_4RMS[i] <- accuracies_2_groups_4RMS$accuracy_pam
    
    acc_KNN_M[i] <- accuracies_2_groups_M$accuracy_knn
    acc_KNN_x2[i] <- accuracies_2_groups_x2$accuracy_knn
    acc_KNN_Mej[i] <- accuracies_2_groups_Mej$accuracy_knn
    acc_KNN_Mej_RMS[i] <- accuracies_2_groups_Mej_RMS$accuracy_knn
    acc_KNN_4RMS[i] <- accuracies_2_groups_4RMS$accuracy_knn
  }
  
  return(list(acc_PAM_M=acc_PAM_M, acc_PAM_x2=acc_PAM_x2, acc_PAM_Mej=acc_PAM_Mej, acc_PAM_Mej_RMS=acc_PAM_Mej_RMS, acc_PAM_4RMS=acc_PAM_4RMS,
              acc_KNN_M=acc_KNN_M, acc_KNN_x2=acc_KNN_x2, acc_KNN_Mej=acc_KNN_Mej, acc_KNN_Mej_RMS=acc_KNN_Mej_RMS, acc_KNN_4RMS=acc_KNN_4RMS))
}



acc.distrib.tecator <- function(data_tecator, endpoints, omega, alpha){
  # data_tector: absorp from tecator
  # endpoints from tecator
  # omega: tunning parameter. High => more importance to the differences of the warping functions (h_i)
  # alpha: tunning parameter. High => more importance to the differences of X' and h'
  n <- nrow(data_tecator)
  m <- ncol(data_tecator)
  
  # Real clusterig labels based on the fat percentage (3 classes: low, medium, high)
  fat_content <- endpoints[, 2]
  
  # Si el % en grasa es menor o igual a 20 se asigna Clase 1, si es mayor se asigna Clase 2
  labels_reales <- ifelse(fat_content <= 20, 1, 2)
  
  t = seq(0, 1, length.out = m)
  num1 <- sum(labels_reales == 1) # number of curves belonging to class 1
  num2 <- sum(labels_reales == 2) # number of curves belonging to class 2
  
  # Distance matrices
  DBest <- sq_dist(data_tecator, t, smooth = FALSE, omega = omega, alpha = alpha)
  D2_M = DBest$D2_M
  D2_x2 = DBest$D2_x2
  D2_Mej = DBest$D2_Mejorada
  D2_Mej_RMS = DBest$D2_Mej_RMS
  D2_4RMS = DBest$D2_4RMS
  
  
  # K-means Align Tucker clustering 
  data_transpose <- t(data_tecator) 
  
  time <- proc.time()
  out_srvf <- kmeans_align(
    f = data_transpose,    
    time = t,              
    K = 2, 
    alignment = TRUE,      
    parallel = FALSE,      
    showplot = FALSE       
  )
  new_time <- proc.time() - time
  time_tucker <- as.numeric(new_time["elapsed"])
  
  labels_pred <- out_srvf$labels
  tucker_ari <- adjustedRandIndex(labels_pred, labels_reales)
  
  
  # Clustering with our distances
  accuracies_M <- accuracies_2_groups(D2_M, num1, real_labels = labels_reales)
  accuracies_x2 <- accuracies_2_groups(D2_x2, num1, real_labels = labels_reales)
  accuracies_Mej <- accuracies_2_groups(D2_Mej, num1, real_labels = labels_reales)
  accuracies_Mej_RMS <- accuracies_2_groups(D2_Mej_RMS, num1, real_labels = labels_reales)
  accuracies_4RMS <- accuracies_2_groups(D2_4RMS, num1, real_labels = labels_reales)
  
  ## Execution Times
  PAM_M_time = DBest$time_M + accuracies_M$PAM_time 
  PAM_x2_time = DBest$time_delta_2 + accuracies_x2$PAM_time 
  PAM_Mej_time = DBest$time_Mej + accuracies_Mej$PAM_time
  PAM_Mej_RMS_time = DBest$time_Mej_RMS + accuracies_Mej_RMS$PAM_time
  PAM_4RMS_time = DBest$time_delta_3 + accuracies_4RMS$PAM_time
  
  ## ARI for PAM
  ari_PAM_M <- accuracies_M$ari_pam
  ari_PAM_x2 <- accuracies_x2$ari_pam
  ari_PAM_Mej <- accuracies_Mej$ari_pam
  ari_PAM_Mej_RMS <- accuracies_Mej_RMS$ari_pam
  ari_PAM_4RMS <- accuracies_4RMS$ari_pam
  
  
  return(list(time_tucker=time_tucker, 
              PAM_M_time = PAM_M_time, PAM_x2_time = PAM_x2_time, PAM_Mej_time = PAM_Mej_time, PAM_Mej_RMS_time = PAM_Mej_RMS_time, PAM_4RMS_time = PAM_4RMS_time,
              tucker_ari=tucker_ari,
              ari_PAM_M=ari_PAM_M, ari_PAM_x2=ari_PAM_x2, ari_PAM_Mej=ari_PAM_Mej, ari_PAM_Mej_RMS=ari_PAM_Mej_RMS, ari_PAM_4RMS=ari_PAM_4RMS))
}







# Clustering for 3 groups ----

# Function to calculate the accuracy of a given classification of curves belonging to 3 groups
accuracy.3 <- function(classification_vector, num1, num2){
  n <- length(classification_vector)
  num3 <- n - (num1 + num2)  # number of curves belonging to fun3 group
  
  ## Correct classification
  real_classification <- c(rep(1, num1), rep(2, num2), rep(3, num3))
  
  tab <- table(real_classification, classification_vector)
  
  accuracy <- sum(diag(tab)) / sum(tab)
  
  # In case the labels are disordered
  permutations <- matrix(data=c(1,2,3,1,3,2,2,1,3,2,3,1,3,1,2,3,2,1), ncol = 3, byrow = T)
  
  accuracies <- apply(permutations, 1, function(p){
    sum(diag(tab[, p])) / sum(tab)
  })
  
  accuracy <- max(accuracies)
  return(accuracy)
}




# KNN accuracy, PAM accuracy and PAM ARI for a set of curves belonging to 3 groups
accuracies_3_groups <- function(D2, num1, num2, k=3, real_labels){
  # D2: Squared distance matrix 
  # num1: number of curves belonging to fun1 group 
  # num2: number of curves belonging to fun2 group
  # k: number of neighbours in KNN
  n <- nrow(D2)
  
  D <- sqrt(D2)
  Ddist <- as.dist(D) # convert to dist object
  
  # PAM Algorithm Accuracy
  time <- proc.time()
  classification_pam <- pam(Ddist, 3, diss = TRUE)$clustering # we only keep the classification of each curve
  new_time <- proc.time() - time
  PAM_time <- as.numeric(new_time["elapsed"])
  
  accuracy_pam <- accuracy.3(classification_pam, num1, num2)
  
  ### PAM Adjusted Rand Index (ARI) 
  ari_pam <- adjustedRandIndex(classification_pam, real_labels)
  
  # KNN Algorithm Accuracy
  classification_knn <- knn(D, real_labels, k = k)
  accuracy_knn <- accuracy.3(classification_knn, num1, num2)

  return(list(PAM_time = PAM_time, accuracy_pam = accuracy_pam, ari_pam = ari_pam, 
              accuracy_knn = accuracy_knn))
}






  





# Accuracy distribution for the five distances when curves come from 3 groups
acc.distrib.3 <- function(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter, k=3){
  # n: number of curves in each generated set
  # m: size of the grid of points in the interval I = [0,1]
  # fun1: function that will generate the first group of curves
  # fun2: function that will generate the second group of curves 
  # fun3: function that will generate the third group of curves 
  # prop1: proportion of curves that are generated with fun1
  # prop2: proportion of curves that are generated with fun2
  # omega: tunning parameter. High => more importance to the differences of the warping functions (h_i)
  # alpha: tunning parameter. High => more importance to the differences of X' and h'
  # N_iter: number of generated sets of curves
  # k: Numver og neighbours in KNN
  
  
  # Data simulation 
  t = seq(0, 1, length.out = m)
  
  # Where we will save the execution time of each generated set of curves 
  time_tucker <- rep(0, N_iter) 
  PAM_M_time <- rep(0, N_iter)
  PAM_x2_time <- rep(0, N_iter)
  PAM_Mej_time <- rep(0, N_iter)
  PAM_Mej_RMS_time <- rep(0, N_iter)
  PAM_4RMS_time <- rep(0, N_iter)
  
  # Where we will save the accuracy of each generated set of curves for every distance
  # K-means Align Tucker clustering 
  tucker_ari <- rep(0, N_iter)
  
  # ARI PAM
  ari_PAM_M <- rep(0, N_iter)
  ari_PAM_x2 <- rep(0, N_iter)
  ari_PAM_Mej <- rep(0, N_iter)
  ari_PAM_Mej_RMS <- rep(0, N_iter)
  ari_PAM_4RMS <- rep(0, N_iter)
  
  
  # KNN
  acc_KNN_M <- rep(0, N_iter)
  acc_KNN_x2 <- rep(0, N_iter)
  acc_KNN_Mej <- rep(0, N_iter)
  acc_KNN_Mej_RMS <- rep(0, N_iter)
  acc_KNN_4RMS <- rep(0, N_iter)
  
  for (i in 1:N_iter){
    # Simulation of the three groups of curves:
    num1 <- round(n*prop1)    # number of curves belonging to fun1 group
    num2 <- round(n*prop2)    # number of curves belonging to fun2 group
    num3 <- n - (num1 + num2) # number of curves belonging to fun3 group

    simdata = sim_data_3groups(n, sigma_amp=4, sigma_warp=1, sigma_error=0, sigma_dist=0, amean=100, t, m,
                               group_ratio = c(prop1, prop2), out=F, out_prop=0.05, fun1, fun2, fun3)
    
    # Depth-based estimation
    data = simdata$simdata
    
    #### K-means Align Tucker clustering 
    data_transpose <- t(data) 
    
    time <- proc.time()
    out_srvf <- kmeans_align(
      f = data_transpose,    
      time = t,              
      K = 3, # number of clusters 
      alignment = TRUE,      
      parallel = FALSE,      
      showplot = FALSE       
    )
    new_time <- proc.time() - time
    time_tucker[i] <- as.numeric(new_time["elapsed"])
    
    labels_pred <- out_srvf$labels
    # ARI
    labels_reales <- simdata$groups
    tucker_ari[i] <- adjustedRandIndex(labels_pred, labels_reales)
    
    
    
    #### Distance based clustering
    DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)
    
    # Squared Distance Matrices
    D2_M = DBest$D2_M             # delta_1 squared distance matrix
    D2_x2 = DBest$D2_x2           # delta_2 squared distance matrix
    D2_Mej = DBest$D2_Mejorada    # delta_1 mejorada squared distance matrix
    D2_Mej_RMS = DBest$D2_Mej_RMS # delta_1 mejorada con RMS squared distance matrix
    D2_4RMS = DBest$D2_4RMS       # RMS applied to the squared distance matrices of X, X', h and h'
    
    accuracies_3_groups_M <- accuracies_3_groups(D2_M, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_x2 <- accuracies_3_groups(D2_x2, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_Mej <- accuracies_3_groups(D2_Mej, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_Mej_RMS <- accuracies_3_groups(D2_Mej_RMS, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_4RMS <- accuracies_3_groups(D2_4RMS, num1, num2, k, real_labels = labels_reales)
    
    # Execution times
    PAM_M_time[i] = DBest$time_M + accuracies_3_groups_M$PAM_time 
    PAM_x2_time[i] = DBest$time_delta_2 + accuracies_3_groups_x2$PAM_time 
    PAM_Mej_time[i] = DBest$time_Mej + accuracies_3_groups_Mej$PAM_time
    PAM_Mej_RMS_time[i] = DBest$time_Mej_RMS + accuracies_3_groups_Mej_RMS$PAM_time
    PAM_4RMS_time[i] = DBest$time_delta_3 + accuracies_3_groups_4RMS$PAM_time
    
    # ARI for PAM
    ari_PAM_M[i] <- accuracies_3_groups_M$ari_pam
    ari_PAM_x2[i] <- accuracies_3_groups_x2$ari_pam
    ari_PAM_Mej[i] <- accuracies_3_groups_Mej$ari_pam
    ari_PAM_Mej_RMS[i] <- accuracies_3_groups_Mej_RMS$ari_pam
    ari_PAM_4RMS[i] <- accuracies_3_groups_4RMS$ari_pam
    
    # KNN
    acc_KNN_M[i] <- accuracies_3_groups_M$accuracy_knn
    acc_KNN_x2[i] <- accuracies_3_groups_x2$accuracy_knn
    acc_KNN_Mej[i] <- accuracies_3_groups_Mej$accuracy_knn
    acc_KNN_Mej_RMS[i] <- accuracies_3_groups_Mej_RMS$accuracy_knn
    acc_KNN_4RMS[i] <- accuracies_3_groups_4RMS$accuracy_knn
  }
  
  return(list(time_tucker=time_tucker, 
              PAM_M_time = PAM_M_time, PAM_x2_time = PAM_x2_time, PAM_Mej_time = PAM_Mej_time, PAM_Mej_RMS_time = PAM_Mej_RMS_time, PAM_4RMS_time = PAM_4RMS_time,
              tucker_ari=tucker_ari, 
              ari_PAM_M=ari_PAM_M, ari_PAM_x2=ari_PAM_x2, ari_PAM_Mej=ari_PAM_Mej, ari_PAM_Mej_RMS=ari_PAM_Mej_RMS, ari_PAM_4RMS=ari_PAM_4RMS,
              acc_KNN_M=acc_KNN_M, acc_KNN_x2=acc_KNN_x2, acc_KNN_Mej=acc_KNN_Mej, acc_KNN_Mej_RMS=acc_KNN_Mej_RMS, acc_KNN_4RMS=acc_KNN_4RMS
  )
  )}








# Accuracy distribution for the five distances when curves come from 3 groups where the area is the same
acc.distrib.same_area.3 <- function(n, m, k1, k2, k3, s1, s2, s3, tau, prop1, prop2, omega, alpha, N_iter, k=3){
  # n: number of curves in each generated set
  # m: size of the grid of points in the interval I = [0,1]
  # ki: number of modes in the group i curves
  # si: standard deviation of the group i curves
  # tau: amplitud variability
  # prop1: proportion of curves in group 1 
  # prop2: proportion of curves in group 2 
  # omega: tunning parameter. High => more importance to the differences of the warping functions (h_i)
  # alpha: tunning parameter. High => more importance to the differences of X' and h'
  # N_iter: number of generated sets of curves
  # k: number of neighbours in KNN
  
  
  # Data simulation 
  t = seq(0, 1, length.out = m)
  
  # Where we will save the accuracy of each generated set of curves for every distance
  acc_PAM_M <- rep(0, N_iter)
  acc_PAM_x2 <- rep(0, N_iter)
  acc_PAM_Mej <- rep(0, N_iter)
  acc_PAM_Mej_RMS <- rep(0, N_iter)
  acc_PAM_4RMS <- rep(0, N_iter)
  
  acc_KNN_M <- rep(0, N_iter)
  acc_KNN_x2 <- rep(0, N_iter)
  acc_KNN_Mej <- rep(0, N_iter)
  acc_KNN_Mej_RMS <- rep(0, N_iter)
  acc_KNN_4RMS <- rep(0, N_iter)
  
  for (i in 1:N_iter){
    # Simulation of the three groups of curves:
    num1 <- round(n*prop1)    # number of curves belonging to fun1 group
    num2 <- round(n*prop2)    # number of curves belonging to fun2 group
    num3 <- n - (num1 + num2) # number of curves belonging to fun3 group
    
    labels_reales <- as.factor(c(rep(1,num1),rep(2,num2),rep(3, n - (num1 + num2))))
    
    f1 <- f(num1,t,k=k1,s1,tau)
    f2 <- f(num2,t,k=k2,s2,tau)
    f3 <- f(num3,t,k=k3,s3,tau)
    
    
    # Depth-based estimation
    data <- rbind(f1, f2, f3)
    DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)
    
    
    
    # Squared Distance Matrices
    D2_M = DBest$D2_M             # delta_1 squared distance matrix
    D2_x2 = DBest$D2_x2           # delta_2 squared distance matrix
    D2_Mej = DBest$D2_Mejorada    # delta_1 mejorada squared distance matrix
    D2_Mej_RMS = DBest$D2_Mej_RMS # delta_1 mejorada con RMS squared distance matrix
    D2_4RMS = DBest$D2_4RMS       # RMS applied to the squared distance matrices of X, X', h and h'
    
    # Accuracy of this set of curves for every distance
    accuracies_3_groups_M <- accuracies_3_groups(D2_M, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_x2 <- accuracies_3_groups(D2_x2, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_Mej <- accuracies_3_groups(D2_Mej, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_Mej_RMS <- accuracies_3_groups(D2_Mej_RMS, num1, num2, k, real_labels = labels_reales)
    accuracies_3_groups_4RMS <- accuracies_3_groups(D2_4RMS, num1, num2, k, real_labels = labels_reales)
    
    acc_PAM_M[i] <- accuracies_3_groups_M$accuracy_pam
    acc_PAM_x2[i] <- accuracies_3_groups_x2$accuracy_pam
    acc_PAM_Mej[i] <- accuracies_3_groups_Mej$accuracy_pam
    acc_PAM_Mej_RMS[i] <- accuracies_3_groups_Mej_RMS$accuracy_pam
    acc_PAM_4RMS[i] <- accuracies_3_groups_4RMS$accuracy_pam
    
    acc_KNN_M[i] <- accuracies_3_groups_M$accuracy_knn
    acc_KNN_x2[i] <- accuracies_3_groups_x2$accuracy_knn
    acc_KNN_Mej[i] <- accuracies_3_groups_Mej$accuracy_knn
    acc_KNN_Mej_RMS[i] <- accuracies_3_groups_Mej_RMS$accuracy_knn
    acc_KNN_4RMS[i] <- accuracies_3_groups_4RMS$accuracy_knn
    
  }
  
  return(list(acc_PAM_M=acc_PAM_M, acc_PAM_x2=acc_PAM_x2, acc_PAM_Mej=acc_PAM_Mej, acc_PAM_Mej_RMS=acc_PAM_Mej_RMS, acc_PAM_4RMS=acc_PAM_4RMS,
              acc_KNN_M=acc_KNN_M, acc_KNN_x2=acc_KNN_x2, acc_KNN_Mej=acc_KNN_Mej, acc_KNN_Mej_RMS=acc_KNN_Mej_RMS, acc_KNN_4RMS=acc_KNN_4RMS))
}




# Accuracy distribution for the curves in Qiao Simulation 1 
acc.distrib.qiao.3 <- function(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter){
  # n: number of curves in each generated set
  # m: size of the grid of points in the interval I = [0,1]
  # fun1: function that will generate the first group of curves
  # fun2: function that will generate the second group of curves 
  # fun3: function that will generate the third group of curves 
  # prop1: proportion of curves that are generated with fun1
  # prop2: proportion of curves that are generated with fun2
  # omega: tunning parameter. High => more importance to the differences of the warping functions (h_i)
  # alpha: tunning parameter. High => more importance to the differences of X' and h'
  # N_iter: number of generated sets of curves
  
  
  # For data simulation 
  t = seq(0, 1, length.out = m)
  num1 <- round(n*prop1)    # number of curves belonging to fun1 group
  num2 <- round(n*prop2)    # number of curves belonging to fun2 group
  num3 <- n - (num1 + num2) # number of curves belonging to fun3 group
  
  #amplitude noise for curves
  tau<-.1
  
  # Where we will save the execution time of each generated set of curves 
  time_tucker <- rep(0, N_iter) 
  PAM_M_time <- rep(0, N_iter)
  PAM_x2_time <- rep(0, N_iter)
  PAM_Mej_time <- rep(0, N_iter)
  PAM_Mej_RMS_time <- rep(0, N_iter)
  PAM_4RMS_time <- rep(0, N_iter)
  
  # Where we will save the accuracy of each generated set of curves for every distance
  # K-means Align Tucker clustering 
  tucker_ari <- rep(0, N_iter)
  
  # ARI PAM
  ari_PAM_M <- rep(0, N_iter)
  ari_PAM_x2 <- rep(0, N_iter)
  ari_PAM_Mej <- rep(0, N_iter)
  ari_PAM_Mej_RMS <- rep(0, N_iter)
  ari_PAM_4RMS <- rep(0, N_iter)
  
  
  for (i in 1:N_iter){
    # Simulation of the three groups of curves:
    z1<-rnorm(n,mean=1,sd=tau)
    z2<-rnorm(n,mean=1,sd=tau)
    z3<-rnorm(n,mean=1,sd=tau)
    z4<-rnorm(n,mean=1,sd=tau)
    z5<-rnorm(n,mean=1,sd=tau)
    
    # warping function matrix
    gamma<-matrix(nrow=n,ncol=length(t))
    for (k in 1:n){
      alph=runif(1,-3,3)
      if(alph!= 0){
        gamma[k,] = (exp(alph*t)-1)/(exp(alph)-1)
      }else{
        gamma[k,] = t
      }    
    }
    
    # curves
    data<-matrix(nrow=n,ncol=length(t))
    
    for (k in 1:num1) for (j in 1:length(t)) {data[k,j]<-z1[k]*exp(-(1/(2*(1/9)^2))*(gamma[k,j]-(1/6))^2)+z2[k]*exp(-(1/(2*(1/9)^2))*(gamma[k,j]-(3/6))^2)+z3[k]*exp(-(1/(2*(1/9)^2))*(gamma[k,j]-(5/6))^2)}
    
    for (k in (num1+1):(num1 + num2)) for (j in 1:length(t)) {data[k,j]<-z1[k]*exp(-(1/(2*(1/12)^2))*(gamma[k,j]-(1/8))^2)+z2[k]*exp(-(1/(2*(1/12)^2))*(gamma[k,j]-(3/8))^2)+z3[k]*exp(-(1/(2*(1/12)^2))*(gamma[k,j]-(5/8))^2)+z4[k]*exp(-(1/(2*(1/12)^2))*(gamma[k,j]-(7/8))^2)}
    
    for (k in (num1 + num2 + 1):n) for (j in 1:length(t)) {data[k,j]<-z1[k]*exp(-(1/(2*(1/16)^2))*(gamma[k,j]-(1/10))^2)+z2[k]*exp(-(1/(2*(1/16)^2))*(gamma[k,j]-(3/10))^2)+z3[k]*exp(-(1/(2*(1/16)^2))*(gamma[k,j]-(5/10))^2)+z4[k]*exp(-(1/(2*(1/16)^2))*(gamma[k,j]-(7/10))^2)+z5[k]*exp(-(1/(2*(1/16)^2))*(gamma[k,j]-(9/10))^2)}
    
    
    #### K-means Align Tucker clustering 
    data_transpose <- t(data) 
    
    time <- proc.time()
    out_srvf <- kmeans_align(
      f = data_transpose,    
      time = t,              
      K = 3, # number of clusters 
      alignment = TRUE,      
      parallel = FALSE,      
      showplot = FALSE       
    )
    
    # sample <- sample(1:n, 3)
    # testKMAFDA <- kmeans_align(
    #   f = data_transpose,
    #   time = t,
    #   K = 3,
    #   seeds = c(sample),
    #   nonempty = 0,
    #   lambda = 0,
    #   showplot = FALSE,
    #   smooth_data = FALSE,
    #   sparam = 25,
    #   parallel = FALSE,
    #   alignment = TRUE,
    #   omethod = "DP",
    #   MaxItr = 50,
    #   thresh = 0.01
    # )
    new_time <- proc.time() - time
    time_tucker[i] <- as.numeric(new_time["elapsed"])
    
    labels_pred <- out_srvf$labels
    
    # ARI
    labels_reales <- c(rep(1, num1), rep(2, num2), rep(3, num3))
    tucker_ari[i] <- adjustedRandIndex(labels_pred, labels_reales)
    
    
    
    #### Distance based clustering
    DBest <- sq_dist(data, t, smooth=F, omega = omega, alpha = alpha)
    
    # Squared Distance Matrices
    D2_M = DBest$D2_M             # delta_1 squared distance matrix
    D2_x2 = DBest$D2_x2           # delta_2 squared distance matrix
    D2_Mej = DBest$D2_Mejorada    # delta_1 mejorada squared distance matrix
    D2_Mej_RMS = DBest$D2_Mej_RMS # delta_1 mejorada con RMS squared distance matrix
    D2_4RMS = DBest$D2_4RMS       # RMS applied to the squared distance matrices of X, X', h and h'
    
    accuracies_3_groups_M <- accuracies_3_groups(D2_M, num1, num2, real_labels = labels_reales)
    accuracies_3_groups_x2 <- accuracies_3_groups(D2_x2, num1, num2, real_labels = labels_reales)
    accuracies_3_groups_Mej <- accuracies_3_groups(D2_Mej, num1, num2, real_labels = labels_reales)
    accuracies_3_groups_Mej_RMS <- accuracies_3_groups(D2_Mej_RMS, num1, num2, real_labels = labels_reales)
    accuracies_3_groups_4RMS <- accuracies_3_groups(D2_4RMS, num1, num2, real_labels = labels_reales)
    
    # Execution times
    PAM_M_time[i] = DBest$time_M + accuracies_3_groups_M$PAM_time 
    PAM_x2_time[i] = DBest$time_delta_2 + accuracies_3_groups_x2$PAM_time 
    PAM_Mej_time[i] = DBest$time_Mej + accuracies_3_groups_Mej$PAM_time
    PAM_Mej_RMS_time[i] = DBest$time_Mej_RMS + accuracies_3_groups_Mej_RMS$PAM_time
    PAM_4RMS_time[i] = DBest$time_delta_3 + accuracies_3_groups_4RMS$PAM_time
    
    
    # ARI for PAM
    ari_PAM_M[i] <- accuracies_3_groups_M$ari_pam
    ari_PAM_x2[i] <- accuracies_3_groups_x2$ari_pam
    ari_PAM_Mej[i] <- accuracies_3_groups_Mej$ari_pam
    ari_PAM_Mej_RMS[i] <- accuracies_3_groups_Mej_RMS$ari_pam
    ari_PAM_4RMS[i] <- accuracies_3_groups_4RMS$ari_pam
  }
  
  return(list(time_tucker=time_tucker, 
              PAM_M_time = PAM_M_time, PAM_x2_time = PAM_x2_time, PAM_Mej_time = PAM_Mej_time, PAM_Mej_RMS_time = PAM_Mej_RMS_time, PAM_4RMS_time = PAM_4RMS_time,
              tucker_ari=tucker_ari,
              ari_PAM_M=ari_PAM_M, ari_PAM_x2=ari_PAM_x2, ari_PAM_Mej=ari_PAM_Mej, ari_PAM_Mej_RMS=ari_PAM_Mej_RMS, ari_PAM_4RMS=ari_PAM_4RMS))
}








# Auxiliar functions ----

# KNN Algorithm 
knn <- function(dist_mat, labels, k = 3) {
  n <- nrow(dist_mat)
  predicciones <- character(n)
  
  for (i in 1:n) {
    distancias_i <- dist_mat[i, ]
    
    indices_vecinos <- order(distancias_i)[2:(k + 1)]
    votos_vecinos <- labels[indices_vecinos]
    
    predicciones[i] <- names(which.max(table(votos_vecinos)))
  }
  return(as.factor(predicciones))
}






# Function that generates n curves with k modes with area = 1
f <- function(n,t,k,sigma,tau){
  
  z <- matrix(rnorm(n*k,1,tau), nrow=n)
  f <- matrix(0,nrow=n,ncol=length(t))
  
  for (j in 1:k){
    mu = (2*j-1)/(2*k)
    
    for (i in 1:n){
      curva_i <-  f[i,] + z[i,j]*dnorm(t, mean = mu, sd = sigma)
      area<- sum(curva_i) * (t[2] - t[1]) # aprox de la integral
      f[i, ] <- curva_i/area
      
    }  
    
  }
  
  return(f)
}





# Functions that generate curves with the univariate distirbution
# Base distribution 
generate_dist_params <- function(n1, n2) {
  # Lambdas 
  lambda_vals <- runif(4, 0, 4)
  
  a <- numeric(n1)
  b <- numeric(n1)
  c <- numeric(n2)
  d <- numeric(n2)
  
  for (i in 1:n1){
    a[i] <- 4*(i-1) + lambda_vals[1]
    b[i] <- 195 + 5*i + lambda_vals[2]
  }
  
  for (i in 1:n2){
    c[i] <- 805 - 5*i - lambda_vals[3]
    d[i] <- 1004 - 4*i - lambda_vals[4]
  }
  
  return(list(a=a,b=b,c=c,d=d)) # a = a_1j, ...., a_n1j, ...
}
