rm(list = ls())
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/Base_Codes")
source("Clustering.R") 
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_3_Tecator")

library(caret)
data(tecator)


# absorp: matriz con las curvas espectrales
# endpoints: Moisture (Col 1), Fat (Col 2), Protein (Col 3)
data_tecator <- absorp  


# Visualization ----
n <- nrow(data_tecator)
m <- ncol(data_tecator)

fat_content <- endpoints[, 2]

# Si el % en grasa es menor o igual a 20 se asigna Clase 1, si es mayor se asigna Clase 2
labels_reales <- ifelse(fat_content <= 20, 1, 2)


t = seq(0, 1, length.out = m)
num1 <- sum(labels_reales == 1) # number of curves belonging to class 1
num2 <- sum(labels_reales == 2) # number of curves belonging to class 2
colores_grupos <- ifelse(labels_reales == 1, "blue", "red")



##  2 grupos por separado ----
par(mfrow=c(1,2)) 
matplot(t, t(data_tecator[labels_reales == 1, ]), type="l", col="blue", lty=1,
        main="Bajo en Grasa (<=20%)", xlab="t", ylab="Absorbancia", ylim=range(data_tecator))
matplot(t, t(data_tecator[labels_reales == 2, ]), type="l", col="red", lty=1,
        main="Alto en Grasa (>20%)", xlab="t", ylab="Absorbancia", ylim=range(data_tecator))
dev.off()



##  Depth-based estimation ----
omega = 0.5
alpha = 0.5

DBest <- sq_dist(data_tecator, t, smooth=F, omega = omega, alpha = alpha)

par(mfrow=c(1,3))
matplot(t,t(data_tecator),type="l",lty=1, main ="Tecator curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2)))
matplot(t,t(DBest$x_reg),type="l",lty=1, main ="Registered curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat) ,type="l",lty=1, main ="Warping function estimates", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
dev.off()

# Derivatives
par(mfrow=c(1,3))
matplot(t,apply(data_tecator, 1, derivative, dt = t[2] - t[1]),type="l",lty=1, main ="Derivative of the Tecator curves", xlab="t", ylab=bquote(X[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$x_reg_deriv),type="l",lty=1, main ="Registered derivative curves", xlab="t", ylab=bquote(Xreg[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
matplot(t,t(DBest$h_hat_deriv) ,type="l",lty=1, main ="Warping estimates of the derivatives", xlab="t", ylab=bquote(hat(h)[i]~(t)), col = c(rep("blue",num1), rep("red", num2)))
dev.off()




## Curvas medias por grupo ----
media_bajo <- colMeans(data_tecator[labels_reales == 1, ])
media_alto <- colMeans(data_tecator[labels_reales == 2, ])

plot(t, media_bajo, type="l", col="blue", lwd=3, ylim=range(data_tecator),
     main="Espectros Medios de Tecator", xlab="Longitud de onda", ylab="Absorbancia")
lines(t, media_alto, col="red", lwd=3)
legend("topleft", legend=c("Media Bajo en Grasa (<=20%)", "Media Alto en Grasa (>20%)"), 
       col=c("blue", "red"), lwd=3)



# Clustering ----
# Primera derivada
primera_deriv <- t(apply(data_tecator, 1, derivative, dt = t[2] - t[1]))

## omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <-  acc.distrib.tecator(primera_deriv, endpoints, omega, alpha)

#### ARI results ----
tucker_ari <- acc$tucker_ari
ari_PAM_M <- acc$ari_PAM_M 
ari_PAM_x2 <- acc$ari_PAM_x2
ari_PAM_Mej <- acc$ari_PAM_Mej
ari_PAM_Mej_RMS <- acc$ari_PAM_Mej_RMS
ari_PAM_4RMS <- acc$ari_PAM_4RMS

#### Execution times ----
time_tucker = acc$time_tucker
PAM_M_time = acc$PAM_M_time
PAM_x2_time = acc$PAM_x2_time
PAM_Mej_time = acc$PAM_Mej_time
PAM_Mej_RMS_time = acc$PAM_Mej_RMS_time
PAM_4RMS_time = acc$PAM_4RMS_time


# Save the results
ARI_results <- data.frame(
  Method = c(
    "$\\delta_1(\\cdot, \\cdot \\ ; \\  \\delta_{\\Ltwo}, 0.1)$",
    "$\\delta_1(\\cdot, \\cdot \\ ; \\  \\delta_{\\Ltwo}, 0.5)$",
    "$\\delta_1(\\cdot, \\cdot \\ ; \\  \\delta_{\\Ltwo}, 0.9)$",
    "$\\delta_1\\Big(\\cdot, \\cdot \\ ; \\delta_d(\\cdot, \\cdot \\ ;0.1), 0.1 \\Big)$",
    "$\\delta_1\\Big(\\cdot, \\cdot \\ ; \\delta_d(\\cdot, \\cdot \\ ;0.9), 0.1 \\Big)$",
    "$\\delta_1\\Big(\\cdot, \\cdot \\ ; \\delta_d(\\cdot, \\cdot \\ ;0.5), 0.5 \\Big)$",
    "$\\delta_1\\Big(\\cdot, \\cdot \\ ; \\delta_d(\\cdot, \\cdot \\ ;0.1), 0.9 \\Big)$",
    "$\\delta_1\\Big(\\cdot, \\cdot \\ ; \\delta_d(\\cdot, \\cdot \\ ;0.9), 0.9 \\Big)$",
    "$\\delta_1(\\cdot, \\cdot \\ ; \\  \\delta_{RelMS(d)}, 0.1)$",
    "$\\delta_1(\\cdot, \\cdot \\ ; \\  \\delta_{RelMS(d)}, 0.5)$",
    "$\\delta_1(\\cdot, \\cdot \\ ; \\  \\delta_{RelMS(d)}, 0.9)$",
    "$\\delta_2(\\cdot, \\cdot \\ ; \\  0.1)$",
    "$\\delta_2(\\cdot, \\cdot \\ ; \\  0.5)$",
    "$\\delta_2(\\cdot, \\cdot \\ ; \\  0.9)$",
    "$\\delta_3$ ",
    "Tucker"
  ),
  
  ARI = rep(0, 16),
  
  Execution_Time = rep(0,16)
)


ARI_results[1,2] <- round(ari_PAM_M, digits = 4)  
ARI_results[4,2] <- round(ari_PAM_Mej, digits = 4) 
ARI_results[9,2] <- round(ari_PAM_Mej_RMS, digits = 4) 
ARI_results[12,2] <- round(ari_PAM_x2, digits = 4)
ARI_results[15,2] <- round(ari_PAM_4RMS, digits = 4)
ARI_results[16,2] <- round(tucker_ari, digits = 4) 


ARI_results[1,3] <- PAM_M_time 
ARI_results[4,3] <- PAM_Mej_time  
ARI_results[9,3] <- PAM_Mej_RMS_time  
ARI_results[12,3] <- PAM_x2_time
ARI_results[15,3] <- PAM_4RMS_time  
ARI_results[16,3] <- time_tucker




## omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9


# Accuracy metrics
acc <-  acc.distrib.tecator(primera_deriv, endpoints, omega, alpha)

#### ARI results ----
ari_PAM_Mej <- acc$ari_PAM_Mej

#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[5,2] <- round(ari_PAM_Mej, digits = 4)      

ARI_results[5,3] <- PAM_Mej_time




## omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5


# Accuracy metrics
acc <-  acc.distrib.tecator(primera_deriv, endpoints, omega, alpha)

#### ARI results ----
ari_PAM_M <- acc$ari_PAM_M
ari_PAM_x2 <- acc$ari_PAM_x2
ari_PAM_Mej <- acc$ari_PAM_Mej
ari_PAM_Mej_RMS <- acc$ari_PAM_Mej_RMS


#### Execution times ----
PAM_M_time = acc$PAM_M_time
PAM_x2_time = acc$PAM_x2_time
PAM_Mej_time = acc$PAM_Mej_time
PAM_Mej_RMS_time = acc$PAM_Mej_RMS_time

# Save the results
ARI_results[2,2] <- round(ari_PAM_M, digits = 4)
ARI_results[6,2] <- round(ari_PAM_Mej, digits = 4)  
ARI_results[10,2] <- round(ari_PAM_Mej_RMS, digits = 4)  
ARI_results[13,2] <- round(ari_PAM_x2, digits = 4)  


ARI_results[2,3] <- PAM_M_time    
ARI_results[6,3] <- PAM_Mej_time
ARI_results[10,3] <- PAM_Mej_RMS_time
ARI_results[13,3] <- PAM_x2_time





## omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1

# Accuracy metrics
acc <-  acc.distrib.tecator(primera_deriv, endpoints, omega, alpha)

#### ARI Results ----
ari_PAM_M <- acc$ari_PAM_M
ari_PAM_x2 <- acc$ari_PAM_x2
ari_PAM_Mej <- acc$ari_PAM_Mej
ari_PAM_Mej_RMS <- acc$ari_PAM_Mej_RMS


#### Execution times ----
PAM_M_time = acc$PAM_M_time
PAM_x2_time = acc$PAM_x2_time
PAM_Mej_time = acc$PAM_Mej_time
PAM_Mej_RMS_time = acc$PAM_Mej_RMS_time

# Save the results
ARI_results[3,2] <- round(ari_PAM_M, digits = 4)
ARI_results[7,2] <- round(ari_PAM_Mej, digits = 4)  
ARI_results[11,2] <- round(ari_PAM_Mej_RMS, digits = 4)  
ARI_results[14,2] <- round(ari_PAM_x2, digits = 4) 


ARI_results[3,3] <- PAM_M_time
ARI_results[7,3] <- PAM_Mej_time 
ARI_results[11,3] <- PAM_Mej_RMS_time 
ARI_results[14,3] <- PAM_x2_time







## omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9


# Accuracy metrics
acc <-  acc.distrib.tecator(primera_deriv, endpoints, omega, alpha)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej

#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- round(ari_PAM_Mej, digits = 4)  

ARI_results[8,3] <- PAM_Mej_time   






### Table ----
write.csv(ARI_results, file = "SS3_Tecator.csv", row.names = FALSE)
source("LateX_Table_Tecator.R")



### Pareto chart ----
library(ggplot2)
library(latex2exp)

# Tu dataframe se mantiene igual
plot_data <- data.frame(
  Method = c(
    "$\\delta_1(\\delta_{L^2}, 0.1)$",
    "$\\delta_1(\\delta_{L^2}, 0.5)$",
    "$\\delta_1(\\delta_{L^2}, 0.9)$",
    "$\\delta_1(\\delta_d(0.1), 0.1 )$",
    "$\\delta_1(\\delta_d(0.9), 0.1 )$",
    "$\\delta_1(\\delta_d(0.5), 0.5 )$",
    "$\\delta_1(\\delta_d(0.1), 0.9 )$",
    "$\\delta_1(\\delta_d(0.9), 0.9 )$",
    "$\\delta_1(\\delta_{RelMS(d)}, 0.1)$",
    "$\\delta_1(\\delta_{RelMS(d)}, 0.5)$",
    "$\\delta_1(\\delta_{RelMS(d)}, 0.9)$",
    "$\\delta_2(0.1)$",
    "$\\delta_2(0.5)$",
    "$\\delta_2(0.9)$",
    "$\\delta_3$ ",
    "Tucker"
  ),
  ARI = as.numeric(gsub(" *\\(.*\\)", "", ARI_results$ARI)),
  Execution_Time = as.numeric(gsub(" *\\(.*\\)", "", ARI_results$Execution_Time))
)


nuevo_orden <- c(1:12, 16, 13:15)
plot_data <- plot_data[nuevo_orden, ]


colores_distintos <- c(
  "#DEEBF7", "#9ECAE1", "#3182BD", # delta_1(L2)
  "#FC9272", "#DE2D26", "#B31012", # delta_1(RelMS)
  "#FDE0EF", "#F1B6DA", "#DE77AE", "#C51B7D", "#8E0152", # delta_1(delta_d)
  "#AADB71", "#A1D99B", "#31A354", # delta_2
  "#F0E442", # delta_3
  "#54260C"  # Tucker
)

ggplot(plot_data, aes(x = Execution_Time, y = ARI)) +
  geom_point(aes(color = Method), size = 3) + 
  theme_minimal() +
  labs(
    title = "Case Study 3 Pareto Chart",
    x = "Execution time (s)",
    y = "ARI"
  ) +
  scale_color_manual(
    values = colores_distintos, 
    labels = TeX(plot_data$Method)
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8.5),
    legend.box.just = "left",
    legend.spacing.x = unit(0.3, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  ) +
  guides(
    color = guide_legend(
      nrow = 3, 
      byrow = FALSE, 
      override.aes = list(size = 3) 
    )
  )
ggsave(
  filename = "CS3_Pareto_Chart.png", 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)















# # B-spline Expansion with K-means Clustering ----
# 
# library(fda)
# library(mclust)
# 
# curvas_matriz <- t(data_tecator) # fda prefiere las curvas en las columnas 
# t_grid <- seq(0, 1, length.out = 100)
# 
# n <- nrow(data_tecator)
# m <- ncol(data_tecator)
# 
# # Real clusterig labels based on the fat percentage (3 classes: low, medium, high)
# fat_content <- endpoints[, 2]
# breaks <- quantile(fat_content, probs = c(0, 1/3, 2/3, 1))
# labels_reales <- cut(fat_content, breaks = breaks, include.lowest = TRUE, labels = FALSE)
# 
# 
# tiempo_inicial <- proc.time()
# 
# # Definir una base de B-splines 
# n_basis <- 15
# base_splines <- create.bspline.basis(rangeval = c(0, 1), nbasis = n_basis, norder = 4)
# 
# # Suavizar las curvas y extraer los coeficientes de la base
# curvas_smooth <- smooth.basis(t_grid, curvas_matriz, base_splines)
# coeficientes <- t(curvas_smooth$fd$coefs) 
# 
# # Aplicar K-means sobre los coeficientes funcionales 
# set.seed(123) 
# kmeans_funcional <- kmeans(coeficientes, centers = 3, nstart = 25)
# 
# tiempo_final <- proc.time() - tiempo_inicial
# tiempo_bsplines_kmeans <- as.numeric(tiempo_final["elapsed"])
# 
# 
# 
# # Adjusted Rand Index
# labels_pred_kmeans <- kmeans_funcional$cluster
# ari_bsplines_kmeans <- adjustedRandIndex(labels_pred_kmeans, labels_reales)
# 
# # Resultados
# cat(" Método: B-splines + K-means \n")
# cat("ARI:", round(ari_bsplines_kmeans, 4), "\n")
# cat("Execution Time:", round(tiempo_bsplines_kmeans, 4), "seconds\n")











# # Qiao ----
# library(fdasrvf)  
# Define required functions
# 
# gaussianKernel <- function( x ){
#   
#   ## function to evaluate the truncated Gaussian kernel	
#   computeGaussianKernel <- function( y ){
#     
#     if( 0 <= y ){
#       
#       value <- 2 / 0.388 * dnorm( y / 0.388 )
#       
#     } else{
#       
#       value <- 0
#       
#     }
#     
#     return( value )
#     
#   }
#   
#   output <- sapply( x, computeGaussianKernel )
#   
#   return( output )
#   
# }
# 
# ## efficient connected components algorithm (for cluster assignments)
# 
# connectedComponents <- function( X, time, epscluster ){
#   
#   N <- ncol(X)
#   
#   ## initialize components matrix
#   C <- X
#   
#   ## initialize components vector
#   labels <- vector( mode="integer", length=N )
#   
#   K <- 1 
#   labels[1] <- 1
#   C[,1] <- X[,1]
#   
#   pb <- txtProgressBar( min=0, max=N, style=3 )
#   
#   
#   for( n in 2:N ){
#     
#     assigned <- FALSE
#     
#     for( k in 1:K ){
#       
#       distance <- elastic.distance(f1=as.vector(srvf_to_f(X[,n],time=t)),f2=as.vector(srvf_to_f(C[,k],time=t)),time=t)$Dy
#       
#       if( distance < epscluster ){
#         
#         labels[n] <- k
#         assigned <- TRUE
#         break
#         
#       }
#       
#     }
#     
#     if( !assigned ){
#       
#       K <- K + 1
#       labels[n] <- K
#       C[,K] <- X[,n]
#       
#     }
#     
#     setTxtProgressBar( pb, n )
#     
#   }
#   
#   C <- as.matrix( C[,1:K] )
#   colnames( C ) <- paste( "mode", 1:K, sep="" )
#   
#   labels <- as.integer( labels )
#   
#   output <- list(labels=labels ) #removed components=C
#   
#   close( pb )
#   
#   message( "\nThe algorithm found ", as.character( K ),
#            " clusters.\n")
#   
#   return( output )
#   
# }
# ## function to calculate L^2 distance
# trapz <- function(x,y,dims=1){
#   if ((dims-1)>0){
#     perm = c(dims:max(ndims(y),dims), 1:(dims-1))
#   } else {
#     perm = c(dims:max(ndims(y),dims))
#   }
#   
#   if (ndims(y) == 0){
#     m = 1
#   } else {
#     if (length(x) != dim(y)[dims])
#       stop('Dimension Mismatch')
#     y = aperm(y, perm)
#     m = nrow(y)
#   }
#   
#   if (m==1){
#     M = length(y)
#     out = sum(diff(x)*(y[-M]+y[-1])/2)
#   } else {
#     slice1 = y[as.vector(outer(1:(m-1), dim(y)[1]*( 1:prod(dim(y)[-1])-1 ), '+')) ]
#     dim(slice1) = c(m-1, length(slice1)/(m-1))
#     slice2 = y[as.vector(outer(2:m, dim(y)[1]*( 1:prod(dim(y)[-1])-1 ), '+'))]
#     dim(slice2) = c(m-1, length(slice2)/(m-1))
#     out = t(diff(x)) %*% (slice1+slice2)/2.
#     siz = dim(y)
#     siz[1] = 1
#     out = array(out, siz)
#     perm2 = rep(0, length(perm))
#     perm2[perm] = 1:length(perm)
#     out = aperm(out, perm2)
#     ind = which(dim(out) != 1)
#     out = array(out, dim(out)[ind])
#   }
#   
#   return(out)
# }
# 
# ndims <- function(x){
#   return(length(dim(x)))
# }
# 
# 
# 
# #Elastic distance function for Karcher mean shift algorithm
# elastic.distance2 <- function(q1,f2,time,lambda = 0){
#   q1<-q1
#   q2 <- f_to_srvf(f2,time)
#   gam <- optimum.reparam(q1,time,q2,time,method="DP")
#   fw <- approx(time,f2,xout=(time[length(time)]-time[1])*gam + time[1])$y
#   qw <- f_to_srvf(fw,time)
#   Dy <- sqrt(trapz(time, (q1-qw)^2))
#   
#   return(list(Dy=Dy,qw=qw))
# }
# 
# 
# ##Karcher mean shift algorithm function##
# 
# Karcher_MS<-function(X, eps1, eps2, h, epscluster, time, innerloop) {
#   
#   qtildekm<-matrix(nrow=nrow(X),ncol=length(time))
#   mukm<-matrix(nrow=innerloop,ncol=length(time))
#   numeratorkm<-matrix(nrow=nrow(X),ncol=length(time))
#   denominatorkm<-vector(length=nrow(X))
#   edkm<-vector(length=nrow(X))
#   qtildemean<-matrix(nrow=innerloop,ncol=length(time))
#   modekm<-matrix(nrow=nrow(X),ncol=length(time)) 
#   qmatrix<-matrix(nrow=nrow(X),ncol=length(time))
#   trajkm<-vector("list", nrow(X))
#   
#   
#   for (i in 1:nrow(X))qmatrix[i,]<-f_to_srvf(X[i,],time=time)
#   
#   for (k in 1:nrow(X)) {
#     print(sprintf("Starting Karcher mean shift on curve %d", k))
#     mukm<-matrix(nrow=innerloop,ncol=length(time))
#     mukm[1,]<-qmatrix[k,]
#     i<-1
#     repeat{
#       qtildemean[1,]<-mukm[i,]
#       r<-1
#       repeat{
#         #qtildekm<-t(multiple_align_functions(t(f),time=t,mu=as.vector(srvf_to_f(qtildemean[r,],time=t)),lambda = 0,showplot = FALSE,smooth_data = FALSE,sparam = 25,parallel = FALSE,omethod = "DP",MaxItr = 20,iter = 2000)$qn)
#         for (j in 1:nrow(X)) {
#           qtildekm[j,]<-elastic.distance2(q1=qtildemean[r,],f2=X[j,],time=t)$qw
#           edkm[j]<-elastic.distance2(q1=qtildemean[1,],f2=X[j,],time=t)$Dy
#           numeratorkm[j,]<-gaussianKernel(edkm[j]/h)*qtildekm[j,]
#           denominatorkm[j]<-gaussianKernel(edkm[j]/h)}
#         if (r == innerloop) {
#           print("Reached manually set inner loop iteration threshold, try a finer grid for curves")
#         }
#         qtildemean[r+1,]<-colSums(numeratorkm)/sum(denominatorkm)
#         print(sprintf("inner loop iteration number %d", r))
#         if (sqrt(trapz(time, (qtildemean[r+1,]-qtildemean[r,])^2))< eps1) {mukm[i+1,]<-qtildemean[r+1,]}
#         if (sqrt(trapz(time, (qtildemean[r+1,]-qtildemean[r,])^2))< eps1) {break}
#         r<-r+1}
#       
#       print(paste(
#         "Inner loop converged, elastic distance between Karcher means is:",
#         elastic.distance(
#           f1 = as.vector(srvf_to_f(mukm[i+1,], time = time)),
#           f2 = as.vector(srvf_to_f(mukm[i,], time = time)),
#           time = time
#         )$Dy
#       ))
#       if (elastic.distance(f1=as.vector(srvf_to_f(mukm[i+1,],time=time)),f2=as.vector(srvf_to_f(mukm[i,],time=time)),time=time)$Dy< eps2) {modekm[k,]<-mukm[i+1,]}
#       if (elastic.distance(f1=as.vector(srvf_to_f(mukm[i+1,],time=time)),f2=as.vector(srvf_to_f(mukm[i,],time=time)),time=time)$Dy< eps2) {break}
#       i<-i+1 }
#     
#   }
#   
#   assignments<-connectedComponents(t(modekm),time=time,epscluster)
#   
#   
#   return(list(assignments))
#   
# }
# 
# 
# 
# t <- seq(0, 1, length.out = ncol(data_tecator))
# 
# fat_content <- endpoints[, 2]
# breaks <- quantile(fat_content, probs = c(0, 1/3, 2/3, 1))
# labels_reales <- cut(fat_content, breaks = breaks, include.lowest = TRUE, labels = FALSE)
# 
# 
# time_start <- proc.time()
# 
# mean_shift_result <- Karcher_MS(
#   X = data_tecator, 
#   eps = 0.05,            # Tolerancia bucle interno (puedes reducir a 0.01 si buscas ultra-precisión)
#   eps2 = 0.05,           # Tolerancia bucle externo
#   h = 0.4,               # Ancho de banda del Kernel Gaussiano (Bandwidth)
#   epscluster = sqrt(0.05), # Umbral para agrupar las componentes conectadas en modos comunes
#   time = t, 
#   innerloop = 200
# )
# 
# time_total <- proc.time() - time_start
# cat("Tiempo de ejecución del algoritmo KMS:", time_total["elapsed"], "segundos.\n")
# 
# 
# # 5. EVALUAR LA EFICACIA DEL CLUSTERINGespectral
# labels_predichas <- mean_shift_result$labels
# 
# # Calcular el Adjusted Rand Index (ARI) comparando con las clases de grasa de Tecator
# ari_kms <- adjustedRandIndex(labels_predichas, labels_reales)
# cat("El Índice de Rand Ajustado (ARI) para Karcher Mean Shift es:", round(ari_kms, 4), "\n")
# 
# 
# 


