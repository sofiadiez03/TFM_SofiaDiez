rm(list = ls())
source("R/Clustering.R") 


# Parameters to tune for the curves generation:
n = 120 # number of curves
m = 101 # grid size in I = [0,1]

prop1 = 1/3
prop2 = 1/3

N_iter = 100 # number of generated sets of curves



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.qiao.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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


ARI_results[1,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_M), sd(ari_PAM_M))    
ARI_results[4,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))  
ARI_results[9,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej_RMS), sd(ari_PAM_Mej_RMS)) 
ARI_results[12,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_x2), sd(ari_PAM_x2))
ARI_results[15,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_4RMS), sd(ari_PAM_4RMS))
ARI_results[16,2] <- sprintf("%.4f (%.4f)", mean(tucker_ari), sd(tucker_ari)) 


ARI_results[1,3] <- sprintf("%.4f (%.4f)", mean(PAM_M_time), sd(PAM_M_time))  
ARI_results[4,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  
ARI_results[9,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_RMS_time), sd(PAM_Mej_RMS_time))  
ARI_results[12,3] <- sprintf("%.4f (%.4f)", mean(PAM_x2_time), sd(PAM_x2_time))  
ARI_results[15,3] <- sprintf("%.4f (%.4f)", mean(PAM_4RMS_time), sd(PAM_4RMS_time))  
ARI_results[16,3] <- sprintf("%.4f (%.4f)", mean(time_tucker), sd(time_tucker)) 




### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.qiao.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI results ----
ari_PAM_Mej <- acc$ari_PAM_Mej

#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[5,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[5,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  




### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5


# Accuracy metrics
acc <- acc.distrib.qiao.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI results ----
ari_PAM_M <- acc$ari_PAM_M
ari_PAM_x2 <- acc$ari_PAM_x2
ari_PAM_Mej <- acc$ari_PAM_Mej
ari_PAM_Mej_RMS <- acc$ari_PAM_Mej_RMS
ari_PAM_4RMS <- acc$ari_PAM_4RMS



#### Execution times ----
PAM_M_time = acc$PAM_M_time
PAM_x2_time = acc$PAM_x2_time
PAM_Mej_time = acc$PAM_Mej_time
PAM_Mej_RMS_time = acc$PAM_Mej_RMS_time

# Save the results
ARI_results[2,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_M), sd(ari_PAM_M))  
ARI_results[6,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))  
ARI_results[10,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej_RMS), sd(ari_PAM_Mej_RMS))   
ARI_results[13,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_x2), sd(ari_PAM_x2))  


ARI_results[2,3] <- sprintf("%.4f (%.4f)", mean(PAM_M_time), sd(PAM_M_time))    
ARI_results[6,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  
ARI_results[10,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_RMS_time), sd(PAM_Mej_RMS_time)) 
ARI_results[13,3] <- sprintf("%.4f (%.4f)", mean(PAM_x2_time), sd(PAM_x2_time)) 





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1

# Accuracy metrics
acc <- acc.distrib.qiao.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
ARI_results[3,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_M), sd(ari_PAM_M))  
ARI_results[7,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))  
ARI_results[11,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej_RMS), sd(ari_PAM_Mej_RMS))  
ARI_results[14,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_x2), sd(ari_PAM_x2))


ARI_results[3,3] <- sprintf("%.4f (%.4f)", mean(PAM_M_time), sd(PAM_M_time))  
ARI_results[7,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  
ARI_results[11,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_RMS_time), sd(PAM_Mej_RMS_time)) 
ARI_results[14,3] <- sprintf("%.4f (%.4f)", mean(PAM_x2_time), sd(PAM_x2_time))







### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.qiao.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))    





### Pareto chart ----
library(ggplot2)
library(latex2exp)

plot_data <- data.frame(
  Method = c(
    "Tucker",
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
    "$\\delta_3$ "
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
    title = "Simulation Study 2 Pareto Chart",
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



# Data simulation ----

# Parameters to tune for the curves generation:
n = 120 # number of curves
m = 101 # grid size in I = [0,1]

prop1 = 1/3
prop2 = 1/3


num1 <- round(n*prop1)    # number of curves belonging to fun1 group
num2 <- round(n*prop2)    # number of curves belonging to fun2 group
num3 <- n - (num1 + num2) # number of curves belonging to fun3 group

t = seq(0, 1, length.out = m)

#amplitude noise for curves
tau<-.1

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



matplot(t,t(data),type="l",lty=1, main ="Qiao Data", xlab="t", 
        ylab=bquote(X[i]~(t)), col = c(rep("blue", num1), rep("red", num2), rep("green", num3) )) 






funciones_base <- matrix(nrow = 3, ncol = length(t))

# No hay ruido de amplitud (los coeficientes z valen exactamente su media: 1)
# No hay deformación temporal (se usa t directamente en lugar de gamma)

# Función con 3 jorobas
for (j in 1:length(t)) {
  funciones_base[1, j] <- 1 * exp(-(1 / (2 * (1 / 9)^2)) * (t[j] - (1 / 6))^2) + 
    1 * exp(-(1 / (2 * (1 / 9)^2)) * (t[j] - (3 / 6))^2) + 
    1 * exp(-(1 / (2 * (1 / 9)^2)) * (t[j] - (5 / 6))^2)
}

# Función con 4 jorobas
for (j in 1:length(t)) {
  funciones_base[2, j] <- 1 * exp(-(1 / (2 * (1 / 12)^2)) * (t[j] - (1 / 8))^2) + 
    1 * exp(-(1 / (2 * (1 / 12)^2)) * (t[j] - (3 / 8))^2) + 
    1 * exp(-(1 / (2 * (1 / 12)^2)) * (t[j] - (5 / 8))^2) + 
    1 * exp(-(1 / (2 * (1 / 12)^2)) * (t[j] - (7 / 8))^2)
}

# Función con 5 jorobas
for (j in 1:length(t)) {
  funciones_base[3, j] <- 1 * exp(-(1 / (2 * (1 / 16)^2)) * (t[j] - (1 / 10))^2) + 
    1 * exp(-(1 / (2 * (1 / 16)^2)) * (t[j] - (3 / 10))^2) + 
    1 * exp(-(1 / (2 * (1 / 16)^2)) * (t[j] - (5 / 10))^2) + 
    1 * exp(-(1 / (2 * (1 / 16)^2)) * (t[j] - (7 / 10))^2) + 
    1 * exp(-(1 / (2 * (1 / 16)^2)) * (t[j] - (9 / 10))^2)
}

matplot(t, t(funciones_base), type = "l", lty = 1, lwd = 3,
        main = "Base Functions", 
        xlab = "t", ylab = "X(t)", 
        col = c("blue", "red", "green"))

matplot(t, funciones_base[1,], type = "l", lty = 1, lwd = 2,
        xlab = "t", ylab = "X(t)", 
        col = "blue")
matplot(t, funciones_base[2,], type = "l", lty = 1, lwd = 2,
        xlab = "t", ylab = "X(t)", 
        col = "red")
matplot(t, funciones_base[3,], type = "l", lty = 1, lwd = 2,
        xlab = "t", ylab = "X(t)", 
        col = "green")



