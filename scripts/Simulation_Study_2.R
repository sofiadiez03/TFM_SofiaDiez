rm(list = ls())
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/Base_Codes")
source("Clustering.R") 
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_2")

# Save tables as .png
library(gt)
library(webshot2)


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





### Table ----
write.csv(ARI_results, file = "SS2_qiao_data.csv", row.names = FALSE)
source("LateX_Table_Qiao.R")



### Pareto chart ----
library(ggplot2)
library(latex2exp)

# Tu dataframe se mantiene igual
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
ggsave(
  filename = "SS2_Pareto_Chart.png", 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
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









# Tucker and Karcher Mean Shift  ----
packageVersion("fdasrvf")
source("Karcher_Mean_Shift_Functions.R") 

library(fdasrvf) #Version: 1.9.7, Date: 2021-06-21
library(mclust)

packageVersion("fdasrvf")

n = 120 # number of curves
m = 100 # grid size in I = [0,1]

prop1 = 1/3
prop2 = 1/3

N_iter <- 50 

# For data simulation
t = seq(0, 1, length.out = m)
num1 <- round(n*prop1)    # number of curves belonging to fun1 group
num2 <- round(n*prop2)    # number of curves belonging to fun2 group
num3 <- n - (num1 + num2) # number of curves belonging to fun3 group

#amplitude noise for curves
tau<-.1

h<-c(0.5461710, 0.9906185, 0.9096607, 0.7003990, 0.9430789, 0.9013393, 0.8201396, 0.6996902, 0.4751539, 0.8057006, 0.8398958, 0.8002882, 1.1078241, 0.8289315, 0.7768685, 0.5544602, 0.6203421, 0.7460041, 0.8069796, 0.7642226, 0.8386111, 0.9140896, 0.8771492, 0.9491793, 0.8118279, 0.9343795, 1.1245782, 0.7559554, 0.8516821, 0.7799492, 0.7287381, 1.1329632, 0.9959563, 1.1198550, 0.8600129, 0.7600584, 0.8399729, 0.5856628, 0.7318086, 0.8877527, 0.7414924, 0.5255973, 0.9410758, 0.8003435, 1.0981878, 0.9067302, 0.7137977, 0.8539703, 0.7593000, 0.8366808, 0.9221916, 0.7443951, 0.9916906, 0.8790883, 1.0423065, 0.5935858, 0.9101831, 1.1237697, 0.5328602, 0.7573253, 0.7550486, 0.6987488, 0.8124773, 0.7326490, 0.8500177, 0.6780878, 0.7677130, 0.7818782, 0.8020317, 0.7979682, 0.5450964, 0.8399962, 0.7616372, 0.8175320, 0.8626390, 0.8370306, 0.7985120, 0.8306082, 0.7334682, 0.7127659, 0.8472428, 0.6112950, 1.0760167, 1.0860609, 0.8105940, 0.5770805, 0.7929931, 0.7682931, 0.9275719, 0.8183782, 1.0302836, 0.5559352, 0.8131827, 0.7274012, 0.6054997, 0.8359593, 0.8885915, 0.7864697, 0.8263914, 0.8306527, 0.4912951, 0.8938091, 0.5576674, 0.8141240, 0.7608933, 0.9600966, 0.8022555, 0.7525501, 0.7448055, 0.8108658, 0.7757528, 0.7760289, 0.7938699, 0.8013416, 0.8390951, 0.9602507, 0.8138077, 1.0831407, 0.8058181, 0.8255363, 0.6296492, 1.1050163, 1.1173883, 0.7022747, 0.8003984, 1.0319178, 0.8024673, 0.8536290, 0.8726801, 0.4948051, 1.1196217, 0.6593592, 0.7978556, 0.7217074, 0.7746187, 0.8055911, 0.7863433, 0.6393814, 1.0778792, 0.7973426, 0.8941934, 0.9439542, 0.7928117, 1.0954089, 0.8182428, 0.7061878, 0.5112381, 0.8314383, 0.8488709, 0.5709319, 1.0011333, 0.8221152, 0.9768341, 0.6201749, 0.8367649, 0.7639670, 0.8049209, 0.8837965, 0.9473141, 0.8063799, 1.1002807, 0.6315090, 0.8123355, 0.4964344, 0.5540297, 1.0260429, 0.5395357, 0.7867451, 0.7121018, 0.5887347, 0.9456109, 0.8313517, 0.8702387, 0.7741904, 0.6136608, 1.0127369, 0.8052641, 1.1699728, 0.8681824, 0.5365037, 0.7419322, 0.7956624, 0.7760680, 0.6564801, 0.8157243, 0.8814984, 0.5462957, 1.1096283, 1.1000343, 0.6671231, 0.7460836, 0.8793692, 0.5672261, 0.4566819, 0.6598521, 0.8189628, 0.6542413, 0.8118571, 0.8549935, 1.1273665)

time_tucker <- rep(0, N_iter)
tucker_ari <- rep(0, N_iter)

time_karcher <- rep(0, N_iter)
karcher_ari <- rep(0, N_iter)

for (i in 1:N_iter){
  set.seed(17*i + 35)
  
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
  
  # #### Karcher Mean Shift
  # time <- proc.time()
  # g<-h[i]
  # 
  # #Output of the function is cluster assignments.
  # mean_shift_result <- Karcher_MS(X=data, 
  #                                 eps1=.1,
  #                                 eps2=.1,
  #                                 h = g,
  #                                 epscluster=sqrt(.1),
  #                                 time=t,
  #                                 innerloop=400)
  # new_time <- proc.time() - time
  # 
  # time_karcher[i] <- as.numeric(new_time["elapsed"])
  # 
  # labels_pred <- unlist(mean_shift_result)
  # 
  # # ARI
  # labels_reales <- c(rep(1, num1), rep(2, num2), rep(3, num3))
  # karcher_ari[i] <- adjustedRandIndex(labels_reales, labels_pred)
  # 

  #### K-means Align Tucker clustering
  data_transpose <- t(data)
  
  time <- proc.time()
  
  set.seed(i)
  sample <- sample(1:n, 3)
  # out_srvf <- kmeans_align(
  #   f = data_transpose,
  #   time = t,
  #   K = 3, # number of clusters
  #   alignment = TRUE,
  #   parallel = FALSE,
  #   showplot = FALSE
  # )
  testKMAFDA <- kmeans_align(
    f = data_transpose,
    time = t,
    K = 3,
    seeds = c(sample),
    nonempty = 0,
    lambda = 0,
    showplot = FALSE,
    smooth_data = FALSE,
    sparam = 25,
    parallel = FALSE,
    alignment = TRUE,
    omethod = "DP",
    max_iter = 50,# NO MaxItr
    thresh = 0.01
  )
  new_time <- proc.time() - time
  time_tucker[i] <- as.numeric(new_time["elapsed"])

  labels_pred <- testKMAFDA$labels

  # ARI
  labels_reales <- c(rep(1, num1), rep(2, num2), rep(3, num3))
  tucker_ari[i] <- adjustedRandIndex(labels_pred, labels_reales)
}

mean(karcher_ari)
sd(karcher_ari)

mean(time_karcher)
sd(time_karcher)



mean(tucker_ari)
sd(tucker_ari)

mean(time_tucker)
sd(time_tucker)


# For N_iter = 50
#
# > mean(tucker_ari)
# [1] 0.5664467
# > sd(tucker_ari)
# [1] 0.2648221
# > mean(time_tucker)
# [1] 7.982553
# > sd(time_tucker)
# [1] 2.927685






