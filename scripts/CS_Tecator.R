rm(list = ls())
source("R/Clustering.R") 

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




### Pareto chart ----
library(ggplot2)
library(latex2exp)

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
