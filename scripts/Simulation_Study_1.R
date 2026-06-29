rm(list = ls())
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/Base_Codes")
source("Clustering.R") 

# Save tables as .png
library(gt)
library(webshot2)

library(ggplot2)
library(tidyr)
library(dplyr)


# Parameters to tune for the curves generation:
n = 100 # number of curves
m = 101 # grid size in I = [0,1]

N_iter = 100 # number of generated sets of curves



# L1 Vs L2 ----
fun1 = L1
fun2 = L2





## 25% Vs 75% ----
prop = 0.25 # proportion of curves that belong to L1 group 
proportion = "25_75"




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  





### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2")

write.csv(ARI_results, file = "SS1_L1_L2_25_75.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)







## 50% Vs 50% ----
prop = 0.5 # proportion of curves that belong to L1 group 
proportion = "50_50"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  


### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2")

write.csv(ARI_results, file = "SS1_L1_L2_50_50.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)









### 75% Vs 25% ----
prop = 0.75 # proportion of curves that belong to L1 group 
proportion = "75_25"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  


### Final Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2")

write.csv(ARI_results, file = "SS1_L1_L2_75_25.csv", row.names = FALSE)
source("LateX_Table_Two_Groups.R")  



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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)




















# L2 Vs L3 ----
fun1 = L2
fun2 = L3



## 25% Vs 75% ----
prop = 0.25 # proportion of curves that belong to L1 group 
proportion = "25_75"




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  





### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L2_L3")

write.csv(ARI_results, file = "SS1_L2_L3_25_75.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)







## 50% Vs 50% ----
prop = 0.5 # proportion of curves that belong to L1 group 
proportion = "50_50"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  


### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L2_L3")

write.csv(ARI_results, file = "SS1_L2_L3_50_50.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)









### 75% Vs 25% ----
prop = 0.75 # proportion of curves that belong to L1 group 
proportion = "75_25"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

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
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))  


### Final Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L2_L3")

write.csv(ARI_results, file = "SS1_L2_L3_75_25.csv", row.names = FALSE)
source("LateX_Table_Two_Groups.R")  



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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)



























# L1 Vs L2 Vs L4 ----
fun1 = L1
fun2 = L2
fun3 = L4




## 33% Vs 33% Vs 33% ----
prop1 = 1/3 # proportion of curves that belong to L1 group 
prop2 = 1/3 # proportion of curves that belong to L2 group 
proportion = "33_33_33"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))    





### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2_L4")

write.csv(ARI_results, file = "SS1_L1_L2_L4_33_33_33.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)










## 60% Vs 20% Vs 20% ----
prop1 = 0.6 # proportion of curves that belong to L1 group 
prop2 = 0.2 # proportion of curves that belong to L2 group 
proportion = "60_20_20"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))    





### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2_L4")

write.csv(ARI_results, file = "SS1_L1_L2_L4_60_20_20.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)












## 20% Vs 60% Vs 20% ----
prop1 = 0.2 # proportion of curves that belong to L1 group 
prop2 = 0.6 # proportion of curves that belong to L2 group 
proportion = "20_60_20"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))    





### Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2_L4")

write.csv(ARI_results, file = "SS1_L1_L2_L4_20_60_20.csv", row.names = FALSE)




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)







## 20% Vs 20% Vs 60% ----
prop1 = 0.2 # proportion of curves that belong to L1 group 
prop2 = 0.2 # proportion of curves that belong to L2 group 
proportion = "20_20_60"




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

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
acc <- acc.distrib.3(n, m, fun1, fun2, fun3, prop1, prop2, omega, alpha, N_iter)

#### ARI Results ----
ari_PAM_Mej <- acc$ari_PAM_Mej


#### Execution times ----
PAM_Mej_time = acc$PAM_Mej_time

# Save the results
ARI_results[8,2] <- sprintf("%.4f (%.4f)", mean(ari_PAM_Mej), sd(ari_PAM_Mej))     

ARI_results[8,3] <- sprintf("%.4f (%.4f)", mean(PAM_Mej_time), sd(PAM_Mej_time))    





### Final Table ----
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/SS_1/L1_L2_L4")

write.csv(ARI_results, file = "SS1_L1_L2_L4_20_20_60.csv", row.names = FALSE)
source("LateX_Table_Three_Groups.R")




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
    title =  paste0("Pareto Chart ", proportion) ,
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
  filename = paste0("Pareto_Chart_", proportion, ".png"), 
  device = "png",                    
  width = 10,                        
  height = 5,                        
  units = "in",                      
  dpi = 300                          
)



                                                                                                                                                                                                                