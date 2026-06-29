rm(list = ls())
setwd("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/Base_Codes")
source("Clustering.R") 

# Save tables as .png
library(gt)
library(webshot2)

library(ggplot2)
library(tidyr)
library(dplyr)

library(latex2exp)


# Parameters to tune for the curves generation:
n = 100 # number of curves
m = 101 # grid size in I = [0,1]

N_iter = 100# number of generated sets of curves

# # Cambiar L2 Vs L3 por L1 Vs L2
# directory <- "L1_L2"
# fun1 = L1
# fun2 = L2

# Cambiar L1 Vs L2 por L2 Vs L3
directory <- "L2_L3"
fun1 = L2
fun2 = L3

# KNN dataframe
KNN_results <- data.frame(
  M = rep(NA, N_iter*3*5),
  x2 = rep(NA, N_iter*3*5),
  Mej = rep(NA, N_iter*3*5),
  Mej_RMS = rep(NA, N_iter*3*5),
  RMS4 = rep(NA, N_iter*3*5),
  
  omega = rep(NA, N_iter*3*5),
  alpha = rep(NA, N_iter*3*5),
  
  Proportion = rep(NA, N_iter*3*5)
)

## 25% Vs 75% ----
prop = 0.25 # proportion of curves that belong to L1 group 
proportion = "25_75"




### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### KNN ----
i <- 0
KNN_results$M[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter + 1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter + 1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter + 1):((i+1)*N_iter)] <- proportion

i <- i+1



### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1




### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$M[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1

# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$M[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1





### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1







## 50% Vs 50% ----
prop = 0.5 # proportion of curves that belong to L1 group 
proportion = "50_50"



### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### KNN ----
KNN_results$M[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter + 1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter + 1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter + 1):((i+1)*N_iter)] <- proportion

i <- i+1



### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1



### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$M[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion
i <- i+1





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1

# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$M[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1


### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1









### 75% Vs 25% ----
prop = 0.75 # proportion of curves that belong to L1 group 
proportion = "75_25"


### omega = 0.1, alpha = 0.1 ----
omega = 0.1
alpha = 0.1


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)

#### KNN ----
KNN_results$M[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter + 1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter + 1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter + 1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter + 1):((i+1)*N_iter)] <- proportion

i <- i+1



### omega = 0.1, alpha = 0.9 ----
omega = 0.1
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1




### omega = 0.5, alpha = 0.5 ----
omega = 0.5
alpha = 0.5


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$M[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1





### omega = 0.9, alpha = 0.1 ----
omega = 0.9
alpha = 0.1

# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$M[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_M
KNN_results$x2[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_x2
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej
KNN_results$Mej_RMS[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej_RMS
KNN_results$RMS4[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_4RMS

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion

i <- i+1





### omega = 0.9, alpha = 0.9 ----
omega = 0.9
alpha = 0.9


# Accuracy metrics
acc <- acc.distrib.2(n, m, fun1, fun2, prop, omega, alpha, N_iter)


#### KNN ----
KNN_results$Mej[(i*N_iter+1):((i+1)*N_iter)] <- acc$acc_KNN_Mej

KNN_results$omega[(i*N_iter+1):((i+1)*N_iter)] <- omega
KNN_results$alpha[(i*N_iter+1):((i+1)*N_iter)] <- alpha

KNN_results$Proportion[(i*N_iter+1):((i+1)*N_iter)] <- proportion









# KNN ----
# Long Format
KNN_results_long <- KNN_results %>%
  pivot_longer(
    cols = c(M, x2, Mej, Mej_RMS, RMS4), 
    names_to = "Distance", 
    values_to = "Accuracy"
  ) %>%
  mutate(
    Distance = factor(Distance, levels = c("M", "Mej", "Mej_RMS", "x2", "RMS4")),
    Proportion = factor(Proportion, levels = c("25_75", "50_50", "75_25"))
  )

omega_alpha_values <- list(
  c(0.1, 0.1), 
  c(0.1, 0.9), 
  c(0.5, 0.5), 
  c(0.9, 0.1), 
  c(0.9, 0.9)
)



omega_values <- c(0.1, 0.5, 0.9)

# To save the KNN images:
setwd(paste0("C:/Users/sofia/OneDrive - Universidad Carlos III de Madrid (1)/Proyecto/TFM/Codigos/KNN/",directory))
for (omega_val in omega_values) {
  
  df_plot <- KNN_results_long %>% 
    filter(omega == omega_val)
  
  if (omega_val == 0.5) {
    df_plot <- df_plot %>% filter(alpha == 0.5)
    
    p <- ggplot(df_plot, aes(x = Distance, y = Accuracy, fill = Proportion)) +
      geom_boxplot(outlier.size = 0.5, position = position_dodge(0.8)) + 
      scale_fill_brewer(palette = "Set1") +
      labs(title = paste("L2 Vs L3 for omega =", omega_val, "and alpha = 0.5"),
           fill = "Groups' Proportions")
    
    name <- paste0("KNN_distrib_omega_", omega_val*100, "_alpha_50.png")
    
  } else {
    df_plot <- df_plot %>% filter(!is.na(Accuracy))
    
    df_plot <- df_plot %>%
      mutate(
        Alpha_Factor = factor(paste0("alpha = ", alpha), levels = c("alpha = 0.1", "alpha = 0.9")),
        Proportion = factor(Proportion, levels = c("25_75", "50_50", "75_25"))
      )

    p <- ggplot(df_plot, aes(x = Distance, y = Accuracy, fill = Proportion)) +
      geom_boxplot(
        aes(alpha = ifelse(Distance == "Mej", as.character(Alpha_Factor), "alpha = 0.9")), 
        outlier.size = 0.5, 
        position = position_dodge(0.8)
      ) + 
      
      scale_alpha_manual(
        values = c("alpha = 0.1" = 0.4, "alpha = 0.9" = 1.0),
        labels = c("alpha = 0.1 (Light colour)", "alpha = 0.9 (Dark colour)")
      ) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = paste("L2 Vs L3 for omega =", omega_val),
        fill = "Groups' Proportions",
        alpha = "Parameter Alpha"
      ) +
      
      guides(
        fill = guide_legend(override.aes = list(alpha = 1)),
        alpha = guide_legend(override.aes = list(fill = "gray60"))
      )
    
    name <- paste0("KNN_distrib_omega_", omega_val*100, ".png")
  }

  p <- p +
    scale_x_discrete(labels = c(
      "M"       = TeX('$\\delta_1(\\delta_{L^2})$'), 
      "Mej"     = TeX('$\\delta_1(\\delta_d)$'), 
      "Mej_RMS" = TeX('$\\delta_1(\\delta_{RelMS(d)})$'), 
      "x2"      = TeX('$\\delta_2$'),
      "RMS4"    = TeX('$\\delta_3$')
    )) +
    labs(x = "Distance", y = "Accuracy") +
    theme_minimal() + 
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      
      legend.title = element_text(size = 12, face = "bold"), # Tamaño del título ("Parameter Alpha", etc.)
      legend.text = element_text(size = 10),                 # Tamaño del texto de las opciones
      legend.key.size = unit(1.2, "cm"),                   # Agranda el tamaño de los cuadraditos de color
      legend.spacing.x = unit(0.5, "cm")
    )
  
  # Guardar imagen
  png(name, width = 3000, height = 1600, res = 300)
  print(p)
  dev.off()
}





