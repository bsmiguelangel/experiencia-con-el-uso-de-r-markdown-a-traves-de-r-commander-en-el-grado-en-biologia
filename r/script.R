#### Paquetes requeridos ####

## install.packages("pacman")

pacman::p_load(foreign, readxl, faraway, ggplot2, RColorBrewer, graphics, 
               ggpubr, nimble, ggmcmc, extraDistr, parallel, MCMCvis, 
               gridExtra, corrplot, ggcorrplot, readr, lattice, install = FALSE)

#### Carga de datos ####

rm(list = ls())
encuesta <- read_excel(file.path("data", "EncuestaRMD2025.xlsx"), col_names = TRUE)

encuesta$P0 <- factor(encuesta$P0, levels = c("Castellano", "Valenciano"))
encuesta$P1 <- factor(encuesta$P1, levels = c("BI1", "BI2", "AI1", "AI2"))
encuesta$P2 <- factor(encuesta$P2, levels = c("No", "Si"))
encuesta$P3 <- factor(encuesta$P3, levels = c("No", "Si"))
encuesta$P4 <- factor(encuesta$P4, levels = c("No", "Si"))
encuesta$P5 <- factor(encuesta$P5, levels = c("No", "Si"))
encuesta$P6 <- factor(encuesta$P6, levels = c("No", "Si"))
encuesta$P7 <- factor(encuesta$P7, levels = c("Muy facil", "Facil", "Normal", "Dificil", "Muy dificil"))
encuesta$P8 <- factor(encuesta$P8, levels = c("Facilitado mucho", "Facilitado", "Normal", "Dificultado", "Dificultado mucho"))
encuesta$P9 <- factor(encuesta$P9, levels = c("Muy satisfecho", "Satisfecho", "Normal", "Insatisfecho", "Muy insatisfecho"))
encuesta$P10 <- factor(encuesta$P10, levels = c("No", "Si"))
encuesta$P11 <- factor(encuesta$P11, levels = c("Documento de texto", "R Markdown"))
encuesta$P12 <- factor(encuesta$P12, levels = c("No", "Si"))

str(encuesta)

#### Descriptiva numérica y gráfica ####

summary(encuesta[, -c(14:17)])

### Ítem 1: ¿Cuál es tu Subgrupo de Prácticas? ###

df <- data.frame(grupo = rep(levels(encuesta$P0), each = 2),
                 subgrupo = levels(encuesta$P1),
                 frecuencia = as.numeric(table(encuesta$P1)))
df$porcentaje <- paste0("(", round(df$frecuencia/sum(df$frecuencia) * 100, 2), "%", ")")
df

p <- ggplot(data = df, aes(x = subgrupo, y = frecuencia, fill = grupo)) + 
  geom_bar(stat = "identity") +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Grupo") +
  geom_text(aes(label = frecuencia), vjust = 2.0, color = "white", size = 3.5) +
  geom_text(aes(label = porcentaje), vjust = 4.0, color = "white", size = 3.5) +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue"))
p

### Ítem 4: ¿Has sentido frustración mientras aprendías R Markdown? ###

df4 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P4))),
                  respuesta = rep(levels(encuesta$P4), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P4, encuesta$P1, table))))
df4$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P4, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df4

p <- ggplot(data = df4, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  scale_fill_manual(values = c("No" = "forestgreen", "Si" = "firebrick"))
p

### Ítem 5: En caso afirmativo, ¿esa frustración ha terminado por desaparecer? ###

df5 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P5))),
                  respuesta = rep(levels(encuesta$P5), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P5[which(encuesta$P4 == "Si")], encuesta$P1[which(encuesta$P4 == "Si")], table))))
df5$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P5[which(encuesta$P4 == "Si")], encuesta$P1[which(encuesta$P4 == "Si")], function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df5

p <- ggplot(data = df5, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p

### Ítem 6: ¿Consideras que R Markdown te ha facilitado la entrega de tareas? ###

df6 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P6))),
                  respuesta = rep(levels(encuesta$P6), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P6, encuesta$P1, table))))
df6$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P6, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df6

p <- ggplot(data = df6, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen"))
p

### Ítem 7: ¿Cómo calificarías la dificultad de aprender R Markdown? ###

df7 <- data.frame(grupo = as.character(encuesta$P0),
                  subgrupo = as.character(encuesta$P1),
                  respuesta = as.numeric(encuesta$P7))
df7

p <- ggplot(data = subset(df7, !is.na(subgrupo)), aes(x = subgrupo, y = respuesta)) + 
  geom_dotplot(aes(fill = grupo), binaxis = "y", stackdir = "center", dotsize = 0.75) +
  theme_bw() + labs(x = "Subgrupo", y = "Respuesta", fill = "Grupo") +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_y_continuous(breaks = 1:length(levels(encuesta$P7)),
    labels = c("Muy facil (1)", "Facil (2)", "Normal (3)", "Dificil (4)", "Muy dificil (5)"))
p

### Ítem 8: ¿R Markdown te ha facilitado comprender los análisis estadísticos? ###

df8 <- data.frame(grupo = as.character(encuesta$P0),
                  subgrupo = as.character(encuesta$P1),
                  respuesta = as.numeric(encuesta$P8))
df8

p <- ggplot(data = subset(df8, !is.na(subgrupo)), aes(x = subgrupo, y = respuesta)) + 
  geom_dotplot(aes(fill = grupo), binaxis = "y", stackdir = "center", dotsize = 0.75) +
  theme_bw() + labs(x = "Subgrupo", y = "Respuesta", fill = "Grupo") +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_y_continuous(breaks = 1:length(levels(encuesta$P7)),
                     labels = c("Facil. mucho (1)", "Facilitado (2)", "Normal (3)", "Dificultado (4)", "Dific. mucho (5)"))
p

### Ítem 9: ¿Cuál ha sido tu grado de satisfacción utilizando R Markdown? ###

df9 <- data.frame(grupo = as.character(encuesta$P0),
                  subgrupo = as.character(encuesta$P1),
                  respuesta = as.numeric(encuesta$P9))
df9

p <- ggplot(data = subset(df9, !is.na(subgrupo)), aes(x = subgrupo, y = respuesta)) + 
  geom_dotplot(aes(fill = grupo), binaxis = "y", stackdir = "center", dotsize = 0.75) +
  theme_bw() + labs(x = "Subgrupo", y = "Respuesta", fill = "Grupo") +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_y_continuous(breaks = 1:length(levels(encuesta$P7)),
                     labels = c("Muy satisf. (1)", "Satisfecho (2)", "Normal (3)", "Insatisfecho (4)", "Muy insatisf. (5)"))
p

### Ítem 10: ¿Consideras que R Markdown es una herramienta útil? ###

df10 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P10))),
                  respuesta = rep(levels(encuesta$P10), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P10, encuesta$P1, table))))
df10$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P10, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df10

p <- ggplot(data = df10, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen"))
p

### Ítem 11: En el futuro, ¿recurrirías a \texttt{Word} o R Markdown? ###

levels(encuesta$P11) <- c("Word", "R Markdown")
df11 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P11))),
                   respuesta = rep(levels(encuesta$P11), times = length(levels(encuesta$P1))),
                   frecuencia = as.numeric(unlist(by(encuesta$P11, encuesta$P1, table))))
df11$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P11, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df11

p <- ggplot(data = df11, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  scale_fill_manual(values = c("Word" = "steelblue", "R Markdown" = "forestgreen")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p

### Ítem 12: ¿Recomendarías el aprendizaje de R Markdown? ###

df12 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P12))),
                   respuesta = rep(levels(encuesta$P12), times = length(levels(encuesta$P1))),
                   frecuencia = as.numeric(unlist(by(encuesta$P12, encuesta$P1, table))))
df12$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P12, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df12

p <- ggplot(data = df12, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p

### Ítem 13: Lo que más me ha gustado de R Markdown es... ###



### Ítem 14: Lo que menos me ha gustado de R Markdown es... ###



### Ítem 15: Lo que más me ha costado de R Markdown es... ###



### Ítem 16: ¿Cómo mejorarías las clases en el uso de R Markdown? ###



#### Variables de interés ####

# P2 Existencia de R
levels(encuesta$P2)
table(encuesta$P2)
P2 <- as.numeric(encuesta$P2)

# P3 Existencia de R Markdown
levels(encuesta$P3)
table(encuesta$P3)
P3 <- as.numeric(encuesta$P3)

# P4 Frustración
levels(encuesta$P4)
table(encuesta$P4)
P4 <- as.numeric(encuesta$P4)

# P5 Desaparición de la frustración
levels(encuesta$P5)
table(encuesta$P5)
P5 <- as.numeric(encuesta$P5)

# P6 Facilitar entregas
levels(encuesta$P6)
table(encuesta$P6)
P6 <- as.numeric(encuesta$P6)

# P7 Dificultad
levels(encuesta$P7)
table(encuesta$P7)
P7 <- as.numeric(encuesta$P7)

# P8 Facilitar la comprensión de análisis estadísticos
levels(encuesta$P8)
table(encuesta$P8)
P8 <- as.numeric(encuesta$P8)

# P9 Grado de satisfacción
levels(encuesta$P9)
table(encuesta$P9)
P9 <- as.numeric(encuesta$P9)

# P10 Herramienta útil
levels(encuesta$P10)
table(encuesta$P10)
P10 <- as.numeric(encuesta$P10)

# P11 Futuro informe
levels(encuesta$P11)
table(encuesta$P11)
P11 <- as.numeric(encuesta$P11)

# P12 Recomendación
levels(encuesta$P12)
table(encuesta$P12)
P12 <- as.numeric(encuesta$P12)

y <- data.frame(P7, P8, P9)
# y <- data.frame(P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)

# Tamaño muestral
NResp <- nrow(encuesta)
# Número de variables respuesta
NVars <- ncol(y)

# Número de categorías
NCats <- unique(apply(y, 2, function(x) {length(table(x))}))
ones <- rep(1, NCats)

# NCats <- as.numeric(apply(y, 2, function(x) {length(table(x))}))
# MaxCats <- max(NCats)
# ones <- rep(1, MaxCats + 1)

rm(list = c("P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12"))

colnames(y)

#### Predictor lineal ####

# P0 Grupo
levels(encuesta$P0)
table(encuesta$P0)
grupo <- as.numeric(encuesta$P0)

# P1 Subgrupo
levels(encuesta$P1) <- c("I1", "I2", "I1", "I2")
table(encuesta$P1)
subgrupo <- as.numeric(encuesta$P1)

# Número de encuestados por grupo y subgrupo
table(grupo, subgrupo)

# Number of levels of each (categorical) covariate
NGroups <- length(table(grupo))
NSubgroups <- length(table(subgrupo))

#### Modelo sin EAI ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

modelCode <- nimbleCode(
  {
    for(Var in 1:NVars) {
      # Likelihood
      for (Resp in 1:NResp) {
        y[Resp, Var] ~ dcat(prlevels[Resp, Var, 1:NCats])
        
        # Definition of the probabilities of each category as a function of the
        # cumulative probabilities
        prlevels[Resp, Var, 1] <- p.gamma[Resp, Var, 1]
        for (Cat in 2:(NCats-1)) {
          prlevels[Resp, Var, Cat] <- p.gamma[Resp, Var, Cat] - p.gamma[Resp, Var, Cat-1]
        }
        prlevels[Resp, Var, NCats] <- 1 - p.gamma[Resp, Var, NCats-1]
        
        # Linear predictor
        for (Cat in 1:(NCats-1)) {
          logit(p.gamma[Resp, Var, Cat]) <- kappa[Cat, Var] + 
            alpha[grupo[Resp], Var] + beta[grupo[Resp], subgrupo[Resp], Var]
        }
      }
    }
    
    # Prior distributions
    
    # kappa[1:(NCats-1), 1:NVars] cut points
    # Monotonic transformation
    for (Var in 1:NVars) {
      for (Cat in 1:(NCats-1)) {
        kappa[Cat, Var] <- logit(sum(delta[Var, 1:Cat]))
      }
      # delta[1:NVars, 1:NCats] Dirichlet prior
      delta[Var, 1:NCats] ~ ddirch(ones[1:NCats])
    }
    
    # alpha[1:NGroups, 1:NVars] fixed/random effects
    for (Var in 1:NVars) {
      alpha[1, Var] <- 0
      alpha[2, Var] ~ dflat()
      # for (Group in 1:NGroups) {
      #   alpha[Group, Var] ~ dnorm(0, sd = sd.alpha[Var])
      # }
    }
    
    # beta[1:NSub, 1:NVars] fixed/random effects
    for (Var in 1:NVars) {
      for (Group in 1:NGroups) {
        beta[Group, 1, Var] <- 0
        beta[Group, 2, Var] ~ dflat()
      }
      # for (Group in 1:NGroups) {
      #   for (Subgroup in 1:NSubgroups) {
      #     beta[Group, Subgroup, Var] ~ dnorm(0, sd = sd.beta[Group, Var])
      #   }
      # }
    }
    
    # # Hyperparameters
    # for (Var in 1:NVars) {
    #   sd.alpha[Var] ~ dhalfflat()
    #   for (Group in 1:NGroups) {
    #     sd.beta[Group, Var] ~ dhalfflat()
    #   }
    # }
    
    # NA in subgrupo[38]
    subgrupo[38] ~ dcat(prlevels.subgrupo[1:NSubgroups])
    prlevels.subgrupo[1:NSubgroups] ~ ddirch(ones[1:NSubgroups])
    
  }
)

### Data to be loaded ###

modelData <- list(y = as.matrix(y))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, 
                       grupo = grupo, NGroups = NGroups, ones = ones, 
                       subgrupo = subgrupo, NSubgroups = NSubgroups
)

### Parameters to be saved ###

modelParameters <- c("kappa", "alpha", "delta", 
                     # "sd.alpha", "sd.beta", 
                     "beta", "subgrupo", "prlevels.subgrupo"
)

# Create a function with all the needed code
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  NGroups <- constants$NGroups
  NSubgroups <- constants$NSubgroups
  NCats <- constants$NCats
  ones <- constants$ones
  NResp <- constants$NResp
  NVars <- constants$NVars
  
  # Let’s create the Nimble model, creates the nodes (inits should be passed now)
  model <- nimbleModel(code = code, 
                       constants = constants,
                       data = data, 
                       inits = list(delta = matrix(rdirichlet(NVars, ones),
                                                   nrow = NVars, ncol = NCats),
                                    alpha = matrix(c(rep(NA, NVars), rnorm((NGroups - 1) * NVars)), nrow = NGroups, ncol = NVars, byrow = TRUE),
                                    beta = array(c(rep(NA, NGroups), rnorm(NSubgroups), rep(NA, NGroups), rnorm(NSubgroups), rep(NA, NGroups), rnorm(NSubgroups)), dim = c(NGroups, NSubgroups, NVars)),
                                    alpha = matrix(rnorm(NGroups * NVars), nrow = NGroups, ncol = NVars),
                                    beta = array(rnorm(NGroups * NSubgroups * NVars), dim = c(NGroups, NSubgroups, NVars)),
                                    sd.alpha = runif(NVars), sd.beta = matrix(runif(NGroups * NVars), nrow = NGroups, ncol = NVars),
                                    prlevels.subgrupo = as.numeric(rdirichlet(1, ones[1:NSubgroups]))
                                    )
                       , calculate = FALSE
  )
  
  # Compile the model, which means generating C++ code, compiling that code, and loading it back into R
  Cmodel <- compileNimble(model)
  
  # model$getParents(model$getNodeNames(dataOnly = TRUE), stochOnly = TRUE)
  
  # Configuration
  modelMCMCconfiguration <- configureMCMC(model, useConjugacy = FALSE,
                                          enableWAIC = TRUE)
  
  # # Remove desire samplers
  # modelMCMCconfiguration$removeSamplers(c("sd.alpha", "sd.beta"))
  # 
  # # Add slice sd.alpha sampler
  # sd.alphas <- character(NVars)
  # for (Var in 1:NVars) {
  #   sd.alphas[Var] <- paste0("sd.alpha[",Var,"]")
  # }
  # 
  # for (Var in 1:NVars) {
  #   modelMCMCconfiguration$addSampler(target = sd.alphas[Var], type = "slice")
  # }
  # 
  # # Add slice sd.beta sampler
  # sd.betas <- matrix(nrow = NGroups, ncol = NVars)
  # for (Var in 1:NVars) {
  #   for (Group in 1:NGroups) {
  #     sd.betas[Group, Var] <- paste0("sd.beta[",Group,",",Var,"]")
  #   }
  # }
  # 
  # for (Var in 1:NVars) {
  #   for (Group in 1:NGroups) {
  #     modelMCMCconfiguration$addSampler(target = sd.betas[Group, Var], type = "slice")
  #   }
  # }
  
  # Add new monitors
  modelMCMCconfiguration$monitors <- c()
  modelMCMCconfiguration$addMonitors(monitors)
  # Build MCMC object
  modelMCMC <- buildMCMC(modelMCMCconfiguration)
  # Need to reset the nimbleFunctions in order to add the new MCMC
  CmodelMCMC <- compileNimble(modelMCMC, project = model,
                              resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

system.time(salnimble <- parLapply(cl = this_cluster, X = 1:n.chains, 
                                   fun = run_MCMC_allcode, 
                                   code = modelCode,
                                   constants = modelConstants,
                                   data = modelData,
                                   monitors = modelParameters))

# It's good practice to close the cluster when you're done with it.
stopCluster(this_cluster)

#### Modelo con EAI ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

modelCode <- nimbleCode(
  {
    for(Var in 1:NVars) {
      # Likelihood
      for (Resp in 1:NResp) {
        y[Resp, Var] ~ dcat(prlevels[Resp, Var, 1:NCats])
        
        # Definition of the probabilities of each category as a function of the
        # cumulative probabilities
        prlevels[Resp, Var, 1] <- p.gamma[Resp, Var, 1]
        for (Cat in 2:(NCats-1)) {
          prlevels[Resp, Var, Cat] <- p.gamma[Resp, Var, Cat] - p.gamma[Resp, Var, Cat-1]
        }
        prlevels[Resp, Var, NCats] <- 1 - p.gamma[Resp, Var, NCats-1]
        
        # Linear predictor
        for (Cat in 1:(NCats-1)) {
          logit(p.gamma[Resp, Var, Cat]) <- kappa[Cat, Var] + 
            alpha[grupo[Resp], Var] + beta[grupo[Resp], subgrupo[Resp], Var] +
            psi[Resp, Var]
        }
      }
    }
    
    # Prior distributions
    
    # kappa[1:(NCats-1), 1:NVars] cut points
    # Monotonic transformation
    for (Var in 1:NVars) {
      for (Cat in 1:(NCats-1)) {
        kappa[Cat, Var] <- logit(sum(delta[Var, 1:Cat]))
      }
      # delta[1:NVars, 1:NCats] Dirichlet prior
      delta[Var, 1:NCats] ~ ddirch(ones[1:NCats])
    }
    
    # alpha[1:NGro, 1:NVars] fixed effects
    for (Var in 1:NVars) {
      alpha[1, Var] <- 0
      alpha[2, Var] ~ dflat()
    }
    
    # beta[1:NSub, 1:NVars] fixed effects
    for (Var in 1:NVars) {
      for (Group in 1:NGro) {
        beta[Group, 1, Var] <- 0
        beta[Group, 2, Var] ~ dflat()
      }
    }
    
    # psi[1:NResp, 1:NVars] individual random effects
    for(Var in 1:NVars) {
      for (Resp in 1:NResp) {
        psi[Resp, Var] <- inprod(sub.Resp[Resp, ], M.Resp[, Var])
        # sub.Resp[1:NResp, 1:NVars] underlying individual REs
        sub.Resp[Resp, Var] ~ dnorm(0, 1)
      }
    }
    
    # M.Resp[1:NVars, 1:NVars] M-matrix
    for (Var1 in 1:NVars) {
      for (Var2 in 1:NVars) {
        M.Resp[Var1, Var2] ~ dnorm(0, tau.M.Resp)
      }
    }
    
    # Prior for precision of M.Resp
    tau.M.Resp <- pow(sd.M.Resp, -2)
    sd.M.Resp ~ dhalfflat()
    
    # NA in subgrupo
    subgrupo[38] ~ dcat(prlevels.subgrupo[1:NSub])
    prlevels.subgrupo[1:NSub] ~ ddirch(ones[1:NSub])
    
  }
)

### Data to be loaded ###

modelData <- list(y = as.matrix(y))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, 
                       grupo = grupo, subgrupo = subgrupo, NGro = NGro, 
                       NSub = NSub, ones = ones)

### Parameters to be saved ###

modelParameters <- c("kappa", "alpha", "beta", "psi", "M.Resp", "sd.M.Resp",
                     "subgrupo", "prlevels.subgrupo", "delta", "sub.Resp")

# Create a function with all the needed code
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  NGro <- constants$NGro
  NSub <- constants$NSub
  NCats <- constants$NCats
  ones <- constants$ones
  NResp <- constants$NResp
  NVars <- constants$NVars
  
  # Let’s create the Nimble model, creates the nodes (inits should be passed now)
  model <- nimbleModel(code = code, 
                       constants = constants,
                       data = data, 
                       inits = list(delta = matrix(rdirichlet(NVars, ones),
                                                   nrow = NVars, ncol = NCats),
                                    alpha = matrix(c(rep(NA, NVars), rnorm((NGro - 1) * NVars)), nrow = NGro, ncol = NVars, byrow = TRUE),
                                    beta = array(c(rep(NA, NGro), rnorm(NSub), rep(NA, NGro), rnorm(NSub), rep(NA, NGro), rnorm(NSub)), dim = c(NGro, NSub, NVars)),
                                    sub.Resp = matrix(rnorm(NResp * NVars, sd = 0.01), nrow = NResp, ncol = NVars),
                                    M.Resp = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                    sd.M.Resp = runif(1, min = 0.2, max = 0.8),
                                    prlevels.subgrupo = as.numeric(rdirichlet(1, ones[1:NSub])))
                       , calculate = FALSE
  )
  
  # Compile the model, which means generating C++ code, compiling that code, and loading it back into R
  Cmodel <- compileNimble(model)
  
  # model$getParents(model$getNodeNames(dataOnly = TRUE), stochOnly = TRUE)
  
  # Configuration
  modelMCMCconfiguration <- configureMCMC(model, useConjugacy = FALSE,
                                          enableWAIC = TRUE)
  
  # Remove desire samplers
  modelMCMCconfiguration$removeSamplers(c("sd.M.Resp"))
  
  # Add slice sd.M.Resp sampler
  modelMCMCconfiguration$addSampler(target = "sd.M.Resp", type = "slice")
  
  # Add new monitors
  modelMCMCconfiguration$monitors <- c()
  modelMCMCconfiguration$addMonitors(monitors)
  # Build MCMC object
  modelMCMC <- buildMCMC(modelMCMCconfiguration)
  # Need to reset the nimbleFunctions in order to add the new MCMC
  CmodelMCMC <- compileNimble(modelMCMC, project = model,
                              resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

system.time(salnimble <- parLapply(cl = this_cluster, X = 1:n.chains, 
                                   fun = run_MCMC_allcode, 
                                   code = modelCode,
                                   constants = modelConstants,
                                   data = modelData,
                                   monitors = modelParameters))

# It's good practice to close the cluster when you're done with it.
stopCluster(this_cluster)

#### Function: salnimble to salwinbugs for Model con EAI ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NCats, NVars))
  psi <- array(dim = c(n.sims, NResp, NVars))
  sd.M.Resp <- numeric(length = n.sims)
  M.Resp <- array(dim = c(n.sims, NVars, NVars))
  
  for (Var in 1:NVars) {
    for (Cat in 1:(NCats - 1)) {
      kappa[, Cat, Var] <- c(salnimble[[1]][,  paste0("kappa[", Cat, ", ", Var, "]")],
                             salnimble[[2]][,  paste0("kappa[", Cat, ", ", Var, "]")],
                             salnimble[[3]][,  paste0("kappa[", Cat, ", ", Var, "]")],
                             salnimble[[4]][,  paste0("kappa[", Cat, ", ", Var, "]")],
                             salnimble[[5]][,  paste0("kappa[", Cat, ", ", Var, "]")])
    }
  }
  
  for (Var in 1:NVars) {
    for (Resp in 1:NResp) {
      psi[, Resp, Var] <- c(salnimble[[1]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[2]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[3]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[4]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[5]][, paste0("psi[", Resp, ", ", Var, "]")])
    }
  }
  
  sd.M.Resp <- c(salnimble[[1]][, "sd.M.Resp"], salnimble[[2]][, "sd.M.Resp"], 
                 salnimble[[3]][, "sd.M.Resp"], salnimble[[4]][, "sd.M.Resp"], 
                 salnimble[[5]][, "sd.M.Resp"])
  
  for (Var1 in 1:NVars) {
    for (Var2 in 1:NVars) {
      M.Resp[, Var1, Var2] <- c(salnimble[[1]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[2]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[3]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[4]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[5]][, paste0("M.Resp[", Var1, ", ", Var2, "]")])
    }
  }
  
  summary <- MCMCsummary(object = salnimble, round = 4)
  # summary <- "not available"
  sims.list <- list("kappa" = kappa, "psi" = psi, 
                    "sd.M.Resp" = sd.M.Resp, "M.Resp" = M.Resp)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "n.chains" = n.chains, "n.sims" = n.sims)
  
  return(salwinbugs)
}

salwinbugs <- NimToWin(salnimble = salnimble)

salwinbugs$summary[startsWith(labels(salwinbugs$summary)[[1]], "kappa"), ]

#### Convergencia ####

MCMCsummary(object = salnimble, params = "beta",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = salnimble,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "beta")

#### WAIC sin EAI ####

# Let’s create the Nimble model, creates the nodes
modelWAIC <- nimbleModel(code = modelCode, 
                         constants = modelConstants,
                         data = modelData, 
                         inits = list(delta = matrix(rdirichlet(NVars, ones),
                                                     nrow = NVars, ncol = NCats),
                                      alpha = matrix(c(rep(NA, NVars), rnorm((NGroups - 1) * NVars)), nrow = NGroups, ncol = NVars, byrow = TRUE),
                                      beta = array(c(rep(NA, NGroups), rnorm(NSubgroups), rep(NA, NGroups), rnorm(NSubgroups), rep(NA, NGroups), rnorm(NSubgroups)), dim = c(NGroups, NSubgroups, NVars)),
                                      prlevels.subgrupo = as.numeric(rdirichlet(1, ones[1:NSubgroups])))
                         , calculate = FALSE)
CmodelWAIC <- compileNimble(modelWAIC)         # calculateWAIC needs compiled model to exist
samples <- do.call(rbind, salnimble)           # single matrix of samples
waic <- calculateWAIC(samples, modelWAIC)

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 394.9772
# Field "lppd":
#   [1] -176.1987
# Field "pWAIC":
#   [1] 21.28994

#### WAIC con EAI ####

# Let’s create the Nimble model, creates the nodes
modelWAIC <- nimbleModel(code = modelCode, 
                         constants = modelConstants,
                         data = modelData, 
                         inits = list(delta = matrix(rdirichlet(NVars, ones),
                                                     nrow = NVars, ncol = NCats),
                                      alpha = matrix(c(rep(NA, NVars), rnorm((NGro - 1) * NVars)), nrow = NGro, ncol = NVars, byrow = TRUE),
                                      beta = array(c(rep(NA, NGro), rnorm(NSub), rep(NA, NGro), rnorm(NSub), rep(NA, NGro), rnorm(NSub)), dim = c(NGro, NSub, NVars)),
                                      sub.Resp = matrix(rnorm(NResp * NVars, sd = 0.01), nrow = NResp, ncol = NVars),
                                      M.Resp = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                      sd.M.Resp = runif(1, min = 0.2, max = 0.8),
                                      prlevels.subgrupo = as.numeric(rdirichlet(1, ones[1:NSub])))
                         , calculate = FALSE)
CmodelWAIC <- compileNimble(modelWAIC)         # calculateWAIC needs compiled model to exist
samples <- do.call(rbind, salnimble)           # single matrix of samples
waic <- calculateWAIC(samples, modelWAIC)

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 309.1068
# Field "lppd":
#   [1] -103.2027
# Field "pWAIC":
#   [1] 51.35073

#### Individual correlation matrix for Model con EAI ####

# orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
orden <- 1:NVars

SurveyMapping.Sigma.Resp <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  M.Resp <- salwinbugs$sims.list$M.Resp
  
  Sigma.Resp <- array(dim = c(n.sims, NVars, NVars))
  
  for (sim in 1:n.sims) {
    Sigma.Resp[sim, , ] <- t(M.Resp[sim, , ]) %*% M.Resp[sim, , ]
  }
  return(Sigma.Resp)
}

# Conjunto de las n.sims matrices de varianzas-covarianzas
n.sims <- salwinbugs$n.sims
Sigma.Respsim <- SurveyMapping.Sigma.Resp(salwinbugs = salwinbugs)
Corr <- array(dim = c(n.sims, NVars, NVars))

# Conjunto de las n.sims matrices de correlaciones
for (sim in 1:n.sims) {
  Corr[sim, , ] <- diag(diag(Sigma.Respsim[sim, , ])^(-1/2)) %*% Sigma.Respsim[sim, , ] %*%  diag(diag(Sigma.Respsim[sim, , ])^(-1/2))
}

Corr.mean <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileL <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileU <- matrix(ncol = NVars, nrow = NVars)
Sigma.Respmean <- matrix(ncol = NVars, nrow = NVars)
for (Var1 in 1:NVars) {
  for (Var2 in 1:NVars) {
    Corr.mean[Var1, Var2] <- mean(Corr[, Var1, Var2])
    Corr.quantileL[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.025)
    Corr.quantileU[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.975)
    Sigma.Respmean[Var1, Var2] <- mean(Sigma.Respsim[, Var1, Var2])
  }
}

eigen(Corr.mean)
eigen(Corr.mean)$values[1]/NVars
eigen(Corr.mean)$values[2]/NVars

labels <- colnames(y)
Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels; colnames(Corr.mean) <- labels

# orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
orden <- 1:NVars

Corr.mean.orden <- as.matrix(Corr.mean)
Corr.mean.orden <- Corr.mean.orden[orden, orden]

Corr.quantileL <- data.frame(Corr.quantileL); rownames(Corr.quantileL) <- labels;
colnames(Corr.quantileL) <- labels

Corr.quantileL.orden <- as.matrix(Corr.quantileL)
Corr.quantileL.orden <- Corr.quantileL.orden[orden, orden]

Corr.quantileU <- data.frame(Corr.quantileU); rownames(Corr.quantileU) <- labels;
colnames(Corr.quantileU) <- labels

Corr.quantileU.orden <- as.matrix(Corr.quantileU)
Corr.quantileU.orden <- Corr.quantileU.orden[orden, orden]

### Adding relevances ###

# replace on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Corr.mean.orden
Corr.quantileL.orden
Corr.quantileU.orden
Relevance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Relevance) <- rownames(Relevance) <-  colnames(Corr.mean.orden)
Relevance <- (Relevance - 1) * (-1)
for (Var in 1:NVars) { Relevance[Var, Var] <- 1 }

corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", tl.cex = 0.9,
         p.mat = Relevance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8, diag = FALSE)

corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", tl.cex = 0.9,
         p.mat = Relevance, sig.level = 0.05, insig = "blank",
         addCoef.col = "black", number.cex = 0.8, diag = FALSE)

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Relevance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
