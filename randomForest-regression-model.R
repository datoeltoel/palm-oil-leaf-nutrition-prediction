# -------------------------------------------------------------------------
# title       : Using Random Forest Regression for Estimate
#               Leaf Nutritions of Oil Palm Based on UAV Imagery
# data source : 
# author      : Ziyadatul Hikmah 
# date        : 01/03/2024
# -------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES  ----------------------------------------------
# libraries we need
libs <- c("randomForest", # for build randomForest model
          "tidyverse",    # handling and visualization data
          "caret"         # for model validation
          )

# install packages
installed_libs <- libs %in% row.names(
  install.packages()
)

if(any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

# load packages
invisible(lapply(libs, library, character.only = T))

# SET WORKING DIRECTORY ---------------------------------------------------
setwd("D:/Ziy/Course GIS & RS/Drone/RFR")

# IMPORTING DATA ----------------------------------------------------------
# dataset
get_nutrition_dataset <- function(){
  dataset <- readxl::read_excel("hasil_nilai.xlsx")
  new_dataset <- dataset %>% dplyr::select(
    N, P, K, Mg, ndvi, ndre,
    savi, arvi, gndvi
  ) %>% mutate()
}

nutrition_data <- get_nutrition_dataset()
new_colnames <- c("N", "P","K","Mg","NDVI","NDRE","SAVI","ARVI","GNDVI")
colnames(nutrition_data) <- new_colnames

# SPLIT DATA --------------------------------------------------------------
set.seed(500)

# split dataset
trainSamples <- sample(1:nrow(nutrition_data), 0.75*nrow(nutrition_data))
trainDat <- nutrition_data[trainSamples, ]
testDat <- nutrition_data[-trainSamples, ]

# VISUALIZED RELATIONSHIPS ------------------------------------------------
featurePlot(x = trainDat[, c("N","P","K","Mg","NDVI")], 
            y = factor(trainDat$Block), 
            plot = "pairs",
            auto.key = list(columns = 5))

# BUILD RANDOM FOREST REGRESSION MODEL  -----------------------------------
# define variable as independent and dependent variables
varInd <- c("NDVI","NDRE","SAVI") # independent variables
varDep <- c("N","P","K","Mg")     # dependent variables

# N RFR
N_RFR <- randomForest::randomForest(
  N ~ NDVI + NDRE + SAVI + ARVI + GNDVI,
  data = trainDat,
  importance = T,
  ntree = 500,
  mtry = 3
)

print(N_RFR)      # randomForest model print
plot(N_RFR)       # randomForest model plot
varImpPlot(N_RFR) # variable importance plot

# P RFR
P_RFR <- randomForest::randomForest(
  P ~ NDVI + NDRE + SAVI + ARVI + GNDVI,
  data = trainDat,
  importance = T,
  ntree = 500,
  mtry = 3
)

print(P_RFR)      # randomForest model print
plot(P_RFR)       # randomForest model plot
varImpPlot(P_RFR) # variable importance plot

# K RFR
K_RFR <- randomForest::randomForest(
  K ~ NDVI + NDRE + SAVI + ARVI + GNDVI,
  data = trainDat,
  importance = T,
  ntree = 500,
  mtry = 3
)

print(K_RFR)      # randomForest model print
plot(K_RFR)       # randomForest model plot
varImpPlot(K_RFR) # variable importance plot

# Mg RFR
Mg_RFR <- randomForest::randomForest(
  Mg ~ NDVI + NDRE + SAVI + ARVI + GNDVI,
  data = trainDat,
  importance = T,
  ntree = 500,
  mtry = 3
)

print(Mg_RFR)      # randomForest model print
plot(Mg_RFR)       # randomForest model plot
varImpPlot(Mg_RFR) # variable importance plot

# MODEL PREDICTION --------------------------------------------------------
N_Pred <- predict(N_RFR, testDat)
P_Pred <- predict(P_RFR, testDat)
K_Pred <- predict(K_RFR, testDat)
Mg_Pred <- predict(Mg_RFR, testDat)

# MODEL VALIDATION --------------------------------------------------------
# The lower value of RMSE preferred the better model performance, conversely, 
# the higher value of R2 (closer to 1) shows that the regression line fits the
# data well and the model performance is better 

# 1. RMSE: Root Mean Squared Error
rmse_N <- caret::RMSE(N_Pred, as.numeric(testDat$N))
rmse_P <- caret::RMSE(P_Pred, as.numeric(testDat$P))
rmse_K <- caret::RMSE(K_Pred, as.numeric(testDat$K))
rmse_Mg <- caret::RMSE(Mg_Pred, as.numeric(testDat$Mg))

print(paste("RMSE N; P; K; Mg:", rmse_N,
            ";", rmse_P,
            ";", rmse_K,
            ";", rmse_Mg))

# 2. R2: R-Squared
R2_N <- caret::R2(N_Pred, as.numeric(testDat$N))
R2_P <- caret::R2(P_Pred, as.numeric(testDat$P))
R2_K <- caret::R2(K_Pred, as.numeric(testDat$K))
R2_Mg <- caret::R2(Mg_Pred, as.numeric(testDat$Mg))

print(paste("R2 N; P; K; Mg:", R2_N,
            ";", R2_P,
            ";", R2_K,
            ";", R2_Mg))

# done !!! ----------------------------------------------------------------







