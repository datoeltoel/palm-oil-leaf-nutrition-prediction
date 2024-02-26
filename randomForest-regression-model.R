# -------------------------------------------------------------------------
# title       : Using Random Forest Regression for Estimate
#               Leaf Nutritions of Oil Palm Based on UAV Imagery
# data source : https://github.com/datoeltoel/palm-oil-leaf-nutrition-randomForest-model/blob/main/NDRE.csv
#               https://github.com/datoeltoel/palm-oil-leaf-nutrition-randomForest-model/blob/main/NDVI.csv
#               https://github.com/datoeltoel/palm-oil-leaf-nutrition-randomForest-model/blob/main/SAVI.csv
# author      : Ziyadatul Hikmah 
# date        : 21/02/2024
# -------------------------------------------------------------------------

# install and load packages -----------------------------------------------
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

# set working directory ---------------------------------------------------
setwd("D:/Ziy/Course GIS & RS/Drone/RFR")

# importing dataset into R ------------------------------------------------
# NDVI dataset
ndvi_dataset <- readr::read_csv("NDVI.csv")

# NDRE dataset
ndre_dataset <- readr::read_csv("NDRE.csv")

# SAVI dataset
savi_dataset <- readr::read_csv("SAVI.csv")

# split data  -------------------------------------------------------------
set.seed(500)

# split NDVI dataset
trainSamplesNDVI <- sample(1:nrow(ndvi_dataset), 0.75*nrow(ndvi_dataset))
trainDatNDVI <- ndvi_dataset[trainSamplesNDVI, ]
testDatNDVI <- ndvi_dataset[-trainSamplesNDVI, ]

# split NDRE dataset
trainSamplesNDRE <- sample(1:nrow(ndre_dataset), 0.75*nrow(ndre_dataset))
trainDatNDRE <- ndre_dataset[trainSamplesNDRE, ]
testDatNDRE <- ndre_dataset[-trainSamplesNDRE, ]

# split SAVI dataset
trainSamplesSAVI <- sample(1:nrow(savi_dataset), 0.75*nrow(savi_dataset))
trainDatSAVI <- savi_dataset[trainSamplesSAVI, ]
testDatSAVI <- savi_dataset[-trainSamplesSAVI, ]

# visualize relationships -------------------------------------------------
featurePlot(x = trainDatNDVI[, c("N","P","K","Mg","NDVI")], 
            y = factor(trainDatNDVI$Block), 
            plot = "pairs",
            auto.key = list(columns = 5))

# build random forest regression model ------------------------------------
# define variable as independent and dependent variables
varInd <- c("NDVI","NDRE","SAVI") # independent variables
varDep <- c("N","P","K","Mg")     # dependent variables

# NDVI RFR
ndviRFR <- randomForest::randomForest(
  NDVI ~ N + P + K + Mg,
  data = trainDatNDVI,
  importance = T,
  ntree = 500,
  mtry = 2
)

print(ndviRFR)      # randomForest model print
plot(ndviRFR)       # randomForest model plot
varImpPlot(ndviRFR) # variable importance plot

# NDRE RFR
ndreRFR <- randomForest::randomForest(
  NDRE ~ N + P + K + Mg,
  data = trainDatNDRE,
  importance = T,
  ntree = 500,
  mtry = 2
)

print(ndreRFR)      # randomForest model print
plot(ndreRFR)       # randomForest model plot
varImpPlot(ndreRFR) # variable importance plot

# NDVI RFR
saviRFR <- randomForest::randomForest(
  SAVI ~ N + P + K + Mg,
  data = trainDatSAVI,
  importance = T,
  ntree = 500,
  mtry = 2
)

print(saviRFR)      # randomForest model print
plot(saviRFR)       # randomForest model plot
varImpPlot(saviRFR) # variable importance plot

# model prediction --------------------------------------------------------
# NDVI RFR prediction
ndviPred <- predict(ndviRFR, testDatNDVI)

# NDRE RFR prediction
ndrePred <- predict(ndreRFR, testDatNDRE)

# SAVI RFR prediction
saviPred <- predict(saviRFR, testDatSAVI)

# model validation --------------------------------------------------------
# NDVI RFR validation -----------------------------------------------------
# 1. RMSE: Root Mean Squared Error
rmseNDVI_N <- caret::RMSE(ndviPred, as.numeric(testDatNDVI$N))
rmseNDVI_P <- caret::RMSE(ndviPred, as.numeric(testDatNDVI$P))
rmseNDVI_K <- caret::RMSE(ndviPred, as.numeric(testDatNDVI$K))
rmseNDVI_Mg <- caret::RMSE(ndviPred, as.numeric(testDatNDVI$Mg))

print(paste("Root Mean Squared Error (RMSE NDVI N; P; K; Mg):", rmseNDVI_N,
            ";", rmseNDVI_P,
            ";", rmseNDVI_K,
            ";", rmseNDVI_Mg
))

# 2. R2: R-Squared
R2NDVI_N <- caret::R2(ndviPred, as.numeric(testDatNDVI$N))
R2NDVI_P <- caret::R2(ndviPred, as.numeric(testDatNDVI$P))
R2NDVI_K <- caret::R2(ndviPred, as.numeric(testDatNDVI$K))
R2NDVI_Mg <- caret::R2(ndviPred, as.numeric(testDatNDVI$Mg))

print(paste("Coeficient Determination (R-squared NDVI N; P; K; Mg):", R2NDVI_N,
            ";", R2NDVI_P,
            ";", R2NDVI_K,
            ";", R2NDVI_Mg
))

# NDRE RFR validation -----------------------------------------------------
# 1. RMSE: Root Mean Squared Error
rmseNDRE_N <- caret::RMSE(ndrePred, as.numeric(testDatNDRE$N))
rmseNDRE_P <- caret::RMSE(ndrePred, as.numeric(testDatNDRE$P))
rmseNDRE_K <- caret::RMSE(ndrePred, as.numeric(testDatNDRE$K))
rmseNDRE_Mg <- caret::RMSE(ndrePred, as.numeric(testDatNDRE$Mg))

print(paste("Root Mean Squared Error (RMSE NDRE N; P; K; Mg):", rmseNDRE_N,
            ";", rmseNDRE_P,
            ";", rmseNDRE_K,
            ";", rmseNDRE_Mg
            ))

# 2. R2: R-Squared 
R2NDRE_N <- caret::R2(ndrePred, as.numeric(testDatNDRE$N))
R2NDRE_P <- caret::R2(ndrePred, as.numeric(testDatNDRE$P))
R2NDRE_K <- caret::R2(ndrePred, as.numeric(testDatNDRE$K))
R2NDRE_Mg <- caret::R2(ndrePred, as.numeric(testDatNDRE$Mg))

print(paste("Coeficient Determination (R-squared NDRE N; P; K; Mg):", R2NDRE_N,
            ";", R2NDRE_P,
            ";", R2NDRE_K,
            ";", R2NDRE_Mg
))

# SAVI RFR validation -----------------------------------------------------
# 1. RMSE: Root Mean Squared Error
rmseSAVI_N <- caret::RMSE(saviPred, as.numeric(testDatSAVI$N))
rmseSAVI_P <- caret::RMSE(saviPred, as.numeric(testDatSAVI$P))
rmseSAVI_K <- caret::RMSE(saviPred, as.numeric(testDatSAVI$K))
rmseSAVI_Mg <- caret::RMSE(saviPred, as.numeric(testDatSAVI$Mg))

print(paste("Root Mean Squared Error (RMSE SAVI N; P; K; Mg):", rmseSAVI_N,
            ";", rmseSAVI_P,
            ";", rmseSAVI_K,
            ";", rmseSAVI_Mg
))

# 2. R2: R-Squared
R2SAVI_N <- caret::R2(saviPred, as.numeric(testDatSAVI$N))
R2SAVI_P <- caret::R2(saviPred, as.numeric(testDatSAVI$P))
R2SAVI_K <- caret::R2(saviPred, as.numeric(testDatSAVI$K))
R2SAVI_Mg <- caret::R2(saviPred, as.numeric(testDatSAVI$Mg))

print(paste("Coeficient Determination (R-squared SAVI N; P; K; Mg):", R2SAVI_N,
            ";", R2SAVI_P,
            ";", R2SAVI_K,
            ";", R2SAVI_Mg
            ))

# done !!! ----------------------------------------------------------------









