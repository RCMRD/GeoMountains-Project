    ## biomod2 video: Single species modelling ----
## Required data are available at:
## https://github.com/biomodhub/biomod2-tutorial-material/raw/master/biomod2_video_single_species_modelling.zip

## setting up the environment ----
getwd()
setwd('C:/Users/USER/Desktop/Anita2023/speciesModeling/changesSingleSpecies/geoMountains/workdir')
#setwd('C:/Users/HP 1030/Documents/Desktop/speciesModeling/changesSingleSpecies/geoMountains/workdir')



#********************************************************************************************************************************************************************
#----------------------------- INSTALLING THE REQUIRED PACKAGES
#********************************************************************************************************************************************************************
## Installing the latest release of biomod2
#install.packages('devtools')
#devtools::install_github('biomodhub/biomod2')
#install.packages('biomod2')
#install.packages('gridExtra')
#install.packages('raster')
#install.packages("lattice")
#install.packages('rasterVis')
#install.packages('tidyterra')
#install.packages('ggtext')
#install.packages('mapview')
#-----------------------------Suppressing package startup messages
#suppressPackageStartupMessages()




#********************************************************************************************************************************************************************
#----------------------------- LOADING THE REQUIRED PACKAGES
#********************************************************************************************************************************************************************
library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)
#Added libraries
library(tidyterra)
library(ggtext)
library(mapview)
library(sf)




#********************************************************************************************************************************************************************
#----------------------------- READING DATA
#********************************************************************************************************************************************************************
## Reading data ----
Opuntia_occ <- read.csv('../data/opuntia.csv')
summary(Opuntia_occ)
# Specify the path to the AOI shapefile
aoiPath <- c("C:/Users/USER/Desktop/Anita2023/speciesModeling/changesSingleSpecies/geoMountains/data/conservancyArea")
#aoiPath <- c("C:/Users/HP 1030/Documents/Desktop/speciesModeling/changesSingleSpecies/geoMountains/data/conservancyArea")
# Read the shapefile using the sf package
conservancyArea <- st_read(aoiPath)




#********************************************************************************************************************************************************************
#-----------------------------LISTING RELEVANT FILES
#********************************************************************************************************************************************************************
# List all ASC files in the respective directories
asc_Current <- list.files("../data/current_1960_1990_ASC", pattern = ".asc", full.names = TRUE)
asc_Future_2.6 <- list.files("../data/worldclim_KE", pattern = "worldclim_KE_2050_BC26_", full.names = TRUE)
asc_Future_8.5 <- list.files("../data/worldclim_KE", pattern = "worldclim_KE_2050_BC85_", full.names = TRUE)




#********************************************************************************************************************************************************************
#-----------------------------RUNNING AN INITIAL MODEL
#********************************************************************************************************************************************************************
# 1) Initialize an empty raster stack
bioclim_KE_sub <- stack()


# 2)  Loop through the list of ASC files and stack them
for (asc_file in asc_Current) {
                                  # Read each ASC file and add it to the stack
                                  raster_data <- raster(asc_file)
                                  # Add the raster to the stack
                                  bioclim_KE_sub <- stack(bioclim_KE_sub, raster_data)
                              }


# 3) Call to print
bioclim_KE_sub


# 4) Format the data ----
Opuntia_data <- BIOMOD_FormatingData(
                                      resp.var = Opuntia_occ['Opuntia'],
                                      resp.xy = Opuntia_occ[, c('Long', 'Lat')],
                                      expl.var = bioclim_KE_sub,
                                      resp.name = "Opuntia",
                                      PA.nb.rep = 2,
                                      PA.nb.absences = 500,
                                      PA.strategy = 'random',
                                      filter.raster = TRUE
                                    )


# 5) Obtain the formatted object summary
Opuntia_data


# 6) Plot the selected pseudo-absences
plot(Opuntia_data)


# 7) Define individual models options ---- 
Opuntia_opt <- BIOMOD_ModelingOptions(
                                      GLM = list(type = 'quadratic', interaction.level = 1),
                                      GBM = list(n.trees = 1000),
                                      GAM = list(algo = 'GAM_mgcv')
                                     )


# 11) Run the individual models ----
Opuntia_models <- BIOMOD_Modeling(
                                  Opuntia_data,
                                  modeling.id = "demo1",
                                  models = c("GLM", "GBM", "RF", "GAM"),
                                  bm.options = Opuntia_opt,
                                  CV.nb.rep = 2,
                                  CV.perc = 0.8,
                                  var.import = 3
                                 )


# 12) Assessing individual models quality ----
# ---- Getting models evaluation scores
Opuntia_models_scores <- get_evaluations(Opuntia_models)
View(Opuntia_models_scores)

# 13) Define an output path for R outputs
outPath <- "../outputs"
# ---- Write the selected data to a CSV file under the specified directory
write.csv(Opuntia_models_scores, file.path(outPath, "csvs/Opuntia/Opuntia_models_scores.csv"), row.names = FALSE)


# 14) Getting the dimension array and the names contained within
dim(Opuntia_models_scores)
dimnames(Opuntia_models_scores)


# 15) Evaluating model evaluation scores through plot
#The closer you are to the upper right corner, the better your model is
plotByALGO <- bm_PlotEvalMean(
                                bm.out = Opuntia_models, 
                                metric.eval = c("ROC","TSS"), 
                                group.by = "algo", #Using the models
                                do.plot = TRUE,
                                xlim = c(0.5,1), 
                                ylim = c(0.5,1),
                             )
# Call the plot
plotByALGO                          
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/plotByALGO.png"), plot = plotByALGO$plot, width = 8, height = 6)
# Export the statistics as a csv file (e.g., CSV)
write.csv(plotByALGO$tab, file.path(outPath, "csvs/Opuntia/plotByALGO.csv"), row.names = FALSE)

plotByRUN <- bm_PlotEvalMean(
                                bm.out = Opuntia_models, 
                                metric.eval =  c("ROC","TSS"), 
                                group.by = "run" , #Using cross-validation run
                                do.plot = TRUE,
                                xlim = c(0.5,1), 
                                ylim = c(0.5,1)
                            )
# Call the plot
plotByRUN
# Export the plot as an image file (e.g., PNG)
ggsave("../outputs/plots/Opuntia/plotByRUN.png", plot = plotByRUN$plot, width = 8, height = 6)
# Export the statistics as a csv file (e.g., CSV)
write.csv(plotByRUN$tab, file.path(outPath, "csvs/Opuntia/plotByRUN.csv"), row.names = FALSE)

plotByPA <- bm_PlotEvalMean(
                              bm.out = Opuntia_models, 
                              metric.eval =  c("ROC","TSS"), 
                              group.by = "PA", #Using the pseudo absences dataset
                              do.plot = TRUE,
                              xlim = c(0.5,1), 
                              ylim = c(0.5,1)
                           ) #Absolutely no difference between the pseudo-absences is good for code
# Call the plot
plotByPA
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/plotByPA.png"), plot = plotByPA$plot, width = 8, height = 6)
# Export the statistics as a csv file (e.g., CSV)
write.csv(plotByPA$tab, file.path(outPath, "csvs/Opuntia/plotByPA.csv"), row.names = FALSE)


# 16) Checking variable importance
Opuntia_models_var_import <- get_variables_importance(Opuntia_models)
View(Opuntia_models_var_import)
# Export the statistics as a csv file (e.g., CSV)
write.csv(Opuntia_models_var_import, file.path(outPath, "csvs/Opuntia/Opuntia_models_variable_importance.csv"), row.names = FALSE)


# 17) Making the mean of variable importance by algorithm
columnNames <- colnames(Opuntia_models_var_import)
columnNames
mean_VariableImportance <- Opuntia_models_var_import %>%
                            group_by(algo, expl.var) %>%
                            summarize(Mean_Importance = mean(var.imp, na.rm = TRUE))
View(mean_VariableImportance)
# Export the statistics as a csv file (e.g., CSV)
write.csv(mean_VariableImportance, file.path(outPath, "csvs/Opuntia/Opuntia_models_mean_VariableImportance.csv"), row.names = FALSE)


# 18) individual models response plots
#---- Step 1: Subsetting by loading individual models
Opuntia_glm <- BIOMOD_LoadModels(Opuntia_models, algo='GLM')
Opuntia_gbm <- BIOMOD_LoadModels(Opuntia_models, algo='GBM')
Opuntia_rf <- BIOMOD_LoadModels(Opuntia_models, algo='RF')
Opuntia_gam <- BIOMOD_LoadModels(Opuntia_models, algo='GAM')
#---- Step 2: Calling plots
glm_eval_strip <- biomod2::bm_PlotResponseCurves(
                                                  bm.out = Opuntia_models,
                                                  models.chosen  = Opuntia_glm,
                                                  new.env = get_formal_data(Opuntia_models,'expl.var'), 
                                                  show.variables= get_formal_data(Opuntia_models,'expl.var.names'),
                                                  fixed.var = 'median',
                                                  do.bivariate = FALSE,
                                                  do.plot = TRUE,
                                                  #Add legend only for univariate cases
                                                  main = 'Generalized Linear Model',
                                                  data_species = get_formal_data(Opuntia_models,'resp.var')
                                                )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/Generalized Linear Model.png"), plot = glm_eval_strip$plot, width = 12, height = 8)

gbm_eval_strip <- biomod2::bm_PlotResponseCurves(
                                                  bm.out = Opuntia_models,
                                                  models.chosen = Opuntia_gbm,
                                                  new.env = get_formal_data(Opuntia_models,'expl.var'), 
                                                  show.variables= get_formal_data(Opuntia_models,'expl.var.names'),
                                                  fixed.var = 'median',
                                                  do.bivariate = FALSE,
                                                  do.plot = TRUE,
                                                  main = 'Generalized Boosted Regression Model',
                                                  data_species = get_formal_data(Opuntia_models,'resp.var')
                                                )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/Generalized Boosted Regression Model.png"), plot = gbm_eval_strip$plot, width = 12, height = 8)

rf_eval_strip <- biomod2::bm_PlotResponseCurves(
                                                bm.out = Opuntia_models,
                                                models.chosen = Opuntia_rf,
                                                new.env = get_formal_data(Opuntia_models,'expl.var'), 
                                                show.variables= get_formal_data(Opuntia_models,'expl.var.names'),
                                                fixed.var = 'median',
                                                do.bivariate = FALSE,
                                                do.plot = TRUE,
                                                main = 'Random Forest Model',
                                                data_species = get_formal_data(Opuntia_models,'resp.var')
                                               )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/Random Forest Model.png"), plot = rf_eval_strip$plot, width = 12, height = 8)

gam_eval_strip <- biomod2::bm_PlotResponseCurves(
                                                  bm.out = Opuntia_models,
                                                  models.chosen  = Opuntia_gam,
                                                  new.env = get_formal_data(Opuntia_models,'expl.var'), 
                                                  show.variables= get_formal_data(Opuntia_models,'expl.var.names'),
                                                  fixed.var = 'median',
                                                  do.bivariate = FALSE,
                                                  do.plot = TRUE,
                                                  main = 'Generalized Additive Model',
                                                  data_species = get_formal_data(Opuntia_models,'resp.var')
                                                )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/Generalized Additive Model.png"), plot = gam_eval_strip$plot, width = 12, height = 8)


# 19) Running the ensemble models ----
Opuntia_ensemble_models <- BIOMOD_EnsembleModeling(
                                                    bm.mod = Opuntia_models,
                                                    models.chosen = 'all',
                                                    em.by = 'all',
                                                    metric.select = c('TSS'),
                                                    metric.select.thresh = c(0.8),
                                                    metric.eval = c('TSS', 'ROC'),
                                                    var.import = 0,
                                                    EMwmean.decay = 'proportional',
                                                    em.algo = c('EMmean', 'EMcv', 'EMca', 'EMwmean')
                                                    #EMcv Estimates the coefficient of variation across predictions
                                                    #EMca Estimates the committee averaging across predictions
                                                    #EMwmean Estimates the weighted sum of probabilities
                                                  )
# Check to see whether any models failed
Opuntia_ensemble_models

# Assess ensemble models quality ----
Opuntia_ensemble_models_scores <- get_evaluations(Opuntia_ensemble_models)
View(Opuntia_ensemble_models_scores)




#********************************************************************************************************************************************************************
#-----------------------------RUNNING A SELECTED MODEL
#********************************************************************************************************************************************************************
# 1) Stack the selected .ASC files
bioclim_KE_selection <- stack(
                              c(
                                bio15 = '../data/current_1960_1990_ASC/worldclim_KE_bio15.asc',
                                bio4 = '../data/current_1960_1990_ASC/worldclim_KE_bio4.asc',
                                bio7  = '../data/current_1960_1990_ASC/worldclim_KE_bio7.asc',                               
                                bio6 = '../data/current_1960_1990_ASC/worldclim_KE_bio6.asc'
                              )
                             )


# 2) Call to print
bioclim_KE_selection


# 3) Format the data ----
Opuntia_data_1.0 <- BIOMOD_FormatingData(
                                          resp.var = Opuntia_occ['Opuntia'],
                                          resp.xy = Opuntia_occ[, c('Long', 'Lat')],
                                          expl.var = bioclim_KE_selection,
                                          resp.name = "Opuntia",
                                          PA.nb.rep = 3,
                                          PA.nb.absences = 500,
                                          PA.strategy = 'random',
                                          filter.raster = TRUE
                                        )


# 4) Obtain the formatted object summary
Opuntia_data_1.0


# 5) Plot the selected pseudo-absences
plot(Opuntia_data_1.0)


# ---- Define individual models options (otherwise, redefine) ---- 


# 6) Run the individual models ----
Opuntia_models_selection <- BIOMOD_Modeling(
                                              Opuntia_data_1.0,
                                              modeling.id = "demo2",
                                              models = c("RF", "GBM"),
                                              bm.options = Opuntia_opt,
                                              CV.nb.rep = 3,
                                              CV.perc = 0.8,
                                              var.import = 3 
                                           )


# 7) Assessing individual models quality ----
# ---- Getting models evaluation scores
Opuntia_models_selection_scores <- get_evaluations(Opuntia_models_selection)
View(Opuntia_models_selection_scores)


#---- The Definition of an output path for R outputs is already defined (otherwise, redefine) -----


# ---- Write the selected data to a CSV file under the specified directory
write.csv(Opuntia_models_selection_scores, file.path(outPath, "csvs/Opuntia/Opuntia_models_selection_scores.csv"), row.names = FALSE)


# 8) Getting the dimension array and the names contained within
dim(Opuntia_models_selection_scores)
dimnames(Opuntia_models_selection_scores)


# 9) Evaluating model evaluation scores through plot
#The closer you are to the upper right corner, the better your model is
plotSelectByALGO <- bm_PlotEvalMean(
                                      bm.out = Opuntia_models_selection, 
                                      metric.eval = c("ROC","TSS"), 
                                      group.by = "algo", #Using the models
                                      do.plot = TRUE,
                                      xlim = c(0.5,1), 
                                      ylim = c(0.5,1),
                                   )
# Call the plot
plotSelectByALGO                          
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/plotSelectByALGO.png"), plot = plotSelectByALGO$plot, width = 8, height = 6)
# Export the statistics as a csv file (e.g., CSV)
write.csv(plotSelectByALGO$tab, file.path(outPath, "csvs/Opuntia/plotSelectByALGO.csv"), row.names = FALSE)

plotSelectByRUN <- bm_PlotEvalMean(
                                      bm.out = Opuntia_models_selection, 
                                      metric.eval =  c("ROC","TSS"), 
                                      group.by = "run" , #Using cross-validation run
                                      do.plot = TRUE,
                                      xlim = c(0.5,1), 
                                      ylim = c(0.5,1)
                                  )
# Call the plot
plotSelectByRUN
# Export the plot as an image file (e.g., PNG)
ggsave("../outputs/plots/Opuntia/plotSelectByRUN.png", plot = plotSelectByRUN$plot, width = 8, height = 6)
# Export the statistics as a csv file (e.g., CSV)
write.csv(plotSelectByRUN$tab, file.path(outPath, "csvs/Opuntia/plotSelectByRUN.csv"), row.names = FALSE)

plotSelectByPA <- bm_PlotEvalMean(
                                    bm.out = Opuntia_models_selection, 
                                    metric.eval =  c("ROC","TSS"), 
                                    group.by = "PA", #Using the pseudo absences dataset
                                    do.plot = TRUE,
                                    xlim = c(0.5,1), 
                                    ylim = c(0.5,1)
                                 ) #Absolutely no difference between the pseudo-absences is good for code
# Call the plot
plotSelectByPA
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/plotSelectByPA.png"), plot = plotSelectByPA$plot, width = 8, height = 6)
# Export the statistics as a csv file (e.g., CSV)
write.csv(plotSelectByPA$tab, file.path(outPath, "csvs/Opuntia/plotSelectByPA.csv"), row.names = FALSE)


# 10) Checking variable importance
Opuntia_models_selection_var_import <- get_variables_importance(Opuntia_models_selection)
View(Opuntia_models_selection_var_import)
# Export the statistics as a csv file (e.g., CSV)
write.csv(Opuntia_models_selection_var_import, file.path(outPath, "csvs/Opuntia/Opuntia_models_selection_variable_importance.csv"), row.names = FALSE)


# 11) Making the mean of variable importance by algorithm
columnNames_1.0 <- colnames(Opuntia_models_selection_var_import)
columnNames_1.0
mean_VariableImportance_selection <- Opuntia_models_selection_var_import %>% group_by(algo, expl.var) %>%
                                                  summarize(Mean_Importance = mean(var.imp, na.rm = TRUE))
View(mean_VariableImportance_selection)
# Export the statistics as a csv file (e.g., CSV)
write.csv(mean_VariableImportance_selection, file.path(outPath, "csvs/Opuntia/Opuntia_models_selection_mean_VariableImportance.csv"), row.names = FALSE)


# 12) individual models response plots
#---- Step 1: Subsetting by loading individual models
Opuntia_selection_rf <- BIOMOD_LoadModels(Opuntia_models_selection, algo='RF')
Opuntia_selection_gbm <- BIOMOD_LoadModels(Opuntia_models_selection, algo='GBM')

#---- Step 2: Calling plots (ordered)
rf_eval_selection <- biomod2::bm_PlotResponseCurves(
                                                      bm.out = Opuntia_models_selection,
                                                      models.chosen  = Opuntia_selection_rf,
                                                      new.env = get_formal_data(Opuntia_models_selection,'expl.var'), 
                                                      show.variables= get_formal_data(Opuntia_models_selection,'expl.var.names'),
                                                      fixed.var = 'median',
                                                      do.bivariate = FALSE,
                                                      do.plot = TRUE,
                                                      #Add legend only for univariate cases
                                                      main = 'Random Forest Model',
                                                      data_species = get_formal_data(Opuntia_models_selection,'resp.var')
                                                    )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/Random Forest Model (selection).png"), plot = rf_eval_selection$plot, width = 12, height = 8)

gbm_eval_selection <- biomod2::bm_PlotResponseCurves(
                                                      bm.out = Opuntia_models_selection,
                                                      models.chosen  = Opuntia_selection_rf,
                                                      new.env = get_formal_data(Opuntia_models_selection,'expl.var'), 
                                                      show.variables= get_formal_data(Opuntia_models_selection,'expl.var.names'),
                                                      fixed.var = 'median',
                                                      do.bivariate = FALSE,
                                                      do.plot = TRUE,
                                                      #Add legend only for univariate cases
                                                      main = 'Generalized Boosted Regression Model',
                                                      data_species = get_formal_data(Opuntia_models_selection,'resp.var')
                                                    )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/Generalized Boosted Regression Model (selection).png"), plot = gbm_eval_selection$plot, width = 12, height = 8)


# 13) Running the ensemble models ----
Opuntia_ensemble_models_1.0 <- BIOMOD_EnsembleModeling(
                                                        bm.mod = Opuntia_models_selection,
                                                        models.chosen = 'all',
                                                        em.by = 'all',
                                                        metric.select = c('TSS'),
                                                        metric.select.thresh = c(0.8),
                                                        metric.eval = c('TSS', 'ROC'),
                                                        var.import = 0,
                                                        EMwmean.decay = 'proportional',
                                                        em.algo = c('EMmean', 'EMcv', 'EMca', 'EMwmean')
                                                      )
# Check to see whether any models failed
Opuntia_ensemble_models_1.0

# Assess ensemble models quality ----
Opuntia_ensemble_models_scores_1.0 <- get_evaluations(Opuntia_ensemble_models_1.0)
View(Opuntia_ensemble_models_scores_1.0)


# 14) Proceed & Do models projections ----
#**********A. current projections**************************************************************
Opuntia_models_proj_current <- BIOMOD_Projection(
                                                  bm.mod = Opuntia_models_selection,
                                                  proj.name = "current",
                                                  new.env = bioclim_KE_selection,
                                                  metric.binary = "TSS",
                                                  do.stack = FALSE,
                                                  output.format = ".img"
                                                )

Opuntia_ensemble_models_proj_current <- BIOMOD_EnsembleForecasting(
                                                                    bm.em = Opuntia_ensemble_models_1.0,
                                                                    bm.proj = Opuntia_models_proj_current,
                                                                    metric.binary = "TSS",
                                                                    do.stack = FALSE,
                                                                    output.format = ".img"
                                                                  )

#**********B. future projections**************************************************************
#------------------------------------------------------------------------------
## Load 2050 bioclim variables (RCP 2.6)
bioclim_KE_2050_BC26 <- stack(
                              c(
                                bio15 = '../data/worldclim_KE/worldclim_KE_2050_BC26_Bio15.asc',
                                bio4 = '../data/worldclim_KE/worldclim_KE_2050_BC26_Bio4.asc',
                                bio7  = '../data/worldclim_KE/worldclim_KE_2050_BC26_Bio7.asc',                             
                                bio6 = '../data/worldclim_KE/worldclim_KE_2050_BC26_Bio6.asc'
                              )
                             )

Opuntia_models_proj_2050_BC26 <- BIOMOD_Projection(
                                                    bm.mod = Opuntia_models_selection,
                                                    proj.name = "2050_BC26",
                                                    new.env = bioclim_KE_2050_BC26,
                                                    metric.binary = "TSS",
                                                    do.stack = FALSE,
                                                    output.format = ".img"
                                                  )

Opuntia_ensemble_models_proj_2050_BC26 <- BIOMOD_EnsembleForecasting(
                                                                      bm.em = Opuntia_ensemble_models_1.0,
                                                                      bm.proj = Opuntia_models_proj_2050_BC26,
                                                                      metric.binary = "TSS",
                                                                      do.stack = FALSE,
                                                                      output.format = ".img"
                                                                    )

## check how projections looks like
plot(Opuntia_ensemble_models_proj_2050_BC26, str.grep = "EMca|EMwmean")
# Call the plot as a variable
emCAmeanWM_2.6 <- plot(Opuntia_ensemble_models_proj_2050_BC26, str.grep = "EMca|EMwmean")
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/RCP 2.6 (caMEANwm).png"), plot = emCAmeanWM_2.6, width = 12, height = 8)

#------------------------------------------------------------------------------
## Load 2050 bioclim variables (RCP 8.5)
bioclim_KE_2050_BC85 <- stack(
                              c(
                                bio15 = '../data/worldclim_KE/worldclim_KE_2050_BC85_Bio15.asc',
                                bio4 = '../data/worldclim_KE/worldclim_KE_2050_BC85_Bio4.asc',
                                bio7  = '../data/worldclim_KE/worldclim_KE_2050_BC85_Bio7.asc',                               
                                bio6 = '../data/worldclim_KE/worldclim_KE_2050_BC85_Bio6.asc'
                              )
                             )

Opuntia_models_proj_2050_BC85 <- BIOMOD_Projection(
                                                    bm.mod = Opuntia_models_selection,
                                                    proj.name = "2050_BC85",
                                                    new.env = bioclim_KE_2050_BC85,
                                                    metric.binary = "TSS",
                                                    do.stack = FALSE,
                                                    output.format = ".img"
                                                  )


Opuntia_ensemble_models_proj_2050_BC85 <- BIOMOD_EnsembleForecasting(
                                                                      bm.em = Opuntia_ensemble_models_1.0,
                                                                      bm.proj = Opuntia_models_proj_2050_BC85,
                                                                      metric.binary = "TSS",
                                                                      do.stack = FALSE,
                                                                      output.format = ".img"
                                                                    )

## check how projections looks like
plot(Opuntia_ensemble_models_proj_2050_BC85, str.grep = "EMca|EMwmean")
# Call the plot as a variable
emCAmeanWM_8.5 <- plot(Opuntia_ensemble_models_proj_2050_BC85, str.grep = "EMca|EMwmean")
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/RCP 8.5 (caMEANwm).png"), plot = emCAmeanWM_8.5, width = 12, height = 8)


# 15) Compute Species Range Change (SRC) ----
## load binary projections
Opuntia_bin_proj_current <- stack( 
                                  c(
                                    ca = "Opuntia/proj_current/individual_projections/Opuntia_EMcaByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img",
                                    mn = "Opuntia/proj_current/individual_projections/Opuntia_EMmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img",
                                    wm = "Opuntia/proj_current/individual_projections/Opuntia_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img"
                                  )
                                )

Opuntia_bin_proj_2050_BC26 <- stack( 
                                    c(
                                      ca = "Opuntia/proj_2050_BC26/individual_projections/Opuntia_EMcaByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img",
                                      mn = "Opuntia/proj_2050_BC26/individual_projections/Opuntia_EMmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img",
                                      wm = "Opuntia/proj_2050_BC26/individual_projections/Opuntia_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img"
                                    )
                                  )

Opuntia_bin_proj_2050_BC85 <- stack( 
                                    c(
                                      ca = "Opuntia/proj_2050_BC85/individual_projections/Opuntia_EMcaByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img",
                                      mn = "Opuntia/proj_2050_BC85/individual_projections/Opuntia_EMmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img",
                                      wm = "Opuntia/proj_2050_BC85/individual_projections/Opuntia_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img"
                                    )
                                  )


# 16) Check for consistency in dimensions (extent & resolution)
# Step 1 - Perform a full check the resolution & dimensions
Opuntia_bin_proj_current #Full check
Opuntia_bin_proj_2050_BC26 #Full check
# Step 2 - Perform a dimensions check
dim(Opuntia_bin_proj_current)
dim(Opuntia_bin_proj_2050_BC26)
# Step 3 - Perform a resolution check
res(Opuntia_bin_proj_current)
res(Opuntia_bin_proj_2050_BC26)
# Step 4 - check the extent by min & max values
Opuntia_bin_proj_current@extent
extent(Opuntia_bin_proj_2050_BC26)


# 17) Reset the extent & resolution
# Step 1 - Crop the current raster stack to fit in the X and Y limits of the 2050 2.6 dataset
Opuntia_bin_proj_current_cropped <- crop(Opuntia_bin_proj_current, extent(Opuntia_bin_proj_2050_BC26))
# Step 2 - Set the resolution of the current stack to match that of the projected stack
res(Opuntia_bin_proj_current_cropped) <- res(Opuntia_bin_proj_2050_BC26)
# Step 3 - Set the extent of the current stack to match that of the projected stack
extent(Opuntia_bin_proj_current_cropped) <- extent(Opuntia_bin_proj_2050_BC26)
# Step 4 - Copy the values from current stack (retention of data)
Opuntia_bin_proj_current_cropped[] <- Opuntia_bin_proj_current_cropped[]
Opuntia_bin_proj_current_matched <- Opuntia_bin_proj_current_cropped


# 18) Compute the RangeSize
#---------SRC current -> 2050 (RCP 2.6)
SRC_current_2050_BC26 <- BIOMOD_RangeSize(
                                          Opuntia_bin_proj_current_matched,
                                          Opuntia_bin_proj_2050_BC26
                                         )
View(SRC_current_2050_BC26$Compt.By.Models)
write.csv(SRC_current_2050_BC26$Compt.By.Models, file.path(outPath, "csvs/Opuntia/SRC_current_2050_BC26-Compt.By.Models.csv"), row.names = FALSE)

#---------SRC current -> 2050 (RCP 8.5)
SRC_current_2050_BC85 <- BIOMOD_RangeSize(
                                          Opuntia_bin_proj_current_matched,
                                          Opuntia_bin_proj_2050_BC85
                                         )
View(SRC_current_2050_BC85$Compt.By.Models)
write.csv(SRC_current_2050_BC85$Compt.By.Models, file.path(outPath, "csvs/Opuntia/SRC_current_2050_BC85-Compt.By.Models.csv"), row.names = FALSE)


# 19) Calling plots
plot(SRC_current_2050_BC26$Diff.By.Pixel)
plot(SRC_current_2050_BC85$Diff.By.Pixel)


# 20) Checking the naming convention
names(SRC_current_2050_BC26$Diff.By.Pixel)
names(SRC_current_2050_BC85$Diff.By.Pixel)


# 21) RCP 2.6 vs RCP 8.5
#-------A) RCP 2.6
#---------------Re-assignment of new names
names(SRC_current_2050_BC26$Diff.By.Pixel)<- c("ca cur-2050 (2.6)", "mn cur-2050 (2.6)", "wm cur-2050 (2.6)")
#---------------Recalling of plot
diffByPixel2.6 <- plot(SRC_current_2050_BC26$Diff.By.Pixel)
#---------------Exporting the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/RCP 2.6 (DiffByPixel).png"), plot = diffByPixel2.6, width = 8, height = 6)

#-------B) RCP 8.5
#---------------Re-assignment of new names
names(SRC_current_2050_BC85$Diff.By.Pixel)<- c("ca cur-2050 (8.5)", "mn cur-2050 (8.5)", "wm cur-2050 (8.5)")
diffByPixel8.5 <- plot(SRC_current_2050_BC85$Diff.By.Pixel)
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/RCP 8.5 (DiffByPixel).png"), plot = diffByPixel8.5, width = 8, height = 6)


# 22) This function represents species range change from object 
#install.packages('ggpubr')
library(ggpubr)
#-------A) RCP 2.6
speciesRangeChangePlots2.6 <- bm_PlotRangeSize(
                                                SRC_current_2050_BC26,
                                                do.count = TRUE,#A logical value defining whether the count plot is to be computed or not
                                                do.perc = TRUE, #A logical value defining whether the percentage plot is to be computed or not
                                                do.maps = TRUE, #A logical value defining whether the maps plot is to be computed or not
                                                do.mean = TRUE, #A logical value defining whether the mean maps plot is to be computed or not
                                                do.plot = TRUE, #A logical value defining whether the plots are to be rendered or not
                                                row.names = c("ca")
                                              )
#-------Only 3 plots were returned ('do.mean' is only available if several maps are provided)
#-----------Export each plot 
#**************(ggsave fails to work on the called plots as bm_PlotRangeSize fails to store the plots in a variable)
#**************The plots were thus exported manually from R
# Save each plot using ggsave
#-----------Count barplot represents the absolute number of locations (pixels) lost, stable and gained
##ggsave(file.path(outPath, "plots/Opuntia/speciesRangeChangeCountPlot2.6.png"), plot = speciesRangeChangePlots2.6$count, width = 8, height = 6, units = "in")
#-----------Percentage barplot represents percentage of locations (pixels) lost, stable, and the corresponding Species Range Change (PercGain - PercLoss)
##ggsave(file.path(outPath, "plots/Opuntia/speciesRangeChangePercentagePlot2.6.png"), plot = speciesRangeChangePlots2.6$percentage, width = 8, height = 6, units = "in")
#-----------SRC models maps representing spatially locations (pixels) lost, stable and gained for each single distribution model
##ggsave(file.path(outPath, "plots/Opuntia/speciesRangeChangeMapsPlot2.6.png"), plot = speciesRangeChangePlots2.6$maps, width = 8, height = 6, units = "in")
#-----------SRC community averaging maps represent spatially locations (pixels) lost, stable and gained, taking the majoritary value across single distribution models (and representing the percentage of models' agreement)

#-------B) RCP 8.5
bm_PlotRangeSize(
                  SRC_current_2050_BC85,
                  do.count = TRUE,#A logical value defining whether the count plot is to be computed or not
                  do.perc = TRUE, #A logical value defining whether the percentage plot is to be computed or not
                  do.maps = TRUE, #A logical value defining whether the maps plot is to be computed or not
                  do.mean = FALSE, #A logical value defining whether the mean maps plot is to be computed or not
                  do.plot = TRUE, #A logical value defining whether the plots are to be rendered or not
                  row.names = c("ca")
                )
#**************The plots were thus exported manually from R
#--When exporting on R, save the 3 PNG files as;
# countPlot = 'speciesRangeChangeCountPlot8.5'
# percentagePlot = 'speciesRangeChangePercentagePlot8.5'
# mapsPlot = 'speciesRangeChangeMapsPlot8.5'


# 22) Mean evaluation scores (and their standard deviation) of species distribution models
#-------A) Using the calibration dataset and grouping by;
#----------a) Full Name
meanEvalScores_caliFN <- bm_PlotEvalMean(
                                          Opuntia_models_selection,
                                          metric.eval = NULL,
                                          dataset = "calibration",
                                          group.by =  'full.name',
                                          do.plot = TRUE,
                                          main = 'Calibration dataset by full name'
                                        )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/meanEvalScores_caliFN.png"), plot = meanEvalScores_caliFN$plot, width = 8, height = 6)

#----------b) Algorithm
meanEvalScores_caliAlgo <- bm_PlotEvalMean(
                                          Opuntia_models_selection,
                                          metric.eval = NULL,
                                          dataset = "calibration",
                                          group.by =  'algo',
                                          do.plot = TRUE,
                                          main = 'Calibration dataset by algorithm'
                                        )               
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/meanEvalScores_caliAlgo.png"), plot = meanEvalScores_caliAlgo$plot, width = 8, height = 6)

#-------B) Using the validation dataset and grouping by;
#----------a) Full Name
meanEvalScores_valiFN <- bm_PlotEvalMean(
                                          Opuntia_models_selection,
                                          metric.eval = NULL,
                                          dataset = "validation",
                                          group.by = "full.name",
                                          do.plot = TRUE,
                                          main = 'Validation dataset by full name'
                                        )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/meanEvalScores_valiFN.png"), plot = meanEvalScores_valiFN$plot, width = 8, height = 6)

#----------b) Algorithm
meanEvalScores_valiAlgo <- bm_PlotEvalMean(
                                            Opuntia_models_selection,
                                            metric.eval = NULL,
                                            dataset = "validation",
                                            group.by = "algo",
                                            do.plot = TRUE,
                                            main = 'Validation dataset by algorithm'
                                          )
# Export the plot as an image file (e.g., PNG)
ggsave(file.path(outPath, "plots/Opuntia/meanEvalScores_valiAlgo.png"), plot = meanEvalScores_valiAlgo$plot, width = 8, height = 6)


#install.packages('stars')
#install.packages("lwgeom")
library(mapview)
library(stars)
library(lwgeom)


# 23) Call the mapview function
mapview(SRC_current_2050_BC26$Diff.By.Pixel)
mapview(SRC_current_2050_BC85$Diff.By.Pixel)


# 24) Personalize the legend appearance with custom color codes
legend_options <- list(
                        position = "topright",
                        title = "Projected Species behaviour",
                        col.regions = c("#f03b20", "#fec44f", "#bcbddc", "#31a354"),  # Red, Yellow, Purple, Green
                        col.regions.title = "Loss-Gain-Stability",
                        values = c(-2, -1, 0, 1)
                      )

# 25) Create mapview with customized legend
#-------A) RCP 2.6
map2050rcp2.6 <- mapview(
                          SRC_current_2050_BC26$Diff.By.Pixel,
                          col.regions = legend_options$col.regions,
                          at = legend_options$at,
                          legend = TRUE,
                          legend.style = "classic",  # Use classic style for custom legend
                          legend.title.txt = 'RCP 2.6 (AOI)',
                          legend.values.raster = TRUE,
                          alpha.regions = 0.7,
                          position = legend_options$position
                        )
#--- Display the mapview on Viewer
map2050rcp2.6

#-------B) RCP 8.5
map2050rcp8.5 <- mapview(
                          SRC_current_2050_BC85$Diff.By.Pixel,
                          col.regions = legend_options$col.regions,
                          at = legend_options$at,
                          legend = TRUE,
                          legend.style = "classic",  # Use classic style for custom legend
                          legend.title.txt = 'RCP 8.5 (AOI)',
                          legend.values.raster = TRUE,
                          alpha.regions = 0.7,
                          position = legend_options$position
                        )
#--- Display the mapview on Viewer
map2050rcp8.5


# 270 Display both RCP plots on a single Mapview element
map2050rcps<- map2050rcp2.6 + map2050rcp8.5
map2050rcps
