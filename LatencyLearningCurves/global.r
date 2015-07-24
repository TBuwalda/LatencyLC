rm(list = setdiff(ls(), lsf.str()))
setwd("/Users/tabuwalda/Documents/2015LearnLabSummerSchool/ShinyApp/LatencyLearningCurves/")

library(lme4)
library(dplyr)
options(dplyr.width = Inf)

source("helpers.r")

load("./Data/listKCShiny.Rdat")
oldModels <- NULL

cat("\n\n\n")

