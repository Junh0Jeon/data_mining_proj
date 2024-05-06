rm(list=ls())

####
## Work Directory
if(sum(dir("C:/Users/") %in% "p5682") == 1) {
    setwd("C:/Users/p5682/Desktop/R_cloud")
} else if (sum(dir("C:/Users") %in% "JeonJunho") == 1) {
    setwd("C:/Users/JeonJunho/Desktop/R_cloud")
} else {
    print("Set right or new directory")
}
dir()
#####
## Supplies
#####
# source("./np_supp.R")
# source("./Selfmade_functions.R")
#####
## Packages
##
##### Package Bundles
# install.packages("tidyverse")
# install.packages("car")
# install.packages("MASS")
# install.packages("caret")
##### R function extention Packages
# install.packages("psych")
# install.packages("gtools")
# install.packages("Matrix")
# install.packages("installr")
# install.packages("WRS") # WRS should install from github
# install.packages("WRS2")
# install.packages("ks")
# install.packages("forecast")
# install.packages("hash")
##### Extra Distribution Packages
# install.packages("SuppDists")
# install.packages("extraDistr")
# install.packages("Rfit")
# install.packages("weibullness")
###### Test Packages
# install.packages("BSDA")
# install.packages("nortest")
# install.packages("moments")
# install.packages("DescTools")
# install.packages("nptest")
##### Occasionally Used Packages
# install.packages("geosphere")
# install.packages("maps")
# install.packages("lubridate")
##### Visualization Packages
# install.packages("DT")
# install.packages("showtext")
# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("latex2exp")
# install.packages("xtable")
# install.packages("GGally")
# install.packages("plotly")
# install.packages("ggfortify")

##### Extra Datapack Packages
# install.packages("alr4")
# install.packages("palmerpenguins")
# install.packages("faraway")
##### Web Crawling
# install.packages("rvest")
# install.packages("readtext")
# install.packages("webdriver")
# install.packages("flextable")
# install.packages("httr")
# install.packages("urltools")
#####
## Library
##
##### Package Bundles
library(tidyverse)
library(car) # Boot, Anova, qqPlot,.... # Functions from "R Companion to Applied Regression"
library(MASS) # boxcox, stures, ... # Functions from "Mordern Applied Statistics with S"
library(caret) # ML
##### R function extention Packages
library(psych) # describe
library(gtools) # gegen, gduplicates,...(optimized function)
library(Matrix) # Diagonal, rankMatrix,... (extra Matrix function)
library(installr) # install.github, updateR,... (extra install function)
# library(WRS) # tsgreg, tsreg,... (Robust Statistical Methods) 
library(WRS2) # mest, ancova, Qanova,... (A Collection of Robust Statistical Methods)
library(ks) # Hpi, kcde, kde, kfe,.... (Kernal Smoothing Packages)
library(forecast) # BoxCox, CV, ndiff,... (control arima model)
library(hash)
##### Extra Distribution Packages
library(SuppDists) # dFriedman, dKendall,...
library(extraDistr)
library(Rfit)
library(weibullness) # wp.test, weibull.mle # Goodness of Fit test for weibull distribution
###### Test Packages
library(BSDA) # sign.test
library(nortest) # ad.test, pearson.test,...
library(moments) # skewness, agostino.test, anscombe.test,..
library(DescTools) # SigegelTukeyTest,...
library(nptest) # np.boot,.. # Nonparametric Bootstrap and Permutation Tests
##### Occasionally Used Packages
library(geosphere) # distGeo
library(maps) # map, map.cities, ...
library(lubridate) # as_date,...
##### Visualization Packages
library(DT) # datatable
library(showtext) # set my own text
library(gridExtra) # grid.arrange,...
library(cowplot) # plot.grid,.. # Help data visualization
library(latex2exp) # latex2exp_supported,... # Help to use LaTeX expression
library(xtable) # xtable,... # Visualize table using LaTeX
library(GGally) # ggpairs,... # Extra ggplot function
library(plotly) # plotly # 3d plot
library(ggfortify) # Now ggplot2 can interpret Time Series
##### Extra Datapack Packages
# library(alr4) # Accompany linear regression data
# library(palmerpenguins) # Iris replaceable data
# library(faraway) # Data from books " Linear Models with R" 
##### Web Crawling
library("rvest")
library("readtext")
library("webdriver")
library("flextable")
library("httr")
library("urltools")
#####
search()

# ls(package:cowplot)

##################################################
# Use the following steps to install `WRS` package
# (Visit https://github.com/nicebread/WRS )
# # first: install dependent packages
# install.packages(c("MASS", "akima", "robustbase"))
# install.packages(c("akima", "robustbase"))
# 
# # second: install suggested packages
# install.packages(c("akima", "cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "mc2d", "psych", "Rfit", "DepthProc", "class", "fda", "rankFD"))
# install.packages()
# 
# # third: install an additional package which provides some C functions
# # install.packages("devtools")
# # NOTE: This seems to be stalled and not functional any more
# # devtools::install_github("mrxiaohe/WRScpp")
# 
# # fourth: install WRS
# devtools::install_github("nicebread/WRS", subdir="pkg")
#################################################
## install latest R Code
# installr::check.for.updates.R()
# installr::install.R()