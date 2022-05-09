#load("~/onedrive/Scenic/data/r/initial.RData")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

#--------------------------
# LIBRARIES
#--------------------------

library(tidyverse)
library(dplyr)
library(DT)
library(knitr)
library(stringr)
library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(chemometrics)
library(cluster)
library(factoextra)
library(FeatureImpCluster)
library(flexclust)
library(qpcR)
library(egg)

#--------------------------
# FUNCTIONS
#--------------------------
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

is.nan.data.frame <- function(x) {do.call(cbind, lapply(x, is.nan))} #function to replace NaNs



#---------------------------------------------------
# Getting data and initial clean/organising of data
#---------------------------------------------------

#raw31 <- read.csv("~/onedrive/Scenic/data/indexG220/2022-01-31.csv", comment.char="#", stringsAsFactors=FALSE)
#raw09 <- read.csv("~/onedrive/Scenic/data/indexG220/220209.csv", comment.char="#", stringsAsFactors=FALSE)
#raw10 <- read.csv("~/onedrive/Scenic/data/indexG220/220210.csv", comment.char="#", stringsAsFactors=FALSE)
#raw11 <- read.csv("~/onedrive/Scenic/data/indexG220/220211.csv", comment.char="#", stringsAsFactors=FALSE)
raw24 <- read.csv("~/onedrive/Scenic/data/indexG220/220224.csv", comment.char="#", stringsAsFactors=FALSE)
raw25 <- read.csv("~/onedrive/Scenic/data/indexG220/220225.csv", comment.char="#", stringsAsFactors=FALSE)

#raw11 <- read.csv("C:/Users/3048318/OneDrive - Queen's University Belfast/Scenic/data/indexG220/220211.csv", comment.char="#", stringsAsFactors=FALSE)
