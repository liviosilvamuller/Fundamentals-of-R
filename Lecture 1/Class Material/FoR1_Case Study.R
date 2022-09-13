# Title: Who makes international regimes possible?
# Purpose: showcase a conceptually sound programming workflow.
# Author: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

setwd("~/Desktop/L1_Case Study")

library(tidyverse)
library(readr)
library(skimr)
library(scales)
library(RColorBrewer)
library(gt)

io_dat <- read_csv("io_income_rs.csv") 

skim(io_dat)

Sys.setlocale("LC_ALL", "C")
options(stringsAsFactors = FALSE, scipen = 999)
