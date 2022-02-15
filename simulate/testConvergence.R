#!/usr/bin/env Rscript

# ICF - Anna Belova - October 2021
# Test convergence of the results at county/age/sex resolution by model and HIF
#
# The code is to be run from command line with arguments:
# - Configuration file name (which is pointing to the input file name)
# - Warming degree for which calculations are to be executed
# - Flag for running calculations in point mode or uncertainty mode
# - Discount rate
# - Discount year
#
# The code produces
# - A file with convergence test result county/age/sex resolution by model and HIF


library(progress)
library(tidyverse)   
library(yaml)    
library(readxl)
library(lhs)
library(triangle)

setwd(Sys.getenv("PROJECT_LOC"))
DATA_DIR <- Sys.getenv("DATA_LOC")
WORKBOOK_DIR <- Sys.getenv("WORKBOOK_LOC")

SEED = 31416
set.seed(SEED)

DEBUG <- TRUE


#----------Read inputs, process parameters--------

args = commandArgs(trailingOnly=TRUE)

CONFIG          <-  args[1] #"configuration.yaml"
DEGREE          <-  args[2] # "D1" or "D2" or "D3" or "D4" or "D5" or "D6"
POINT_MODE      <-  as.logical(args[3])       # "TRUE" or "FALSE"
POPULATION_YEAR <-  args[4] # "PRESENT" or "FUTURE"
INCOME_YEAR     <-  args[5] # "PRESENT" or "FUTURE"
DISCOUNT_RATE   <-  as.numeric(args[6])       # 0.03
DISCOUNT_YEAR   <-  as.numeric(args[7])       # 2015

config <- read_yaml(file=CONFIG)

source("simulate/estimateUtils.R")

print(CONFIG)
print(DEGREE)
print(POINT_MODE)
print(POPULATION_YEAR)
print(INCOME_YEAR)
print(DISCOUNT_RATE)
print(DISCOUNT_YEAR)


#-------Initiate output files------

CASE_FILE_NAME_STUB <- getOutFileHandle( DATA_DIR, config$results_file_names_stubs$all_results[[DEGREE]], 
                                         POINT_MODE, POPULATION_YEAR, INCOME_YEAR, DISCOUNT_RATE, DISCOUNT_YEAR, NA , TRUE )

if (DEBUG) { print(CASE_FILE_NAME_STUB)}

IN_FILE_NAME_SET <- list.files(path = file.path(DATA_DIR,"results"), pattern= paste("^",CASE_FILE_NAME_STUB,sep="") )

# If there is more than one results file, take the latest
if ( length(IN_FILE_NAME_SET) > 1) {
  IN_FILE_NAME_SET <- rev(sort(IN_FILE_NAME_SET))
}
IN_FILE_NAME <- file.path(DATA_DIR,"results",IN_FILE_NAME_SET[1])

if (DEBUG) { print(IN_FILE_NAME)}

OUT_FILE_NAME <- getOutFileHandle( DATA_DIR, paste("convTest",config$results_file_names_stubs$all_results[[DEGREE]],sep="-"), 
                                   POINT_MODE, POPULATION_YEAR, INCOME_YEAR, DISCOUNT_RATE, DISCOUNT_YEAR,
                                   format(Sys.time(), format="%Y-%m%d-%H%M%S") , FALSE )

if (DEBUG) { print(OUT_FILE_NAME)}


#---- Import and process the file ----------
indata <- read_csv(IN_FILE_NAME )


#------ Convergence evaluation summary ----------
testFun <- eval(parse(text=config$convergence[["rel_tol_fun"]]))

if ( !(POINT_MODE) ) {
  
  res_conv_list <- list()
  
  N_full <- config$convergence$N_start + config$convergence$N_inc
  N_subs <- config$convergence$N_inc
  
  if (DEBUG) {print(config$convergence$conv_variab)}
  
  for ( n in c(N_subs,N_full) ) {
    
    summ <- indata %>% filter(ITER <= n) %>% 
      group_by_at(vars(config$convergence$conv_variab ) ) %>% 
      summarise(CASES_MEAN = mean(CASES_ITER), 
                CASES_LCB = quantile(CASES_ITER,config$convergence$conv_quant$LCB), 
                CASES_UCB = quantile(CASES_ITER,config$convergence$conv_quant$UCB),
                PDV_MEAN = mean(PDV_ITER), 
                PDV_LCB = quantile(PDV_ITER,config$convergence$conv_quant$LCB), 
                PDV_UCB = quantile(PDV_ITER,config$convergence$conv_quant$UCB)
                )  %>% 
      ungroup() %>%
      gather(stat,value,CASES_MEAN:PDV_UCB) 
    
    LAB <- ifelse(n<100 , paste("N0",n,sep=""), paste("N",n,sep=""))
    res_conv_list[[LAB]] <- summ
  }
  
  if (DEBUG) { print(res_conv_list)}
  
  res_conv_dat <- bind_rows(res_conv_list, .id="NITER")
  print(res_conv_dat)
  
  testing <- res_conv_dat %>% 
    arrange_at( vars(c(config$convergence$conv_variab,"stat", "NITER")) ) %>% 
    group_by_at( vars(c(config$convergence$conv_variab,"stat")) )  %>% 
    mutate(TEST = testFun(value,lag(value)) ) %>%
    ungroup() %>%
    filter( NITER==paste("N",N_full,sep="") )
  
  write_csv( testing , OUT_FILE_NAME )
  
}
