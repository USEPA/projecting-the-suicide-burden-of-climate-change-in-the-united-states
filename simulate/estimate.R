#!/usr/bin/env Rscript

# ICF - Anna Belova - October 2021
# Evaluate changes in suicide cases attributable to future temperature changes and estimate valuation
#
# The code is to be run from command line with arguments:
# - Configuration file name
# - Warming degree for which calculations are to be executed
# - Flag for running calculations in point mode or uncertainty mode
# - Flag for whether to assume baseline population or future population for calculations
# - Discount rate
# - Discount year
#
# The code produces
# - Attributable suicide cases by county, age group, sex, climate model
# - Value of avoiding attributable suicide cases by county, age group, sex, climate model
# - Summary files


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

DEBUG <- FALSE
DOLLAR_SCALE <- 1000000000 # Convert to billions


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

base_climate <- readClimate(DATA_DIR, config$input_data$climate[["BASE"]][["Livneh"]], "Baseline",
                            config$input_data$climate$columns, config$hif_transformations)
population_data  <- readPopulation(DATA_DIR, config$input_data$demographic$population)
incidence_data  <- readIncidence(DATA_DIR, config$input_data$demographic$incidence)

vsl_incgf_data  <- readVSL(WORKBOOK_DIR, config$input_workbooks$vsl_income_growth_factors)

#-------Initiate output files------

OUT_FILE_NAME <- getOutFileHandle( DATA_DIR, config$results_file_names_stubs$all_results[[DEGREE]], 
                                   POINT_MODE, POPULATION_YEAR, INCOME_YEAR, DISCOUNT_RATE, DISCOUNT_YEAR,
                                   format(Sys.time(), format="%Y-%m%d-%H%M%S") , FALSE )

OUT_SUMFILE_NAME <- getOutFileHandle( DATA_DIR, paste("sum", config$results_file_names_stubs$all_results[[DEGREE]],sep="-"), 
                                   POINT_MODE, POPULATION_YEAR, INCOME_YEAR, DISCOUNT_RATE, DISCOUNT_YEAR,
                                   format(Sys.time(), format="%Y-%m%d-%H%M%S") , FALSE )


APPEND_FLAG <- FALSE
APPEND_SUMFLAG <- FALSE


if ( !(POINT_MODE) ) {

  OUT_PAR_FILE_NAME <- file.path( DATA_DIR, "results",
                                  paste(config$results_file_names_stubs$parset, 
                                        config$convergence$N_start + config$convergence$N_inc, ".csv", sep="" ) ) 
  
  PAR_FILE_EXISTS <- file.exists(OUT_PAR_FILE_NAME)
    
}



#-------Define the set of climate models ------

model_set <- c()
for (m in names(config$input_data$climate[[DEGREE]]) )  { 
  if (  config$input_data$climate[[DEGREE]][[m]][["year"]] != "NA" ) {  
      print("")
      print(m)
      print(config$input_data$climate[[DEGREE]][[m]])
      model_set <- c(model_set, m) 
    } 
  }
N_mod <- length(model_set)

#-------Sample parameter sets ---------

hif_names <- names( config$hif_parameters )
hif_param_set <- list()
hif_param_set[["SAMPLE"]] <- list()
hif_param_set[["POINT"]] <- list()
for ( h in hif_names) { hif_param_set[["POINT"]][[h]]  <- createValueList(  config$hif_parameters[[h]] , NA ) }

vsl_param_set <- list()
vsl_param_set[["POINT"]] <- list()
vsl_param_set[["POINT"]][["vsl"]] <- createValueList(  config$vsl_information$parameters , NA )
vsl_param_set[["POINT"]][["cpi"]] <- config$vsl_information$bls_cpi_factor
vsl_param_set[["SAMPLE"]] <- list()
vsl_param_set[["SAMPLE"]][["cpi"]] <- config$vsl_information$bls_cpi_factor

if ( !(POINT_MODE) ) {
  
  if (PAR_FILE_EXISTS) {
    
    param_iter_data <- read_csv(OUT_PAR_FILE_NAME, progress=FALSE )
    
    print(glimpse(param_iter_data))
    
    for ( h in hif_names) {
      hifSet <- c(paste(names( config$hif_parameters[[h]] ), "val" , sep="_"), 
                  paste( names(config$hif_parameters[[h]] ), "scale" , sep="_"), "ITER" )
      hif_param_set[["SAMPLE"]][[h]] <- param_iter_data %>% filter(HIF==h) %>% select_at( hifSet )
    }
    
    valSet <- c(paste(names(config$vsl_information$parameters), "val" , sep="_"), 
                paste( names(config$vsl_information$parameters), "scale" , sep="_"), "ITER" )
    
    print(valSet)
    
    vsl_param_set[["SAMPLE"]][["vsl"]] <- param_iter_data %>%  select_at( valSet ) %>% distinct()
    
  } else {

    hif_param_iter_data_list <- list()
    for ( h in hif_names) {
        hif_param_set[["SAMPLE"]][[h]] <- createParamIterData_inner( createValueList( config$hif_parameters[[h]] , config$convergence ), 
                                                                     config$convergence )
        hif_param_iter_data_list[[h]] <- hif_param_set[["SAMPLE"]][[h]]
    }
    hif_param_iter_data <- bind_rows(hif_param_iter_data_list, .id = "HIF")
    
    vsl_param_set[["SAMPLE"]][["vsl"]] <- createParamIterData_inner(createValueList( config$vsl_information$parameters , config$convergence ), 
                                                                    config$convergence)
    vsl_param_iter_data <- vsl_param_set[["SAMPLE"]][["vsl"]]
    
    param_iter_data <- inner_join(hif_param_iter_data,vsl_param_iter_data,by="ITER")
    write_csv( param_iter_data, OUT_PAR_FILE_NAME )
  
  }
}

if (DEBUG) { print(hif_param_set[["POINT"]]) }
if (DEBUG) { print(hif_param_set[["SAMPLE"]]) }

if (DEBUG) { print(vsl_param_set[["POINT"]]) }
if (DEBUG) { print(vsl_param_set[["SAMPLE"]]) }

if ( !(POINT_MODE) ) {
  if (DEBUG) { print(param_iter_data) }
}



#-------Loop though climate models ---------

pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = N_mod, clear = FALSE, width= 100)

print(paste("Processing", N_mod, "climate models"))

start_time <- Sys.time()
for (i in 1:N_mod) {
  
  if (DEBUG) { print(model_set[i]) }
  
  # Create input list
  inp_data <- prepInput(readClimate(DATA_DIR, config$input_data$climate[[DEGREE]][[model_set[i]]], model_set[i], 
                                    config$input_data$climate$columns, config$hif_transformations), 
                       base_climate, population_data, incidence_data,  
                       POPULATION_YEAR  )
  
  state_set <- inp_data %>% distinct(ST) %>% arrange(ST) %>% pull(ST)
  
  for (stAbbr in state_set) {
  
    # Compute cases and values
    res_data <- computeResults( inp_data %>% filter(ST==stAbbr), POINT_MODE, hif_param_set, vsl_param_set, vsl_incgf_data , 
                                INCOME_YEAR, DISCOUNT_YEAR, DISCOUNT_RATE )
    
    if (DEBUG) {print(glimpse(res_data))}
    
    # Aggregate up to state to control file sizes
    if ( !(POINT_MODE) ){
      
      res_data <- res_data %>% group_by(HIF,ST,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP,YEAR_INC,INCGF,ITER) %>%
        mutate(D_le_30 = POP_SIZE * D_le_30,
               D_30_40 = POP_SIZE * D_30_40,
               D_40_50 = POP_SIZE * D_40_50,
               D_50_60 = POP_SIZE * D_50_60,
               D_80_ge = POP_SIZE * D_80_ge,
               TEMP   =  POP_SIZE * TEMP,
               PREC =    POP_SIZE * PREC,
               IR100K=   POP_SIZE *  IR100K / 100000
        ) %>%
        summarise(D_le_30=sum(D_le_30),
                  D_30_40 = sum(D_30_40),
                  D_40_50 = sum(D_40_50),
                  D_50_60= sum(D_50_60),
                  D_80_ge = sum(D_80_ge),
                  TEMP=sum(TEMP),
                  PREC = sum(PREC),
                  POP_SIZE = sum(POP_SIZE),
                  IR100K= sum(IR100K),
                  CASES_PT = sum(CASES_PT),
                  HIF_NORM = mean(HIF_NORM),
                  VSL_PT = mean(VSL_PT),
                  PDV_PT = sum(PDV_PT),
                  CASES_ITER = sum(CASES_ITER),
                  VSL_ITER = mean(VSL_ITER),
                  PDV_ITER = sum(PDV_ITER)
                  ) %>% 
        mutate(D_le_30 = D_le_30 / POP_SIZE  ,
               D_30_40 = D_30_40 / POP_SIZE  ,
               D_40_50 = D_40_50 / POP_SIZE  ,
               D_50_60 = D_50_60 / POP_SIZE  ,
               D_80_ge = D_80_ge / POP_SIZE  ,
               TEMP   =  TEMP / POP_SIZE  ,
               PREC =    PREC / POP_SIZE  ,
               IR100K=   100000 * IR100K / POP_SIZE )
    }
    
    # Record results
    write_csv( res_data, OUT_FILE_NAME , append = APPEND_FLAG)
    APPEND_FLAG <- TRUE
    
    # Print summary
    
    if ( POINT_MODE ) {
      
      summ_cases <- res_data %>% group_by(HIF,MODEL) %>% summarise(cases = sum(CASES_PT) ) %>% ungroup() 
      
      print(stAbbr)
      print(summ_cases %>% spread(HIF,cases) )
      
      summ_pdv <- res_data %>% group_by(HIF,MODEL) %>% summarise( pdv =  sum(PDV_PT) / DOLLAR_SCALE ) %>% ungroup() 
      
      print(stAbbr)
      print(summ_pdv %>% spread(HIF,pdv) )
      
      summ_data <- inner_join(summ_cases,summ_pdv,by=c("HIF","MODEL")) %>% mutate(DEG=DEGREE,ST=stAbbr)
      
    } else {
      
      summ_cases <- res_data %>% group_by(HIF,MODEL,ITER) %>% summarise(cases = sum(CASES_ITER), cases_pt = sum(CASES_PT)) %>% ungroup() %>% 
        group_by(HIF,MODEL) %>% 
        summarise(NITER = max(ITER),
                  PT = mean(cases_pt),
                  MEAN = mean(cases), 
                  LCB = quantile(cases,config$convergence$conv_quant$LCB), 
                  UCB = quantile(cases,config$convergence$conv_quant$UCB)  )  %>% 
        ungroup() %>%
        gather(stat,cases,NITER:UCB) 
      
      print(stAbbr)
      print(summ_cases %>% spread(stat,cases) )
      
      summ_pdv <- res_data %>% group_by(HIF,MODEL,ITER) %>% summarise(pdv =  sum(PDV_ITER) / DOLLAR_SCALE, pdv_pt =  sum(PDV_PT) / DOLLAR_SCALE ) %>% ungroup() %>% 
        group_by(HIF,MODEL) %>% 
        summarise(NITER = max(ITER),
                  PT = mean(pdv_pt),
                  MEAN = mean(pdv), 
                  LCB = quantile(pdv,config$convergence$conv_quant$LCB), 
                  UCB = quantile(pdv,config$convergence$conv_quant$UCB)  )  %>% 
        ungroup() %>%
        gather(stat,pdv,NITER:UCB) 

      print(stAbbr)
      print(summ_pdv %>% spread(stat,pdv) )
      
      summ_data <- inner_join(summ_cases,summ_pdv,by=c("HIF","MODEL","stat")) %>% mutate(DEG=DEGREE,ST=stAbbr)
      
    }
    
    
    
    write_csv( summ_data, OUT_SUMFILE_NAME , append = APPEND_SUMFLAG)
    APPEND_SUMFLAG <- TRUE
  
  }
  
  pb$tick()
}
end_time <- Sys.time()
print(end_time - start_time)






