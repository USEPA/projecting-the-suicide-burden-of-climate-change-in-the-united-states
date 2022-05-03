#!/usr/bin/env Rscript

# ICF - Anna Belova - May 2022
# Evaluate changes in suicide cases attributable to future temperature changes 
# to serve as inputs to EPA's Framework for Evaluating Damages and Impacts (FrEDI)
# https://www.epa.gov/cira/fredi
#
# The code is to be run from command line with arguments:
# - Configuration file name
# - Warming degree for which calculations are to be executed
# - Flag for whether to assume baseline population or future population for calculations
#
#
# The code produces a CSV file with the following elements
# - State abbreviation
# - County FIPS
# - Calendar year
# - GCM
# - Warming degree
# - HIF option, including an average over HIF
# - Population projection option (2015 or future year corresponding to the warming degree and GCM combination)
# - Age group (<=18, >=65, 19-64)
# - Population size
# - Baseline suicide rate per 100K 
# - Number of excess suicides (point)
# - Number of excess suicides (mean)
# - Number of excess suicides (P10)
# - Number of excess suicides (P25)
# - Number of excess suicides (P50)
# - Number of excess suicides (P75)
# - Number of excess suicides (P90)


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


#----------Read inputs, process parameters--------

args = commandArgs(trailingOnly=TRUE)

CONFIG          <-  args[1] #"configuration.yaml"
DEGREE          <-  args[2] # "D1" or "D2" or "D3" or "D4" or "D5" or "D6"
POPULATION_YEAR <-  args[3] # "PRESENT" or "FUTURE"

# Unused parameters 
POINT_MODE      <-  FALSE
INCOME_YEAR     <-  "PRESENT"
DISCOUNT_RATE   <-  0.03
DISCOUNT_YEAR   <-  2015

config <- read_yaml(file=CONFIG)

source("simulate/estimateUtils.R")

print(CONFIG)
print(DEGREE)
print(POPULATION_YEAR)

base_climate <- readClimate(DATA_DIR, config$input_data$climate[["BASE"]][["Livneh"]], "Baseline",
                            config$input_data$climate$columns, config$hif_transformations)
population_data  <- readPopulation(DATA_DIR, config$input_data$demographic$population)
incidence_data  <- readIncidence(DATA_DIR, config$input_data$demographic$incidence)

vsl_incgf_data  <- readVSL(WORKBOOK_DIR, config$input_workbooks$vsl_income_growth_factors)

#-------Initiate output files------

OUT_SUMFILE_NAME <- file.path( DATA_DIR, paste("FrEDI_input_", 
                                               config$results_file_names_stubs$all_results[[DEGREE]], "_", 
                                               POPULATION_YEAR,"_", 
                                               format(Sys.time(), format="%Y-%m%d-%H%M%S") , ".csv", sep="" ) )


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
    # Recode age groups: <=18, >=65, 19-64
    
      res_data_by_HIF <- res_data %>% 
        mutate(AGEGR  = recode(AGE, 
                               "5-14 years" = "5-24 years",
                               "15-24 years" = "5-24 years",
                               "25-34 years" = "25-64 years",
                               "35-44 years" = "25-64 years",
                               "45-54 years" = "25-64 years",
                               "55-64 years" = "25-64 years",
                               "65-74 years" = "65+ years",
                               "75-84 years" = "65+ years",
                               "85+ years" = "65+ years" ) ,
               DEGREE = DEGREE) %>%
        group_by(ST,FIPS,AGEGR,DEGREE,HIF,MODEL,YEAR_CLIM,YEAR_POP,YEAR_INC,INCGF,ITER) %>%
        mutate(IR100K=   POP_SIZE *  IR100K / 100000) %>%
        summarise(POP_SIZE = sum(POP_SIZE),
                  IR100K= sum(IR100K),
                  CASES_PT = sum(CASES_PT),
                  CASES_ITER = sum(CASES_ITER),
                  ) %>% 
        mutate(IR100K=   100000 * IR100K / POP_SIZE ) %>%
        ungroup() %>%
        select(ST,FIPS,AGEGR,DEGREE, HIF,MODEL,YEAR_CLIM,YEAR_POP,ITER, POP_SIZE, IR100K, CASES_PT, CASES_ITER)
    
   res_data_average_HIF <- res_data_by_HIF %>%
     group_by(ST,FIPS,AGEGR,DEGREE, MODEL,YEAR_CLIM,YEAR_POP,ITER) %>%
     summarise( POP_SIZE = mean(POP_SIZE),
                IR100K = mean(IR100K),
                CASES_PT = mean(CASES_PT),
                CASES_ITER = mean(CASES_ITER)
                ) %>%
     mutate( HIF = "AVERAGE") %>%
     ungroup()
  
   res_data_extended <- bind_rows(res_data_by_HIF,res_data_average_HIF )
   
    
    summ_cases <- res_data_extended %>% group_by(ST,FIPS,AGEGR,DEGREE, HIF,MODEL,YEAR_CLIM,YEAR_POP) %>% 
        summarise(POP_SIZE = mean(POP_SIZE),
                  IR100K = mean(IR100K), 
                  CASES_PT = mean(CASES_PT),
                  CASES_MEAN = mean(CASES_ITER), 
                  CASES_P10 = quantile(CASES_ITER,0.1), 
                  CASES_P25 = quantile(CASES_ITER,0.25), 
                  CASES_P50 = quantile(CASES_ITER,0.5), 
                  CASES_P75 = quantile(CASES_ITER,0.75), 
                  CASES_P90 = quantile(CASES_ITER,0.9) )  %>% 
        ungroup() 
      
    print(stAbbr)
    print(glimpse(summ_cases) )
      
    
    write_csv( summ_cases, OUT_SUMFILE_NAME , append = APPEND_SUMFLAG)
    APPEND_SUMFLAG <- TRUE
  
  }
  
  pb$tick()
}
end_time <- Sys.time()
print(end_time - start_time)






