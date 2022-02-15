# ICF - Anna Belova - October 2021
# Support  functions for  evaluate attributable suicide cases and valuation


#------Read input functions-------------

readClimate <- function(dir_name, file_names, model_name, col_names, transformations ){
  

  # Climate data transformations to fit HIF
  trans_D_le_30 <- eval(parse(text=transformations$binned[["D_le_30"]]))
  trans_D_30_40 <- eval(parse(text=transformations$binned[["D_30_40"]]))
  trans_D_40_50 <- eval(parse(text=transformations$binned[["D_40_50"]]))
  trans_D_50_60 <- eval(parse(text=transformations$binned[["D_50_60"]]))
  trans_D_70_80 <- eval(parse(text=transformations$binned[["D_70_80"]]))
  trans_D_80_ge <- eval(parse(text=transformations$binned[["D_80_ge"]]))
  trans_temp    <- eval(parse(text=transformations$linear[["temp"]]))
  trans_precip  <- eval(parse(text=transformations$linear[["precip"]])) # Not needed now since precip data are monthly
  
  # Import and process temperature data
  temp_data_wide <- read_csv( file.path(dir_name,file_names$temp) ) 
  
  temp_data_wide <- temp_data_wide %>% 
    rename_at(col_names$USFIPS , ~"USFIPS") %>%
    mutate( FIPS = as.numeric(str_sub(USFIPS,start=3))) %>%
    select( FIPS, contains( "_" )   ) 
  
  temp_data_list <- list()
  for (m in col_names$MTH) {
    d <- temp_data_wide %>% 
      select( FIPS , contains(m) )  %>%
      gather(DAY, daytemp, -FIPS) %>%
      group_by( FIPS ) %>%
      summarise(
        D_le_30 = trans_D_le_30(daytemp),
        D_30_40 = trans_D_30_40(daytemp),
        D_40_50 = trans_D_40_50(daytemp),
        D_50_60 = trans_D_50_60(daytemp),
        D_70_80 = trans_D_70_80(daytemp),
        D_80_ge = trans_D_80_ge(daytemp),
        temp    = trans_temp(daytemp)
      ) %>% 
      ungroup()
    temp_data_list[[m]] <- d 
  }
  monthly_temp_data <- bind_rows(temp_data_list, .id = "MTH")

  # Import and process precipitation data
  precip_data_wide <- read_csv( file.path(dir_name,file_names$precip) ) 
  monthly_precip_data <- precip_data_wide %>% 
    rename_at(col_names$USFIPS , ~"USFIPS") %>%
    rename_at("June" , ~"Jun") %>%
    mutate( FIPS = as.numeric(str_sub(USFIPS,start=3))) %>%
    select_at( c("FIPS", col_names$MTH) ) %>%
    gather(MTH, precip, -FIPS)

  # Merge climate data
  out_data <- list(monthly_temp_data, monthly_precip_data) %>% 
    reduce(inner_join, by = c("FIPS","MTH")) %>%
    mutate( YEAR_CLIM = file_names$year, MODEL = model_name )

  if (DEBUG) { print(glimpse( out_data )) }
    
  return( out_data)
}

readPopulation <- function( dir_name, file_list ){
  
  data <- read_csv( file.path(dir_name,file_list$name) ) 
  
  out_data <- data %>%
    rename_at( file_list$columns$FIPS , ~"FIPS") %>%
    rename_at( file_list$columns$YEAR , ~"YEAR") %>%
    rename_at( file_list$columns$SEX , ~"SEX") %>%
    rename_at( file_list$columns$AGE , ~"AGE") %>%
    rename_at( file_list$columns$POP , ~"POP") %>% 
    select(FIPS,YEAR,SEX,AGE,POP) %>%
    mutate(
      SEX = tolower(SEX),
      AGE = tolower(AGE),
      FIPS = as.numeric(FIPS)
    )
  
  if (DEBUG) { print(glimpse( out_data )) }
  
  return( out_data)
}

readIncidence <- function( dir_name, file_list) {

  data <- read_csv( file.path(dir_name,file_list$name) ) 
  
  out_data <- data %>%
    rename_at( file_list$columns$ST , ~"ST") %>%
    rename_at( file_list$columns$FIPS , ~"FIPS") %>%
    rename_at( file_list$columns$MTH , ~"MTH") %>%
    rename_at( file_list$columns$SEX , ~"SEX") %>%
    rename_at( file_list$columns$AGE , ~"AGE") %>%
    rename_at( file_list$columns$IR100K , ~"IR100K") %>% 
    select(ST,FIPS,MTH,SEX,AGE,IR100K) %>%
    mutate(
      SEX = tolower(SEX),
      AGE = tolower(AGE)
    )
  
  if (DEBUG) { print(glimpse( out_data )) }
  
  
  return( out_data)
}

readVSL <- function( dir_name, data_info) {
  
  data <- read_excel( file.path(dir_name,data_info$name), sheet=data_info$sheet, range=data_info$range ) 

  out_data <- data %>%
    rename_at( data_info$year_col_num , ~"YEAR_INC") %>%
    rename_at( data_info$incgf_col_num , ~"INCGF") %>%
    select(YEAR_INC,INCGF)
  
  if (DEBUG) { print(glimpse( out_data )) }
  
  
  return( out_data)
}


#------Initiate and append to output functions-------------

getOutFileHandle <- function( dir_name, file_name_stub, point, pop_year, inc_year, disc_rate, disc_yr,  time_stamp, stub ) {
  
  if (!is.na(inc_year)) {
    
    if (stub) {

      handle <- file.path(
                          paste(file_name_stub,
                                "PTMODE-", point, "_",
                                "POPYR-", pop_year, "_",
                                "INCYR-", inc_year, "_",
                                "DR-", disc_rate, "_",
                                "DY-", disc_yr, "_", sep="") )
      
            
    } else {
      
      handle <- file.path(dir_name,"results",
                          paste(file_name_stub,
                                "PTMODE-", point, "_",
                                "POPYR-", pop_year, "_",
                                "INCYR-", inc_year, "_",
                                "DR-", disc_rate, "_",
                                "DY-", disc_yr, "_",
                                time_stamp,".csv",sep="") )
    }
        
  } else {
    
    if (stub) {
      
      handle <- file.path(
                          paste(file_name_stub,
                                "PTMODE-", point, "_",
                                "POPYR-", pop_year, "_",sep="") )
      
      
    } else {
      
      handle <- file.path(dir_name,"results",
                          paste(file_name_stub,
                                "PTMODE-", point, "_",
                                "POPYR-", pop_year, "_",
                                time_stamp,".csv",sep="") )
      
      
    }
    
  }

  return( handle )
  
}


#------Prepare input data list functions-------------------------

prepInput <- function(future_climate_data, base_climate_data, pop_data, inc_data, pop_year_flag) {
  
  if (pop_year_flag == "FUTURE") {
    
    pop_year <- future_climate_data %>% pluck("YEAR_CLIM",1)
    
  } else {
    
    pop_year <- base_climate_data %>%  pluck("YEAR_CLIM",1)
    
  }

  pop_year_data <- pop_data %>% filter( YEAR == pop_year) %>% rename(YEAR_POP = YEAR)
  
  model_name <- future_climate_data %>% pluck("MODEL",1) %>% as.character()
  
  climate_data <- inner_join(future_climate_data, base_climate_data, by = c("FIPS","MTH"), suffix = c(".f", ".b")) %>%
    mutate( 
      MODEL = model_name,
      YEAR_CLIM = YEAR_CLIM.f,
      D_le_30 = D_le_30.f - D_le_30.b,
      D_30_40 = D_30_40.f - D_30_40.b,
      D_40_50 = D_40_50.f - D_40_50.b,
      D_50_60 = D_50_60.f - D_50_60.b,
      D_80_ge = D_80_ge.f - D_80_ge.b,
      TEMP = temp.f - temp.b,
      PREC = precip.f - precip.b
      ) %>%
    select( FIPS, MTH, MODEL, YEAR_CLIM, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge, TEMP, PREC )
  
  if (DEBUG) { print(glimpse(climate_data)) }
  
  demogr_data <- inner_join( inc_data , pop_year_data, by=c("FIPS","SEX","AGE")) 

  if (DEBUG) { print(glimpse(demogr_data)) }
  
  out_data <- inner_join( demogr_data , climate_data, by=c("FIPS","MTH")) 

  if (DEBUG) { print(glimpse(out_data)) }
  
  return( out_data )
}


#------Apply HIF functions--------------------

computeCases  <- function( inp_data, point, hif_list , conv){
  
  hif_names <- names( hif_list[["POINT"]] )
  
  # Obtain point estimates
  res_point_list <- list()
  
  for ( h in hif_names) {
    
    param_point_list <- hif_list[["POINT"]][[h]] 
    
    if ( h %in% c("binned","binned_displ") ) {

      d <- inp_data %>%  mutate( cases_per_cap =  applyBinnedHifValList( IR100K, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge , param_point_list ) ) 
      
    } else {

      d <- inp_data %>%  mutate( cases_per_cap =  applyLinearHifValList( IR100K, TEMP, PREC, param_point_list ) ) 
      
    }
    
    if (DEBUG) { print(glimpse(d)) }
    
    d <- d %>% group_by(ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP) %>%
      summarise( POP_SIZE = mean(POP),
                 IR100K = sum(IR100K),
                 cases_per_cap = sum(cases_per_cap),
                 D_le_30       = mean(D_le_30),
                 D_30_40       = mean(D_30_40),
                 D_40_50       = mean(D_40_50),
                 D_50_60       = mean(D_50_60),
                 D_80_ge       = mean(D_80_ge),
                 TEMP          = mean(TEMP),
                 PREC          = mean(PREC) ) %>%
      ungroup() %>%
      mutate( CASES_PT = POP_SIZE * cases_per_cap ) %>%
      select( ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP, 
              D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge, TEMP, PREC , POP_SIZE, IR100K, CASES_PT )
    
    res_point_list[[h]] <- d
    
  }
  res_point_data <-  bind_rows(res_point_list, .id = "HIF")
  
  if (DEBUG) { print(glimpse( res_point_data )) }
  
  # Obtain sample estimates
  if ( !( point ) ) {
    
    if (DEBUG) { print(conv) }
    
    # Obtain sample estimates
    res_samp_list <- list()
    
    for ( h in hif_names) {
      
      param_samp_list <- hif_list[["SAMPLE"]][[h]] 
      
      param_samp_dat <- createParamIterData( hif_param_set[["SAMPLE"]], config$convergence ) %>%
        filter(HIF==h) %>% select(-HIF)
      
      if (DEBUG) { print(h); print(param_samp_list) ; print(param_samp_dat) }
      
      if ( h %in% c("binned","binned_displ") ) {
        
        if (DEBUG) { print("HERE! bin") }
        
       d <- inp_data %>% 
         select(ST,FIPS,SEX,AGE,MTH,MODEL,YEAR_CLIM,YEAR_POP,POP,IR100K,D_le_30,D_30_40,D_40_50,D_50_60,D_80_ge) 
       
       d <-  expand_grid( d , param_samp_dat) %>%
         mutate( cases_per_cap =  applyBinnedHifValCols( IR100K, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge , 
                                                         beta_le_30_scale, beta_le_30_val,
                                                         beta_30_40_scale, beta_30_40_val,
                                                         beta_40_50_scale, beta_40_50_val,
                                                         beta_50_60_scale, beta_50_60_val,
                                                         beta_80_ge_scale, beta_80_ge_val) )
       
               
                
      } else {
        
        if (DEBUG) { print("HERE! lin") }
        
        d <- inp_data %>%
          select(ST,FIPS,SEX,AGE,MTH,MODEL,YEAR_CLIM,YEAR_POP,POP,IR100K,TEMP,PREC) 
        
        d <-  expand_grid( d , param_samp_dat)  %>%
          mutate( cases_per_cap =  applyLinearHifValCols( IR100K, TEMP, PREC, 
                                                          alpha_temp_scale, alpha_temp_val, 
                                                          alpha_precip_scale, alpha_precip_val ) ) 
        
                        
      }
      
      if (DEBUG) { print("HERE! aggr ") }
      
      d <- d %>% group_by(ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP,POP,ITER) %>%
        summarise( POP_SIZE = mean(POP),
                   cases_per_cap = sum(cases_per_cap) ) %>%
        ungroup() %>%
        mutate( CASES_ITER = POP_SIZE * cases_per_cap ) %>%
        select( ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP, ITER, CASES_ITER )
      
      res_samp_list[[h]] <- d
      
      if (DEBUG) { print(glimpse(d)) }
      
    }
    
    res_samp_data <-  bind_rows(res_samp_list, .id = "HIF")
    
    res_data <- inner_join( res_point_data , res_samp_data , by=c("ST","FIPS","SEX","AGE",
                                                                  "MODEL","YEAR_CLIM","YEAR_POP", "HIF" ) ) 
     
  } else {
    
    res_data <- res_point_data
    
  }
  
  return( res_data )
}


#------Apply HIF and Valuation functions--------------------

computeResults  <- function( inp_data, point, hif_list , vsl_list, gf_data , INCOME_YEAR, DISCOUNT_YEAR, DISCOUNT_RATE){
  
  hif_names <- names( hif_list[["POINT"]] )
  
  # Obtain point estimates
  res_point_list <- list()
  
  for ( h in hif_names) {
    
    param_point_list <- hif_list[["POINT"]][[h]] 
    
    if ( h %in% c("binned","binned_displ") ) {
      
      d <- inp_data %>%  mutate( cases_per_cap =  applyBinnedHifValList( IR100K, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge , param_point_list ) ) 
      
    } else {
      
      d <- inp_data %>%  mutate( cases_per_cap =  applyLinearHifValList( IR100K, TEMP, PREC, param_point_list ) ) 
      
    }
    
    if (DEBUG) { print(glimpse(d)) }
    
    d <- d %>% group_by(ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP) %>%
      summarise( POP_SIZE = mean(POP),
                 IR100K = sum(IR100K),
                 cases_per_cap = sum(cases_per_cap),
                 D_le_30       = mean(D_le_30),
                 D_30_40       = mean(D_30_40),
                 D_40_50       = mean(D_40_50),
                 D_50_60       = mean(D_50_60),
                 D_80_ge       = mean(D_80_ge),
                 TEMP          = mean(TEMP),
                 PREC          = mean(PREC) ) %>%
      ungroup() %>%
      mutate( CASES_PT = POP_SIZE * cases_per_cap ) %>%
      select( ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP, 
              D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge, TEMP, PREC , POP_SIZE, IR100K, CASES_PT ) %>%
      mutate( YEAR_INC = ifelse( INCOME_YEAR=="FUTURE", YEAR_CLIM, DISCOUNT_YEAR ) )
    
    if (DEBUG) {print(vsl_param_set[["POINT"]][["vsl"]])}
    
    d <- inner_join( d , gf_data, by=c("YEAR_INC")) %>% 
      mutate( VSL_PT = adjustVSLList(INCGF, vsl_param_set[["POINT"]][["vsl"]],vsl_param_set[["POINT"]][["cpi"]]),
              PDV_PT = applyVSL(YEAR_CLIM, CASES_PT, VSL_PT, DISCOUNT_YEAR, DISCOUNT_RATE))
    
    res_point_list[[h]] <- d
    
  }
  res_point_data <-  bind_rows(res_point_list, .id = "HIF")
  
  if (DEBUG) { print(glimpse( res_point_data )) }
  
  # Obtain sample estimates
  if ( !( point ) ) {
    
    # Obtain sample estimates
    res_samp_list <- list()
    
    for ( h in hif_names) {
      
      param_samp_dat <- inner_join(hif_list[["SAMPLE"]][[h]], vsl_param_set[["SAMPLE"]][["vsl"]] , by="ITER")
      
      if (DEBUG) { print(h);  print(param_samp_dat) }
      
      if ( h %in% c("binned","binned_displ") ) {
        
        if (DEBUG) { print("HERE! bin") }
        
        d <- inp_data %>% 
          select(ST,FIPS,SEX,AGE,MTH,MODEL,YEAR_CLIM,YEAR_POP,POP,IR100K,D_le_30,D_30_40,D_40_50,D_50_60,D_80_ge) 
        
        d <-  expand_grid( d , param_samp_dat) %>%
          mutate( cases_per_cap =  applyBinnedHifValCols( IR100K, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge , 
                                                          beta_le_30_scale, beta_le_30_val,
                                                          beta_30_40_scale, beta_30_40_val,
                                                          beta_40_50_scale, beta_40_50_val,
                                                          beta_50_60_scale, beta_50_60_val,
                                                          beta_80_ge_scale, beta_80_ge_val),
                  HIF_NORM = (beta_le_30_val^2+ beta_30_40_val^2+beta_40_50_val^2+beta_50_60_val^2+beta_80_ge_val^2)^(1/2)
                  )
        
        
        
      } else {
        
        if (DEBUG) { print("HERE! lin") }
        
        d <- inp_data %>%
          select(ST,FIPS,SEX,AGE,MTH,MODEL,YEAR_CLIM,YEAR_POP,POP,IR100K,TEMP,PREC) 
        
        d <-  expand_grid( d , param_samp_dat)  %>%
          mutate( cases_per_cap =  applyLinearHifValCols( IR100K, TEMP, PREC, 
                                                          alpha_temp_scale, alpha_temp_val, 
                                                          alpha_precip_scale, alpha_precip_val ) ,
                  HIF_NORM = (alpha_temp_val^2+ alpha_precip_val^2)^(1/2)
                  ) 
        
        
      }
      
      if (DEBUG) { print("HERE! aggr ") }
      
      d <- d %>% group_by(ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP,POP,ITER, HIF_NORM, refval_scale, refval_val, elas_scale, elas_val) %>%
        summarise( POP_SIZE = mean(POP),
                   cases_per_cap = sum(cases_per_cap) ) %>%
        ungroup() %>%
        mutate( CASES_ITER = POP_SIZE * cases_per_cap  ,
                YEAR_INC = ifelse( INCOME_YEAR=="FUTURE", YEAR_CLIM, DISCOUNT_YEAR ) )
      
      if (DEBUG) { print(glimpse(d)) }  
      
        d <- inner_join( d , gf_data, by=c("YEAR_INC")) %>% 
          mutate(
            VSL_ITER = adjustVSLCols(INCGF, refval_scale, refval_val, elas_scale, elas_val, vsl_param_set[["SAMPLE"]][["cpi"]]),
            PDV_ITER = applyVSL(YEAR_CLIM, CASES_ITER, VSL_ITER, DISCOUNT_YEAR, DISCOUNT_RATE)
          ) %>%
          select( ST,FIPS,SEX,AGE,MODEL,YEAR_CLIM,YEAR_POP, ITER, CASES_ITER , HIF_NORM, YEAR_INC, VSL_ITER, PDV_ITER )
      
      res_samp_list[[h]] <- d
      
      if (DEBUG) { print(glimpse(d)) }
      
    }
    
    res_samp_data <-  bind_rows(res_samp_list, .id = "HIF")
    
    res_data <- inner_join( res_point_data , res_samp_data , by=c("ST","FIPS","SEX","AGE",
                                                                  "MODEL","YEAR_CLIM","YEAR_POP", "HIF" , "YEAR_INC") ) 
    
    if (DEBUG) { print(glimpse(res_point_data)) }
    if (DEBUG) { print(glimpse(res_samp_data)) }
    
  } else {
    
    res_data <- res_point_data
    
  }
  
  return( res_data )
}




#-------Helper functions--------------------------


createValueList <- function( hif ,  conv_par ) {
  
  params <- list()
  K_num <- length(hif)
  
  if ( !is.na(conv_par) ) {
    lhs <- randomLHS( conv_par$N_start , K_num )
    U <- augmentLHS(lhs, m = conv_par$N_inc)
  }
  
  for ( k in 1:K_num ) {
    
    c <- names(hif)[k]
    
    params[[c]] <- list()
    
    
    if ( !is.na(conv_par) ) {

      params[[c]][["scale"]] <- rep( hif[[c]][["scale"]], conv_par$N_start + conv_par$N_inc )
      marg <- eval(parse(text=hif[[c]][["distr"]] ))
      params[[c]][["val"]] <- marg(U[,k])
      
    } else {
      
      params[[c]][["scale"]] <- hif[[c]][["scale"]]
      params[[c]][["val"]] <- hif[[c]][["point"]]
      
    }
    
  }
  
  return( params)
}

createParamIterData <- function( param_set, conv_par ){
  
  hif_names <- names( param_set )
  
  out_list <- list()
  
  for (h in hif_names) {
   
    out_list[[h]] <- createParamIterData_inner(param_set[[h]],conv_par)
  }
  
  out <-  bind_rows(out_list, .id = "HIF")
  
  return(out)
}

createParamIterData_inner <- function( param_set, conv_par ){
  
  N = conv_par$N_start + conv_par$N_inc
  
  par_names <- names( param_set )
  par_list <- list()
  for ( p in par_names) {
    par_list[[p]] <- tibble( scale=param_set[[p]][["scale"]], val=param_set[[p]][["val"]] ) %>% 
      rename_at( "scale" , ~paste(p,"_scale",sep="")) %>%
      rename_at( "val" , ~paste(p,"_val",sep="")) 
  }
  par_dat <- bind_cols(par_list)
  out <- par_dat %>% mutate(ITER = 1:N )

  return(out)
}

applyBinnedHifValList <- function( IR100K, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge , param ) {
  
  if (DEBUG) { print("HERE!-> apply bin") }
  
  if (DEBUG) { print(param) }
  
  cases_per_cap <- (IR100K/100000) * ( param$beta_le_30$scale * param$beta_le_30$val * D_le_30  + 
                                       param$beta_30_40$scale * param$beta_30_40$val * D_30_40  + 
                                       param$beta_40_50$scale * param$beta_40_50$val * D_40_50  + 
                                       param$beta_50_60$scale * param$beta_50_60$val * D_50_60  + 
                                       param$beta_80_ge$scale * param$beta_80_ge$val * D_80_ge  )
  
  return( cases_per_cap )
}


applyLinearHifValList <- function(  IR100K, temp, prec, param ) {
  
  if (DEBUG) { print("HERE!-> apply lin") }
  
  if (DEBUG) { print(param) }
   
  cases_per_cap <- (IR100K/100000) * ( param$alpha_temp$scale * param$alpha_temp$val * temp  + 
                                       param$alpha_precip$scale * param$alpha_precip$val * prec )
  
  return( cases_per_cap ) 
}

applyBinnedHifValCols <- function( IR100K, D_le_30, D_30_40, D_40_50, D_50_60, D_80_ge , 
                                   beta_le_30_scale, beta_le_30_val,
                                   beta_30_40_scale, beta_30_40_val,
                                   beta_40_50_scale, beta_40_50_val,
                                   beta_50_60_scale, beta_50_60_val,
                                   beta_80_ge_scale, beta_80_ge_val) {
  
  if (DEBUG) { print("HERE!-> apply bin") }

  cases_per_cap <- (IR100K/100000) * ( beta_le_30_scale * beta_le_30_val * D_le_30  + 
                                         beta_30_40_scale * beta_30_40_val * D_30_40  + 
                                         beta_40_50_scale * beta_40_50_val * D_40_50  + 
                                         beta_50_60_scale * beta_50_60_val * D_50_60  + 
                                         beta_80_ge_scale * beta_80_ge_val * D_80_ge  )
  
  return( cases_per_cap )
}


applyLinearHifValCols <- function(  IR100K, temp, prec, 
                                    alpha_temp_scale, alpha_temp_val, 
                                    alpha_precip_scale, alpha_precip_val  ) {
  
  if (DEBUG) { print("HERE!-> apply lin") }

  cases_per_cap <- (IR100K/100000) * ( alpha_temp_scale * alpha_temp_val * temp  + 
                                         alpha_precip_scale * alpha_precip_val * prec )
  
  return( cases_per_cap ) 
}


applyVSL <- function(year, cases, vsl, startYr, dr) {
  
  uv <- vsl * 1 / (dr/100 + 1)^(year - startYr + 1)
  
  return( uv * cases)
}

adjustVSLList <- function(gf, vsl_data,cpi) {
  
  vsl <- (cpi * vsl_data$refval$val * vsl_data$refval$scale) * gf^(vsl_data$elas$val * vsl_data$elas$scale)
  
  return( vsl )
}

adjustVSLCols <- function(gf, refval_scale, refval_val, elas_scale, elas_val, cpi) {
  
  vsl <- (cpi * refval_val * refval_scale) * gf^(elas_val*elas_scale)
  
  return( vsl )
}
