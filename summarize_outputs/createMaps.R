# ICF - Anna Belova - October 2021
# Mapping results for the manuscript
# UPDATED - April 2022

library(rgeos)
library(raster)
library(tmap)      
library(tmaptools)
library(maptools)
library(tidyverse)
library(RColorBrewer)

DATA_DIR <- Sys.getenv("DATA_LOC")


# SHAPE FILE FROM download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = f)
shp <- shapefile(file.path(DATA_DIR,"gis/gz_2010_us_050_00_20m.shp"))
shp <- shp[!(shp$STATE %in% c("02","15","72")),] 
shp$FIPS <- paste0(shp$STATE, shp$COUNTY)

US_states <- unionSpatialPolygons(shp, IDs=shp$STATE)

# Load data ------ 
res1<- read_csv(file.path(DATA_DIR,"results/D1_Scenario_PTMODE-TRUE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015_2021-1018-212335.csv"))
res3<- read_csv(file.path(DATA_DIR,"results/D3_Scenario_PTMODE-TRUE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015_2021-1018-214844.csv"))

# County recoding
# Shannon County (FIPS 46113) was renamed to Oglala Lakota County (FIPS 46102) in 2015.
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# The distributed suicide incidence data was representative of Shannon county, FIPS 46113

res1 <- res1 %>% 
  mutate(FIPS = ifelse(FIPS=="46102","46113",FIPS))
res3 <- res3 %>% 
  mutate(FIPS = ifelse(FIPS=="46102","46113",FIPS))



# Map cases ------ 

pal <- brewer.pal(11,"RdBu")
pal1 <- rev(pal[1:7]) 
pal3 <- rev(pal[1:6]) 

pd1 <- res1 %>% 
  group_by(FIPS,HIF,MODEL) %>% 
  summarize(value=sum(CASES_PT)) %>% 
  ungroup() %>% 
  group_by(FIPS) %>% 
  summarise(values=mean(value)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd1, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd1 %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette =pal1, breaks = c(-Inf, 0, 0.02, 0.05, 0.1, 10, 100, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(a) Number of Excess Annual Suicides at 1°C", 
            main.title.size = 5,
            main.title.position="center",
            legend.text.size=3.5) 

tmap_save(mm, file.path(DATA_DIR,"results/D1_cases.png"), width=32, dpi=600)


pd3 <- res3 %>% 
  group_by(FIPS,HIF,MODEL) %>% 
  summarize(value=sum(CASES_PT)) %>% 
  ungroup() %>% 
  group_by(FIPS) %>% 
  summarise(values=mean(value)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd3, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd3 %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette = pal3,breaks = c(0, 0.02, 0.05, 0.1, 10, 100, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(b) Number of Excess Annual Suicides at 3°C",
            main.title.size = 5,
            main.title.position="center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,"results/D3_cases.png"), width=32, dpi=600)


# Map incidence ------ 


pal <- brewer.pal(11,"RdBu")
pal1 <- rev(pal[1:7]) 
pal3 <- rev(pal[1:6]) 

pd1 <- res1 %>% 
  group_by(FIPS,HIF,MODEL) %>% 
  summarize(cases=sum(CASES_PT), pop=sum(POP_SIZE)) %>% 
  ungroup() %>% 
  mutate(values = 100000 * cases / pop) %>%
  group_by(FIPS) %>% 
  summarise(values=mean(values)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd1, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd1 %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette =  pal1 ,breaks = c(-Inf, 0, 0.05, 0.1, 0.2, 0.3, 0.5, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(c) Excess Annual Suicides per 100K at 1°C",
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,"results/D1_inc.png"), width=32, dpi=600)


pd3 <- res3 %>% 
  group_by(FIPS,HIF,MODEL) %>% 
  summarize(cases=sum(CASES_PT), pop=sum(POP_SIZE)) %>% 
  ungroup() %>% 
  mutate(values = 100000 * cases / pop) %>%
  group_by(FIPS) %>% 
  summarise(values=mean(values)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd3, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd3 %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette = pal3 ,breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.5, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(d) Excess Annual Suicides per 100K at 3°C", 
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,"results/D3_inc.png"), width=32, dpi=600)


# Map PAF ------ 

pal <- brewer.pal(11,"RdBu")
pal1 <- rev(pal[1:7]) 
pal3 <- rev(pal[1:6])

pd1 <- res1 %>% 
  group_by(FIPS,HIF,MODEL) %>% 
  summarize(cases=sum(CASES_PT), inc=sum(IR100K * POP_SIZE / 100000 )) %>% 
  ungroup() %>% 
  mutate(values = 100 * cases / inc) %>%
  group_by(FIPS) %>% 
  summarise(values=mean(values)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd1, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd1 %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette = pal1 ,breaks = c(-Inf, 0, 0.25,  0.5, 1, 2, 3, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(e) Percent Change in Baseline Suicide Incidence at 1°C",
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,"results/D1_paf.png"), width=32, dpi=600)


pd3 <- res3 %>% 
  group_by(FIPS,HIF,MODEL) %>% 
  summarize(cases=sum(CASES_PT), inc=sum(IR100K * POP_SIZE / 100000 )) %>% 
  ungroup() %>% 
  mutate(values = 100 * cases / inc) %>%
  group_by(FIPS) %>% 
  summarise(values=mean(values)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd3, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd3 %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette =  pal3 ,breaks = c(0,0.25, 0.5, 1, 2, 3, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(f) Percent Change in Baseline Suicide Incidence at 3°C",
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,"results/D3_paf.png"), width=32, dpi=600)


# Map climate by GCM ------ 

pal <- brewer.pal(11,"RdBu")
pal_mod <- rev(pal[1:6]) 

MODEL_LIST <- res1 %>% distinct(MODEL) %>% pull(MODEL)

for (mod in MODEL_LIST) {

pdm <- res3 %>% filter(MODEL==mod) %>%
  group_by(FIPS) %>% 
  summarize(values=mean(TEMP) ) %>% 
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

avgVal <- res3 %>% filter(MODEL==mod) %>%
  group_by(FIPS,AGE,SEX) %>% 
  summarize(values=mean(TEMP), pop=mean(POP_SIZE), ir=mean(IR100K/100000) ) %>% 
  ungroup() %>%
  group_by(FIPS) %>%
  summarize(values=mean(values), inc=sum(pop*ir) ) %>% 
  ungroup() %>%
  summarize(avg=sum(values*inc) / sum(inc) ) %>% pull(avg)


shp_pd <- merge(shp, pdm, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pdm %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette = pal_mod ,breaks = c(0,  1, 2, 3, 4, 5, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title=paste0(mod, " (incidence-weighted increase of ",signif(avgVal,3),"°C)"),
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,paste0("results/D3_avgTemp_",mod,".png")), width=32, dpi=600)

}


for (mod in MODEL_LIST) {
  
  pdm <- res3 %>% filter(MODEL==mod) %>%
    group_by(FIPS) %>% 
    summarize(values=mean(D_80_ge) ) %>% 
    ungroup() %>%
    mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 
  
  avgVal <- res3 %>% filter(MODEL==mod) %>%
    group_by(FIPS,AGE,SEX) %>% 
    summarize(values=mean(D_80_ge), pop=mean(POP_SIZE), ir=mean(IR100K/100000) ) %>% 
    ungroup() %>%
    group_by(FIPS) %>%
    summarize(values=mean(values), inc=sum(pop*ir) ) %>% 
    ungroup() %>%
    summarize(avg=sum(values*inc) / sum(inc) ) %>% pull(avg)
  
  
  shp_pd <- merge(shp, pdm, by = "FIPS",all.x=TRUE,all.y=FALSE)
  
  # County recoding
  # Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
  # Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
  # We assign values from 51019 to 51515.
  val <- pdm %>% filter(FIPS=="51019") %>% pull(values)
  shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 
  
  
  mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
    tm_polygons("values", border.col = "grey30", title="",palette = pal_mod,breaks = c(0,  1, 4, 6, 8, 10, Inf)) +
    tm_shape(US_states) +
    tm_borders(lwd=2, col = "black", alpha = .5) +
    tm_layout(main.title=paste0(mod, " (incidence-weighted increase of ",signif(avgVal,3)," days)"),
              main.title.size = 5,
              main.title.position = "center",
              legend.text.size=3.5)
  
  tmap_save(mm, file.path(DATA_DIR,paste0("results/D3_D80ge_",mod,".png")), width=32, dpi=600)
  
}


for (mod in MODEL_LIST) {
  
  pdm <- res3 %>% filter(MODEL==mod) %>%
    group_by(FIPS) %>% 
    summarize(values=mean(PREC) ) %>% 
    ungroup() %>%
    mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 
  
  avgVal <- res3 %>% filter(MODEL==mod) %>%
    group_by(FIPS,AGE,SEX) %>% 
    summarize(values=mean(PREC), pop=mean(POP_SIZE), ir=mean(IR100K/100000) ) %>% 
    ungroup() %>%
    group_by(FIPS) %>%
    summarize(values=mean(values), inc=sum(pop*ir) ) %>% 
    ungroup() %>%
    summarize(avg=sum(values*inc) / sum(inc) ) %>% pull(avg)
  
  
  shp_pd <- merge(shp, pdm, by = "FIPS",all.x=TRUE,all.y=FALSE)
  
  # County recoding
  # Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
  # Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
  # We assign values from 51019 to 51515.
  val <- pdm %>% filter(FIPS=="51019") %>% pull(values)
  shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 
  
  
  mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
    tm_polygons("values", border.col = "grey30", title="",palette = "seq",breaks = c(-Inf, -8, -3,  0, 3, 8, 20,  Inf)) +
    tm_shape(US_states) +
    tm_borders(lwd=2, col = "black", alpha = .5) +
    tm_layout(main.title=paste0(mod, " (incidence-weighted increase of ",signif(avgVal,3)," mm)"),
              main.title.size = 5,
              main.title.position = "center",
              legend.text.size=3.5, aes.palette = list(seq = "RdBu"))
  
  tmap_save(mm, file.path(DATA_DIR,paste0("results/D3_PREC_",mod,".png")), width=32, dpi=600)
  
}

# Map population and age- and sex- standardized incidence rate -----


pal <- brewer.pal(11,"RdBu")
pal_pop <- rev(pal[1:6]) 
pal_inc <- rev(pal[1:5])

pd_p <- res1 %>% 
  group_by(FIPS,AGE,SEX) %>% 
  summarise(pop=mean(POP_SIZE )) %>% 
  ungroup() %>% 
  group_by(FIPS) %>%
  summarise(values = sum(pop)) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd_p, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd_p %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette = pal_pop,breaks = c(100, 10000, 20000, 50000, 100000, 1000000, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(a) Population Size",
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5)

tmap_save(mm, file.path(DATA_DIR,"results/Pop2015.png"), width=32, dpi=600)

pd_distr <- res1 %>% 
  group_by(FIPS,AGE,SEX) %>% 
  summarise(pop=mean(POP_SIZE) ) %>%
  ungroup() %>%
  group_by(AGE,SEX) %>%
  summarise(nat_pop=sum(pop)) %>%
  ungroup()

pd_ir <- inner_join(res1 ,pd_distr,by=c("AGE","SEX") ) %>% 
  group_by(FIPS,AGE,SEX) %>% 
  summarise(pop=mean(POP_SIZE), inc=mean(IR100K /100000 ), nat_pop=mean(nat_pop) ) %>% 
  ungroup() %>% 
  group_by(FIPS) %>% 
  summarise(values=100000 * sum(inc * nat_pop) / sum(nat_pop), values_fips= 100000 * sum(inc*pop) / sum(pop) ) %>%
  ungroup() %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),paste(FIPS))) 

shp_pd <- merge(shp, pd_ir, by = "FIPS",all.x=TRUE,all.y=FALSE)

# County recoding
# Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into Bedford County (FIPS 51019).
# Source: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
# We assign values from 51019 to 51515.
val <- pd_ir %>% filter(FIPS=="51019") %>% pull(values)
shp_pd[(shp_pd$FIPS=="51515"),"values"] <- val 


mm <- tm_shape(shp_pd, projection="+init=epsg:2163") +
  tm_polygons("values", border.col = "grey30", title="",palette = pal_inc,breaks = c( 5, 14, 16, 18, 24, Inf)) +
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .5) +
  tm_layout(main.title="(b) Standardized Suicide Rate per 100K",
            main.title.size = 5,
            main.title.position = "center",
            legend.text.size=3.5 )

tmap_save(mm, file.path(DATA_DIR,"results/SIR100K.png"), width=32, dpi=600)

