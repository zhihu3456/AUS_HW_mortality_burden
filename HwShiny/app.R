#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(spData)
library(dplyr)
library(sf)
library(bslib)
library(DT)
library(tidyverse)

rm(list=ls());gc()
setwd("S:/MNHS-SPHPM-EPM/ZhxClimate/Project/AU-HWproj/HwShiny")
dat <- read_sf('data/mapdata_simplify.shp')
# dat %>% 
#   st_simplify(dTolerance = 10) -> dat
dat %>% 
  mutate_if(is.numeric,round,3) -> dat

dat %>% 
  mutate(rr=ifelse(rr<1,1,rr),
         rrlow=ifelse(rrlow<1,1,rrlow),
         rrhigh=ifelse(rrhigh<1,1,rrhigh),
         an=ifelse(an<0,0,an),
         anlow=ifelse(anlow<0,0,anlow),
         anhigh=ifelse(anhigh<0,0,anhigh),
         af=ifelse(af<0,0,af),
         aflow=ifelse(aflow<0,0,aflow),
         afhigh=ifelse(afhigh<0,0,afhigh),
         amr=ifelse(amr<0,0,amr),
         amrlow=ifelse(amrlow<0,0,amrlow),
         amrhigh=ifelse(amrhigh<0,0,amrhigh),
         amr=ifelse(amr>quantile(amr,0.99),quantile(amr,0.99),amr),
         amrlow=ifelse(amrlow>quantile(amrlow,0.99),quantile(amrlow,0.99),amrlow),
         amrhigh=ifelse(amrhigh>quantile(amrhigh,0.99),quantile(amrhigh,0.99),amrhigh)) %>% 
  rename(RR=rr,RRlow=rrlow,RRhigh=rrhigh,
         AN=an,ANlow=anlow,ANhigh=anhigh,
         AF=af,AFlow=aflow,AFhigh=afhigh,
         AMR=amr,AMRlow=amrlow,AMRhigh=amrhigh) -> dat_plt

dat_plt


