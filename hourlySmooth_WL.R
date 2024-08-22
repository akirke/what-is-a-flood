# Floodpulse Project 2024
# Ashleigh Kirker
# take corrected waterlevel from the updated data sheet and smooth from 15 min
# to hourly data.
# then plot for fun

#-------------------------------------------------------------------------------
#clear environment
remove(list = ls())

#load libraries 
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(patchwork)

#-------------------------------------------------------------------------------
#Function for reading WL data. Takes as arguments the excel file name and the
#number of header rows to skip. The 'guess_max' here is to handle the NAs in 
#these data, which we'll filter out later
read_excel_allsheets <- function(filename, skprws) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) 
  readxl::read_excel(filename, sheet = X, skip=skprws, na="", guess_max=21474836))
  names(x) <- sheets
  x
}

#Function for smoothing individual well data by hour
hoursmooth <- function(wellname){
  newwell <- wellname %>%
    mutate(hour=floor_date(datetime, "hour")) %>%
    group_by(hour) %>%
    summarise(WL = mean(water_level_cm_corrected)) %>%
    select(hour, WL)
  newwell
}

#Function for plotting 
wl_plot <- function(init_wl, gwWell, swWell, wetWell, startdate, enddate){
  wl_df <- init_wl %>%
    select(hour, all_of(gwWell), all_of(swWell), all_of(wetWell)) %>%
    pivot_longer(cols =!hour, names_to="well", values_to="WL") %>%
    filter(hour > ymd(startdate)) %>%
    filter(hour < ymd(enddate)) %>%
    mutate(well = factor(well, levels = c(gwWell, swWell, wetWell)))
  
  wl_plt <- wl_df %>%
    ggplot(aes(x=hour, y=WL, color=well)) +
    geom_line() +
    theme_bw()+
    scale_color_manual(values = c("#1f78b4","#a6cee3", "#b2df8a")) +
    geom_hline(aes(yintercept = 0))+   
    ylab("Water Level relative to Ground Surface (cm)") +
    xlab("Date")
  wl_plt
} 

#-------------------------------------------------------------------------------
#get all the water level data as a list of tibbles
list_wl <- read_excel_allsheets("UpdatedDataSheet.xlsx",5)

#select only the datetime and corrected water level columns, 
#and drop rows with missing data
sel_wl <- lapply(list_wl, function(well){
  well %>% 
    select(datetime, water_level_cm_corrected) %>% 
    drop_na()
})


#Smooth the data so WL is hourly, not 15-minute. 
#This helps with the funky HS data
hr_wl <-lapply(sel_wl, hoursmooth)

#Join the sheets together (this probably should be a function but I am learning still)
well_name_list <- names(hr_wl)
well_num_list <- 2:length(hr_wl)

wl_df <- hr_wl[[1]]
colnames(wl_df)[2] <- well_name_list[1]

for (i in well_num_list){
  cur_wl <- hr_wl[[i]]
  colnames(cur_wl)[2] <- well_name_list[i]
  wl_df <- left_join(wl_df, cur_wl, join_by(hour))
}
  
#-------------------------------------------------------------------------------

#some plots
#input (WL_df, "GW-well", "SW-well", "Wetland-well", "start date", "end date")

hs1_wet <- wl_plot(wl_df, "HS1-2", "HS1-1", "HS1-3", "2024-02-01", "2024-04-15")
hs1_wet

hs1_dry <- wl_plot(wl_df, "HS1-2", "HS1-1", "HS1-3", "2023-06-15", "2023-09-01")
hs1_dry

it1_wet <-wl_plot(wl_df, "IT1-4", "IT1-1", "IT1-3", "2024-02-01", "2024-04-15")
it1_wet

it1_dry <- wl_plot(wl_df,  "IT1-4", "IT1-1", "IT1-3", "2023-06-15", "2023-09-01")
it1_dry

fp1_wet <- wl_plot(wl_df, "FP1-1", "FP2-1", "FP1-2", "2024-02-01", "2024-04-15")
fp1_wet

fp1_dry <- wl_plot(wl_df, "FP1-1", "FP2-1", "FP1-2", "2023-06-15", "2023-09-01")
fp1_dry
