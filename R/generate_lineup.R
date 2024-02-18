## ---------------------------
##
## Script name: generate_lineup.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2024-02-17
##
## ---------------------------
##
## Notes:
##   Block is subject
##   Use 5 ratios per block (20 total runs)
##   CRD with plot types
##
## ---------------------------

library(tidyverse)

#Treatments
ratio <- factor(seq(0.1, 0.9, by = 0.1))
plot <- c('2d', '3ddc', '3dds', '3dp')

#Whole-plot 
bib <- agricolae::design.bib(
  trt = ratio,
  k = 5
)

wp <- bib$book %>% 
  mutate(block = as.numeric(block)-1,
         trt.ratio = ratio) %>% 
  select(-c(ratio, plots))

#Split-plot
sp <- tibble(
  trt.ratio = rep(ratio, times = length(plot)),
  plot = rep(plot, each = length(ratio))
)

#Design
design <- left_join(wp, sp, by = 'trt.ratio',
          relationship = 'many-to-many')


#Assign datasets
#9 datasets per type? or 3?

datasets <- tibble(
  plot = rep(plot, each = length(ratio))
) %>% 
  group_by(plot) %>% 
  mutate(plotID = 1:n(), dummy = 1) %>% 
  mutate(df = map(dummy, generate_data)) %>% 
  unnest(df)

datasets %>% 
  filter(plotID == 1 & plot == '2d')  %>% View()


#-----------------------------------------------------------------------------#
generate_lineup <- function(ratio = NULL, plot = NULL, wp.size = 5, file=NULL){
  
  
  #Default treatments
  if(is.null(ratio)){
    ratio <- factor(seq(0.1, 0.9, by = 0.1))
  } else{
    ratio <- factor(ratio)
  }
  
  if(is.null(plot)){
    plot <- c('2d', '3ddc', '3dds', '3dp')
  } else{
    plot <- factor(plot)
  }
  
  
  
  
  
  
  #Whole-plot 
  bib <- agricolae::design.bib(
    trt = ratio,
    k = wp.size
  )
  
  wp <- bib$book %>% 
    mutate(block = as.numeric(block)-1,
           trt.ratio = ratio) %>% 
    select(-c(ratio, plots))
  
  #Split-plot
  sp <- tibble(
    trt.ratio = rep(ratio, times = length(plot)),
    plot = rep(plot, each = length(ratio))
  )
  
  #Design
  design <- left_join(wp, sp, by = 'trt.ratio',
                      relationship = 'many-to-many')
  
  
  #Assign datasets
  #9 datasets per type? or 3?
  
  datasets <- tibble(
    plot = rep(plot, each = length(ratio))
  ) %>% 
    group_by(plot) %>% 
    mutate(plotID = 1:n(), dummy = 1) %>% 
    mutate(nest(generate_data())) %>% 
    unnest(data)
  
  if(!is.null(file)) write.csv(datasets, file)
  
  return(datasets)
  
}

