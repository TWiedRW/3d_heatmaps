## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2024-02-14
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(tidyverse)
# library(MASS)

base_grid <- expand.grid(x=1:10, y=1:10)
compare_values <- sample(seq(10, 90, by = 10))
bar_labels <- sample(letters[1:length(compare_values)])
base_grid

value_locs <- sample(1:nrow(base_grid), size = length(compare_values))


base_grid[value_locs, 'z'] <- compare_values
base_grid[value_locs, 'Label'] <- bar_labels



expand.grid(compare_values, compare_values) %>% 
  filter(Var1 < Var2) %>% 
  mutate(ratio = Var1/Var2 * 100) %>% 
  arrange(ratio, Var1)


from_ratio <- function(ratio){
  if(any(ratio>1)) stop('Ratio should be a value between 0 and 1')
  larger <- runif(length(ratio), min = 40, max = 100)
  smaller <- larger * ratio
  return(as_tibble(cbind(smaller, larger, ratio)))
}

from_ratio(seq(0.1, 0.9, by = 0.1)) %>% 
  pivot_longer(smaller:larger)





ggplot(base_grid, aes(x = x, y = y, fill = z)) + 
  geom_raster() + 
  geom_text(aes(label = Label)) + 
  scale_fill_gradient(low = 'white', high = 'darkblue')




generate_kernel = function(x, y, z, kernel_dist = 3, ...){
  if(is.na(z)) return(NA)
  x.save = x; y.save = y; z.save = z
  x <- seq(x - kernel_dist, x + kernel_dist, by = 1)
  y <- seq(y - kernel_dist, y + kernel_dist, by = 1)
  
  # z.star <- rnorm(length(x)*length(y), mean = z, ...)
  z.star <- rbeta(length(x)*length(y), 3, 2)*z
  
  res <- expand.grid(x=x, y=y) %>% 
    mutate(z.star = z.star, 
           z.star = ifelse(x==x.save & y==y.save, z.save, z.star)) %>% 
    filter(x > 0 & y > 0) 
  return(res)
  
}

generate_kernel(6,3, 40, 2) %>% 
  ggplot(mapping = aes(x = x, y = y, fill = z.star)) + 
  geom_raster() + 
  scale_fill_gradient(low = 'white', high = 'purple') + 
  theme_bw()



tmp <- base_grid %>% 
  mutate(contribution = pmap(list(x, y, z), generate_kernel, sd = 5)) %>% 
  dplyr::select(contribution) %>% 
  filter(!is.na(contribution)) %>% 
  unnest(contribution) %>% 
  right_join(base_grid, by = c('x'='x', 'y'='y')) %>% 
  filter(is.na(z) | z.star == z) %>% 
  group_by(x,y, Label) %>% 
  summarize(z = mean(z.star, na.rm = T)) %>% 
  mutate(z = ifelse(is.nan(z), 0, z))

p <- ggplot(tmp, aes(x = x, y = y, fill = z)) + 
  geom_raster() + 
  geom_text(aes(label = Label), size = 3) + 
  scale_fill_gradient(low = 'white', high = 'darkblue') +
  theme_bw() + 
  theme(aspect.ratio = 1/1)
p


# rayshader::plot_gg(p)


#-----------------------------------------------------------------------------#



generate_grid <- function(values, grid_dimensions = c(10, 10), use_ratios = T, ...){
  #Check valid conditions
  if(length(use_ratios) > 1 | !is.logical(use_ratios)) stop('use_ratios must be a single logical value.')
  
  #Create grid
  x <- 1:grid_dimensions[1]; y <- 1:grid_dimensions[2]
  base_grid <- expand_grid(x,y)
  
  #Generate values from ratios
  if(use_ratios){
    ratio_values <- function(values){
      larger <- runif(n = length(values), min = 50, max = 100)
      smaller <- values*larger
      comparisons <- tibble(smaller, larger, ratio = values) %>% 
        pivot_longer(smaller:larger, names_to = 'bar', values_to = 'z') %>% 
        mutate(rownum = 1:n(),
               label = letters[rownum]) %>% 
        select(-rownum)
      return(comparisons)
    }
    vals <- ratio_values(values)
  } else {
    # #Generate ratios from values
    # values_ratio <- function(values){
    #   comparisons <- expand_grid(smaller = values, larger = values) %>% 
    #     filter(smaller < larger) %>% 
    #     mutate(ratio = smaller/larger) %>% 
    #     pivot_longer(smaller:larger, names_to = 'bar', values_to = 'z') %>% 
    #     mutate(rownum = 1:n(), label = letters[rownum]) %>% 
    #     select(ratio, bar, z, label)
    #   return(comparisons)
    # }
    vals <- values_ratio(values)
  }
  #Location of values on grid
  value_locs <- sample(1:nrow(base_grid), size = nrow(vals))
  
  #Insert values into grid
  base_grid[value_locs, c('ratio', 'bar', 'z', 'label')] <- vals

  #Return grid
  return(base_grid)
}








generate_kernel = function(x, y, z, kernel_dist = 2, ...){
  if(is.na(z)) return(NA)
  x.save = x; y.save = y; z.save = z
  x <- seq(x - kernel_dist, x + kernel_dist, by = 1)
  y <- seq(y - kernel_dist, y + kernel_dist, by = 1)
  
  # z.star <- rnorm(length(x)*length(y), mean = z, ...)
  z.star <- rbeta(length(x)*length(y), ...)*z
  
  res <- expand.grid(x=x, y=y) %>% 
    mutate(z.star = z.star, 
           z.star = ifelse(x==x.save & y==y.save, z.save, z.star)) %>% 
    filter(x > 0 & y > 0) 
  return(res)
}

trial_data <- generate_grid(seq(0.1, 0.9, by = 0.1))






tmp <- trial_data %>% 
  mutate(contribution = pmap(list(x, y, z), generate_kernel, shape1 = 4, shape2 = 1)) %>% 
  dplyr::select(contribution) %>% 
  filter(!is.na(contribution)) %>% 
  unnest(contribution) %>% 
  right_join(trial_data, by = c('x'='x', 'y'='y')) %>% 
  filter(is.na(z) | z.star == z) %>% 
  group_by(x,y, label, ratio, bar) %>% 
  summarize(z = mean(z.star, na.rm = T)) %>% 
  mutate(z = ifelse(is.nan(z), rnorm(1, 50, 10), z))


generate_data <- function(ratios = NULL){
  if(is.null(ratios)) ratios <- seq(0.1, 0.9, by = 0.1)
  value_grid <- generate_grid(ratios)
  dataset <- value_grid %>% 
    mutate(contribution = pmap(list(x, y, z), generate_kernel, shape1 = 4, shape2 = 1)) %>% 
    dplyr::select(contribution) %>% 
    filter(!is.na(contribution)) %>% 
    unnest(contribution) %>% 
    right_join(value_grid, by = c('x'='x', 'y'='y')) %>% 
    filter(is.na(z) | z.star == z) %>% 
    group_by(x,y, label, ratio, bar) %>% 
    summarize(z = mean(z.star, na.rm = T),
              .groups = 'drop') %>% 
    mutate(z = ifelse(is.nan(z), rnorm(1, 50, 10), z))
  return(dataset)
}

generate_data()
