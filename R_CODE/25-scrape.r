# 25-scrape.R - R code to scrape dog information from http://www.akc.org/dog-breeds 
# Output: file DATA/akc_dog_breeds.csv which contains text descriptions (aka features) for dog breeds
library(tidyverse)
library(rvest)

# set your working directory
setwd("C:/TEMP/DATA/kaggle-animal-shelter-outcomes")

# scraping function
get_dogs <- function(url) {
  html <- read_html(url)
  
  dogs <- html %>% 
    html_nodes('.scale-img-parent')
  
  dog <- dogs %>% 
    html_nodes('h2 a') %>% 
    html_text()
  
  desc <- dogs %>% 
    html_nodes('p') %>% 
    html_text()
  
  link <- dogs %>% 
    html_nodes('p+ a') %>% 
    html_attr('href')
  
  tibble(dog = dog, desc = desc, link = link)
}

# not using this function, but can be used to get all the dogs on the main page (not in groups)
get_dogs_pagination <- function(url) {
  tibble(url = url, letter = LETTERS) %>% 
    mutate(url = paste0(url, '?letter=',letter)) %>% 
    group_by(url) %>% 
    do(get_dogs(.$url[[1]]))
}

# initial data.frame (tibble is same as data.frame)
dog_groups <- tibble(group = c('Herding Group',
                 'Hound',
                 'Working',
                 'Non-Sporting',
                 'Sporting',
                 'Terrier',
                 'Toy',
                 'Foundation Stock Service',
                 'Miscellaneous Class',
                 'Largest Dog Breeds',
                 'Medium Sized Dogs',
                 'Smallest Dog Breeds',
                 'Best Dogs for Apartments',
                 'Best Family Dogs',
                 'Best Guard Dogs',
                 'Best Dogs for Kids',
                 'Hypoallergenic Dogs',
                 'Smartest Dogs',
                 'Hairless Dog Breeds'),
       url = c('http://www.akc.org/dog-breeds/groups/herding/',
               'http://www.akc.org/dog-breeds/groups/hound/',
               'http://www.akc.org/dog-breeds/groups/working/',
               'http://www.akc.org/dog-breeds/groups/non-sporting/',
               'http://www.akc.org/dog-breeds/groups/sporting/',
               'http://www.akc.org/dog-breeds/groups/terrier/',
               'http://www.akc.org/dog-breeds/groups/toy/',
               'http://www.akc.org/dog-breeds/groups/foundation-stock-service/',
               'http://www.akc.org/dog-breeds/groups/miscellaneous-class/',
               'http://www.akc.org/dog-breeds/largest-dog-breeds/',
               'http://www.akc.org/dog-breeds/medium-sized-dogs/',
               'http://www.akc.org/dog-breeds/smallest-dog-breeds/',
               'http://www.akc.org/dog-breeds/best-dogs-for-apartments/',
               'http://www.akc.org/dog-breeds/best-family-dogs/',
               'http://www.akc.org/dog-breeds/best-guard-dogs/',
               'http://www.akc.org/dog-breeds/best-dogs-for-kids/',
               'http://www.akc.org/dog-breeds/hypoallergenic-dogs/',
               'http://www.akc.org/dog-breeds/smartest-dogs/',
               'http://www.akc.org/dog-breeds/hairless-dog-breeds/'),
       type = c(rep('Group',9),rep('Size',3),rep('Characteristic',7))
       )

# do scrape for each group
dogs_df <- dog_groups %>% 
  group_by(group, type) %>% 
  do(get_dogs(.$url[[1]]))

# write to csv
dogs_df %>% 
  write_csv('DATA/akc_dog_breeds.csv')

