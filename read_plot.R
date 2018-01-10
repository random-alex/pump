require(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
require(readr)
require(stringr)
require(data.table)
require(tidyverse)


# prereq ------------------------------------------------------------------

dir_colnames <- 'D:/R projects/itclover/test zad/Расшифровка сигналов.xlsx'
dir <- 'D:/R projects/itclover/test zad/data/'



# functions ---------------------------------------------------------------
my_read <- function(fl_dir,col_nm){
  naColsRemoval <- function (DataTable) {
    na.cols = DataTable [ , .( which ( apply ( is.na ( .SD ) , 2 , all ) ) )]
    DataTable [ , unlist (na.cols) := NULL ]
  }
  dat <- naColsRemoval(fread(fl_dir))[,TIME := str_replace(TIME,',[[:digit:]]{3}','')
                                      ][,c("hour",'min','sec'):= tstrsplit(TIME,
                                                                           ":", 
                                                                           fixed=T)
                                        ][sec == '00'| sec == '15'|
                                            sec == '30'| sec == '45',
                                          ]
  colnames(dat) <- c('DATE','TIME',col_nm,"hour",'min','sec')
  dat <- melt(dat,id.vars = c("DATE", "TIME"),
              variable.name = "par_temp")[,
                                          c("parameter", 
                                            "type") := tstrsplit(par_temp,
                                                                 "_", 
                                                                 fixed=TRUE)][,-3]
  return(dat)  
}

# read file ---------------------------------------------------------------
df_colnm <- read.xlsx(dir_colnames) %>% 
  unite('name',c('KKS','ед.изм')) %>% 
  .$name

df <- tibble(dirs = list.files(dir,full.names = T)) %>% 
  mutate(data = map(dirs,my_read,df_colnm))%>% 
  unnest(data) %>% .[,-1]



# some plots --------------------------------------------------------------

df %>%
  filter(DATE ==  '19.12.2013') %>% 
  ggplot(aes(TIME,value,col = type,group = as.factor(parameter))) +
  geom_point() +
  geom_line() +
  facet_wrap(c('type'),scales = 'free') +
  theme_bw()













