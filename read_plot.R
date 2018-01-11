require(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
require(readr)
require(stringr)
require(data.table)
require(tidyverse)


# prereq ------------------------------------------------------------------

dir_colnames <- 'D:/R projects/itclover/pump/Расшифровка сигналов.xlsx'
dir <- 'D:/R projects/itclover/pump/data/'



# functions ---------------------------------------------------------------
my_read <- function(fl_dir,col_nm){
  naColsRemoval <- function (DataTable) {
    na.cols = DataTable [ , .( which ( apply ( is.na ( .SD ) , 2 , all ) ) )]
    DataTable [ , unlist (na.cols) := NULL ]
  }
  my_prep_value <- function(x) {
    
    as.numeric(str_replace(x,',','.'))
    
  }
  
  dat <- naColsRemoval(fread(fl_dir))[,time := as.numeric(lubridate::hms(str_replace(TIME,',[[:digit:]]{3}','')))/3600
                                      ][,c('day','month','year'):=tstrsplit(DATE,".", fixed=T)
                                        ][,c('DATE','TIME'):=NULL
                                          ][,lapply(.SD, my_prep_value)
                                            ][,
                                              lapply(.SD,median),by = .(time <- round(time,2))
                                              ]
                                      
  colnames(dat) <- c('time',col_nm,'day','month','year')
  dat <- melt(dat,id.vars = c('day','month','year',"time"),
              variable.name = "par_temp")[,
                                          c("parameter", 
                                            "type") := tstrsplit(par_temp,
                                                                 "_", 
                                                                 fixed=TRUE)
                                          ][,
                                            c('par_temp'):=NULL]
  return(dat)  
}

# read file ---------------------------------------------------------------
df_colnm <- read.xlsx(dir_colnames) %>% 
  unite('name',c('KKS','ед.изм')) %>% 
  .$name

df <- tibble(dirs = list.files(dir,full.names = T)) %>% 
  mutate(data = map(dirs,my_read,df_colnm))%>% 
  unnest(data) %>% 
  .[,-1] 



# some plots --------------------------------------------------------------






df %>%
  filter(DATE ==  '19.12.2013') %>% 
  ggplot(aes(as.numeric(time),value,col = type,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() +
  facet_wrap(c('type'),scales = 'free') +
  theme_bw()

df %>%
  filter(DATE ==  '19.12.2013' & type == 'мм') %>% 
  ggplot(aes(as.numeric(time),value,col = parameter,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() +
  facet_wrap(c('type'),scales = 'free') +
  theme_bw()

int_par <- c("30LAC12CY007§§XQ01","30LAC12CY008§§XQ01")

df[ df[['parameter']]%in% int_par  ]
  ggplot(aes(time,value,col = DATE,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() +
  facet_wrap(c('type'),scales = 'free') +
  theme_bw()

  









