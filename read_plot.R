# nes. libs ---------------------------------------------------------------

require(openxlsx)
#dir to Rtools,require to make openxlsx work
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
require(forcats)
require(readr)
require(stringr)
require(data.table)
require(multidplyr)
require(tidyverse)


# dirs  ------------------------------------------------------------------

dir_colnames <- 'Расшифровка сигналов.xlsx'
dir <- 'data'



# functions ---------------------------------------------------------------
my_read <- function(fl_dir,col_nm){
  naColsRemoval <- function (DataTable) {
    na.cols = DataTable [ , .( which ( apply ( is.na ( .SD ) , 2 , all ) ) )]
    DataTable [ , unlist (na.cols) := NULL ]
  }
  my_prep_value <- function(x) {
    
    as.numeric(str_replace(x,',','.'))
    
  }
  
  dat <- naColsRemoval(fread(fl_dir))[,
                                      time := as.numeric(lubridate::hms(str_replace(TIME,
                                                                                    ',[[:digit:]]{3}',
                                                                                    '')))/3600
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

#parallel reading
cl <- 3 #num of clusters

start <- proc.time()
cluster <- create_cluster(cl)
df <- tibble(dirs = list.files(dir,full.names = T),
             id = rep(1:cl, length.out = NROW(dirs))) %>% 
  partition(id,cluster = cluster) %>% 
  cluster_library(c('data.table','lubridate','stringr','purrr')) %>% 
  cluster_assign_value('my_read',my_read) %>% 
  cluster_assign_value('df_colnm',df_colnm) %>% 
  mutate(data = map(dirs,my_read,df_colnm))%>% 
  collect() %>% 
  unnest(data) %>% 
  ungroup %>% 
  .[,-c(1,2)] %>% 
  data.table() %>% 
  .[,
    c('parameter','type'):= list(as_factor(parameter),as_factor(type))
    ]
stopCluster(cluster)
print(proc.time() - start)


# some plots --------------------------------------------------------------



df[day == 19 & month == 12 , ] %>% 
  ggplot(aes(time,value,col = type,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() + 
  geom_vline(data = tibble(time = c(7,8)),aes(xintercept = time) )+
  facet_wrap(c('type'),scales = 'free') +
  theme_bw()

gg <- df[day %in% c(3,19) &  month == 12,] %>% 
  ggplot(aes(time,value,col = parameter,group = as.factor(month):as.factor(parameter))) +
  # geom_point() +
  geom_line() +
  # geom_vline(data = tibble(time = c(7,8)),aes(xintercept = time) )+
  facet_wrap(c('day','type'),scales = 'free',nrow = 2,labeller = "label_both") +
  theme_bw()

name <- 'pics/3-19_comp.png'
ggsave(name, plot = gg, width = 18,height = 10)
df[day == 2 & month == 12, ] %>% 
  ggplot(aes(time,value,col = type,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() + 
  geom_vline(data = tibble(time = c(7,8)),aes(xintercept = time) )+
  facet_wrap(c('type'),scales = 'free') +
  theme_bw()


# pics with month data on one plot ----------------------------------------


df_long <- copy(df)

df_long <- df_long[,
              time:=((month - min(month)) * 30*24  + (day - min(day))*24 + time)/24 - (month - min(month)) * 30
              ]
# date of incedent
df_int <- tibble(time = c(30*24+18*24,30*24+19*24))

# plot of all data
gg_all <- df_long%>% 
  ggplot(aes(time,value,col = parameter,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() +
  # geom_vline(data = df_int,aes(xintercept = time) )+
  facet_wrap(c('type','month'),scales = 'free',nrow = 7,labeller = "label_both") +
  theme_bw()
name <- 'pics/all_param.png'
ggsave(name, plot = gg_all, width = 18,height = 10)




