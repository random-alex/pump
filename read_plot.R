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
dir <- 'data/rare data'



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
                                            c('par_temp','parameter'):=list(NULL,
                                                                            str_replace_all(parameter,
                                                                                            '§§XQ01',
                                                                                            ''))
                                            ]
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

df[,sum(time),by = list(day,month)]


df[day ==20 & month == 12& type == 'мм', ] %>% 
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
#time of end of work


df_int <- tibble(time = c(1.5,4,17.5,20))
# df_int <- tibble(time = c(4.2,15,17.2,28))

# plot of all data
#int par:
# 30XAA12CT055 - температура переднего подшибника турб ПТН
# 30XAA12CY001 - ВИБР(В) ПОДШ1 ТУРБ ПТН возможно при скорости больше 3 все вырубается
# 30LAC12CY008 - Перемещение рот ПТН - доходит до -0.1 и вырубается все
gg <- df_long %>% 
  filter((parameter == '30XAA12CT055' | parameter == '30XAA12CS001')&month == 11) %>% 
  ggplot(aes(time,value,col = parameter,group = as.factor(parameter))) +
  # geom_point() +
  geom_line() +
  # geom_hline(data = tibble(value = -0.1),aes(yintercept = value)) +
  geom_vline(data = df_int,aes(xintercept = time) )+
  facet_wrap(c('type','month'),scales = 'free',nrow = 7,labeller = "label_both") +
  theme_bw()


name <- 'pics/температура_m11.png'
ggsave(name, plot = gg, width = 10,height = 8)







# save files for future proccesing ----------------------------------------
write_csv(df,'data/rare_data.csv')




df_n <-  as.tibble(df_long) %>% 
  .[,-7] %>% 
  spread(parameter,value)

write_csv(df_n,'data/prep_data_for_nnet.csv')





# tresh -------------------------------------------------------------------


write_csv(df_long,'data/prep_data.csv')

df_n <- df %>% 
  mutate_at(vars(5:49),funs(runmed,.args = list(k = 21)))

df_n[,c(1:5)] %>% 
  filter(month == 12 &day == 19) %>% 
  ggplot(aes(time,`30LAB12CF901`)) +
  # geom_point() +
  geom_line() +
  # geom_vline(data = df_int,aes(xintercept = time) )+
  facet_wrap(c('month'),scales = 'free',nrow = 7,labeller = "label_both") +
  theme_bw()

write_csv(df_n,'data/prep_data.csv')

