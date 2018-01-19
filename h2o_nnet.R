
# prereq ------------------------------------------------------------------
require(tidyverse)
require(h2o)
h2o.init(nthreads = 3,max_mem_size = '7g')
seed <-  20171027

dir <- 'data/prep_data_for_nnet.csv'


# funcions ----------------------------------------------------------------


# read and prep data ------------------------------------------------------

df <- h2o.importFile(dir)
id_train <- 1:NROW(df) *0.35

train <- df[id_train,-c(1:4)]
test <- df[,-c(1:4)]

# Identify response and predictor variables
#30XAA12CS001 - количество оборотов в минуту, интересный кандитат
y <- "30LAB12CP902"
x <- setdiff(names(df), 
             c(y,'day','month','year','time')) 

# modeling ----------------------------------------------------------------

model <- h2o.deeplearning(x,y,train,
                          nfolds = 3,
                          # activation = "RectifierWithDropout",
                          hidden = c(100,100,100,50),seed = seed
                          # variable_importances = F
                          )

pred <- as.tibble(as.numeric(h2o.predict(model, test)[,1]))
act <- as.tibble(as.numeric(test[,y]))

df_plot <-bind_cols(as.tibble(df[,c('day','month','year','time')]),pred,act) %>% 
  gather(type,value,-c(1:4))


df_plot %>% 
  # filter(month == 12 &day >15 &day<21) %>%
  ggplot(aes(time,value,col = type)) +
  geom_line() +
  facet_wrap(c('month')) +
  theme_bw()

model_glm <- h2o.glm(x,y,train)


pred <- as.tibble(as.numeric(h2o.predict(model_glm, test)[,1]))
act <- as.tibble(as.numeric(test[,y]))

df_plot <-bind_cols(as.tibble(df[,c('day','month','year','time')]),pred,act) %>% 
  gather(type,value,-c(1:4))


df_plot %>% 
  # filter(month == 12 &day >15 &day<21) %>%
  ggplot(aes(time,value,col = type)) +
  geom_line() +
  facet_wrap(c('month')) +
  theme_bw()


model_glm
ss <- h2o.coef(model_glm) %>% as.data.frame()
tibble(true_names = colnames(df)[-c(1:4)]) %>% 
  arrange(true_names)
