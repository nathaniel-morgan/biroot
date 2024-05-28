# Load in Packages --------------------------------------------------------

library(tidyverse)
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library(tibble)
library(tidyr)
library(dplyr)
library(MASS)
library(tidymodels)
library(tidyclust) 
library(smoothr)
library(tictoc)
#Alerts
library(cli)
#parsnip extension
library(kknn)
library(themis)
library(klaR)
library(discrim)
#Just for Visuals
library(patchwork)
#Datasets
library(readr)
library(modeldata)
library(dataset)
tuesdata <- tidytuesdayR::tt_load(2022,week =13)
sports <- tuesdata$sports
library(titanic)
titanic <- bind_rows(titanic_train,titanic_test) |> as_tibble()
math <- read.csv("/Users/nathaniel_morgan1/Library/CloudStorage/Box-Box/Advanced Data Driven Methods/student/student-mat.csv",sep = ";") |> 
  as_tibble() |> janitor::clean_names() |> 
  mutate(fail1=as.factor(as.numeric(g1<10)),
         fail2=as.factor(as.numeric(g2<10)),
         fail3=as.factor(as.numeric(g3<10)))
por <- read.csv("/Users/nathaniel_morgan1/Library/CloudStorage/Box-Box/Advanced Data Driven Methods/student/student-por.csv",sep = ";") |> 
  as_tibble() |> janitor::clean_names() |> 
  mutate(fail1=as.factor(as.numeric(g1<10)),
         fail2=as.factor(as.numeric(g2<10)),
         fail3=as.factor(as.numeric(g3<10))) |> filter(g1!=0)
library(baseballr)
#Testing
library(ggtrace)

rdata <- function(n, n_groups = 3, radius = 3) {
  list_of_dfs <- lapply(0:(n_groups-1), function(k) {
    mu <- c(cos(2*k*pi/n_groups), sin(2*k*pi/n_groups))
    m <- MASS::mvrnorm(n, radius*mu, diag(2))
    structure(data.frame(m, as.character(k)), names = c("x", "y", "c"))
  })
  do.call("rbind", list_of_dfs)
}

select <- dplyr::select

# Set up Data -------------------------------------------------------------

set.seed(17)

df1 <- tibble(x = rnorm(40),y=rnorm(40)+2,class = factor("A")) |> 
  bind_rows(tibble(x = rnorm(40,sd=2)+2,y=rnorm(40,sd=2)-1,class = factor("B"))) |> 
  bind_rows(tibble(x = rnorm(40,sd=1/2)+2,y=rnorm(40,sd=1/2)+1,class = factor("C")))

si = 3/4
n = 50
df2 <- tibble(x = rnorm(n,sd=si),y=rnorm(n,sd=si)+2,class = factor("A")) |> 
  bind_rows(tibble(x = rnorm(n,sd=si)+2,y=rnorm(n,sd=si)-1,class = factor("B"))) |> 
  bind_rows(tibble(x = rnorm(n,sd=si)+2,y=rnorm(n,sd=si)+1,class = factor("C"))) |> 
  bind_rows(tibble(x = rnorm(n,sd=si)-2,y=rnorm(n,sd=si)+1,class = factor("D"))) |> 
  bind_rows(tibble(x = rnorm(n,sd=si)-2,y=rnorm(n,sd=si)-1,class = factor("E")))

mesh1 <- expand_grid(x=seq(min(df1$x),max(df1$x),length.out = 100),
                     y=seq(min(df1$y),max(df1$y),length.out = 100))
mesh2 <- expand_grid(x=seq(min(df2$x),max(df2$x),length.out = 100),
                     y=seq(min(df2$y),max(df2$y),length.out = 100))


# Plot testing -----------------------------------------------------------


ggplot(data=df2, aes(x, y))+
  geom_point_fill(aes(fill = class))+
  geom_classify(res = 1e2,method = kmeans_method,args = list(n=5,method="lda"))

qda_mod <- discrim_quad() %>%
  set_mode("classification") %>%
  set_engine("MASS")

rf_mod <- rand_forest(mode="classification") |> 
  set_engine("ranger",importance = "impurity")

ggplot(data=df1, aes(x, y, class = class))+
  geom_point_fill(aes(fill = class))+
  stat_classify(alpha = 0.35,res = 1e2,method = rf_mod)+
  ggplot(data=df1, aes(x, y, class = class))+
  geom_point_fill(aes(fill = class))+
  stat_classify(alpha = 0.35,res = 1e2,method = qda_mod)


# Example Code for geom_classify --------------------------------------

set.seed(17)

si = 3/4
n = 25

df <- tibble(x = rnorm(n,si),y=rnorm(n,si)+2,class = factor("A")) |> 
  bind_rows(tibble(x = rnorm(n,sd=2*si)+2,y=rnorm(n,sd=2*si)-1,class = factor("B"))) |> 
  bind_rows(tibble(x = rnorm(n,sd=1/2*si)+2,y=rnorm(n,sd=1/2*si)+1,class = factor("C")))

ggplot(data=df, aes(x, y, class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify(method = "lda")

ggplot(data=df, aes(x, y, class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify(method = "qda")


# Custom mesh -------------------------------------------------------------

rdata <- function(n, n_groups = 3, radius = 3) {
  list_of_dfs <- lapply(0:(n_groups-1), function(k) {
    mu <- c(cos(2*k*pi/n_groups), sin(2*k*pi/n_groups))
    m <- MASS::mvrnorm(n, radius*mu, diag(2))
    structure(data.frame(m, as.character(k)), names = c("x", "y", "c"))
  })
  do.call("rbind", list_of_dfs)
}

custom_mesh <- expand.grid(x=seq(-10,10,by=0.1),y=seq(-10,10,by=0.1))

rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_classify()+
  geom_point_fill(aes(fill=c))+
  coord_equal()+
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_classify(mesh = custom_mesh)+
  geom_point_fill(aes(fill=c))+
  coord_equal()+plot_layout(guides = "collect")


# More advanced tidymodel objects -----------------------------------------

bike <- ISLR2::Bikeshare

bike |> mutate(hr = as.double(hr)) |> filter(weathersit != "heavy rain/snow") |>
  droplevels.data.frame() |> 
  ggplot(aes(x=bikers,y=hr,fill=weathersit,class = weathersit))+
  geom_classify(method = "qda")+
  geom_point_fill(alpha = 0.5)
bike |> mutate(hr = as.double(hr)) |> filter(weathersit != "heavy rain/snow") |>
  droplevels.data.frame() |> 
  ggplot(aes(x=bikers,y=hr,col=weathersit,class = weathersit))+
  geom_classify(method = "randomForest")+
  geom_jitter(width = 0,height = 0.2,alpha = 0.5)
bike |> mutate(hr = as.double(hr),holiday = as.factor(holiday)) |>
  droplevels.data.frame() |> 
  ggplot(aes(x=bikers,y=registered,col=holiday,class = holiday))+
  geom_classify(method = "qda")+
  geom_jitter(alpha = 0.5)

qda_mod <- 
  discrim_regularized(frac_common_cov = 0, frac_identity = 0) %>% 
  set_engine("klaR")

imbal_rec <- 
  recipe(holiday ~ .,data=bike) %>%
  step_rose(holiday)

qda_rose_wflw <- workflow() |>  
  add_model(qda_mod) |>  
  add_recipe(imbal_rec)

bike |> mutate(hr = as.double(hr),holiday = as.factor(holiday)) |>
  droplevels.data.frame() |> 
  ggplot(aes(x=bikers,y=registered,col=holiday,class = holiday))+
  geom_classify(method = qda_mod)+
  geom_jitter(alpha = 0.5)+
bike |> mutate(hr = as.double(hr),holiday = as.factor(holiday)) |>
  droplevels.data.frame() |> 
  ggplot(aes(x=bikers,y=registered,col=holiday,class = holiday))+
  geom_classify(method = qda_rose_wflw)+
  geom_jitter(alpha = 0.5)
# Not working yet, need to test what a training split does to it


# Climbing Subsampling ----------------------------------------------------

members <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv")

members |> filter(peak_name == "Everest") |> select(year,age,died,sex) |> drop_na() |> 
  ggplot(aes(age,year,class = died))+
  geom_point_fill(aes(fill = died))+
  geom_classify(method = "qda")+
  facet_wrap(vars(sex))

# Too rare an event, testing method from tidytuesday https://juliasilge.com/blog/himalayan-climbing/

members_df <- members %>%
  filter(season != "Unknown", !is.na(sex), !is.na(citizenship)) %>%
  select(peak_id, year, season, sex, age, citizenship, hired, success, died) %>%
  mutate(died = case_when(
    died ~ "died",
    TRUE ~ "survived"
  )) %>%
  mutate_if(is.character, factor) %>%
  mutate_if(is.logical, as.integer)

members_prune <- members_df |> select(age,year,died) |> drop_na()
set.seed(123)
members_smote <- themis::smote(members_prune,"died",over_ratio = 0.2)


glm_spec <- logistic_reg() %>%
  set_engine("glm")

rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

members_smote |> ggplot(aes(age,year,class = died))+
  geom_classify(method = glm_spec)+
  geom_point_fill(aes(fill = died))+
members_smote |> ggplot(aes(age,year,class = died))+
  geom_classify(method = rf_spec)+
  geom_point_fill(aes(fill = died))
members_smote |> ggplot(aes(age,year,class = died))+
  geom_classify(method = "qda")+
  geom_point_fill(aes(fill = died),alpha = 0.2)

# I guess that worked kind of


# Custom Function ---------------------------------------------------------

custom_method <- function(data,mesh){
  mesh$fill <- factor(mesh$x^2+mesh$y^2 < 2)
  return(mesh)
}

ggplot(data=df, aes(x, y, class = class))+
  geom_classify(method = custom_method)+
  geom_point_fill(aes(fill = class))+
  coord_equal()


# New Patchwork Wrapper ---------------------------------------------------

simpleplot <- ggplot(df,aes(x,y,fill = class))+
  geom_point_fill()

classify_multi(simpleplot,methods = list("lda","qda"))
classify_multi(simpleplot,methods = list("lda","qda","knn"))
classify_multi(simpleplot,methods = list("lda","qda","knn","randomForest"),
               boundary = TRUE)

qda_mod <- discrim_quad() %>%
  set_mode("classification") %>%
  set_engine("MASS")

classify_multi(simpleplot,methods = list("lda",qda_mod,"knn","randomForest"))

# lr_spec <- logistic_reg() %>%
#   set_engine("glm") %>%
#   set_mode("classification")

mn_spec <- multinom_reg() |> 
  set_engine("nnet") |> 
  set_mode("classification")


classify_multi(simpleplot,methods = list("lda",qda_mod,"knn",mn_spec))+
  ggtitle(testvec[2])

#add titles

classify_multi(simpleplot,methods = list("lda",qda_mod,"knn",mn_spec),
               titles = c("lda","qda","knn","multinomial regression"))

nnet_spec <- 
  mlp(epochs = 1000, hidden_units = 10, penalty = 0.01, learn_rate = 0.1) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")

classify_multi(simpleplot,methods = list("lda",qda_mod,"knn",nnet_spec),
               titles = c("lda","qda","knn","neural network"))

classify_multi(simpleplot,methods = list("lda",qda_mod,"knn",nnet_spec,
                                         "randomForest"),
               titles = c("lda","qda","knn","neural network","Random Forest"))

# New base plot 

simpleplot2 <- ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(col = class,shape = class))

classify_multi(simpleplot2,methods = list("lda",qda_mod,"knn",nnet_spec,
                                         "randomForest"),
               titles = c("lda","qda","knn","neural network","Random Forest"))


# Testing multiple neural net configurations ------------------------------

nnet_spec1 <- 
  mlp(epochs = 100, hidden_units = 10, penalty = 0.01, learn_rate = 0.1) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")
nnet_spec2 <- 
  mlp(epochs = 100, hidden_units = 10, penalty = 0.01, learn_rate = 0.2) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")
nnet_spec3 <- 
  mlp(epochs = 100, hidden_units = 10, penalty = 0.01, learn_rate = 0.3) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")
nnet_spec4 <- 
  mlp(epochs = 100, hidden_units = 10, penalty = 0.01, learn_rate = 0.4) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")

classify_multi(simpleplot2,methods = list(nnet_spec1,nnet_spec2,nnet_spec3,nnet_spec4))

 rdata <- function(n, n_groups = 3, radius = 3) {
   list_of_dfs <- lapply(0:(n_groups-1), function(k) {
     mu <- c(cos(2*k*pi/n_groups), sin(2*k*pi/n_groups))
     m <- MASS::mvrnorm(n, radius*mu, diag(2))
     structure(data.frame(m, as.factor(k)), names = c("x", "y", "c"))
   })
   do.call("rbind", list_of_dfs)
 }
 
 # Plot with basic tidymodels methods
 
 simpleplot3a <- rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()
 simpleplot3b <- rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point(aes(color = c, shape = c))+
  coord_equal()

 m1 <- classify_multi(simpleplot3a,
                methods = list("lda",qda_mod,"knn","randomForest"),
                titles = c("lda","qda","knn","Random Forest"))
 m2 <- classify_multi(simpleplot3b,
                methods = list("lda",qda_mod,"knn","randomForest"),
                titles = c("lda","qda","knn","Random Forest"))   
#m1+m2+plot_layout(guides = "collect")


# Classify Boundary -------------------------------------------------------

 rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
   geom_point_fill(aes(fill = c))+
   coord_equal()+
   geom_classify_boundary(lty = 2)+
  rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
   geom_point_fill(aes(fill = c))+
   coord_equal()+
   geom_classify()
 
 (df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify_boundary(lty = 2)+
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify())/
 
 (df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify_boundary(lty = 2,method = "qda")+
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify(method = "qda"))
 
  
 (df1 |> ggplot(aes(x, y, class = class))+
     geom_point_fill(aes(fill = class))+
     coord_equal()+
     geom_classify_boundary(lty = 2,method = "randomForest")+
  df1 |> ggplot(aes(x, y, class = class))+
     geom_point_fill(aes(fill = class))+
     coord_equal()+
     geom_classify(method = "randomForest"))
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify(method = "randomForest")+
   geom_classify_boundary(method = "randomForest")
# add argument to smooth lines
 #Comparing qda to naive bayes
  
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify(method = "qda")+
   geom_classify_boundary(method = "lda")
 
 naive_mod <- naive_Bayes() %>% 
   set_mode("classification") %>% 
   set_engine("klaR") %>% 
   set_args(usekernel = FALSE)
 
 #Comparing qda to naive bayes
 
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify(method = "qda")+
   geom_classify_boundary(method = naive_mod)
 
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify(method = "qda")+
 df1 |> ggplot(aes(x, y, class = class))+
   geom_point_fill(aes(fill = class))+
   coord_equal()+
   geom_classify(method = naive_mod)
 
 simpleplot <- ggplot(df,aes(x,y,class = class))+
   geom_point_fill(aes(fill = class))
 
 classify_multi(simpleplot,methods = list("lda","qda"),boundary = TRUE)
 classify_multi(simpleplot,methods = list("lda","qda"),boundary = TRUE,res = 1001)
 classify_multi(simpleplot,methods = list("lda","qda","knn"),boundary = TRUE)
 classify_multi(simpleplot,methods = list("lda","qda","knn","randomForest"),
                boundary = TRUE)
 classify_multi(simpleplot,methods = list("lda","qda","knn","randomForest"))
 classify_multi(simpleplot,methods = list("lda","qda","knn","randomForest"),
                boundary = TRUE, res = 501, lty = 2, col = "red")

#Comparing several
 
ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify_boundary(method = "lda", res = 201, col = "darkblue")+
  geom_classify_boundary(method = "qda", res = 201, col = "hotpink")+
  geom_classify_boundary(method = naive_mod, res = 201, col = "forestgreen")
 

# TidyClust integration ---------------------------------------------------
 
kmeans_spec <- k_means(num_clusters = 3) %>%
  set_engine("stats") 

ggplot(df, aes(x,y))+
  geom_classify(method = kmeans_spec)+
  geom_point(aes(color = class))

ggplot(df, aes(x,y))+
  geom_classify(method = kmeans_spec)+
  gginnards::geom_debug()


# Test real world data for README -----------------------------------------

mpg |> ggplot(aes(hwy,displ,class = class))+
  geom_classify()+
  geom_point(aes(col = class))+
  facet_wrap(vars(year))

mpg |> ggplot(aes(x=hwy,y=displ,class = factor(class)))+
  geom_classify(method = "knn")+
  geom_point(aes(col = class))+
  facet_wrap(vars(year))

mpg |> ggplot(aes(x=hwy,y=displ,class = factor(class)))+
  geom_classify(method = "randomForest")+
  geom_point(aes(col = class))+
  facet_wrap(vars(year))

Orange |> ggplot(aes(age,circumference,class = factor(Tree)))+
  geom_point(aes(col = factor(Tree)))+
  geom_classify()

CO2 |> ggplot(aes(conc,uptake,class = factor(Type)))+
  geom_point(aes(col = factor(Type)))+
  geom_classify(method = "qda")+
  geom_classify_boundary(method = "lda")+
  facet_wrap(vars(Treatment))

beaver1 |> ggplot(aes(time,temp,class = factor(activ)))+
  geom_classify(method = "qda")+
  geom_point(aes(col = factor(activ)))

beaver2 |> ggplot(aes(time,temp,class = factor(activ)))+
  geom_classify(method = "qda")+
  geom_point(aes(col = factor(activ)))

data(Glass)

randomForest::randomForest(Type~.,data = Glass,importance = TRUE) |> 
  randomForest::varImpPlot()

Glass |> ggplot(aes(Mg, RI, class = factor(Type)))+
  geom_classify(method = "knn")+
  geom_point(aes(col = factor(Type)))

parabolic |> ggplot(aes(X1, X2, class = class))+
  geom_classify(method = "qda")+
  geom_point(aes(col = class))


# PCA test ----------------------------------------------------------------


pca(iris,"Species") |> ggplot(aes(pca1,pca2,class = Species))+
  geom_classify(method = "knn")+
  geom_point(aes(col = Species))

pca(iris,"Species",scale = TRUE) |> ggplot(aes(pca1,pca2,class = Species))+
  geom_classify(method = "knn")+
  geom_point(aes(col = Species))

pca(iris,"Species") |> ggplot(aes(pca1,pca2,class = Species))+
  geom_classify(method = "qda")+
  geom_point(aes(col = Species))


# pca2(housing,"Infl") |> ggplot(aes(pca1,pca2,class = Sat))+
#   geom_classify(method = "qda")+
#   geom_point(aes(col = Sat))

pca(housing,"Sat") |> ggplot(aes(pca1,pca2,class = Sat))+
  geom_classify(method = "lda")+
  geom_point(aes(col = Sat))+
  scale_fill_viridis_d()
#Not good

pca(txhousing |> drop_na(),"city") |> ggplot(aes(pca1,pca2,class = city))+
  geom_classify(method = "lda")+
  geom_point(aes(col = city))

pca(oils,"class") |> ggplot(aes(pca1,pca2,class = class))+
  geom_classify(method = "lda")+
  geom_point(aes(col = class))

pca(pd_speech,"gender") |> ggplot(aes(pca1,pca2,class = factor(gender)))+
  geom_classify(method = "qda")+
  geom_point(aes(col = factor(gender)))

pca(diamonds,"cut") |> ggplot(aes(pca1,pca2,class = cut))+
  geom_classify(method = "qda")+
  geom_point(aes(col = cut))

diamonds |> ggplot(aes(price,carat,class = cut))+
  geom_classify(method = "qda")+
  geom_point(aes(col = cut))+
  facet_wrap(vars(clarity))

pca(penguins |> drop_na(),"species") |> ggplot(aes(pca1,pca2,class = species))+
  geom_classify(method = "qda")+
  geom_point(aes(col = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(pca1,pca2,class = species))+
  geom_classify(method = "qda")+
  geom_point(aes(col = species))+
  facet_wrap(vars(sex))
  
# Can you classify in PCA and then return to original xy coords?

# Faceting with fixed values.



# Faceting with fixed values ----------------------------------------------
oils
class_grid(x = oils,rows = "linolenic",
           quantiles = c(0.2,0.5,0.8))
class_grid(x = oils,rows = "linolenic",cols = "stearic",
           quantiles = c(0.2,0.5,0.8))

oils |> ggplot(aes(x = palmitic, y = stearic, class = class))+
  geom_classify(grid = list(rows = "linolenic",
                            quantiles = c(0.1,0.5,0.9))) -> oilplot
oils |> ggplot(aes(x = palmitic, y = stearic, class = class))+
  geom_classify()

layer_data(oilplot)
layer_scales(oilplot)
  


  # ggplot(aes(palmitic,linoleic,class = class))+
  # geom_classify(method = "lda")+
  # geom_point(aes(col = class)) |> facet_grid(rows = rows,
  #                                            cols = cols)

# Smooth boundary ---------------------------------------------------------

rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify_boundary()
  
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify_boundary(smooth = TRUE)

simpleplot <- ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))

classify_multi(simpleplot,methods = list("lda","qda"),
               boundary = TRUE,smooth = TRUE)
classify_multi(simpleplot,methods = list("lda","qda","knn"),
               boundary = TRUE,smooth = TRUE)
classify_multi(simpleplot,
               methods = list("lda","qda","knn","randomForest"),
               boundary = TRUE,
               titles = c("lda","qda","knn","randomForest"))
classify_multi(simpleplot,
               methods = list("lda","qda","knn","randomForest"),
               boundary = TRUE,
               titles = c("lda","qda","knn","randomForest"),
               smooth = TRUE)

ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify_boundary(method = "qda")
ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify(method = "qda")+
  geom_classify_boundary(method = "qda",smooth = TRUE)
ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify(method = "lda")+
  geom_classify_boundary(method = "lda",smooth = TRUE)
ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify_boundary(method = "qda",smooth = TRUE)

ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify_boundary(method = "qda")
ggplot(df,aes(x,y,class = class))+
  geom_point_fill(aes(fill = class))+
  geom_classify_boundary(method = "qda",smooth = TRUE)


ggplot(df,aes(x,y,class = class))+
  #geom_point_fill(aes(fill = class))+
  #geom_classify(method = "lda")+
  geom_classify_boundary(method = "lda",smooth = TRUE,
                         aes(col = group))+
  gginnards::geom_debug()


# Split get_classify ------------------------------------------------------

rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify()
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify_boundary()
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify_boundary(smooth = TRUE)
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify_boundary(smooth = TRUE,bandwidth = 1/4)
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_point_fill(aes(fill = c))+
  coord_equal()+
  geom_classify_boundary(smooth = TRUE,bandwidth = 4)

rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  coord_equal()+
  geom_classify(fade = TRUE)+
  geom_point_fill(aes(fill = c))
rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  coord_equal()+
  geom_classify()+
  geom_point_fill(aes(fill = c))

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

df |> ggplot(aes(x,y,class = class))+
  geom_point(aes(col = class))

df |> ggplot(aes(x,y,class = class))+
  geom_classify_boundary()+
  stat_debug_panel() -> innard

xrange <- range(df$x)
yrange <- range(df$y)
mesh <- expand.grid(x=seq(xrange[1], xrange[2], length.out = 101),
                    y=seq(yrange[1], yrange[2], length.out = 101))
mesh <- do.call(get_classify,
                args = list(data=df, mesh=mesh, method="lda"))
res <- 101
ids <- expand.grid(xid = 1:res, yid = 1:res)
meshid <- cbind(ids,mesh)
mesh$alpha <- apply(meshid, MARGIN = 1, FUN = neighbors,
                    df = meshid, n = 5)

ggplot(df,aes(x,y,class = class))+
  geom_classify(method = "qda", fade = TRUE)+
  geom_classify_boundary(method = "qda", smooth = TRUE, bandwidth = 1/3)+
  geom_point_fill(aes(fill = class))+
  scale_fill_viridis_d()


# Fade boundary -----------------------------------------------------------

oils |> ggplot(aes(x = palmitic, y = stearic, class = class))+
  geom_classify(fade = TRUE)+
  geom_classify_boundary(smooth = TRUE)+
  geom_point_fill(aes(fill = class))

oils |> ggplot(aes(x = palmitic, y = stearic, class = class))+
  geom_classify(fade = TRUE)+
  geom_classify_boundary()+
  geom_point_fill(aes(fill = class))

oils |> ggplot(aes(x = palmitic, y = stearic, class = class))+
  geom_classify(fade = TRUE, method = "knn")+
  geom_classify_boundary(smooth = TRUE, method = "knn", bandwidth = 1/3)+
  geom_point_fill(aes(fill = class))

pca(oils, class = "class") |> ggplot(aes(x = pca1, y = pca2, class = class))+
  geom_classify(fade = TRUE)+
  geom_classify_boundary(smooth = TRUE)+
  geom_point_fill(aes(fill = class))

pca(pd_speech,"gender") |> ggplot(aes(pca1,pca2,class = factor(gender)))+
  geom_classify(method = "qda", fade = TRUE)+
  geom_point(aes(col = factor(gender)))+
  scale_fill_viridis_d()+
  scale_color_viridis_d()


# Manually formed mesh and data for testing -------------------------------

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

res <- 101
mesh <- expand.grid(x=seq(min(df$x),max(df$x), length.out = res),
                    y=seq(min(df$y),max(df$y), length.out = res))
names(df)[3] <- "fill"
mesh <- get_classify(df, mesh, method = "qda")
names(mesh)[3] <- "class"

ggplot(mesh, aes(x, y, col = class, fill = class))+
  geom_raster()

meshid <- cbind(expand.grid(xid = 1:res, yid = 1:res),mesh)
# mesh$alpha <- apply(meshid, MARGIN = 1, FUN = neighbors,
#                     df = meshid, n = 10)

x <- 51
y <- 51
n <- 10

#minimesh <- expand.grid(x = seq(x-n,x+n), y = seq(y-n,y+n))

meshid$nei <- 0

meshid[seq2(x,y, n, 101),6] <- 1
meshid[(y-1)*101+x,6] <- 2

ggplot(meshid, aes(x, y, fill = fill))+
  geom_raster(aes(fill = factor(nei)))+
  coord_equal()

library(tictoc)

tic()
oils |> ggplot(aes(x = palmitic, y = stearic, class = class))+
  geom_classify(fade = TRUE)+
  geom_point_fill(aes(fill = class))
toc()

pca(oils, class = "class") |> ggplot(aes(x = pca1, y = pca2, class = class))+
  geom_classify(fade = TRUE)+
  geom_classify_boundary(smooth = TRUE)+
  geom_point_fill(aes(fill = class))


# Overlaying boundaries of same classifier --------------------------------

rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_classify(fade = TRUE, res = 101)+
  #geom_classify_boundary(smooth = TRUE)+
  geom_point_fill(aes(fill = c))

rdata(100, 5, 3) |> ggplot(aes(x, y, class = c))+
  geom_classify_boundary(method = "lda", col = "blue")+
  geom_classify_boundary(method = "qda", col = "red")+
  geom_point_fill(aes(fill = c))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(pca1,pca2,class = species, lty = sex))+
  geom_classify_boundary(method = "qda")+
  geom_point(aes(col = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(pca1,pca2,class = species))+
  geom_classify_boundary(method = "qda")+
  geom_point(aes(col = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_depth_mm,flipper_length_mm,class = species))+
  geom_classify_boundary(method = "qda")+
  geom_point(aes(col = species))


pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_length_mm,flipper_length_mm,class = species))+
  geom_classify_boundary(data = penguins |> drop_na() |> filter(sex == "male"),
                         aes(col = "male"),col = "plum1")+
  geom_classify_boundary(data = penguins |> drop_na() |> filter(sex == "female"),
                         aes(col = "female"),col = "firebrick4")+
  geom_point_fill(aes(fill = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_length_mm,flipper_length_mm,class = species))+
  geom_classify_boundary()+
  geom_point_fill(aes(fill = species))+
  facet_wrap(vars(sex))


# Test why not grouping by color ------------------------------------------

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_length_mm,flipper_length_mm,class = species))+
  geom_classify_boundary(smooth)+
  geom_point_fill(aes(fill = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_length_mm,flipper_length_mm,class = species))+
  geom_classify_boundary(aes(col = sex))+
  geom_point_fill(aes(fill = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_length_mm,flipper_length_mm,class = species))+
  geom_classify_boundary(aes(lty = sex))+
  geom_point(aes(col = species))

pca(penguins |> drop_na(),"species") |> 
  ggplot(aes(bill_length_mm,flipper_length_mm,class = species))+
  geom_classify_boundary(aes(lty = sex), smooth = TRUE, bandwidth = 3)+
  geom_point(aes(col = species))

df |> ggplot(aes(x,y,class = class))+
  geom_classify_boundary(aes(col = x>0), method = "lda", smooth = TRUE)+
  geom_point_fill(aes(fill = class))+
  geom_vline(xintercept = 0)

# diamonds |> slice_sample(n = 1000) |> ggplot(aes(carat, price))+
#   geom_point_fill(aes(fill = cut))+
#   geom_classify_boundary(aes(class = cut, lty = color, color = clarity))

# sports |> filter(sports == "Football") |> ggplot(aes(exp_men,rev_men))+
#   geom_point_fill()

titanic |> mutate(Survived = factor(Survived), Pclass = factor(Pclass)) |>
  drop_na() |>
  ggplot(aes(Age, Fare, class = Survived))+
  geom_point_fill(aes(fill = Survived))+
  geom_classify_boundary()

# titanic |> mutate(Survived = factor(Survived), Pclass = factor(Pclass)) |>
#   drop_na() |>
#   ggplot(aes(Age, Fare, class = Survived))+
#   geom_point_fill(aes(fill = Survived))+
#   geom_classify_boundary(aes(col = Sex))

diamonds |> mutate(cd = color) |> 
  # filter(color == "D" | color == "J",
  #                  clarity == "SI1" | clarity == "VVS1",
  #                  cut == "Good" | cut == "Ideal") |> 
  ggplot(aes(carat, price, class = cut))+
  geom_classify_boundary(aes(lty = color, col = clarity))+
  geom_point_fill(aes(fill = cut))

math |> ggplot(aes(absences,g3))+
  geom_point()

 
math |> ggplot(aes(absences,g1,class = fail3))+
  geom_classify_boundary(aes(lty = sex, col = romantic),
                         smooth = TRUE, method = "knn")+
  geom_point_fill(aes(fill = fail3))+
  scale_colour_viridis_d()

por |> ggplot(aes(absences,g1,class = fail3))+
  geom_classify_boundary(aes(lty = sex, col = age > 17),
                         smooth = TRUE)+
  geom_point_fill(aes(fill = fail3))+
  scale_colour_viridis_d()


# Testing out more examples -----------------------------------------------

baseball <- mlb_pbp(game_pk = c(632970))

baseball |> select(hitData.launchAngle, hitData.launchSpeed,
                   result.eventType) |> drop_na() |> 
  ggplot(aes(x = hitData.launchAngle, y = hitData.launchSpeed,
             class = result.eventType))+
  geom_classify(method = "lda")+
  geom_point_fill(aes(fill = result.eventType))

hits <- statcast_search(start_date = "2016-04-06", 
                        end_date = "2016-06-06", 
                        player_type = 'batter') |> 
  filter(events %in% c("single", "double", "triple", "home_run","field_out"))

hits |> select(launch_angle, launch_speed, events, home_team) |> drop_na() |> 
  filter(events %in% c("single", "double", "triple", "home_run",
                     "field_out")) |> mutate(events = factor(events)) |> 
ggplot(aes(launch_angle, launch_speed, class = events))+ 
  geom_classify(method = "knn")+
  geom_point_fill(aes(fill = events), alpha = 0.1)+
  facet_wrap(vars(home_team))

hits |> mutate(homerun = factor(case_when(events == "home_run" ~ 1,
                                   .default = 0))) |>
  filter(home_team %in% c("BAL","CLE","MIN","PHI","SD","COL"),
         launch_angle > 0) |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  geom_classify(method = "knn")+
  geom_point_fill(aes(fill = homerun), alpha = 0.1)+
  facet_wrap(vars(home_team))

hits |> mutate(homerun = factor(case_when(events == "home_run" ~ 1,
                                   .default = 0))) |>
  filter(home_team %in% c("BAL","CLE","MIN","PHI","SD","COL"),
         launch_angle > 0) |> mutate(home_team = factor(home_team)) |> 
  select(launch_angle, launch_speed, homerun, home_team) |> drop_na() |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  #geom_classify( method = "knn")+
  geom_classify_boundary(aes(col = home_team), method = "knn")+
  geom_point_fill(aes(fill = homerun), alpha = 0.2)

hits |> mutate(homerun = factor(case_when(events == "home_run" ~ 1,
                                          .default = 0))) |> 
  select(homerun, home_team) |> filter(homerun == 1) |> count(home_team) |> 
  print(n = 26)

hits |> mutate(homerun = factor(case_when(events == "home_run" ~ 1,
                                          .default = 0))) |>
  filter(home_team %in% c("BAL","CLE","MIN","PHI","SD","COL"),
         launch_angle > 0) |> mutate(home_team = factor(home_team)) |> 
  select(launch_angle, launch_speed, homerun, home_team, launch_speed_angle) |>
  drop_na() |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  #geom_classify( method = "knn")+
  #geom_classify_boundary(aes(col = home_team), method = "knn")+
  geom_point_fill(aes(fill = homerun), alpha = 0.2)+
  facet_wrap(vars(launch_speed_angle))

bball <- bind_rows(
  statcast_search(start_date = "2015-05-01", 
                  end_date = "2015-06-01", 
                  player_type = 'batter'),
  statcast_search(start_date = "2018-05-01", 
                  end_date = "2018-06-01", 
                  player_type = 'batter'),
  statcast_search(start_date = "2022-05-01", 
                  end_date = "2022-06-01", 
                  player_type = 'batter')) |> 
  filter(events %in% c("single", "double", "triple", "home_run","field_out")) |> 
  mutate(homerun = factor(case_when(events == "home_run" ~ 1,.default = 0)))

# bball <- bball |> mutate(homerun = factor(case_when(events == "home_run" ~ 1,
#                                                     .default = 0))) 

bball |> mutate(homerun = factor(case_when(events == "home_run" ~ 1,
                                          .default = 0))) |>
  filter(home_team %in% c("BAL","CLE","MIN","PHI","SD","COL"),
         launch_angle > 0) |> mutate(home_team = factor(home_team)) |> 
  select(launch_angle, launch_speed, homerun, home_team, game_year) |>
  drop_na() |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  geom_classify( method = "knn")+
  geom_classify_boundary(aes(col = home_team), method = "knn")+
  geom_point_fill(aes(fill = homerun), alpha = 0.2)+
  facet_wrap(vars(game_year))

bball |> 
  select(launch_angle, launch_speed, homerun, home_team, game_year) |>
  drop_na() |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  geom_classify( method = "knn",fade = TRUE)+
  geom_classify_boundary( method = "knn")+
  geom_point_fill(aes(fill = homerun), alpha = 0.2)+
  facet_wrap(vars(game_year))

bball |> select(launch_angle, launch_speed, events, home_team, game_year) |> 
  drop_na() |> filter(events %in% c("single","double")) |> 
  mutate(events = factor(events)) |> 
  ggplot(aes(launch_angle, launch_speed, class = events))+ 
  geom_classify( method = "qda", fade = TRUE)+
  geom_classify_boundary( method = "qda")+
  geom_point_fill(aes(fill = events), alpha = 0.2)+
  facet_wrap(vars(game_year))

bball |> select(launch_angle, launch_speed, homerun, home_team, game_year) |> 
  drop_na() |> filter(home_team %in% c("BOS","SEA")) |> 
  mutate(home_team = factor(home_team)) |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  geom_classify( method = "knn", fade = TRUE)+
  geom_classify_boundary( method = "knn")+
  geom_point_fill(aes(fill = homerun), alpha = 0.2)+
  facet_wrap(vars(home_team))

bball |> select(launch_angle, launch_speed, homerun, home_team, game_year) |> 
  drop_na() |> filter(home_team %in% c("BOS","SEA"), launch_angle > 0) |> 
  mutate(home_team = factor(home_team)) |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  #geom_classify( method = "knn", fade = TRUE)+
  geom_classify_boundary(method = "knn", aes(col = home_team))+
  geom_point_fill(aes(fill = homerun), alpha = 0.2)


# bostongames <- statcast_search(start_date = "2015-05-01", 
#                 end_date = "2015-05-03", 
#                 player_type = 'batter',
#                 home_team = "BOS")
# bostongames <- statcast_search(start_date = "2015-05-01", 
#                 end_date = "2015-05-03", 
#                 player_type = 'batter')

bball |> select(launch_angle, launch_speed, homerun, pitch_type, game_year) |> 
  drop_na() |> filter(pitch_type %in% c("CH","FF","SI")) |> 
  mutate(pitch_type = factor(pitch_type)) |> 
  ggplot(aes(launch_angle, launch_speed, class = pitch_type))+ 
  geom_classify( method = "qda", fade = TRUE)+
  #geom_classify_boundary(method = "knn")+
  geom_point(aes(col = pitch_type), alpha = 0.2, size = 0.5)

colpit <- read_csv("/Users/nathaniel_morgan1/Library/CloudStorage/Box-Box/Kahle Research/col-pit.csv") |> 
  mutate(homerun = factor(case_when(events == "home_run" ~ 1,.default = 0)),
         hit = factor(case_when(events != "field_out" ~ 1,.default = 0))) |> 
  filter(home_team %in% c("COL","PIT"))

colpit |> select(launch_angle, launch_speed, homerun, home_team, game_year) |> 
  drop_na() |> filter(launch_angle > 0, launch_angle < 75, launch_speed > 50) |> 
  mutate(home_team = factor(home_team)) |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  #geom_classify( method = "knn", fade = TRUE)+
  geom_classify_boundary(method = "knn", aes(lty = home_team))+
  geom_jitter(aes(col = homerun), alpha = 0.2, size = 0.7,height = 0,width = 0.25)

colpit |> select(launch_angle, launch_speed, homerun, home_team, game_year) |> 
  drop_na() |> filter(launch_angle > 0, launch_angle < 75, launch_speed > 50) |> 
  mutate(home_team = factor(home_team)) |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  #geom_classify( method = "knn", fade = TRUE)+
  geom_classify_boundary(method = "qda", aes(lty = home_team))+
  geom_jitter(aes(col = homerun), alpha = 0.2, size = 0.7,height = 0,width = 0.25)


colpit |> select(launch_angle, launch_speed, homerun, home_team, game_year) |> 
  drop_na() |> filter(launch_angle > 0, launch_angle < 75, launch_speed > 50) |> 
  mutate(home_team = factor(home_team)) |> 
  ggplot(aes(launch_angle, launch_speed, class = homerun))+ 
  geom_classify( method = "qda")+
  geom_classify_boundary(method = "qda")+
  geom_jitter(aes(col = homerun), alpha = 0.2, size = 0.7,height = 0,width = 0.25)

#Meh
colpit |> select(pitch_number, release_spin_rate, events) |> drop_na() |> 
  ggplot(aes(pitch_number, release_spin_rate, class = events, fill = events))+
  geom_classify()+
  geom_jitter(aes(col = events), height = 0, width = 0.3, alpha = 0.2)

colpit |> select(pitch_number, release_spin_rate, hit, launch_angle) |>
  drop_na() |> filter(pitch_number < 10) |> 
  mutate(pitch_number = factor(pitch_number)) |> 
  ggplot(aes(launch_angle, release_spin_rate, class = hit, fill = hit))+
  geom_classify(method = "qda")+
  geom_point_fill(alpha = 0.2)+
  facet_wrap(vars(pitch_number))

# Why no legend? ----------------------------------------------------------

df |> ggplot()+
  geom_classify(aes(x,y,class = class))

df |> ggplot(aes(x,y,class = class))+
  geom_classify()+
  geom_classify_boundary(smooth = TRUE)

#This errors
df |> ggplot()+
  geom_classify(aes(x,y,fill = class), show.legend = TRUE)
  
df |> ggplot(aes(x,y,fill = class))+
  geom_classify()+
  theme(legend.position = "top")+
  scale_fill_viridis_d(option = "A")

df |> ggplot(aes(x,y,fill = class))+
  geom_classify()+
  scale_fill_viridis_d(option = "A")+
  geom_point_fill()

df |> ggplot(aes(x,y,fill = class))+
  geom_classify()+
  geom_debug_npc()+
  geom_null(fill = "black")


df |> ggplot(aes(x,y,fill = class))+
  geom_classify()+
  geom_point_fill()+
  geom_debug()



# Detecting similar sequences ---------------------------------------------

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

df |> ggplot(aes(x,y))+
  geom_classify(aes(fill = class))+
  geom_classify_boundary(aes(group = class))+
  geom_point(aes(col = class))

xrange <- range(df$x)
yrange <- range(df$y)
mesh <- expand.grid(x=seq(xrange[1], xrange[2], length.out = 101),
                    y=seq(yrange[1], yrange[2], length.out = 101))
mesh <- do.call(get_classify,
                args = list(data=df, mesh=mesh, method="lda"))
breaks <- tail(sort(unique(as.double(mesh$fill))), -1) - 1/2
names(mesh)[3] <- "z"
iso_df <- ggplot2:::xyz_to_isolines(mesh, breaks = breaks)

iso_df <- iso_to_path(iso_df)

iso_df |> group_by(group) |> mutate(id = 1:n()) |> ungroup() |> 
  ggplot(aes(x,y,group = group))+
  geom_path(aes(col = id))

splitter <- function(a,n){
  emptylist <- list()
  for (i in 0 : (nrow(a) %/% n)) {
    emptylist[[i+1]] <- apply(iso_df[(1+i*n):min((n+i*n),nrow(a)),c("x","y")],
                              2, mean)
  }
  data.frame(Reduce(rbind,emptylist), row.names = NULL)
}

#z <- unsplit(lapply(split(x, g), scale), g)

z <- unsplit(lapply(split(iso_df, piece), splitter), group)


# Testing implementation of CLI  -----------------------------------------

# Warn that knn needs factor
rdata(100, 5, 3) |> ggplot(aes(x, y, fill = c))+
  geom_classify(method = "knn")+
  geom_point_fill(aes(fill=c))+
  coord_equal()

# inform what strings are recognized. 
rdata(100, 5, 3) |> ggplot(aes(x, y, fill = c))+
  geom_classify(method = "KNN")+
  geom_point_fill()+
  coord_equal()


# Why still no legend -----------------------------------------------------

df |> ggplot(aes(x,y))+
  geom_classify(aes(fill = class))

df |> ggplot(aes(x,y))+
  geom_classify_boundary(aes(group = class))

df |> ggplot(aes(x,y))+
  geom_classify(aes(fill = class))+
  scale_fill_manual(values = c("gray","white","black"),
                    name = "legend")


# Using fill only instead of group for boundary ---------------------------

# Dont have to use group
df |> ggplot(aes(x, y, fill = class))+
  geom_classify() -> p
  #geom_point_fill(aes(alpha = abs(x/6))) -> p
  #geom_classify_boundary()+
  #geom_debug()

#But you can if you want
df |> ggplot(aes(x,y,fill = class))+
  geom_classify()+
  geom_classify_boundary(aes(group = class))


# Implementing C code for fade --------------------------------------------

x <- 51
y <- 51
bench::mark(
  seq2R(x,y, 10, 101),
  seq2(x,y, 10, 101)
)


bench::mark(
  seq2R(x,y, 25, 201),
  seq2(x,y, 25, 201)
)

tic()
oils |> ggplot(aes(x = palmitic, y = stearic, fill = class))+
  geom_classify(fade = TRUE)+
  geom_point_fill()
toc()

tic()
oils |> ggplot(aes(x = palmitic, y = stearic, fill = class))+
  geom_classify(fade = TRUE, res = 201)+
  geom_point_fill()
toc()


# different geom testing --------------------------------------------------

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

df |> ggplot()+
  stat_classify(aes(x, y, fill = class), 
                geom = GeomPointFill, res = 101)

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    shape = after_stat(class)), 
                geom = GeomPoint, res = 101)

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    size = after_stat(class)), 
                geom = GeomPoint, res = 101)

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    col = after_stat(class),
                    shape = after_stat(class)), 
                geom = GeomPoint, res = 101)

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    alpha = after_stat(class)), 
                geom = GeomPoint, res = 101)

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    group = after_stat(class)), 
                geom = GeomPath, res = 101)

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    group = after_stat(class),
                    col = after_stat(class)), 
                geom = GeomPoint, res = 51, method = "qda")
  #geom_point(aes(x, y, col = class))

df |> ggplot()+
  stat_classify(aes(x, y, fill = class,
                    group = after_stat(class),
                    col = after_stat(class)), 
                geom = GeomLine, res = 101)


# Inverse Fade ------------------------------------------------------------

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(fade = TRUE)

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(fade = TRUE, invert = TRUE)

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(fade = TRUE, invert = TRUE,
                method = "qda")

simpleplot <- ggplot(df,aes(x,y,fill = class))+
  geom_point(aes(col = class))

classify_multi(simpleplot,
               methods = list("lda","qda","knn","randomForest"),
               titles = c("lda","qda","knn","randomForest"),
               fade = TRUE, invert = TRUE)


# Uncertainty afterstat ---------------------------------------------------

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

res <- 101
mesh <- expand.grid(x=seq(min(df$x),max(df$x), length.out = res),
                    y=seq(min(df$y),max(df$y), length.out = res))
names(df)[3] <- "fill"
#mesh <- get_classify(df, mesh, method = "qda")
disc <- do.call(qda, args = list(fill~., data=df))
mesh$fill = predict(disc, mesh)$class
mesh$probability = apply(predict(disc, mesh)$posterior, MARGIN = 1, max)
names(mesh)[3] <- "class"

ggplot(mesh, aes(x, y, col = class, fill = class))+
  geom_raster(aes(alpha = probability))

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

df |> ggplot(aes(x, y, fill = class, alpha = after_stat(probability)))+
  geom_classify()+
  geom_point_fill(aes(x, y, fill = class),inherit.aes = FALSE)+
df |> ggplot(aes(x, y, fill = class, alpha = after_stat(probability)))+
  geom_classify(method = "qda")+
  geom_point_fill(aes(x, y, fill = class),inherit.aes = FALSE)+
  plot_layout(guide = "collect")
  
penguins |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(aes(alpha = after_stat(probability)))+
  geom_point_fill()

penguins |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(aes(alpha = after_stat(probability)), method = "qda")+
  geom_point_fill()

penguins |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(aes(alpha = after_stat(probability)), method = "qda")+
  geom_point(aes(col = species))+
  scale_color_manual(values = c("firebrick4","forestgreen","darkblue"))+
  ggtitle("Probability")+
penguins |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(method = "qda",fade = TRUE, invert = TRUE)+
  geom_point(aes(col = species))+
  scale_color_manual(values = c("firebrick4","forestgreen","darkblue"))+
  ggtitle("Inverted Fade")+
  plot_layout(guide = "collect")

penguins |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(method = "qda",fade = TRUE)+
  geom_point(aes(col = species))+
  scale_color_manual(values = c("firebrick4","forestgreen","darkblue"))


df |> ggplot(aes(x, y, fill = class, alpha = after_stat(probability)^2))+
  geom_classify()+
  geom_point_fill(aes(x, y, fill = class),inherit.aes = FALSE)+
  scale_alpha(range = c(0,1))+
df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE)+
  geom_point_fill(aes(x, y, fill = class),inherit.aes = FALSE)+
  scale_alpha(range = c(0,1))+
  plot_layout(guide = "collect")


# Purity and Neighborhood size --------------------------------------------

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE,
                aes(alpha = after_stat(purity)))+
  geom_point_fill(aes(x, y, fill = class))

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE,
                aes(alpha = after_stat(purity)^2))+
  geom_point_fill(aes(x, y, fill = class))

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE,
                aes(alpha = 1-after_stat(purity)))+
  geom_point_fill(aes(x, y, fill = class))

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE,
                aes(alpha = after_stat(purity)), neighborhood = 25)+
  geom_point_fill(aes(x, y, fill = class))

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE,
                aes(alpha = 1-after_stat(purity)), neighborhood = 25)+
  geom_point_fill(aes(x, y, fill = class))+
df |> ggplot(aes(x, y, fill = class))+
  geom_classify(method = "lda", fade = TRUE, invert = TRUE,
                aes(alpha = 1-after_stat(purity)))+
  geom_point_fill(aes(x, y, fill = class))+
  plot_layout(guide = "collect")


# Density afterstat -------------------------------------------------------

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

res <- 101
mesh <- expand.grid(x=seq(min(df$x),max(df$x), length.out = res),
                    y=seq(min(df$y),max(df$y), length.out = res))
names(df)[3] <- "fill"
#mesh <- get_classify(df, mesh, method = "qda")
disc <- do.call(qda, args = list(fill~., data=df))
mesh$fill = predict(disc, mesh)$class
mesh$probability = apply(predict(disc, mesh)$posterior, MARGIN = 1, max)
names(mesh)[3] <- "class"
names(df)[3] <- "class"

mesh$density <- lapply(split(df,df$class),
                       function(x) c(MASS::kde2d(x$x,x$y,n=101,
                                                   lims = c(min(df$x),max(df$x),
                                                           min(df$y),max(df$y)))[["z"]])) |> 
  as.data.frame() |> apply(MARGIN = 1,max)

tibble(density = mesh$density) |> ggplot(aes(density))+
  geom_histogram(bins = 100)

ggplot(mesh, aes(x, y, col = class, fill = class))+
  geom_raster(aes(alpha = density))+
  geom_point(data = df, aes(col = class))

#density(df[,c(1,2)])
MASS::kde2d(df$x,df$y,n=101)

apply(data.frame(x=c(1,2,3),y=c(3,2,1)),MARGIN = 1,max)

lapply(split(df,df$class),function(x) c(MASS::kde2d(x$x,x$y,n=101)[["z"]])) |> 
  as.data.frame() |> apply(MARGIN = 1,max)

df |> ggplot(aes(x, y, fill = class))+
  geom_classify(aes(alpha = after_stat(density)))+
  geom_point(aes(col = class), alpha = 1/3)+
  scale_color_manual(values = c("firebrick4","forestgreen","darkblue"))

penguins |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(aes(alpha = after_stat(density)))+
  geom_point(aes(col = species))+
  scale_color_manual(values = c("firebrick4","forestgreen","darkblue"))

penguins |> drop_na() |> ggplot(aes(bill_length_mm, body_mass_g, fill = species))+
  geom_classify(aes(alpha = after_stat(purity^2)), fade = TRUE)+
  geom_point(aes(col = species))+
  scale_color_manual(values = c("firebrick4","forestgreen","darkblue"))


# Convex Hull -------------------------------------------------------------

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

res <- 101
mesh <- expand.grid(x=seq(min(df$x),max(df$x), length.out = res),
                    y=seq(min(df$y),max(df$y), length.out = res))
names(df)[3] <- "fill"
#mesh <- get_classify(df, mesh, method = "qda")
disc <- do.call(lda, args = list(fill~., data=df))
mesh$fill = predict(disc, mesh)$class
mesh$probability = apply(predict(disc, mesh)$posterior, MARGIN = 1, max)
names(mesh)[3] <- "class"
names(df)[3] <- "class"

splitmesh <- split(mesh,mesh$class)

hulls <- lapply(splitmesh,function(x) x[chull(x),])

for (i in seq_along(hulls)) {
  hulls[[i]]$hull <- names(hulls[i])
}

Reduce(rbind,hulls) |> ggplot(aes(x,y, fill = hull))+
  geom_polygon(alpha = 1/3)

Reduce(rbind,hulls) |> ggplot(aes(x,y, col = hull))+
  geom_polygon(alpha = 0)+
  geom_point(data = df, aes(x,y, col = class), inherit.aes = FALSE)


Reduce(rbind,hulls) |> ggplot(aes(x,y, fill = hull))+
  geom_path(aes(col = hull))
Reduce(rbind,hulls) |> ggplot(aes(x,y, fill = hull))+
  geom_point(aes(col = hull))

Reduce(rbind,hulls) |> ggplot(aes(x,y, col = hull))+
  geom_raster(data = mesh, aes(x,y, fill = class), inherit.aes = FALSE, alpha = 1/3)+
  geom_polygon(alpha = 0)+
  geom_point(data = df, aes(x,y, col = class), inherit.aes = FALSE)

mesh |> ggplot(aes(x,y, fill = class))+
  geom_raster()

# Online example for chull

BH = data.frame(x = rnorm(2000), y= rnorm(2000))  # create a dummy example data.frame
BHL = chull(BH[, c("x", "y")]) # calculate convex hull for x and y columns
#BHL = c(BHL, BHL[1]) # add the first point again at the end, so the path loops back to the beginning

#now plot using geom_path 
ggplot(BH, aes(x,y)) +
  geom_point() +
  geom_path(data=BH[BHL, ])


# Adaptive Gridding tests -------------------------------------------------

# Bisection

sfunc <- function(x) ifelse(x > 3.7, "b", "a")

bisec1 <- function(f, mesh){
  g1 <- f(mesh)
  change <- rle(g1)$lengths[1]
  list(coord = c(mesh[1:change],(mesh[change]+mesh[change+1])/2,mesh[(change+1):length(g1)]),
    class = c(g1[0:change],f((mesh[change]+mesh[change+1])/2),g1[(change+1):length(g1)]))
}

sfunc(0:10) |> rle() |> glimpse()

bisec1(sfunc, 0:10)

bisecn <- function(f, mesh, n){
  g1 <- f(mesh)
  for (i in 1:n) {
    change <- rle(g1)$lengths[1]
    g1 <- c(g1[0:change],f((mesh[change]+mesh[change+1])/2),g1[(change+1):length(g1)])
    mesh <- c(mesh[1:change],(mesh[change]+mesh[change+1])/2,mesh[(change+1):(length(g1)-1)])
  }
  return(list(class = g1,coords = mesh))
}

bisecn(sfunc, 0:10, 10)

bisecn(sfunc, 0:10, 10) |> as.data.frame() |> as_tibble() |> 
  ggplot(aes(x = coords, y = 0, color = class))+
  geom_point()

# More classes

sfunc2 <- function(x) case_when(x < 3.7 ~ "a",
                                x > 3.7 & x < 7.2 ~ "b",
                                x > 7.2 ~ "c")
sfunc2(0:10)

bisecn <- function(f, mesh, n){
  g1 <- f(mesh)
  for (i in 1:n) {
    change <- rle(g1)$lengths[1]
    g1 <- c(g1[0:change],f((mesh[change]+mesh[change+1])/2),g1[(change+1):length(g1)])
    mesh <- c(mesh[1:change],(mesh[change]+mesh[change+1])/2,mesh[(change+1):(length(g1)-1)])
  }
  return(list(class = g1,coords = mesh))
}

sfunc3 <- function(x) case_when(x < 3.7 ~ 1,
                                x > 3.7 & x < 7.2 ~ 2,
                                x > 7.2 ~ 3)

library(interp)

test <- tri.mesh(df$x,df$y)

plot(test)
# Yah I'm not really sure what this does

# Cjat gpst example

library(deldir)

# Generate random data for demonstration
set.seed(42)
points <- matrix(runif(60), ncol = 2)
labels <- sample(0:1, 30, replace = TRUE)

# Perform Delaunay triangulation
triangulation <- deldir(points[, 1], points[, 2])

# Plot the Delaunay triangulation with color-coded classes
plot(triangulation, wlines = "triang", display = "none")
points(points[, 1], points[, 2], col = labels + 1, pch = 16)
# Yah i dont get htis either

# Another one

# Perform Delaunay triangulation
triangulation <- tri.mesh(x = df$x, y = df$y)

# Now triangle_classes contains the classes of points in each triangle
plot(triangulation)
points(df$x,df$y,col = df$fill)

#detect which arcs have a class change
df[triangulation$arcs[1,],"fill"] |> as.integer() |> all()

#make new point in center of arc
cent <- df[triangulation$arcs[1,],c(1,2)] |> apply(FUN = mean, MARGIN = 2, simplify = FALSE)
predict(disc,cent)$class

#Repeat

#Functions to do so

center_arc <- function(x,tri) {
  a <- x[1,c(1,2)]
  for (i in 1:nrow(x)) {
    if (length(unique(x[tri$arcs[i,],"fill"])) == 2){
      a <- rbind(a,x[tri$arcs[i,],c(1,2)] |> apply(FUN = mean, MARGIN = 2, simplify = FALSE))
      a
    }
  }
  a[-1,]
}

centers <- center_arc(df,tri = triangulation)

df2 <- rbind(df,cbind(centers,fill = predict(disc,centers)$class))

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
df2 |> ggplot(aes(x,y, col = fill))+
  geom_point()

enhance <- function(df,n){
  for (i in 1:n) {
    triangulation <- tri.mesh(x = df$x, y = df$y, duplicate = "remove")
    centers <- center_arc(df,tri = triangulation)
    df <- rbind(df,cbind(centers,fill = predict(disc,centers)$class)) 
  }
  df
}

df3 <- enhance(df, 5)

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
df3 |> ggplot(aes(x,y, col = fill))+
  geom_point()

df4 <- enhance(df, 10)

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
df4 |> ggplot(aes(x,y, col = fill))+
  geom_point()


# try with triangle -------------------------------------------------------

df$fill <- df$class
df <- df[,c(1,2,4)]

# Perform Delaunay triangulation
triangulation <- tri.mesh(x = df$x, y = df$y)

# Now triangle_classes contains the classes of points in each triangle
plot(triangulation)
points(df$x,df$y,col = df$fill)

#detect which arcs have a class change
df[triangulation$trlist[1,1:3],"fill"] |> as.integer() |> all()

#make new point in center of arc
cent <- df[triangulation$trlist[1,1:3],c(1,2)] |> apply(FUN = mean, MARGIN = 2, simplify = FALSE)
predict(disc,cent)$class

#Functions to do so

center_arc <- function(x,tri) {
  a <- x[1,c(1,2)]
  for (i in 1:nrow(x)) {
    if (length(unique(x[tri$trlist[i,1:3],"fill"])) != 1){
      a <- rbind(a,x[tri$trlist[i,1:3],c(1,2)] |> apply(FUN = mean, MARGIN = 2, simplify = FALSE))
      a
    }
  }
  a[-1,]
}

centers <- center_arc(df,tri = triangulation)

df2 <- rbind(cbind(df,gen = 1),cbind(centers,fill = predict(disc,centers)$class, gen = 2))

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
df2 |> ggplot(aes(x,y, col = fill))+
  geom_point()

df2 |> ggplot(aes(x,y, col = fill, shape = factor(gen)))+
  geom_point()

centers2 <- center_arc(df2,tri = tri.mesh(x = df2$x, y = df2$y, duplicate = "remove"))

df3 <- rbind(df2,cbind(centers2,fill = predict(disc,centers2)$class, gen = 3))

df3 |> ggplot(aes(x,y, col = fill, shape = factor(gen)))+
  geom_point()

plot(tri.mesh(x = df2$x, y = df2$y, duplicate = "remove"))
points(df2$x,df2$y,col = df2$fill)
points(centers2, col = "blue")

plot(tri.mesh(x = df$x, y = df$y, duplicate = "remove"))
points(df$x,df$y,col = df$fill)
points(centers, col = "blue")

enhance <- function(df,n){
  for (i in 1:n) {
    triangulation <- tri.mesh(x = df$x, y = df$y, duplicate = "remove")
    centers <- center_arc(df,tri = triangulation)
    df <- rbind(df,cbind(centers,fill = predict(disc,centers)$class)) 
  }
  df
}

df3 <- enhance(df, 2)

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
  df3 |> ggplot(aes(x,y, col = fill))+
  geom_point()

df4 <- enhance(df, 5)

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
  df4 |> ggplot(aes(x,y, col = fill))+
  geom_point()

df5 <- enhance(df, 10)

df |> ggplot(aes(x,y, col = fill))+
  geom_point()+
df5 |> ggplot(aes(x,y, col = fill))+
  geom_point()

# Failed attempt at using kernel density smoother on center of triangularizations

kerntest <- kde2d(centers$x,centers$y)
kde2d(centers$x,centers$y) |> glimpse()
maxes <- kde2d(centers$x,centers$y)$z |> apply(FUN = which.max, MARGIN = 2)

plot(kerntest$x[maxes],kerntest$y[1:25], type = "l")

tibble(x = rep(kerntest$x,25),y = rep(kerntest$y,each = 25), z = c(kerntest$z)) |> 
  ggplot(aes(x,y, fill = z))+
  geom_raster()+
  geom_point(data = tibble(x = centers$x, y = centers$y), aes(x, y), inherit.aes = FALSE)+
  geom_path(data = tibble(x = kerntest$x[maxes],y = kerntest$y[1:25]),
            aes(x, y), inherit.aes = FALSE)+
  geom_point(data = df, aes(x,y,col = fill), inherit.aes = FALSE)


# Use isolines on centered and predicted data -----------------------------

all_centers <- rbind(centers,centers2)

all_centers$fill <- predict(disc,all_centers)$class

rbind(all_centers,df) |> ggplot(aes(x,y,col = fill))+
  geom_point()

#form_isolines(rbind(all_centers,df),method = "lda",mesh = rbind(all_centers[,1:2],df[,1:2]),args = NA)

xyz_centers <- rbind(all_centers,df)[,1:2]
xyz_centers$z <- rbind(all_centers,df)$fill |> as.factor() |> as.double()

xyz_to_isolines(data = xyz_centers,breaks = c(1.5,2.5)) |> iso_to_path()

function(data) {
  x_pos <- as.integer(factor(data$x, levels = sort(unique0(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique0(data$y))))
  nrow <- max(y_pos)
  ncol <- max(x_pos)
  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z
  raster
}

x_pos <- as.integer(factor(xyz_centers$x, levels = sort(unique0(xyz_centers$x))))
y_pos <- as.integer(factor(xyz_centers$y, levels = sort(unique0(xyz_centers$y))))
nrow <- max(y_pos)
ncol <- max(x_pos)
raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
raster[cbind(y_pos, x_pos)] <- xyz_centers$z
raster

isoband::isolines(x = sort(unique0(xyz_centers$x)),
         y = sort(unique0(xyz_centers$y)),
         z = raster, levels = c(1.5,2.5))

isoband::isobands(x = sort(unique0(xyz_centers$x)),
         y = sort(unique0(xyz_centers$y)),
         z = raster, levels_low = 1.5,levels_high = 2.5)

plot_iso(raster,1.5,2.5)



# Akima Package -----------------------------------------------------------

library(akima)

data(akima)

akima.si <- interp(akima$x, akima$y, akima$z,
                   xo=seq(min(akima$x), max(akima$x), length = 100),
                   yo=seq(min(akima$y), max(akima$y), length = 100),
                   linear = FALSE, extrap = TRUE)

df.si <- interp(df$x, df$y, df$fill |> as.numeric(),
                   xo=seq(min(akima$x), max(akima$x), length = 100),
                   yo=seq(min(akima$y), max(akima$y), length = 100),
                   linear = FALSE, extrap = TRUE)

#Yah that doesn't really help at all


# Quadtree -----------------------------------------

library(quadtree)
habitat <- terra::rast(system.file("extdata", "habitat.tif", package="quadtree"))

mesh_num <- mesh[,-4]
mesh_num$class <- mesh$class |> as.numeric()

terra::rast(mesh, type = "xyz") |> plot()
class_mesh <- terra::rast(mesh_num, type = "xyz") |> plot()

quadtree(class_mesh, .1)

#Yah that didn't work or help at all


# Writing my own Reverse Quadtree -----------------------------------------

spl_bnd <- function(x) ifelse(x[1]+x[2] > 1, TRUE, FALSE)
spl_bnd2 <- function(x) ifelse(x[1]+x[2] > 1, "a", "b")

spl_grd <- expand.grid(x = seq(0,1,length.out = 11),y = seq(0,1,length.out = 11))

spl_meth <- function(data,mesh){
  mesh$fill <- apply(mesh,FUN = spl_bnd,1)
  return(mesh)
}

tibble(x = runif(20), y = runif(20)) |> ggplot(aes(x,y))+
  geom_classify(aes(fill = "a"), method = spl_meth, mesh = spl_grd)

# custom_method <- function(data,mesh){
#   mesh$fill <- factor(mesh$x^2+mesh$y^2 < 4)
#   return(mesh)
# }
# 
# df |> ggplot(aes(x, y, fill = class))+
#   geom_classify(method = custom_method)+
#   geom_point()+
#   coord_equal()

tibble(spl_grd,class = apply(spl_grd,FUN = spl_bnd2,1) |> factor()) |> 
  ggplot(aes(x,y,col = class))+
  geom_point()

#Honestly not sure i could back out the final product into a usable raster


# Creating own Boundary estimator -----------------------------------------

# Doesn't work because isolines requires an evenly spaced grid to perform marching squares

# LDA only solution maybe -------------------------------------------------


# Flip quadtree 2 ---------------------------------------------------------

spl_bnd2 <- function(x) ifelse(x[1]+x[2] > 1, "a", "b")

split_sq <- function(sq, f){
  cls <- apply(sq,MARGIN = 1,FUN = f)
  if( all(cls == cls[1]) ){
    #sq$class <- cls
    list(sq)
  }
  else center_points(sq)
}

center_points <- function(df){
  list(data.frame(x=df$x+df$x[1],
                  y=df$y+df$y[1])/2,
       data.frame(x=df$x+df$x[2],
                  y=df$y+df$y[2])/2,
       data.frame(x=df$x+df$x[3],
                  y=df$y+df$y[3])/2,
       data.frame(x=df$x+df$x[4],
                  y=df$y+df$y[4])/2)
}

Reduce(rbind,center_points(df)) |> ggplot(aes(x,y))+
  geom_point()

df <- expand.grid(x = 0:1, y = 0:1)

split_sq(df, spl_bnd2) |> 
  lapply(split_sq, f = spl_bnd2) |> 
  purrr::flatten() |> 
  Reduce(rbind,x = _) |> ggplot(aes(x,y))+
  geom_point()

split_sq(df, spl_bnd2) |> 
  lapply(split_sq, f = spl_bnd2) |> 
  lapply(split_sq, f = spl_bnd2) |> 
  lapply(split_sq, f = spl_bnd2) %>%
  Reduce(rbind,.)

recursive_split <- function(sq, f, depth = 0, max_depth = 5, ...) {
  if (depth > max_depth) {  # Prevent infinite recursion
    return(sq)
  }
  
  cls <- apply(sq, MARGIN = 1, FUN = f, ...)
  if (all(cls == cls[1])) {
    return(list(sq))  # No further split needed
  } else {
    # Calculate new squares and recursively split them if necessary
    new_squares <- center_points(sq)  # This needs to be correctly implemented
    return(lapply(new_squares, recursive_split, f, depth + 1))
  }
}

# You'd call it like this:
result <- recursive_split(df, spl_bnd2)

collect_dfs <- function(x, result = list()) {
  if (is.data.frame(x)) {
    result[[length(result) + 1]] <- x
  } else if (is.list(x)) {
    for (item in x) {
      result <- collect_dfs(item, result)
    }
  }
  return(result)
}

final_df <- collect_dfs(result) |> Reduce(rbind,x = _)
final_df$class <- apply(final_df,MARGIN = 1,FUN = spl_bnd2)

final_df |> ggplot(aes(x,y,col = class))+
  geom_point()

# Implementing with LDA 

set.seed(17)

df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
  rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
  rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))

names(df)[3] <- "fill"
#mesh <- get_classify(df, mesh, method = "qda")
disc <- do.call(lda, args = list(fill~., data=df))

lda_square <- expand.grid(x=c(min(df$x),max(df$x)),
                          y=c(min(df$y),max(df$y)))

lda_pred <- function(x, mod){
  as.character(predict(mod,x)$class)
}

recursive_split_df <- function(sq, f, depth = 0, max_depth = 5, ...) {
  if (depth > max_depth) {  # Prevent infinite recursion
    return(sq)
  }
  
  cls <- f(sq,...)
  if (all(cls == cls[1])) {
    return(list(sq))  # No further split needed
  } else {
    # Calculate new squares and recursively split them if necessary
    new_squares <- center_points(sq)  # This needs to be correctly implemented
    return(lapply(new_squares, recursive_split_df, f, depth + 1, ...))
  }
}

lda_result <- recursive_split_df(sq = lda_square, f = lda_pred, mod = disc)

lda_dfs <- collect_dfs(lda_result) |> Reduce(rbind,x = _)
lda_dfs$class <- lda_pred(lda_dfs,disc)

lda_dfs |> ggplot(aes(x,y, col = class))+
  geom_point()

#Now QDA

disc <- do.call(qda, args = list(fill~., data=df))

qda_square <- expand.grid(x=c(min(df$x),max(df$x)),
                          y=c(min(df$y),max(df$y)))

qda_pred <- function(x, mod){
  as.character(predict(mod,x)$class)
}

recursive_split_df2 <- function(sq, f, depth = 0, max_depth = 5, ...) {
  if (depth > max_depth) {  # Prevent infinite recursion
    return(sq)
  }
  
  # Split without checking the criterion if depth is less than 2
  if (depth < 2) {
    new_squares <- center_points(sq)
    return(lapply(new_squares, recursive_split_df2, f,
                  depth + 1, max_depth = max_depth, ...))
  } else {
    cls <- f(sq, ...) 
    if (all(cls == cls[1])) {
      return(list(sq))  # No further split needed
    } else {
      new_squares <- center_points(sq)
      return(lapply(new_squares, recursive_split_df2, f,
                    depth + 1, max_depth = max_depth, ...))
    }
  }
}


qda_result <- recursive_split_df2(sq = qda_square, f = qda_pred, 
                                  mod = disc, max_depth = 10)

qda_dfs <- collect_dfs(qda_result) |> Reduce(rbind,x = _)
qda_dfs$class <- qda_pred(qda_dfs,disc)

qda_dfs |> ggplot(aes(x,y, col = class))+
  geom_point(size = 1/2)

# df |> ggplot(aes(x,y,fill = fill))+
#   geom_classify(method = "qda")


# investigate quadtree structure ------------------------------------------

as_data_frame <- quadtree::as_data_frame
habitat <- terra::rast(system.file("extdata", "habitat.tif", package="quadtree")) # load sample data
qt <- quadtree(habitat, .03, "sd") # create a quadtree

glimpse(qt)
methods(class = "Quadtree")

qt@ptr

quadtree::as_data_frame(qt)

plot(qt)

mat <- rbind(c(1, 1, 0, 1),
             c(1, 1, 1, 0),
             c(1, 0, 1, 1),
             c(0, 1, 1, 1))
qtmat <- quadtree(mat, .1)
plot(qtmat)
quadtree::as_data_frame(qtmat)

mat2 <- rbinom(64,1,p=0.3)

coords <- expand.grid(x = 1:8, y = 1:8)

quadtree::set_values(qtmat,coords,mat2)

plot(qtmat)

qtmat2 <- quadtree(matrix(mat2,nrow = 8),.1)

plot(qtmat2)

habitat_roads <- terra::rast(system.file("extdata", "habitat_roads.tif", package="quadtree"))
template <- habitat_roads

# use a custom function so that a quadrant is split if it contains any 1's
split_if_one <- function(vals, args) {
  if(any(vals == 1, na.rm = TRUE)) return(TRUE)
  return(FALSE)
}
qt_template <- quadtree(template, split_method = "custom",
                        split_fun = split_if_one)

# now use the template to create a quadtree from 'rast'
qt <- quadtree(rast, template_quadtree = qt_template)

par(mfrow = c(1, 3), mar = c(0,0,3,0))
plot(template, axes = FALSE, box = FALSE, legend = FALSE,
     main = "template raster")
plot(qt_template, crop = TRUE, na_col = NULL, border_lwd = .3 ,axes = FALSE,
     legend = FALSE, main = "template quadtree")
plot(qt, crop = TRUE, na_col = NULL, border_lwd = .3, axes = FALSE,
     legend = FALSE, main = "quadtree created using template")

as_data_frame(qtmat2)


# write a method for quadtree ---------------------------------------------

#assuming depth down to 4

bmat <- matrix(0, nrow = 16, ncol = 16)

bmat[1:(16/2),1:(16/2)] <- 1

bmat[1:(16/2),(16/2)+1:(16/2)] <- 2

bmat[(16/2)+1:(16/2),1:(16/2)] <- 3

bmat[(16/2)+1:(16/2),(16/2)+1:(16/2)] <- 4

bmat


# hull instead after quadtree ---------------------------------------------

qda_dfs |> nrow()

qda_dfs |> mutate(id = rep(1:14995,each=4)) |> 
  ggplot(aes(x = x, y = y, fill = class, group = id))+
  geom_polygon()

splitdf <- split(qda_dfs,qda_dfs$class)

hulls <- lapply(splitdf,function(x) x[chull(x),])

for (i in seq_along(hulls)) {
  hulls[[i]]$hull <- names(hulls[i])
}

Reduce(rbind,hulls) |> ggplot(aes(x,y, fill = hull))+
  geom_polygon(alpha = 1/3)

# LDA
tic()

disc_lda <- do.call(lda, args = list(fill~., data=df))

lda_result <- recursive_split_df2(sq = lda_square, f = lda_pred, 
                                  mod = disc_lda, max_depth = 8)

lda_dfs <- collect_dfs(lda_result) |> Reduce(rbind,x = _)
lda_dfs$class <- lda_pred(lda_dfs,disc_lda)

lda_dfs |> ggplot(aes(x,y, col = class))+
  geom_point(size = 1/2)

splitdf_lda <- split(lda_dfs,lda_dfs$class)

hulls_lda <- lapply(splitdf_lda,function(x) x[chull(x),])

for (i in seq_along(hulls_lda)) {
  hulls_lda[[i]]$hull <- names(hulls_lda)[i]
}

Reduce(rbind,hulls_lda) |> ggplot(aes(x,y, fill = hull))+
  geom_polygon(alpha = 1/3)
toc()

# quad_points <- function(df){
#   id = paste0(runif(1))
#   list(data.frame(x=(df$x+df$x[1])/2,
#                   y=(df$y+df$y[1])/2,
#                   id = id),
#        data.frame(x=(df$x+df$x[2])/2,
#                   y=(df$y+df$y[2])/2,
#                   id = id),
#        data.frame(x=(df$x+df$x[3])/2,
#                   y=(df$y+df$y[3])/2,
#                   id = id),
#        data.frame(x=(df$x+df$x[4])/2,
#                   y=(df$y+df$y[4])/2,
#                   id = id))
# }

quad_points <- function(df){
  list(data.frame(x=df$x+df$x[1],
                  y=df$y+df$y[1])/2,
       data.frame(x=df$x+df$x[3],
                  y=df$y+df$y[3])/2,
       data.frame(x=df$x+df$x[4],
                  y=df$y+df$y[4])/2,
       data.frame(x=df$x+df$x[2],
                  y=df$y+df$y[2])/2)
}

recursive_split_df3 <- function(sq, f, depth = 0, max_depth = 5, ...) {
  if (depth > max_depth) {  # Prevent infinite recursion
    return(sq)
  }
  
  # Split without checking the criterion if depth is less than 2
  if (depth < 2) {
    new_squares <- quad_points(sq) |> 
      lapply(function(x) cbind(x,id = paste0(depth,"-",runif(1)),depth = depth))
    return(lapply(new_squares, recursive_split_df3, f,
                  depth + 1, max_depth = max_depth, ...))
  } else {
    cls <- f(sq, ...) 
    if (all(cls == cls[1])) {
      return(list(sq))  # No further split needed
    } else {
      new_squares <- quad_points(sq) |> 
        lapply(function(x) cbind(x,id = paste0(depth,"-",runif(1)),depth = depth))
      return(lapply(new_squares, recursive_split_df3, f,
                    depth + 1, max_depth = max_depth, ...))
    }
  }
}

qda_square <- expand.grid(x=c(min(df$x),max(df$x)),
                          y=c(min(df$y),max(df$y)))[c(1,3,4,2),]
qda_square$id <- "0"
qda_square$depth <- "0"
tic()
disc_qda <- do.call(qda, args = list(fill~., data=df))

qda_result <- recursive_split_df3(sq = qda_square, f = qda_pred, 
                                  mod = disc_qda, max_depth = 9)
toc()
tic()
qda_dfs <- collect_dfs(qda_result) |> Reduce(rbind,x = _)
qda_dfs$class <- qda_pred(qda_dfs,disc_qda)
toc()

# qda_dfs |> ggplot(aes(x,y, col = class))+
#   geom_point(size = 1/2)
qda_dfs |> ggplot(aes(x,y, fill = class , group = id, col = class))+
  geom_polygon()

qda_dfs[1:4,] |> ggplot(aes(x,y, fill = class , group = id))+
  geom_polygon(size = 1/2)

qda_dfs |> group_by(id) |> mutate(xmin = min(x),xmax = max(x),
                                  ymin = min(y),ymax = max(y)) |> ungroup() |> 
  ggplot(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill = class , group = id))+
  geom_rect(alpha = 1/3)+
  geom_point_fill(data = df, aes(x = x, y = y, fill = fill),inherit.aes = FALSE)

qda_dfs |> group_by(id) |> mutate(xmin = min(x),xmax = max(x),
                                  ymin = min(y),ymax = max(y),
                                  depth = factor(depth)) |> ungroup() |> 
  ggplot(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill = depth , group = id))+
  geom_rect(alpha = 1/3,col = "black")+
  coord_equal()

qda_dfs |> ggplot(aes(x,y, col = class))+
  geom_point(size = 1/2)

qda_dfs |> group_by(depth) |> summarise(count = n())


# Now Random Forest -------------------------------------------------------

disc_rf <- do.call(randomForest, args = list(fill~., data=df))

rf_square <- expand.grid(x=c(min(df$x),max(df$x)),
                          y=c(min(df$y),max(df$y)))

rf_mod <- rand_forest(mode="classification") |> 
  set_engine("ranger",importance = "impurity")

rf_pred <- function(x, mod){
  as.character(predict(mod,x))
}

rf_pred_one <- function(x, mod){
  as.character(predict(mod,x))
}

rf_pred_one_mem <- memoise::memoise(rf_pred_one)

rf_pred_tomem <- function(x, mod){
  as.character(lapply(x,rf_pred_one_mem,mod = mod))
}

rf_pred_mem = memoise::memoise(rf_pred_tomem)

rf_square$id <- "0"
rf_square$depth <- "0"

disc_rf <- do.call(randomForest, args = list(fill~., data=df))
tic()
rf_result <- recursive_split_df3(sq = rf_square, f = rf_pred, 
                                  mod = disc_rf, max_depth = 8)

rf_dfs <- collect_dfs(rf_result) |> Reduce(rbind,x = _)
rf_dfs$class <- rf_pred(rf_dfs,disc_rf)

rf_dfs |> group_by(id) |> mutate(xmin = min(x),xmax = max(x),
                                  ymin = min(y),ymax = max(y)) |> ungroup() |> 
  ggplot(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill = class , group = id))+
  geom_rect(alpha = 1/3)+
  geom_point_fill(data = df, aes(x = x, y = y, fill = fill),inherit.aes = FALSE)
toc()
tic()
rf_result <- recursive_split_df3(sq = rf_square, f = rf_pred_mem, 
                                  mod = disc_rf, max_depth = 8)

rf_dfs <- collect_dfs(rf_result) |> Reduce(rbind,x = _)
rf_dfs$class <- rf_pred_mem(rf_dfs,disc_rf)

rf_dfs |> group_by(id) |> mutate(xmin = min(x),xmax = max(x),
                                  ymin = min(y),ymax = max(y)) |> ungroup() |> 
  ggplot(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill = class , group = id))+
  geom_rect(alpha = 1/3)+
  geom_point_fill(data = df, aes(x = x, y = y, fill = fill),inherit.aes = FALSE)
toc()

tic()
df |> ggplot(aes(x,y,fill = fill))+
  geom_classify(method = "randomForest")+
  geom_point_fill()
toc()

# Memoising does not seem worth it


# Non Convex Hull ---------------------------------------------------------

# Example
n <- 200
theta<-runif(n,0,2*pi)
r<-sqrt(runif(n,0.25^2,0.5^2))
x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
alpha <- 0.15
alphahull <- ahull(x, alpha = alpha)

qdabhull <- qda_dfs |> filter(class == "B") |> 
  select(x,y) |> unique() |> ahull(alpha = 0.4)

df |> ggplot(aes(x,y,fill=fill))+
  geom_classify(method = "qda")+
  geom_point(data = qdabhull$arcs |> as_tibble(),aes(c1,c2),inherit.aes = FALSE)+
  ggtitle("alpha = 0.4")

# qdabhull$arcs |> as_tibble() |> arrange(r) |> ggplot(aes(c1,c2))+
#   geom_point()+
#   ggtitle("alpha = 1.5")

# warcs<- which(qdabhull$arcs[,3]>0)
# for (i in warcs) {
#    arc(qdabhull$arcs[i, 1:2], qdabhull$arcs[i,3], c(0,1), pi, col = "gray", lty = 2)
#   }


# C++ implementation ------------------------------------------------------

quad_points <- function(df){
  list(data.frame(x=df$x+df$x[1],
                  y=df$y+df$y[1])/2,
       data.frame(x=df$x+df$x[3],
                  y=df$y+df$y[3])/2,
       data.frame(x=df$x+df$x[4],
                  y=df$y+df$y[4])/2,
       data.frame(x=df$x+df$x[2],
                  y=df$y+df$y[2])/2)
}

recursive_split_df3 <- function(sq, f, depth = 0, max_depth = 5, ...) {
  if (depth > max_depth) {  # Prevent infinite recursion
    return(sq)
  }
  
  # Split without checking the criterion if depth is less than 2
  if (depth < 2) {
    new_squares <- quad_points(sq) |> 
      lapply(function(x) cbind(x,id = paste0(depth,"-",runif(1)),depth = depth))
    return(lapply(new_squares, recursive_split_df3, f,
                  depth + 1, max_depth = max_depth, ...))
  } else {
    cls <- f(sq, ...) 
    if (all(cls == cls[1])) {
      return(list(sq))  # No further split needed
    } else {
      new_squares <- quad_points(sq) |> 
        lapply(function(x) cbind(x,id = paste0(depth,"-",runif(1)),depth = depth))
      return(lapply(new_squares, recursive_split_df3, f,
                    depth + 1, max_depth = max_depth, ...))
    }
  }
}

recursive_split_df4 <- function(sq, f, depth = 0, max_depth = 5, ...) {
  if (depth > max_depth) {  # Prevent infinite recursion
    return(sq)
  }
  
  # Split without checking the criterion if depth is less than 2
  if (depth < 2) {
    new_squares <- quadpointsc(sq) |> 
      lapply(function(x) cbind(x,id = paste0(depth,"-",runif(1)),depth = depth))
    return(lapply(new_squares, recursive_split_df4, f,
                  depth + 1, max_depth = max_depth, ...))
  } else {
    cls <- f(sq, ...) 
    if (all(cls == cls[1])) {
      return(list(sq))  # No further split needed
    } else {
      new_squares <- quadpointsc(sq) |> 
        lapply(function(x) cbind(x,id = paste0(depth,"-",runif(1)),depth = depth))
      return(lapply(new_squares, recursive_split_df4, f,
                    depth + 1, max_depth = max_depth, ...))
    }
  }
}
tic()
qda_result <- recursive_split_df3(sq = qda_square, f = qda_pred, 
                                  mod = disc_qda, max_depth = 7)
toc()
tic()
qda_result <- recursive_split_df4(sq = qda_square, f = qda_pred, 
                                  mod = disc_qda, max_depth = 7)
toc()

tic()
disc_qda <- do.call(qda, args = list(fill~., data=df))

qda_result <- recursive_split_df4(sq = qda_square, f = qda_pred, 
                                  mod = disc_qda, max_depth = 8)
qda_dfs <- collect_dfs(qda_result) |> Reduce(rbind,x = _)
qda_dfs$class <- qda_pred(qda_dfs,disc_qda)
toc()

qda_dfs |> ggplot(aes(x,y, fill = class , group = id, col = class))+
  geom_polygon()

collect_dfs <- function(x, result = list()) {
  if (is.data.frame(x)) {
    result[[length(result) + 1]] <- x
  } else if (is.list(x)) {
    for (item in x) {
      result <- collect_dfs(item, result)
    }
  }
  return(result)
}

tic()
disc_qda <- do.call(qda, args = list(fill~., data=df))

qda_result <- recursive_split_df4(sq = qda_square, f = qda_pred, 
                                  mod = disc_qda, max_depth = 8)
qda_dfs <- collectdfc(qda_result) |> Reduce(rbind,x = _)
qda_dfs$class <- qda_pred(qda_dfs,disc_qda)
toc()