# Set libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(fpp2)
library(GGally)
library(naniar)
library(ggthemes)
library(cluster)
library(Rtsne)
library(xgboost)
library(clustMixType)



#import data, clean names and parse dates, add weekday
df <- read_csv('data/data.csv') %>% 
  mutate(date = ymd(date),
         weekday = wday(date,label = T,abbr = F,week_start = 1)) %>% 
  clean_names()

# EDA

#glimpse
glimpse(df)
summary(df)

#missingness
miss_var_summary(df) # There's no transaction revenue information. There are 1.3% convertions

#Plot 1. Missingness intersections
plot_list <- list()
text_list <- list()

(plot_list[['missingness']] <- gg_miss_upset(df)) # there are 3 important interaction groups between missing variables.
                  # 1) People that hadn`t spent time on site and had no transactions
                  # 2) People that spent time on site and had no transactions
                  # 3) people that spent time on site and had transactions
text_list[[1]] <- 'There are 3 important interaction groups between missing variables. \n 1) People that had not spent time on site and had no transactions \n 2) People that spent time on site and had no transactions \n 3) people that spent time on site and had transactions'

#plot 2. Transactions over time
transactions_df <- df %>% 
  group_by(date, weekday) %>% 
  summarise(transactions = sum(transactions, na.rm = T)) 

(plot_list[['tot']] <-ggplot(transactions_df,aes(x = date, y = transactions))+
  geom_line()+
  geom_smooth(se = F)+ # Transaction behaviour seems affected by seasonality
  theme_bw()+
  xlab('Time')+
  ggtitle("Transactions over time"))

text_list[[2]] <- 'Transaction Behaviour seems affected by seasonality.'

#plot 3. Transactions by day of the week
daily_transactions_summ <- transactions_df %>% 
  group_by(weekday) %>% 
  summarise(mean = mean(transactions),
            min_int = t.test(transactions,conf.level = .95)$conf.int[1],
            max_int = t.test(transactions,conf.level = .95)$conf.int[2])


(plot_list[['daily_trans']] <-ggplot(daily_transactions_summ, aes(x = fct_rev(weekday))) +
  geom_segment( aes(xend=weekday, y=min_int, yend=max_int), color="grey") +
  geom_point( aes(y=min_int), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  geom_point( aes( y=max_int), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point(aes(y= mean), size = 2, shape = 1 )+
  coord_flip()+
  theme_tufte() +
  theme(
    legend.position = "none",
  ) +
  xlab("Day of the Week") +
  ylab("Transactions")+
  ggtitle("Transactions confidence intervals (95%) by day of the week"))

text_list[[3]] <- 'Weekends have fewer transactions than weekdays. Additionally with a confidence level of 95%, there`s no evidence which suggests
that there are differences between weekdays or between weekdays transactions.'


#Plot 4. Daily transaction rate by device category
transactions_cats_df <- df %>% 
  group_by(date, weekday, device_category) %>% 
  summarise(transaction_rate = (sum(transactions, na.rm = T)/ n())) 

(plot_list[['dev_trans']] <-ggplot(transactions_cats_df, aes(x = transaction_rate))+
  geom_histogram(aes(y =..density..), col = "white", bins = 40)+
  geom_density(col = "steelblue", size = 1, fill = "blue", alpha = .2)+
  theme_bw()+
  ggtitle("Daily transaction rate by device")+
  xlab("Transaction rate")+
  ylab("density")+
  facet_wrap(~device_category
             # , scales = 'free_x'
             )+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)))
  
text_list[[4]] <- 'Desktop users have higher conversion rates than mobile and tablet users. And there are many days in which tablet users
 have no conversions because they are scarce.'

#Plot 5. Daily transaction rate by browsers
browsers <- df %>% 
  group_by(browser) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  dplyr::slice(1:4) %>% 
  pull(browser)


transactions_browsers_df <-  df %>% 
  filter(browser %in% browsers) %>% 
  mutate(browser = factor(browser, levels = browsers)) %>% 
  group_by(date, browser) %>% 
  summarise(transaction_rate = (sum(transactions, na.rm = T)/ n())) 


(plot_list[['browser_trans']] <-ggplot(transactions_browsers_df, aes(x = browser, y =transaction_rate))+
  geom_boxplot()+
  geom_jitter(col= "steelblue", alpha = .4)+
  # coord_cartesian(ylim=c(0,75))+
  theme_bw()+
  ggtitle("Daily transaction rate by browser")+
  xlab("browser")+
  ylab("transaction rate"))

text_list[[5]] <- 'Chrome users tend to have higher conversion rates than Safari users. Firefox and IE have a median of 0 because they 
exhibit fewer transactions.'

# model
nas_to_0 <- function(x){
  y = as.numeric(x)
  case_when(is.na(y)~0,
            T~y)
}


df_model <- df %>% 
  mutate(month = month(date,label = T)) %>% 
  select(-transaction_revenue, -date) %>%
  mutate_at(vars(bounces,transactions, time_on_site), nas_to_0) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(!is.na(pageviews)) %>% 
  mutate_at(vars(pageviews, visit_number, hits, time_on_site),~ scale(.))
         

models_list <- list()
withinss <- c()

for (i in 1:10){
  kpres <- kproto(df_model %>% select(-full_visitor_id), i)
  models_list[i] <- kpres
  withinss[i] <- kpres$tot.withinss
}


scree_df <- data.frame(
  clusters = 1:10,
  objfunc = withinss
)

(plot_list[['screeplot']] <-ggplot(scree_df, aes(x = clusters, y = objfunc))+
  geom_line(color = 'steelblue', lwd = 1.5)+
  geom_point(color = 'steelblue', size = 5)+
  geom_point(x = 5, y = scree_df$objfunc[5], color = 'red', size = 6)+
  theme_bw()+
  xlab('# of Clusters')+
  ylab('Objective Function')+
  ggtitle('Scree Plot'))


text_list[[6]] <- 'Since the dataset had numeric and categorical values, kmeans had no use and an implementation of kmeans \n and with Gower distance it was computationally too expensive. 
\n Consequently, the chosen model was K-Prototypes which works really well with mixed data. \n
The model was tested with different number of clusters, evaluating the associated total within-cluster sum of squares and then 5 clusters where chosen'


df_model$cluster <- models_list[[5]]

plot_sample <- sample_n(df_model %>% select(-full_visitor_id), 5e3) %>% unique()
tsne_out <- Rtsne(plot_sample)
tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = plot_sample$cluster)

(plot_list[['tsne']] <-ggplot(tsne_plot) + 
  geom_point(aes(x=x, y=y, color= as.factor(col)),alpha=.8)+
  theme_bw()+
  labs(color = 'Cluster') +
  ggtitle('Clustered observations with t-SNE'))

text_list[[7]] <- 'Using t-SNE in order to visualize the highly dimensional data into a low dimensional embedding, \n we see that the clusters are well differentiated'
  
save(plot_list, text_list, file = 'mhtest/plots/plot_list.RData')
