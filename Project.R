library(tidyverse)
library(corrplot)

df <- read.csv('df_Clean.csv')

df_train <- read.csv('claims.csv')
df_test <- read.csv('test_1.csv')

str(df_train)

df_num_only <- df_train %>% select(AC_1001_Issue:Product_Age,Call_details,Fraud)

df_num_only

corelation <- cor(df_num_only)

corelation[is.na(corelation)] <- 0

corelation

corrplot(corelation,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

a<- as.data.frame(table(df_train$Region, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$State, df_train$Fraud)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq ) %>% 
    mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Area, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$City, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Consumer_profile, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Product_category, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Product_type, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$AC_1001_Issue, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$AC_1002_Issue, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$AC_1003_Issue, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$TV_2001_Issue, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$TV_2002_Issue, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$TV_2003_Issue, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Claim_Value, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Service_Centre, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Product_Age, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Purchased_from, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Call_details, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Purpose, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

a<- as.data.frame(table(df_train$Fraud, df_train$Fraud)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq ) %>% 
  mutate(percent_fraud = (`1`/(`0`+`1`)) * 100)

df <- df_train %>% pivot_longer(AC_1001_Issue:TV_2003_Issue, names_to = "Issue",values_to = "Issue_type",values_transform = as.character())

df

df %>% select(Issue, Issue_type, Fraud) %>% 
  filter(Issue_type != 0, Fraud == 1 ) %>% 
  group_by(Issue, Issue_type) %>%
  ggplot(aes(x = Fraud)) + geom_bar(aes(fill = factor(Issue_type)), position =  position_stack()) + facet_wrap(~ Issue)
         