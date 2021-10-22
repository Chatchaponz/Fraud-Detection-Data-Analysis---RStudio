library(tidyverse)
library(caret)
library(e1071)
# Region - Customer region details
# state - Current location of customer
# Area - Area_Urban/rural
# City- Customers current located city
# Consumer_profile- Customer's work profile
# Product_category- Product category
# Product_type- Type of the product_Tv/Ac
# AC_1001_Issue- 1001 is failure of Compressor in AC
# AC_1002_Issue- 1002 is failure of Condenser Coil in AC
# AC_1003_Issue- 1003 is failure of Evaporator Coil in AC
# TV_2001_Issue- 2001 is failure of power supply in Tv
# TV_2002_Issue- 2002 is failure of Inverter in Tv
# TV_2003_Issue- 2003 is failure of Motherboard in Tv
# claim_value- Customer's claim amount in Rs
# Service_Centre- 7 Different service centers
# Product_Age- Duration of the product purchased by customer
# Purchased_from- From where product is purchased
# Call_details- call duration in mins
# Purpose- Purpose_compliant-Compliant raised by customer claim- claimed for the product Other- Other categories out of this
# Fraud- '1'- fradulent claim, '0' Genuin claim
# NOTE: '0' means to replace the component, '1' means partial damage of the component and with servicing component good work and '2' no issue with the component. This is for all the columns coded with '0','1','2'

# ================= Setup ==================== #

df <- read.csv('claims.csv')

names(df)[1] <- "Case_ID"

df$Fraud <- factor(ifelse(df$Fraud == 0, "No", "Yes"))
df$Purpose <- factor(if_else(df$Purpose == "claim" , "Claim" , df$Purpose))
claims_df <- df # No pivot

df <- df %>% pivot_longer(AC_1001_Issue:TV_2003_Issue, 
                                        names_to = "Issue",
                                        values_to = "Issue_type",
                                        values_transform = as.character())

df <- df[df$Issue_type != 0,]

# ============================================ #

# Issue type given that Fraud of each Issue category

df %>% select(Issue, Issue_type, Fraud) %>% 
  filter(Fraud == "Yes" ) %>% 
  group_by(Issue, Issue_type) %>%
  mutate(Issue_type = factor(Issue_type)) %>% 

  ggplot(aes(x = Fraud)) + 
  geom_bar(aes(fill = Issue_type)) + 
  facet_wrap(~ Issue) + theme_bw()

# Fraud count per Product age (point)

FraudPAge<-claims_df %>% select(Product_Age, Fraud,Product_type) %>%
  filter(Fraud == "Yes") %>%
  group_by(Product_Age)

RealPAge<-claims_df %>% select(Product_Age, Fraud,Product_type) %>%
  filter(Fraud == "No") %>%
  group_by(Product_Age)

    #TV and AC Fraud
  FraudPAge %>% summarise(Fraud_Count = n()) %>% 
    ggplot(aes(x = Product_Age, y = Fraud_Count)) +
    geom_point(size = 2 , color = "red") +
    geom_smooth(method = "lm", se = FALSE , color = "orange") +
    theme_bw()
    
    #TV and AC Real
  RealPAge %>% summarise(Count = n()) %>% 
    ggplot(aes(x = Product_Age, y = Count)) +
    geom_point(size = 2 , color = "blue") +
    geom_smooth(method = "lm", se = FALSE , color = "violet") +
    theme_bw() 

    # IN TV Fraud
  FraudPAge %>% filter(Product_type == "TV") %>% 
    summarise(Fraud_Count = n()) %>% 
    ggplot(aes(x = Product_Age, y = Fraud_Count)) +
    geom_point(size = 2 , color = "red") +
    geom_smooth(method = "lm", se = FALSE , color = "orange") +
    theme_bw()
  
    # IN TV Real
  RealPAge %>% filter(Product_type == "TV") %>% 
    summarise(Count = n()) %>% 
    ggplot(aes(x = Product_Age, y = Count)) +
    geom_point(size = 2 , color = "blue") +
    geom_smooth(method = "lm", se = FALSE , color = "violet") +
    theme_bw() 

  
    # IN AC Fraud
    FraudPAge %>% filter(Product_type == "AC") %>% 
    summarise(Fraud_Count = n()) %>% 
    ggplot(aes(x = Product_Age, y = Fraud_Count)) +
    geom_point(size = 2 , color = "red") +
    geom_smooth(method = "lm", se = FALSE , color = "orange") +
    theme_bw()

  # IN AC Real
  RealPAge %>% filter(Product_type == "AC") %>% 
    summarise(Count = n()) %>% 
    ggplot(aes(x = Product_Age, y = Count)) +
    geom_point(size = 2 , color = "blue") +
    geom_smooth(method = "lm", se = FALSE , color = "violet") +
    theme_bw() 
  
  
# Fraud count per Product age (histogram)

    #In TV and AC
claims_df %>% select(Product_Age, Fraud) %>%
  ggplot(aes(x = Product_Age, fill = Fraud)) + 
  geom_histogram(bins = 30 , position = "stack") +
  theme_bw()

    #IN TV
claims_df %>% select(Product_Age, Fraud,Product_type) %>%
  filter(Product_type == "TV") %>% 
  ggplot(aes(x = Product_Age, fill = Fraud)) + 
  geom_histogram(bins = 30 , position = "stack") +
  theme_bw()

    #IN AC
claims_df %>% select(Product_Age, Fraud,Product_type) %>%
  filter(Product_type == "AC") %>% 
  ggplot(aes(x = Product_Age, fill = Fraud)) + 
  geom_histogram(bins = 30 , position = "stack") +
  theme_bw()

# Fraud count per claim value (histogram)
  #IN TV and AC
claims_df %>% select(Claim_Value, Fraud) %>%
  ggplot(aes(x = Claim_Value, fill = Fraud)) + 
  geom_histogram(bins = 30 , position = "stack") +
  theme_bw()
  
  #IN TV
claims_df %>% select(Claim_Value, Fraud , Product_type) %>%
  filter(Product_type == "TV") %>% 
  ggplot(aes(x = Claim_Value, fill = Fraud)) + 
  geom_histogram(bins = 30 , position = "stack") +
  theme_bw()

  #IN AC
claims_df %>% select(Claim_Value, Fraud , Product_type) %>%
  filter(Product_type == "AC") %>% 
  ggplot(aes(x = Claim_Value, fill = Fraud)) + 
  geom_histogram(bins = 30 , position = "stack") +
  theme_bw()

# Fraud with customer profile

claims_df %>% select(Consumer_profile, Fraud) %>%
  
  ggplot(aes(x = " ", fill = Fraud)) +
  geom_bar(position = position_fill()) +
  facet_grid(~ Consumer_profile) +
  coord_polar(theta = "y") +
  theme_void()

# Fraud percentage

claims_df %>% select(Fraud) %>%
  
  ggplot(aes(x = " ", fill = Fraud)) +
  geom_bar(position = position_fill()) +
  coord_polar(theta = "y") +
  theme_void()

# city and fraud

claims_df %>% select(City , Fraud) %>% 
  ggplot(aes(y = " " , fill = Fraud)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(City) , ncol = 5) +
  coord_polar(theta = "x") +
  theme_bw()

# Purpose and fraud
  claims_df %>% select(Purpose , Fraud) %>% 
    ggplot(aes(x = Purpose , fill = Fraud)) +
    geom_bar(position = "fill") +
    theme_bw()
  
# Purchased from and fraud
  claims_df %>% select(Purchased_from , Fraud) %>% 
    ggplot(aes(x = Purchased_from , fill = Fraud)) +
    geom_bar(position = "fill") +
    theme_bw()

# ============ Predict ==============
summary(claims_df$Fraud)
666/(7675+666) # 0.079

set.seed(123)
test_index <- sample(nrow(claims_df), 0.3*nrow(claims_df))
train_set <- claims_df[-test_index,]
test_set <- claims_df[test_index,]

summary(train_set$Fraud)
471/(5368+471) # 0.0806645

summary(test_set$Fraud)
195/(2307+195) # 0.07793765

model <- glm(Fraud ~. , data = train_set, family = binomial)
summary(model)

model1 <- glm(Fraud ~ City + Area + Product_Age + Product_type + Purchased_from , data = train_set, family = binomial)
model1

summary(model1)

result1 <- predict(model1, train_set, type = "response")
result1

result1_c <- factor(ifelse(result1 >  0.1, "Yes", "No"))
result1_c

confusionMatrix(result1_c, train_set$Fraud, mode = "prec_recall", positive = "Yes")

# use test set
result <- predict(model1, test_set, type = "response")

result_c <- factor(ifelse(result >  0.1, "Yes", "No"))
result_c

confusionMatrix(result_c, test_set$Fraud, mode = "prec_recall", positive = "Yes")
