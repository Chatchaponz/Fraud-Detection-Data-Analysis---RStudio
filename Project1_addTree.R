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
df$Service_Centre <- factor(as.character(df$Service_Centre))
df$AC_1001_Issue <- factor(as.character(df$AC_1001_Issue))
df$AC_1002_Issue <- factor(as.character(df$AC_1002_Issue))
df$AC_1003_Issue <- factor(as.character(df$AC_1003_Issue))
df$TV_2001_Issue <- factor(as.character(df$TV_2001_Issue))
df$TV_2002_Issue <- factor(as.character(df$TV_2002_Issue))
df$TV_2003_Issue <- factor(as.character(df$TV_2003_Issue))

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

#Consumer Profile , City and Fraud
claims_df %>% select(Consumer_profile , City , Fraud) %>% 
  ggplot(aes(y = City , fill = Fraud)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(Consumer_profile))

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

#Call Details and fraud
  claims_df %>% select(Call_details , Fraud) %>% 
    ggplot(aes(x = Call_details , color = Fraud)) +
    geom_density(position = "stack" , size = 1.2) +
    facet_wrap(vars(Fraud))
  
#Service Center and fraud
  claims_df %>% select(Service_Centre , Fraud) %>% 
    ggplot(aes(x = Service_Centre , fill = Fraud)) +
    geom_bar(position = "fill")
  
#Service Center , Call Details and Fraud
  claims_df %>% select(Service_Centre , Call_details , Fraud) %>% 
    ggplot(aes(x = Call_details , fill = Fraud)) +
    geom_histogram() +
    facet_wrap(vars(Service_Centre , Fraud))
  
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

model1 <- glm(Fraud ~ City * Area + State + Region + 
                Consumer_profile * City + Consumer_profile * Product_Age * Product_type + 
                Purchased_from  , data = train_set, family = binomial)
model1

model2 <- glm(Fraud ~ City * Area + State + Region + Consumer_profile + Product_Age * Product_type + Purchased_from , data = train_set, family = binomial)
model2

model3 <- glm(Fraud ~  Consumer_profile * City * Area + State + Region  + 
                Product_Age * Product_type + 
                Purchased_from, data = train_set, family = binomial)
model3

str(claims_df)
model4 <- glm(Fraud ~  Consumer_profile * City + Area + State + Region  + 
                Product_Age * Product_type + 
                Purchased_from + TV_2001_Issue * TV_2002_Issue * TV_2003_Issue +
                AC_1001_Issue * AC_1002_Issue * AC_1003_Issue , data = train_set, family = binomial)
model4

model5 <- glm(Fraud ~  Service_Centre  * Consumer_profile * City * Purpose + Area + State  + Region + 
                Product_Age * Product_type  +
                Purchased_from * Claim_Value  + TV_2001_Issue * TV_2002_Issue * TV_2003_Issue +
                AC_1001_Issue * AC_1002_Issue * AC_1003_Issue , data = train_set, family = binomial)

summary(model1)

result1 <- predict(model5, train_set, type = "response")
result1

result1_c <- factor(ifelse(result1 >  0.1, "Yes", "No"))
result1_c

confusionMatrix(result1_c, train_set$Fraud, mode = "prec_recall", positive = "Yes")

# use test set
result <- predict(model4, test_set, type = "response")

result_c <- factor(ifelse(result >  0.1, "Yes", "No"))
result_c

confusionMatrix(result_c, test_set$Fraud,  mode = "prec_recall" , positive = "Yes")


#===============================================================================#
# Tree
library(rpart)
library(rpart.plot)

str(claims_df)
tree_claims_df <- read.csv("claims.csv", stringsAsFactors = TRUE)
names(tree_claims_df)[1] <- "Case_ID"
tree_claims_df$Fraud <- factor(ifelse(tree_claims_df$Fraud == 0, "No", "Yes"))
tree_claims_df$Purpose <- factor(if_else(tree_claims_df$Purpose == "claim" ,
                                         "Claim" , tree_claims_df$Purpose))
tree_claims_df$Service_Centre <- factor(as.character(tree_claims_df$Service_Centre))
tree_claims_df$AC_1001_Issue <- factor(as.character(tree_claims_df$AC_1001_Issue))
tree_claims_df$AC_1002_Issue <- factor(as.character(tree_claims_df$AC_1002_Issue))
tree_claims_df$AC_1003_Issue <- factor(as.character(tree_claims_df$AC_1003_Issue))
tree_claims_df$TV_2001_Issue <- factor(as.character(tree_claims_df$TV_2001_Issue))
tree_claims_df$TV_2002_Issue <- factor(as.character(tree_claims_df$TV_2002_Issue))
tree_claims_df$TV_2003_Issue <- factor(as.character(tree_claims_df$TV_2003_Issue))

str(tree_claims_df)

# test
set.seed(123) #123 / #1235
test_index <- sample(nrow(tree_claims_df), 0.3*nrow(tree_claims_df))
train_set_T <- tree_claims_df[-test_index,]
test_set_T <- tree_claims_df[test_index,]

b <- summary(train_set_T$Fraud)
b
b[2]/(b[1]+b[2]) # 0.0806645


c <- summary(test_set_T$Fraud)
c
c[2]/(c[1]+c[2]) # 0.07793765

#tree
tree <- rpart(Fraud ~ . - Case_ID, data = train_set_T )
summary(tree)

#tree4
tree <- rpart(Fraud ~ City + Product_Age + Claim_Value +
                Purchased_from  + TV_2002_Issue +
                AC_1001_Issue + AC_1002_Issue, data = train_set_T,
              control = rpart.control(cp = 0.001))
#tree1 #391
tree <- rpart(Fraud ~ Area + Product_Age + Claim_Value +
                Purchased_from  + TV_2002_Issue +
                AC_1001_Issue + AC_1002_Issue, data = train_set_T,
                control = rpart.control(cp = 0.001))
#tree2 #371 #0.8221
tree <- rpart(Fraud ~ Area + Product_type + Product_Age + Claim_Value +
                Purchased_from  + TV_2002_Issue +
                AC_1001_Issue + AC_1002_Issue, data = train_set_T,
              control = rpart.control(cp = 0.001))

#tree3 # 386
tree <- rpart(Fraud ~ Area + Product_Age + Claim_Value +
                Purchased_from  + TV_2002_Issue , data = train_set_T,
              control = rpart.control(cp = 0.001))
#*Product_Age 145 -
#*Claim_Value 82
#*Product_Age + TV_2002_Issue 210
#*Product_Age + TV_2003_Issue 219
#*Product_Age + Area 145
#*Product_Age + City 289 -
#*Product_Age + Region 289 -
#*Product_Age + State 260
#*Product_Age + Claim_Value 372 -
#*Product_Age + AC_XXXX_Issue 145
#*Product_Age + Call_details 362 -
#*Product_Age + Purchased_from 305 -
#*Product_Age + Claim_Value + Call_details 374 -
#*Product_Age + Claim_Value + Purchased_from 377 -
#*Product_Age + Claim_Value + Region 393 ++++++
#*Product_Age + Claim_Value + Service_Centre 382
#*Product_Age + Claim_Value + Region + Service_Centre 390
tree <- rpart(Fraud ~ Product_Age + Claim_Value + Region , data = train_set_T,
              control = rpart.control(cp = 0.001))

#*Product_Age + Claim_Value + Area + Purchased_from 395 ++++++
#*Product_Age + Claim_Value + Area + Purchased_from + State 397 ++++
#*Product_Age + Claim_Value + Area + Purchased_from + State + Region 397 0.805
#*Product_Age + Claim_Value + Area + Purchased_from + State + Region + Call_details + Service_Centre 399 0.8104
tree <- rpart(Fraud ~ Product_Age + Claim_Value + Purchased_from + 
                      State + Region + Call_details + Service_Centre,
                      data = train_set_T, control = rpart.control(cp = 0.001))

# 0.8261 best kappa so far
tree <- rpart(Fraud ~ Product_Age + Claim_Value + Purchased_from + 
                      City + Purpose + Region + Call_details + Service_Centre,
                      data = train_set_T, control = rpart.control(cp = 0.001))



# result as pdf to see *****
pdf('tree.pdf') #start pdf
rpart.plot(tree)
dev.off() #end

rpart.plot(tree)

tree$variable.importance

# Train
# predict(tree, data = train_set_T)
res <- predict(tree, train_set_T, type = 'class')
confusionMatrix(res, train_set_T$Fraud, positive = 'Yes', mode = 'prec_recall')

# Test
#predict(tree, data = test_set_T)

res <- predict(tree, test_set_T, type = 'class')

confusionMatrix(res, test_set_T$Fraud, positive = 'Yes', mode = 'prec_recall')


# lift

res.p <- predict(tree, test_set_T)[,'Yes']

res.p

lift_res <- data.frame(
  prob = res.p,
  fraud =  test_set_T$Fraud
)

lift_obj <- lift(fraud ~ prob,
                 data = lift_res,
                 class = "Yes")

plot(lift_obj)

