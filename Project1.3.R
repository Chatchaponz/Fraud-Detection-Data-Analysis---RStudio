# Library
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

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

# ================= Data Preparation ==================== #

df <- read.csv('claims.csv', stringsAsFactors = TRUE)

names(df)[1] <- "Case_ID"

df$Fraud <- factor(ifelse(df$Fraud == 0, "No", "Yes"))
df$Purpose <- as.character(df$Purpose)
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

str(claims_df)
str(df)
# ====================================================== #

# Issue type given that Fraud of each Issue category
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
df %>% select(Issue, Issue_type, Fraud) %>% 
  filter(Fraud == "Yes" ) %>% 
  group_by(Issue, Issue_type) %>%
  mutate(Issue_type = factor(Issue_type)) %>% 
  ggplot(aes(x = Fraud)) + 
  geom_bar(aes(fill = Issue_type)) + 
  facet_wrap(~ Issue) + labs(y = "Count", fill = "Issue Type") + 
  scale_fill_manual(values = c("#38b6ff","#004bad"), 
                    labels = c("Repair","Replacement")) +
  theme_bw() + theme(strip.background = element_rect(fill = "white"))


# Fraud count per Product age (point)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

FraudPAge<-claims_df %>% select(Product_Age, Fraud,Product_type) %>%
  filter(Fraud == "Yes") %>%
  group_by(Product_Age)

RealPAge<-claims_df %>% select(Product_Age, Fraud,Product_type) %>%
  filter(Fraud == "No") %>%
  group_by(Product_Age)

#TV and AC Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
FraudPAge %>% summarise(Fraud_Count = n()) %>% 
  ggplot(aes(x = Product_Age, y = Fraud_Count)) +
  geom_point(size = 2 , color = "#004bad") +
  geom_smooth(method = "lm", se = FALSE , color = "#004bad") +
  theme_bw() +
  labs(x = "Product Age ( TV & AC )" , y = "Count")

#TV and AC Real
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
RealPAge %>% summarise(Count = n()) %>% 
  ggplot(aes(x = Product_Age, y = Count)) +
  geom_point(size = 2 , color = "#38b6ff") +
  geom_smooth(method = "lm", se = FALSE , color = "#38b6ff") +
  theme_bw() +  labs(x = "Product Age ( TV & AC )" , y = "Count")

# IN TV Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
FraudPAge %>% filter(Product_type == "TV") %>% 
  summarise(Fraud_Count = n()) %>% 
  ggplot(aes(x = Product_Age, y = Fraud_Count)) +
  geom_point(size = 2 , color = "#004bad") +
  geom_smooth(method = "lm", se = FALSE , color = "#004bad") +
  theme_bw() +  labs(x = "Product Age ( TV )" , y = "Count")

# IN TV Real
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
RealPAge %>% filter(Product_type == "TV") %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = Product_Age, y = Count)) +
  geom_point(size = 2 , color = "#38b6ff") +
  geom_smooth(method = "lm", se = FALSE , color = "#38b6ff") +
  theme_bw()  + labs(x = "Product Age ( TV )" , y = "Count")

# IN AC Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
FraudPAge %>% filter(Product_type == "AC") %>% 
  summarise(Fraud_Count = n()) %>% 
  ggplot(aes(x = Product_Age, y = Fraud_Count)) +
  geom_point(size = 2 , color = "#004bad") +
  geom_smooth(method = "lm", se = FALSE , color = "#004bad") +
  theme_bw() +  labs(x = "Product Age ( AC )" , y = "Count")

# IN AC Real
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
RealPAge %>% filter(Product_type == "AC") %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = Product_Age, y = Count)) +
  geom_point(size = 2 , color = "#38b6ff") +
  geom_smooth(method = "lm", se = FALSE , color = "#38b6ff") +
  theme_bw() + labs(x = "Product Age ( AC )" , y = "Count") 


###### Product Age and Type have a relationship in only Fraud ######

# Fraud with customer profile
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Consumer_profile, Fraud) %>%
  ggplot(aes(x = Consumer_profile, fill = Fraud)) +
  geom_bar() +
  scale_fill_manual(values = c("#38b6ff","#004bad")) +
  theme( strip.text.x = element_text( size = 20, face = "bold") ) +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold")) +
  scale_fill_manual(values = c("#38b6ff","#004bad")) +  labs( x = "Consumer Profile" )+ 
  labs( y = "Count" )

#Consumer Profile , City and Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Consumer_profile , City , Fraud) %>% 
  ggplot(aes(y = City , fill = Fraud)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(Consumer_profile)) + labs(x = "Density") +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold")) +
  scale_fill_manual(values = c("#38b6ff","#004bad"))

# city and fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(City , Fraud) %>% 
  ggplot(aes(y = City, fill = Fraud)) +
  geom_bar( ) +
  theme_bw() + theme(strip.text.x = element_text( face = "italic")) +
  scale_fill_manual(values = c("#38b6ff","#004bad")) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text.x = element_text( face = "bold"),
        axis.text.x = element_text(angle = -30, vjust = 0.5, hjust = 0.2)) +
  labs(x = "Count")

# Purpose , City , and fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Purpose , Fraud ) %>% 
  ggplot(aes(x = Purpose , fill = Fraud)) +
  geom_bar(position = "dodge") +
  theme_bw() + theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("#38b6ff","#004bad"))+
  labs(y = "Count")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Purpose , Fraud ,City) %>% 
  ggplot(aes(x = Purpose , fill = Fraud)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(City)) + labs( y = "Count" ) +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold"),
                     axis.text.x = element_text(angle = -30, vjust = 0.5, hjust = 0.2)) +
  scale_fill_manual(values = c("#38b6ff","#004bad"))
###### Purpose and City have significant change in Fraud Percentage

# Purchased from and fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Purchased_from , Fraud) %>% 
  ggplot(aes(x = Purchased_from , fill = Fraud)) +
  geom_bar(position = "dodge") +
  theme_bw() + scale_fill_manual(values = c("#38b6ff","#004bad")) +
  labs(x = "Purchased From" , y = "Count")

#Consumer Profile City Purpose and Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(City , Consumer_profile , Purpose , Fraud) %>%
  ggplot(mapping = aes(x = City  , fill = Fraud)) +
  geom_bar(position = "fill")+
  facet_wrap(vars(  Consumer_profile , Purpose  ) , ncol = 1) +
  labs( y = "Count" ) +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold"),
                     axis.text.x = element_text(angle = -30, vjust = 0.5, hjust = 0.2)) +
  scale_fill_manual(values = c("#38b6ff","#004bad"))

#Area and Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Area, Fraud ) %>%
  ggplot(mapping = aes(x = Area  , fill = Fraud)) +
  geom_bar(position = "dodge") + labs( y = "Count" ) + theme_bw() +
  scale_fill_manual(values = c("#38b6ff","#004bad"))

#State and Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(State, Fraud ) %>%
  ggplot(mapping = aes(y = State  , fill = Fraud)) +
  geom_bar(position = "stack") + labs( x = "Count" ) + theme_bw() +
  scale_fill_manual(values = c("#38b6ff","#004bad"))

#Region and Fraud
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
claims_df %>% select(Region, Fraud ) %>%
  ggplot(mapping = aes(y = Region  , fill = Fraud)) +
  geom_bar(position = "stack") + labs( x = "Count" ) + theme_bw() +
  scale_fill_manual(values = c("#38b6ff","#004bad"))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#Purchased_from Claim_Value , Fraud

claims_df %>% select(Purchased_from, Claim_Value ,Fraud ) %>%
  ggplot(mapping = aes(x = Claim_Value , y = ..density..)) +
  geom_density( aes(color = Fraud) ,size = 1.2) +
  facet_wrap(vars(Purchased_from)) + labs(x = "Claim Value (RS)", y = "Density") +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold")) +
  scale_color_manual(values = c("#38b6ff","#004bad"))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#Product Age ,  Type and Fraud
claims_df %>% select(Product_Age , Product_type , Fraud) %>% 
  ggplot(mapping = aes(x = Product_Age , y = ..density..)) +
  geom_density(aes(color = Fraud),size = 1.2) +
  facet_wrap(vars(Product_type , Fraud))+ 
  labs(x = "Product Age (Day)", y = "Density") +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold")) +
  scale_color_manual(values = c("#38b6ff","#004bad"))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#Claim Value & Fraud  
claims_df %>% select (Claim_Value , Fraud) %>% 
  ggplot(mapping = aes(x = Claim_Value , color = Fraud)) +
  geom_density(size = 1.2) + 
  labs(x = "Claim Value (Rs)", y = "Density") +
  theme_bw() + theme(strip.background = element_rect(fill = "white"),
                     strip.text.x = element_text( face = "bold")) +
  scale_color_manual(values = c("#38b6ff","#004bad"))


# ============ Predict ============== #

# 123 1235

set.seed(123)

test_index <- sample(nrow(claims_df), 0.3*nrow(claims_df))
train_set <- claims_df[-test_index,]
test_set <- claims_df[test_index,]

# Full data
summary_Fraud <- summary(claims_df$Fraud)
summary_Fraud
# Percent Fraud
(summary_Fraud[2]/(summary_Fraud[1] + summary_Fraud[2]))*100

# Train data
summary_Fraud <- summary(train_set$Fraud)
summary_Fraud
# Percent Fraud
(summary_Fraud[2]/(summary_Fraud[1] + summary_Fraud[2]))*100

# Test data
summary_Fraud <- summary(test_set$Fraud)
summary_Fraud
# Percent Fraud
(summary_Fraud[2]/(summary_Fraud[1] + summary_Fraud[2]))*100

# 453x370
#Fraud per count
claims_df %>% select(Fraud) %>% group_by(Fraud) %>%
  ggplot(aes(x = Fraud, fill = Fraud)) + geom_bar() + labs( y = "Count") +
  theme_bw() + scale_fill_manual(values = c("#38b6ff","#004bad"))

train_set %>% select(Fraud) %>% group_by(Fraud) %>%
  ggplot(aes(x = Fraud, fill = Fraud)) + geom_bar() + labs( y = "Count") +
  theme_bw() + scale_fill_manual(values = c("#38b6ff","#004bad"))

test_set %>% select(Fraud) %>% group_by(Fraud) %>%
  ggplot(aes(x = Fraud, fill = Fraud)) + geom_bar() + labs( y = "Count") +
  theme_bw() + scale_fill_manual(values = c("#38b6ff","#004bad"))

# ==================== Previous Model (GLM) ======================== #

model <- glm(Fraud ~. , data = train_set, family = binomial)

model <- glm(Fraud ~ City * Area + State + Region + 
                     Consumer_profile * City + 
                     Consumer_profile * Product_Age * Product_type + 
                     Purchased_from, data = train_set, family = binomial)

model <- glm(Fraud ~ City * Area + State + Region + 
                     Consumer_profile + Product_Age * Product_type + 
                     Purchased_from , data = train_set, family = binomial)

model <- glm(Fraud ~ Consumer_profile * City * Area + State + Region + 
                     Product_Age * Product_type + 
                     Purchased_from, data = train_set, family = binomial)

model <- glm(Fraud ~ Consumer_profile * City * Purpose + 
                     Area + State + Region + 
                     Product_Age * Product_type + Purchased_from + 
                     TV_2001_Issue * TV_2002_Issue * TV_2003_Issue +
                     AC_1001_Issue * AC_1002_Issue * AC_1003_Issue, 
                     data = train_set, family = binomial)

# ================================================================= #

# ==================== Final Model (GLM) ======================== #

model <- glm(Fraud ~ Consumer_profile * City * Purpose + Area + State + Region + 
                     Product_Age * Product_type + Service_Centre +
                     Purchased_from * Claim_Value  + 
                     TV_2001_Issue * TV_2002_Issue * TV_2003_Issue +
                     AC_1001_Issue * AC_1002_Issue * AC_1003_Issue,
                     data = train_set, family = binomial )

model

summary(model)

result1 <- predict(model, train_set, type = "response")
result1

result1_c <- factor(ifelse(result1 > 0.01, "Yes", "No"))
result1_c

confusionMatrix(result1_c, train_set$Fraud, positive = "Yes")

# Use test set
result <- predict(model, test_set, type = "response")
result

result_c <- factor(ifelse(result >  0.1, "Yes", "No"))
result_c

confusionMatrix(result_c, test_set$Fraud, positive = "Yes")

# ==================== Final Model (Tree) ======================== #


# Highest true positive
tree <- rpart(Fraud ~ Product_Age + Claim_Value + Purchased_from + 
                      State + Region + Call_details + Service_Centre,
                      data = train_set, control = rpart.control(cp = 0.001))
# Highest kappa
tree <- rpart(Fraud ~ Product_Age + Claim_Value + Purchased_from + 
                      City + Purpose + Region + Call_details + Service_Centre,
                      data = train_set, control = rpart.control(cp = 0.001))

# result as pdf to see *****
pdf('tree.pdf') #start pdf
rpart.plot(tree)
dev.off() #end

rpart.plot(tree)

tree$variable.importance

# Train
res <- predict(tree, train_set, type = 'class')
res

confusionMatrix(res, train_set$Fraud, positive = 'Yes')

# Test
res <- predict(tree, test_set, type = 'class')
res

confusionMatrix(res, test_set$Fraud, positive = 'Yes')

# lift
#tree
res.p <- predict(tree, test_set)[,'Yes']

# glm
res.p  <- predict(model, test_set, type = "response")
res.p

lift_res <- data.frame(
  prob = res.p,
  fraud = test_set$Fraud
)

lift_obj <- lift(fraud ~ prob,
                 data = lift_res,
                 class = "Yes")

plot(lift_obj)

# ================ use cross varidation  ==================
train_control <- trainControl(method = 'cv', number = 5)

this_model <- train(Fraud ~ ., data = train_set, trControl = train_control, method = "rpart", na.action = na.exclude)

plot(this_model$finalModel)
