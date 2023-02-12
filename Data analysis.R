library(tidyverse)

# Question 1 
hcris <- read_rds("data/output/HCRIS_Data.rds")

filterd_data <- hcris%>% group_by(street, year)%>% 
  summarize(n = n())%>% 
  filter(n>1)%>% 
  group_by(year)%>% 
  summarize(count = n())

graph_1 <- ggplot(filterd_data, aes(year, count))+
  geom_line()+
  labs(title = "Number of hospitals that filed more than one report in the same year", x = "Year", y = "Number of hospitals")+
  theme_bw()

# Question 2 

unique_hospitals <- length(unique(hcris$provider_number))

# Question 3 

graph_3 <- ggplot(hcris, aes(x=as.factor(year), y=tot_charges)) + 
  geom_violin()+
  labs(title = "Distribution of total charges in each year", x = "Year", y = "Total charges")+
  theme_bw()

# Question 4 

hcris$discount_factor <-  1-(hcris$tot_discounts/hcris$tot_charges)
hcris$price_num <- (hcris$ip_charges + hcris$icu_charges + hcris$ancillary_charges)*hcris$discount_factor - hcris$tot_mcare_payment
hcris$price_denom <-  hcris$tot_discharges - hcris$mcare_discharges
hcris$price <- hcris$price_num/hcris$price_denom

graph_4 <- ggplot(hcris, aes(as.factor(year), price))+
  geom_violin()+
  labs(title = "Distribution of hospital prices per year", x = "Year", y = "Hospital price")+
  theme_bw()

# Question 5 

year_2012 <- hcris%>% filter(year == 2012)
year_2012$penalty <- ifelse(year_2012$hvbp_payment + year_2012$hrrp_payment < 0 , 1,0)

table_5 <- year_2012%>% filter(!is.na(penalty))%>%
  group_by(penalty)%>% 
  summarize(price = mean(price, na.rm = TRUE))

# Question 6 
year_2012$quartile <- ntile(year_2012$beds, 4) 

year_2012$quartile_1 <- ifelse(year_2012$quartile == 1, 1,0)
year_2012$quartile_2 <- ifelse(year_2012$quartile == 2, 1,0)
year_2012$quartile_3 <- ifelse(year_2012$quartile == 3, 1,0)
year_2012$quartile_4 <- ifelse(year_2012$quartile == 4, 1,0)

table_6 <- year_2012 %>% filter(!is.na(penalty))%>% group_by(quartile, penalty)%>% summarize(avg_price = mean(price, na.rm = TRUE))

# Question 7 

filtered_2012 <- year_2012[!is.na(year_2012$price) & !is.na(year_2012$penalty) & !is.na(year_2012$quartile),]

# Nearest neighbors with inverse weight 
nn_weight <- Matching::Match(Y=filtered_2012$price,
                           Tr=filtered_2012$penalty,
                           X=filtered_2012$quartile,
                           M=1,
                           Weight=1,
                           estimand="ATE")

#Nearest neighbors with malahanobis 
nn_mahala <- Matching::Match(Y=filtered_2012$price,
                             Tr=filtered_2012$penalty,
                             X=filtered_2012$quartile,
                             M=1,
                             Weight=2,
                             estimand="ATE")

#Intverse propensity weight 
ps <- glm(penalty~ quartile, family=binomial, filtered_2012)
filtered_2012$propensity_score <-predict(ps, filtered_2012[, "quartile"], type='response')

n_1 <- filtered_2012 %>% filter(penalty ==1)
n_0 <- filtered_2012 %>% filter(penalty ==0)

denom_1<- sum(n_1$price/n_1$propensity_score)
nom_1 <- sum(nrow(n_1)/n_1$propensity_score)
mu_1<- denom_1 / nom_1

denom_0<- sum(n_0$price/(1-n_0$propensity_score))
nom_0 <- sum(nrow(n_0)/(1 - n_0$propensity_score))
mu_0<- denom_0 / nom_0

delta <- mu_1 - mu_0

#Regression

reg1 <- lm(penalty ~ as.factor(quartile), data=n_1)
reg0 <- lm(penalty~ as.factor(quartile), data=n_0)
pred1 <- predict(reg1,new=n_1[, "quartile"])
pred0 <- predict(reg0,new=n_0[, "quartile"])
reg_ate <- mean(sum(pred1)-sum(pred0))



save.image("image.Rdata")















