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
  geom_violin(trim = FALSE, fill = 'deepskyblue3')+
  labs(title = "Distribution of total charges in each year", x = "Year", y = "Total charges")+
  stat_summary(fun=median, geom="point", size=0.5)+
  theme_bw()+
  theme(axis.text.x = element_text( angle = 45, size = 9, hjust = 1), plot.title = element_text(hjust = 0.5))

 graph_3

# Question 4 

hcris$discount_factor <-  1-(hcris$tot_discounts/hcris$tot_charges)
hcris$price_num <- (hcris$ip_charges + hcris$icu_charges + hcris$ancillary_charges)*hcris$discount_factor - hcris$tot_mcare_payment
hcris$price_denom <-  hcris$tot_discharges - hcris$mcare_discharges
hcris$price <- hcris$price_num/hcris$price_denom

hcris<- hcris %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30) %>%
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          penalty = (hvbp_payment-hrrp_payment<0))


graph_4 <- ggplot(hcris, aes(as.factor(year), price))+
  geom_violin(trim = FALSE, fill = 'deepskyblue3')+
  labs(title = "Distribution of hospital prices per year", x = "Year", y = "Hospital price")+
  stat_summary(fun=median, geom="point", size=0.5)+
  theme_bw()+
  theme(axis.text.x = element_text( angle = 45, size = 9, hjust = 1), plot.title = element_text(hjust = 0.5))

graph_4
# Question 5 

year_2012 <- hcris%>% filter(year == 2012)


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






#Class notes 

#chagne hrrp for only positive numbers, change NA for 0
#transform violin plot to be more informative(drop beds smaller than 30, price over 10,000, drop thigns with 0 in denominator)
#for 5 mean difference of 335
#love plot using library(cobalt) to see if common support holds, strict overlap 


#  exact matching
# nn_mahala <- Matching::Match(Y=filtered_2012$price,
#                              Tr=filtered_2012$penalty,
#                              X=5 column matrix/ dataframe/ he used medicare discharges,
#                              M=1,
#                              exact = TRUE)

# X is subset of original dataset, tibble (lp.covs %>% select(beds,mcaid_discharge))


#Variables used: beds, medicaid discharges, inpatient charges
# nn_mahala <- Matching::Match(Y=filtered_2012$price,
#                              Tr=filtered_2012$penalty,
#                              X=lp.covs,
#                              M=1,
#                              Weight = 1
#                              estimand= "ATE")









