library(dbplyr)
library(readr)
library(tidyverse)

# Import of the data
df =read.csv("E-Commerce.csv")

# Visualisation of the data information
str(df)

# Rectification of column names
colnames(df)
df=rename(df, Customer_ID=ï..ID)
df=rename(df, Reached_on_Time=Reached.on.Time_Y.N)

# Visualisation of the data information
summary(df)

# Distribution of data
skim(df)

# Warehouse Analysis

# Data by Warehouse

warehouse1=df %>% 
  group_by(Warehouse_block) %>% 
  rename(Warehouse=Warehouse_block) %>%
  summarise(Sales=sum(Cost_of_the_Product),  
            Discounts=sum(Discount_offered),'Care Calls'=sum(Customer_care_calls),
            Rating=mean(Customer_rating))

warehouse1

#Product Cost by Warehouse Block
ggplot(df,aes(Warehouse_block,Cost_of_the_Product,color=Warehouse_block))+
  geom_boxplot()+
  labs(title="Warehouse Distribution", subtitle="Product Cost by Warehouse Block", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       colour="Warehouse Block", x="Warehouse Block", y="Product Cost")

ggplot(warehouse1,aes(Warehouse,Sales,fill=Warehouse))+
  geom_bar(stat='identity')+
  labs(title="Warehouse Distribution", subtitle="Total Sales by Warehouse Block", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Warehouse Block", x="Warehouse Block", y="Total Sales")

#Product Cost and Discount Offered
ggplot(df,aes(Cost_of_the_Product,Discount_offered,colour=Product_importance))+ 
  geom_point()+
  facet_grid(~Warehouse_block)+
  labs(title="Warehouse Distribution", subtitle="Product Cost and Discount Offered", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       colour="Product Importance", x="Product Cost", y="Product Discount")+
  theme_minimal()

ggplot(warehouse1,aes(Warehouse,Discounts,fill=Warehouse))+
  geom_bar(stat='identity')+
  labs(title="Warehouse Distribution", subtitle="Total Discounts by Warehouse Block", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Warehouse Block", x="Warehouse Block", y="Total Discounts")

# Data by Warehouse, Shipment type and Product importance

warehouse2=df %>% 
  group_by(Warehouse_block,Mode_of_Shipment,Product_importance) %>% 
  rename(Warehouse=Warehouse_block,Shipment=Mode_of_Shipment,Importance=Product_importance) %>%
  summarise(Sales=sum(Cost_of_the_Product),  
            Discounts=sum(Discount_offered))

warehouse2

# Product Cost and Discount Offered by Importance
ggplot(df,aes(Cost_of_the_Product,Discount_offered,colour=Product_importance))+ 
  geom_point()+
  facet_grid(Product_importance~Warehouse_block)+
  labs(title="Warehouse Distribution", subtitle="Product Cost and Discount Offered by Importance", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       colour="Product Importance", x="Product Cost", y="Product Discount")+
  theme_minimal()


ggplot(warehouse2,aes(Warehouse,Discounts,fill=Importance))+
  geom_bar(stat='identity')+
  labs(title="Warehouse Distribution", subtitle="Total Saless by Product Importance", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Warehouse Block", x="Warehouse Block", y="Total Sales")

# Data by Warehouse and delivered on time

warehouse3=df %>% 
  group_by(Warehouse_block) %>% 
  rename(Warehouse=Warehouse_block) %>%
  summarise("Delivered on time"=sum(Reached_on_Time))

warehouse3

# Product Cost and Weight of Product by Gender
ggplot(df,aes(Cost_of_the_Product,Weight_in_gms,colour=Product_importance))+ 
  geom_point()+
  facet_grid(Gender~Warehouse_block)+
  labs(title="Warehouse Distribution", subtitle="Product Cost and Weight of Product by Gender", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       colour="Product Importance", x="Product Cost", y="Product Weight")+
  theme_minimal()


# Product Cost and Weight of Product by Transport
ggplot(df,aes(Warehouse_block,Cost_of_the_Product,fill=Product_importance))+ 
  geom_bar(stat='identity')+
  facet_grid(~Mode_of_Shipment)+
  labs(title="Warehouse Distribution", subtitle="Product Cost by Transport", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Product Importance", x="Warehouse Block", y="Product Cost")+
  theme_minimal()

ggplot(warehouse2,aes(Warehouse,Sales,fill=Shipment))+
  geom_bar(stat='identity')+
  labs(title="Warehouse Distribution", subtitle="Total Saless by Shipment", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Warehouse Block", x="Warehouse Block", y="Total Sales")

# Sales Analysis

# Product Sales by Gender
sales1=df %>% 
  group_by(Gender) %>% 
  summarise(Sales=sum(Cost_of_the_Product),  
            Discounts=sum(Discount_offered))

sales1


ggplot(sales1,aes(Gender,Sales,fill=Gender))+
  geom_bar(stat='identity', width=1)+
  coord_polar(start=0)+
  labs(title="Product Sales", subtitle="Total Saless by Gender", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Gender", x=NULL, y=NULL)+
  theme(axis.title = element_blank(),axis.text = element_blank(),
                axis.ticks.y = element_blank(),panel.background = element_blank())


# Product Sales by Transport

sales2=df %>% 
  group_by(Mode_of_Shipment) %>% 
  rename(Shipment=Mode_of_Shipment) %>%
  summarise(Sales=sum(Cost_of_the_Product),  
            Discounts=sum(Discount_offered))

sales2

ggplot(sales2,aes(Shipment,Sales,fill=Shipment))+
  geom_bar(stat='identity')+
  labs(title="Product Sales", subtitle="Total Saless by Shipment Type", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Shipment", x="Shipment Type", y="Total Sales")+
  theme(legend.position = "none")

# Product Sales by Importance

sales3=df %>% 
  group_by(Product_importance) %>% 
  rename(Importance=Product_importance) %>%
  summarise(Sales=sum(Cost_of_the_Product),  
            Discounts=sum(Discount_offered))

sales3

ggplot(sales3,aes(Importance,Sales,fill=Importance))+
  geom_bar(stat='identity')+
  labs(title="Product Sales", subtitle="Total Saless by Importance", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       x="Importance", y="Total Sales")+
  theme(legend.position = "none")

# Product Sales by Delivery on time
dffac=df
dffac$Reached_on_Time=as.logical(dffac$Reached_on_Time)

sales4=dffac %>% 
  group_by(Reached_on_Time) %>% 
  summarise(Sales=sum(Cost_of_the_Product))

sales4

ggplot(sales4,aes(Reached_on_Time,Sales,fill=Reached_on_Time))+
  geom_bar(stat='identity', width=1)+
  coord_polar(start=0)+
  labs(title="Product Sales", subtitle="Product delivered on time", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Delivered on Time", x=NULL, y=NULL)+
  theme(axis.title = element_blank(),axis.text = element_blank(),
        axis.ticks.y = element_blank(),panel.background = element_blank())

# Other variants to consider

# Rating by Delivery on time
rating1=dffac %>% 
  group_by(Reached_on_Time) %>% 
  summarise(Ratings=mean(Customer_rating))

ggplot(rating1,aes(Reached_on_Time,Ratings,fill=Reached_on_Time))+
  geom_bar(stat='identity')+
  labs(title="Ratings", subtitle="Product delivered on time", 
      caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
      fill="Delivered on Time", x=NULL)

# Rating by Discount
rating2=df %>% 
  group_by(Warehouse_block) %>% 
  summarise(Ratings=mean(Customer_rating),Discounts=mean(Discount_offered))

ggplot(rating2,aes(Discounts,Ratings,fill=Warehouse_block))+
  geom_bar(stat='identity')+
  labs(title="Ratings", subtitle="Average Discount", 
       caption="Data from:https://www.kaggle.com/prachi13/customer-analytics", 
       fill="Warehouse")


  
  