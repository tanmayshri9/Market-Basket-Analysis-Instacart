

#############################################################################################################
#############################################################################################################
#### Group # : 4                                                                                   ##########
#### Assignment # : Final Project                                                                  ##########
#### Submitted by: Ashish Saxena, Lohit Borah, Tanmay Shrivastava, Pratibha Prakash Tiwari*/       ##########
#############################################################################################################
#############################################################################################################
# install.packages("tidyverse", dependencies = T)
# Loading required libraries
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)
library(datasets)
library(scales)


# Setting up working directory
setwd("C:/Users/info/OneDrive/Desktop/University of Cincinnati/BANA 6043 Statistical Computing/Project/instacart-market-basket-analysis/")

# Reading required datasets
aisles = read.csv('aisles.csv')
departments = read.csv('departments.csv')
order_products__prior = read.csv('order_products__prior.csv')
order_products__train = read.csv('order_products__train.csv')
orders = read.csv('orders.csv')
products = read.csv('products.csv')

# User-defined function to check data integrity
data_integrity_check = function(x)
{
  print(paste("Null values across columns of : ",deparse(substitute(x))))
  print(colSums(is.na(x)))
  print(paste("Structure of : ",deparse(substitute(x))))
  print(str(x))
  print(paste("Summary of : ",deparse(substitute(x))))
  print(summary(x))
}

data_integrity_check(aisles)
data_integrity_check(departments)
data_integrity_check(order_products__prior)
data_integrity_check(order_products__train)
data_integrity_check(orders)
data_integrity_check(products)


## Creating master dataset
order_products = rbind(order_products__train, order_products__prior)
colnames(order_products)

product_department = left_join(products, departments, by = "department_id")
product_department_aisle = left_join(product_department, aisles, by = "aisle_id")

length(unique(orders$order_id))==nrow(orders)
order_products_department_aisle = left_join(order_products, product_department_aisle, by = "product_id")
colSums(is.na(order_products_department_aisle))
colSums(is.na(orders))

# nrow(orders) == length(unique(orders$order_id))
# nrow(order_products_department_aisle)
# length(unique(paste0(order_products_department_aisle$order_id, order_products_department_aisle$product_id)))

staging = left_join(orders, order_products_department_aisle, by = "order_id")
colSums(is.na(staging))
length(unique(paste0(order_products$order_id,order_products$product_id)))
nrow(order_products)

# order_products_sum = order_products %>%
#   group_by(order_id)%>%
#   summarise(cnt = length(unique(product_id)))%>%
#   group_by(cnt)%>%
#   summarise(cntorder = length(unique(order_id)))
# 
# 
# summ = order_products%>%
#   group_by(order_id, product_id)%>%
#   mutate(cnt = n())%>%
#   filter(cnt>1)


final_staging = staging[!is.na(staging$product_id),]
colnames(final_staging)
str(final_staging)
summary(final_staging)

rm(aisles,departments,order_products__prior,order_products__train,orders,products,staging)

#########################
######## EDA ############
#########################

# 1. Count of orders by Day-of-week
order_dow = orders %>%
  group_by(order_dow) %>%
  summarise(cnt = length(unique(order_id)))

days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

ggplot(data=order_dow, aes(x = reorder(days,-cnt), y=cnt)) +
  geom_bar(stat="identity",color='skyblue',fill='steelblue') +
  xlab("Day of week") + ylab("Number of orders") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-4)) +
  ggtitle("Count of orders v/s Day of the week")

# 2. Count of orders by hour of day
order_hour_of_day = orders %>%
  group_by(order_hour_of_day) %>%
  summarise(cnt = length(unique(order_id)))


ggplot(data=order_hour_of_day, aes(x=order_hour_of_day, y=cnt)) +
  geom_bar(stat="identity",color='skyblue',fill='steelblue') + 
  xlab("Hour of Day") + ylab("Number of orders") + 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-4)) + 
  ggtitle("Count of orders v/s Hour of the day")

# 3. Order count by basket size
order_cnt_by_size = final_staging%>%
  group_by(order_id)%>%
  summarise(basket_size = length(product_id))%>%
  group_by(basket_size)%>%
  summarise(cnt_orders = length(unique(order_id)))
  
ggplot(data=order_cnt_by_size, aes(x=basket_size, y=cnt_orders)) +
  geom_bar(stat="identity",fill='steelblue')


# 4. Count of order by days since prior order
days_since_prior_order = orders %>%
  group_by(days_since_prior_order) %>%
  summarise(cnt = length(unique(order_id)))

ggplot(data=days_since_prior_order, aes(x=days_since_prior_order, y=cnt)) +
  geom_bar(stat="identity",color='skyblue',fill='steelblue') +
  xlab("Days since previous order") + ylab("Number of orders") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-4)) + 
  ggtitle("Count of orders v/s Days since previous order")

# 5. Product count by reorder history
reordered_products = final_staging %>%
  group_by(reordered) %>%
  summarise(cnt = length(unique(product_id)))

ggplot(data=reordered_products, aes(x=reordered, y=cnt)) +
  geom_bar(stat="identity", fill='steelblue')

# 6. Count of orders by no of reordered items in bucket
reordered_orders = final_staging %>%
  group_by(order_id) %>%
  summarise(cnt_reorder = sum(reordered, na.rm = T))%>%
  group_by(cnt_reorder)%>%
  summarise(cnt_order = length(unique(order_id)))

ggplot(data = reordered_orders) + 
  geom_point(mapping = aes(x = cnt_reorder, y = cnt_order), color = "steelblue") +
  xlab("Number of reorders") + ylab("Number of orders") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-4)) +
  ggtitle("Count of orders that were reordered")

# 7. Top 20 products by frequency
prod_freq = as.data.frame(table(final_staging$product_name))
colnames(prod_freq) = c("product_name", "frequency")

ordered_prod_freq = prod_freq[order(prod_freq$frequency, decreasing = T),]
ordered_prod_freq_top20 = ordered_prod_freq[1:10,]
ordered_prod_freq_top20_join = left_join(ordered_prod_freq_top20, unique.data.frame(product_department_aisle[,c("product_name", "aisle","department")]), by = "product_name")

# 8. Top 20 departments by frequency
dept_freq = as.data.frame(table(final_staging$department))
colnames(dept_freq) = c("department", "frequency")

ordered_dept_freq = dept_freq[order(dept_freq$frequency, decreasing = T),]
ordered_dept_freq_top20 = ordered_dept_freq[1:10,]

# 9. Top 20 aisles by frequency
aisle_freq = as.data.frame(table(final_staging$aisle))
colnames(aisle_freq) = c("aisle", "frequency")

aisle_dept_freq = aisle_freq[order(aisle_freq$frequency, decreasing = T),]
aisle_dept_freq_top20 = aisle_dept_freq[1:10,]


# 10. No. of reorders for most ordered products
final_staging_top20 = inner_join(final_staging, ordered_prod_freq_top20, by = c("product_name"))
top20_reordered = final_staging_top20 %>%
  group_by(product_name) %>%
  summarise(cnt_reorder = sum(reordered))%>%
  arrange(desc(cnt_reorder))

ggplot(data=top20_reordered, aes(x=reorder(product_name,cnt_reorder), y=cnt_reorder)) + 
  geom_bar(stat="identity",color='skyblue',fill='steelblue') + 
  xlab("Product name") + 
  ylab("Count of reorders") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-4)) + 
  ggtitle("No. of reorders for most ordered products") +
  coord_flip()

# 11. No. of products by department
products_by_dept = final_staging %>%
  group_by(department) %>%
  summarise(cnt = length(unique(product_id)))

ggplot(data=products_by_dept, aes(x=reorder(department,cnt), y=cnt)) +
  xlab("Department") + 
  ylab("Number of products") +
  geom_bar(stat="identity",color='skyblue',fill='steelblue') + 
  ggtitle("Number of products by department") +
  coord_flip()

# 12. No. of products by aisle
products_by_dept = final_staging %>%
  group_by(aisle) %>%
  summarise(cnt = length(unique(product_id)))%>%
  top_n(10)

ggplot(data=products_by_dept, aes(x=reorder(aisle,cnt), y=cnt)) +
  geom_bar(stat="identity", color='skyblue',fill='steelblue') + 
  xlab("Aisle Category") + 
  ylab("Number of products") +
  ggtitle("Number of products by aisle") +
  coord_flip()

# 13. Heat-map DOW vs. HOD
hm_day_time = orders %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(cnt = length(unique(order_id)))

ggplot(data = hm_day_time, mapping = aes(x = order_dow,
                                       y = order_hour_of_day,
                                       fill = cnt)) +
  geom_tile() +
  xlab(label = "Sample")

# 14. Products in aisles in departments by frequency & corresponding reorders

# top_dept= final_staging%>%
#   group_by(department)%>%
#   summarise(cnt = length(product_name))


# top_aisles= final_staging%>%
#   group_by(department, aisle)%>%
#   summarise(cnt = length(product_name))%>%
#   group_by(department)%>%
#   mutate(arank = dense_rank(desc(cnt)))%>%
#   filter(arank<=3)

top_elements = final_staging%>%
  group_by(department, aisle)%>%
  mutate(cnt = length(product_name))%>%
  mutate(arank = dense_rank(desc(cnt)))%>%
  arrange(cnt)%>%
  filter(arank<=3)%>%
  group_by(department,aisle, arank, product_name)%>%
  summarise(pcnt = length(product_name),
            sumr = sum(reordered, na.rm = T),
            arank = max(arank, na.rm = T))%>%
  mutate(prank = rank(desc(pcnt)))%>%
  filter(prank<=3)

top_elements = arrange(top_elements,desc(pcnt))[1:5,]

# 15. Products ordered along with top products obtained in 14.
top_elements_affines = top_elements%>%
  group_by(product_name)%>%
  select(product_name)%>%
  left_join(unique.data.frame(final_staging[,c("order_id","product_name")]), by = c("product_name" = "product_name"))%>%
  top_n(5)%>%
  left_join(unique.data.frame(final_staging[,c("order_id","product_name")]), by = c("order_id" = "order_id"))


# 16. Standalone ordered products
standalone = final_staging%>%
  group_by(order_id)%>%
  mutate(maxitems = max(add_to_cart_order))%>%
  filter(maxitems==1)%>%
  group_by(product_name)%>%
  summarise(cnt = length(product_name))

standalone = arrange(standalone,desc(cnt))[1:5,]

standalone_ratio = standalone%>%
  group_by(product_name)%>%
  select(product_name)%>%
  left_join(unique.data.frame(final_staging[,c("order_id","product_name")]), by = c("product_name" = "product_name"))%>%
  group_by(product_name)%>%
  summarise(cnt_all = length(unique(order_id)))

standalone_ratio_all = left_join(standalone, standalone_ratio, by = "product_name")


# 17. Products ordered along with standalone products obtained in 16.
standalone_affines = standalone%>%
  group_by(product_name)%>%
  select(product_name)%>%
  left_join(unique.data.frame(final_staging[,c("order_id","product_name")]), by = c("product_name" = "product_name"))%>%
  top_n(5)%>%
  left_join(unique.data.frame(final_staging[,c("order_id","product_name")]), by = c("order_id" = "order_id"))


# 18. Periodically reordered products (7/30 days)
reordered_repititive = orders %>%
  group_by(days_since_prior_order,order_id) %>%
  select(days_since_prior_order,order_id)%>%
  filter(days_since_prior_order==7 | days_since_prior_order==30)

reordered_repititive_all = left_join(reordered_repititive, final_staging, by = "order_id")
reordered_repititive_prod = reordered_repititive_all%>%
  group_by(product_name)%>%
  summarise(cnt = length(unique(order_id)))
reordered_repititive_prod_top20 = reordered_repititive_prod[1:20,]
 colnames(orders)

 
 
 #########################
 ##### Associations ######
 #########################
 
# Creating transaction data - Aisle
basket_data = final_staging %>%
  group_by(order_id) %>%
  summarise(items=as.vector(list(aisle)))

set.seed(14)
transactions=as(basket_data$items, 'transactions')
head(transactions)

rules <- apriori(transactions, parameter = list(support=0.0001, confidence=0.8, maxlen= 5))
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:100])

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:10])
# table(inspect(rules@rhs))

plot(rules[1:20],method="graph",engine='interactive',shading=NA)


# Creating transaction data - Products
basket_data_prod = final_staging %>%
  group_by(order_id) %>%
  summarise(items=as.vector(list(product_name)))

set.seed(14)
transactions_prod=as(basket_data_prod$items, 'transactions')
head(transactions_prod)

rules_prod <- apriori(transactions_prod, parameter = list(support=0.0001, confidence=0.1, maxlen= 5))
rules_prod<-sort(rules_prod, by="lift", decreasing=TRUE)
rules_prod

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules_prod[1:10])
# table(inspect(rules_prod@rhs))

plot(rules_prod[1:20],method="graph",engine='interactive',shading=NA)

unique(final_staging$aisle)
