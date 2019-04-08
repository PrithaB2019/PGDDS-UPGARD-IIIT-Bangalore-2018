# Set 'ONLY_FULL_GROUP_BY' MODE

set global sql_mode = 'only_full_group_by';
select @@global.sql_mode;


#Task 0:#
#Download the data files for this assignment. Your first task is to create tables from these files. #

#Task 1: Understanding the data in hand 

#A. Describe the data in hand in your own words. (Word Limit is 500) 

/*The superstoredb is a simple database with five tables having numerous data 
about its customers, products, orders, shipping and sales separately in five different tables 
namely ‘cust_dimen’, ‘prod_dimen’, ‘order_dimen’, ‘shipping_dimen’ and ‘market_fact’ respectively.

The table cust_dimen has 1833 records with 5 columns about the superstore customers. 
Name, Region, province, customer_segment, cust_id are all Text Type data. 
Each record has unique cust_id which can be found in market_fact table also. 
From the data it can be observed that maximum customers belong to WEST Region. 
Customers can be typically categorized as consumers, corporate, home office, and small business. 
There are no null values in this table.

The table order_dimen has 5506 records with 4 columns about the superstore orders for four years (2009-2012). 
Order_id is INT(11) type data whereas order_date, order_priority and ord_id are all TEXT Type data. 
Each record has unique ord_id which can be found in market_fact table also. 
From the data, it can be observed that superstore has prioritized its order as critical, high, medium and low but not always specified. 
It shall be noted here that same order_id can have different order_priority but unique ord_id. 
It can be further concluded that the superstore has been consistent in receiving (Total_number_of) orders through the four years. 
There are no null values in this table. 
The shipping details against each order_id can be retrieved from shipping_dimen table.

The table prod_dimen has 17 records with 3 columns about the superstore products. 
Product_category, product_subcategory and product_id are all TEXTType data. 
Each record has unique prod_id which can be found in market_fact table also. 
From the data it can be observed that superstore has mainly three types of products namely office supplies, furniture and technology; 
office supplies being the maximum variety (9/ 17). 
There are no null values in this table.

The table shipping_dimen has 7701 records with 4 columns about the orders’ shipping. 
Order_id is INT(11) type data whereas Ship_mode, Ship_date, and ship_id are all TEXT Type data. 
Each record has unique ship_id which can be found in market_fact table also. 
The order_id can be found in order_dimen table too. 
After going through the data in the table, it can be noted that superstore has mainly three modes of shipping – 
delivery truck, express air and regular air; regular air being most frequently used. 
There are no null values in this table. 

The market_fact table has 8399 records with 10 columns. 
It includes all the unique fields of the rest of the four tables. i.e cust_id, ord_id, Prod_id, Ship_id 
which are Text Type data. 
Other information includes sales, discounts, profit and shipping_cost which are double type data. 
order_quantity is an INT(11) type data whereas product base margin is text type. 
Market_fact table can be linked to all other tables on the individual table unique key field.
Certain observations from the four years data are: Profit is nearly 9-10% of sales for every year consistently. 
Order_quantity for every year is unfailing 50-55K.
It has null values.
*/

#B. Identify and list the Primary Keys and Foreign Keys for this dataset 
#(Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.) 

/*cust_dimen: primary key= cust_id, no foreign key;
orders_dimen: primary key= ord_id, no foreign key;
prod_dimen: primary key= prod_id, no foreign key;
shipping_dimen: primary key= ship_id, no foreign key;
market_fact: primary key= no primary key, foreign key: cust_id, ord_id, prod_id, ship_id;
*/
 
#Task 2: Basic Analysis 

#Write the SQL queries for the following:  

#A. Find the total and the average sales (display total_sales and avg_sales) 

select sum(sales) as 'TOTAL_SALES', avg(sales) as'AVERAGE_SALES'
from market_fact;

 
#B. Display the number of customers in each region in decreasing order of no_of_customers. 
#The result should contain columns Region, no_of_customers 

select Region as 'REGION', count(cust_id) as 'NO_OF_CUSTOMERS'
from cust_dimen
group by region
order by count(Cust_id) desc;


#C. Find the region having maximum customers (display the region name and max(no_of_customers) 

select region as 'REGION', count(Cust_id) as 'MAX(NO_OF_CUSTOMERS)'
from cust_dimen
group by region
order by count(Cust_id) desc
limit 1;


#D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold) 

select prod_id as 'PRODUCT_ID', sum(Order_Quantity) as 'NO_OF_PRODUCTS_SOLD'
from market_fact
group by Prod_id
order by sum(Order_Quantity) desc;


#E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased 
#(display the customer name, no_of_tables purchased)

select cust_dimen.Customer_Name as 'CUSTOMER NAME', sum(market_fact.Order_Quantity) as 'NO_OF_TABLES_PURCHASED'
from cust_dimen inner join market_fact on cust_dimen.Cust_id = market_fact.Cust_id 
inner join prod_dimen on market_fact.Prod_id = prod_dimen.Prod_id
WHERE REGION = 'atlantic' and Product_Sub_Category = 'tables'
group by Customer_Name
order by sum(Order_Quantity) desc;

#Task 3: Advanced Analysis 

#Write sql queries for the following:
 
#A. Display the product categories in descending order of profits 
#(display the product category wise profits i.e. product_category, profits)? 

select prod_dimen.product_category as 'PRODUCT_CATEGORY', round(sum(market_fact.profit),2) as 'PROFITS'
from prod_dimen inner join market_fact on prod_dimen.Prod_id = market_fact.Prod_id
group by Product_Category
order by round(sum(profit),2) desc;

#B. Display the product category, product sub-category and the profit within each subcategory in three columns.  

select prod_dimen.Product_Category as 'PRODUCT_CATEGORY', prod_dimen.Product_Sub_Category as 'PRODUCT_SUB_CATEGORY', round(sum(market_fact.profit),2) as 'TOTAL_PROFIT'
from prod_dimen inner join market_fact on prod_dimen.Prod_id = market_fact.Prod_id
group by prod_dimen.Product_Sub_Category, prod_dimen.Product_Category
order by round(sum(market_fact.Profit),2) desc;


#C. Where is the least profitable product subcategory shipped the most? 
#For the least profitable product sub-category, display the  region-wise no_of_shipments and the profit 
#made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region) 
#o Note: You can hardcode the name of the least profitable product subcategory 
 
 select cust_dimen.region as 'REGION', sum(market_fact.profit) as 'PROFIT_IN_EACH_REGION', 
 count(market_fact.ship_id) as 'NUMBER_OF_SHIPMENTS'
 from cust_dimen inner join market_fact on cust_dimen.Cust_id = market_fact.Cust_id
 inner join prod_dimen on market_fact.Prod_id = prod_dimen.Prod_id
 where prod_dimen.Product_Sub_Category = 'tables' 
/*from the output of task 3:part B, it can be observed that Tables is the least profitable product sub category*/
 group by cust_dimen.region
 order by sum(market_fact.profit) desc;
 
 # where is the least profitable product shipped the most?
 
 select cust_dimen.region as 'REGION', 
 count(market_fact.ship_id) as 'MAX_NUMBER_OF_SHIPMENTS'
 from cust_dimen inner join market_fact on cust_dimen.Cust_id = market_fact.Cust_id
 inner join prod_dimen on market_fact.Prod_id = prod_dimen.Prod_id
 where prod_dimen.Product_Sub_Category = 'tables'
 group by cust_dimen.region
 order by count(market_fact.ship_id) desc
 limit 1;
 