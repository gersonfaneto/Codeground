SELECT cust.name, ord.id
FROM customers cust INNER JOIN orders ord ON
  cust.id = ord.id_customers
WHERE ord.orders_date BETWEEN '2016-01-01' AND '2016-06-30';
