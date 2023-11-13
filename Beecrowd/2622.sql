SELECT cust.name
FROM customers cust INNER JOIN legal_person lp ON
  cust.id = lp.id_customers;
