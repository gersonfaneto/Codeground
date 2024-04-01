SELECT cat.name, SUM(prod.amount)
FROM categories cat INNER JOIN products prod ON
  cat.id = prod.id_categories 
GROUP BY cat.name;
