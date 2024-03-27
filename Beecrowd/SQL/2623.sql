SELECT prod.name, cat.name
FROM products prod INNER JOIN categories cat ON
  prod.id_categories = cat.id
WHERE (prod.amount > 100) AND (cat.id IN (1, 2, 3, 6, 9))
ORDER BY cat.id ASC;
