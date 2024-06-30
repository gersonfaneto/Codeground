SELECT prod.name, prov.name, prod.price
FROM
  products prod
    INNER JOIN providers prov ON
      prod.id_providers = prov.id
    INNER JOIN categories cat ON
      prod.id_categories = cat.id
WHERE cat.name = 'Super Luxury' AND prod.price > 1000;
