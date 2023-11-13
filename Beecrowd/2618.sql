SELECT prod.name, prov.name, cat.name
FROM
  products prod
    INNER JOIN providers prov ON
      prod.id_providers = prov.id
    INNER JOIN categories cat ON
      prod.id_categories = cat.id
WHERE prov.name = 'Sansul SA' AND cat.name = 'Imported';
