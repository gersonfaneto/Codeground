SELECT lr.name, ROUND((lr.omega * 1.618), 3) AS "The N Factor"
FROM life_registry lr INNER JOIN dimensions dim ON
  lr.dimensions_id = dim.id
WHERE dim.name IN ('C875', 'C774') AND lr.name LIKE 'Richard%'
ORDER BY lr.omega ASC
