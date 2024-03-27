SELECT mov.id, mov.name
FROM movies mov INNER JOIN prices pr ON
  mov.id_prices = pr.id
WHERE LOWER(pr.categorie) = 'promotion';
