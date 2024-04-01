SELECT cand.name, CAST((((src.math * 2) + (src.specific * 3) + (src.project_plan * 5)) / 10) AS NUMERIC(15, 2)) AS avg
FROM candidate cand INNER JOIN score src ON 
  cand.id = src.candidate_id
ORDER BY avg DESC
