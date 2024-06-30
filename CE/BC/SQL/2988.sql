SELECT
  overview.name,
  overview.matches,
  overview.victories,
  overview.defeats,
  overview.draws,
  (3 * victories + draws) AS score
FROM (
  SELECT
    t.name,
    COUNT(t.id) AS matches,
    SUM(CASE
      WHEN (
        m.team_1 = t.id AND m.team_1_goals > m.team_2_goals
        OR
        m.team_2 = t.id AND m.team_2_goals > m.team_1_goals
      ) THEN 1 ELSE 0
    END) AS victories,
    SUM(CASE
      WHEN (
        m.team_1 = t.id AND m.team_1_goals < m.team_2_goals
        OR
        m.team_2 = t.id AND m.team_2_goals < m.team_1_goals
      ) THEN 1 ELSE 0
    END) AS defeats,
    SUM(CASE
      WHEN (
        m.team_1_goals = m.team_2_goals
      ) THEN 1 ELSE 0
    END) AS draws
  FROM matches m INNER JOIN teams t ON
    m.team_1 = t.id OR m.team_2 = t.id
  GROUP BY t.name, t.id
) AS overview
ORDER BY score DESC, name
