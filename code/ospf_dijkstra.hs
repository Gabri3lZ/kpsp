-- der graph wie er erstellt werden sollte
graphInput = [("a", [("b", 2), ("c", 7), ("d", 5)]),
			("b", [("e", 2), ("f", 6), ("a", 2)]),
			("c", [("d", 1), ("f", 3), ("a", 7)]),
			("d", [("a", 5), ("c", 1), ("f", 4)]),
			("e", [("b", 2), ("f", 1)]),
			("f", [("d", 4), ("c", 3), ("b", 6), ("e", 1)])]

-- zum testen
resultExpected = [("a", 0, []),
					("b", 2, ["a"]),
					("c", 6, ["a", "d"]),
					("d", 5, ["a"]),
					("e", 4, ["a", "b"]),
					("f", 5, ["a", "b", "e"])]

-- Initialisiere die Routes Tabelle anhand des Graphen
setupRoute ((a,_):graph) = (a,0,[]):[(fst g, -1, []) | g <- graph]

-- Route mit kleinster Metrik
minRoute (routeIdL, weightL) (routeIdR,weightR,_) = if weightL < weightR then (routeIdL, weightL) else (routeIdR, weightR)

--- Node mit kleinstem Gewicht
nextNode :: [([Char], t)] -> [([Char], Int, t1)] -> [Char]
nextNode graph routes = nextNodeId
	where
		graphIds = [nodeId | (nodeId,_) <- graph] 
		possibleRoutes = [(routeId, weight, route) | (routeId, weight, route) <- routes, weight >= 0, elem routeId graphIds] 
		nextNodeId = fst (foldl minRoute ("routeId", 1000) possibleRoutes)


dijkstra :: [([Char], [([Char], Integer)])] -> [([Char], Integer, [[Char]])] -> [([Char], Integer, [[Char]])] 
dijkstra graph [] = dijkstra graph (setupRoute graph) -- einstieg
dijkstra [] routes = routes -- basisfall
dijkstra graph routes = dijkstra (takeFromGraph (nextNodeId graph routes)) routes -- rekursionsfall