-- der graph wie er erstellt werden sollte
graphInput = [("a", [("b", 2), ("c", 7), ("d", 5)]),
			("b", [("e", 2), ("f", 6), ("a", 2)]),
			("c", [("d", 1), ("f", 3), ("a", 7)]),
			("d", [("a", 5), ("c", 1), ("f", 4)]),
			("e", [("b", 2), ("f", 1)]),
			("f", [("d", 4), ("c", 3), ("b", 6), ("e", 1)])]

-- Das Resultat, das wir erwarten
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

--- NodeId mit kleinstem Gewicht
--nextNode :: [([Char], t)] -> [([Char], Integer, t1)] -> [Char]
nextNode graph routes = nextNodeId
	where
		graphIds = [nodeId | (nodeId,_) <- graph] 
		possibleRoutes = [(routeId, weight, route) | (routeId, weight, route) <- routes, weight >= 0, elem routeId graphIds] 
		nextNodeId = fst (foldl minRoute ("routeId", 1000) possibleRoutes)

refreshRoute ((neighbourId, neibourDistance):ns) currentRoute routes = refreshRoute ns currentRoute updated
	where
		(currentNodeId, currentNodeMetrik, currentNodeRoute) = currentRoute		
		updated = [if (metrik == -1 || currentNodeMetrik + neibourDistance < metrik) && neighbourId == nodeId
			then (nodeId, currentNodeMetrik + neibourDistance, currentNodeRoute ++ [currentNodeId])
			else (nodeId, metrik, route) | (nodeId, metrik, route) <- routes]

refreshRoute [] _ routes = routes

dijkstra :: [([Char], [([Char], Integer)])] -> [([Char], Integer, [[Char]])] -> [([Char], Integer, [[Char]])] 
dijkstra graph [] = dijkstra graph (setupRoute graph) -- einstieg
dijkstra [] routes = routes -- basisfall
dijkstra graph routes = dijkstra restGraph refreshRoutes  ---dijkstra (takeFromGraph (nextNodeId graph routes)) routes -- rekursionsfall
	where
		nextNodeId = nextNode graph routes
		currentNode = head [(nodeId, neighbours) | (nodeId, neighbours) <- graph, nodeId == nextNodeId]
		restGraph = [(nodeId, neighbours) | (nodeId, neighbours) <- graph, nodeId /= nextNodeId]
		currentRoute = head [(nodeId, metrix, route) | (nodeId, metrix, route) <- routes, nodeId == nextNodeId]
		
		(_, neighbours) = currentNode
		refreshRoutes = refreshRoute neighbours currentRoute routes
		