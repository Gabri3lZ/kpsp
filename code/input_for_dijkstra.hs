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

setupRoute ((a,_):graph) = (a,0,[]):[(fst g, -1, []) | g <- graph]


dijkstra :: [([Char], [([Char], Integer)])] -> [([Char], Integer, [[Char]])] -> [([Char], Integer, [[Char]])] 
dijkstra graph [] = dijkstra graph (setupRoute graph) -- einstieg
dijkstra [] routes = routes -- basisfall
dijkstra graph routes = dijkstra [] routes -- rekursionsfall

