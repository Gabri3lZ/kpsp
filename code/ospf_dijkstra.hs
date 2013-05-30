import Prelude

-- Typendeklaration: Bessere lesbarkeit

type Address = [Char]
type Interface = [Char]
type Priority = Integer
type Dead = Integer
type RouterId = Integer
type Distance = Integer
type Metric = Integer
type Route = [RouterId]
type ShortestPath = (RouterId, Metric, Route)
type Graph = [(RouterId, [(RouterId, Distance)])]

-- Daten Deklarationen
data State = Down | Attempt | Init | TwoWay | ExStart | Exchange | Loading | Full



-- Längen und Offset Konstanten für die LinkStateUpdates in Anzahl Bytes
dataLinkFrameHeaderLength = 14

ipPacketHeaderLength = 20

ospfPacketHeaderLength = 24
messageTypeOffset = 1
messageTypeLength = 1
routerIdOffset = 4
routerIdLength = 4

lsuHeaderLength = 4
numberOfLsaOffset = 0
numberOfLsaLength = 4

lsaHeaderLength = 20
lsaTypeOffset = 3
lsaTypeLength = 1

lsaContentLength = 4
numberOfLinksOffset = 2;
numberOfLinksLength = 2

linkLength = 12
metricOffset = 10
metricLength = 2



-- Nimt einen Hex String entgegen und gibt eine Integer Räpresentation als String zurück
--hexStringToIntString :: [Char] -> [Char]
--hexStringToInt string =

-- Liest die Nachbarschaftstabelle aus einem File ein und gibt eine Liste mit Nachbarn und deren Eigenschaften zurück
--readNeighbourTable :: [Char] -> [([Char], [Char], [Char], Integer, Integer)]

-- Liest ein LinkStateUpdate aus einem File ein und gibt die Topologie Tabelle als Liste zurück
--readLinkStateUpdate :: [Char] -> [([Char], [([Char], Integer)]]
--readLinkStateUpdate filePath = do
--	update <- readFile "linkstateupdates/linkstateupdate1.txt"


-- der graph wie er erstellt werden sollte
graphInput = [(1, [(2, 2), (3, 100), (4, 20)]),
			(2, [(5, 2), (6, 50), (1, 2)]),
			(3, [(4, 1), (6, 5), (1, 100)]),
			(4, [(1, 20), (3, 1), (6, 10)]),
			(5, [(2, 2), (6, 1)]),
			(6, [(4, 10), (3, 5), (2, 50), (5, 1)])]

-- Das Resultat, das wir erwarten
resultExpected :: [(RouterId, Metric, Route)]
resultExpected = [(1, 0, []),
					(2, 2, [1]),
					(3, 10, [1, 2, 5, 6]),
					(4, 11, [1, 2, 5, 6, 3]),
					(5, 4, [1, 2]),
					(6, 5, [1, 2, 5])]

-- Initialisiere die Routes Tabelle anhand des Graphen
setupRoute ((a,_):graph) = (a,0,[]):[(fst g, -1, []) | g <- graph]

-- Route mit kleinster Metrik
minRoute (routeIdL, weightL) (routeIdR,weightR,_) = if weightL < weightR then (routeIdL, weightL) else (routeIdR, weightR)

-- NodeId mit kleinstem Gewicht
nextNode :: Graph -> [ShortestPath] -> RouterId
nextNode graph routes = nextNodeId
	where
		graphIds = [nodeId | (nodeId,_) <- graph]
		possibleRoutes = [(routeId, weight, route) | (routeId, weight, route) <- routes, weight >= 0, elem routeId graphIds]
		nextNodeId = fst (foldl minRoute (99, 10000) possibleRoutes)


refreshRoute ((neighbourId, neibourDistance):ns) currentRoute routes = refreshRoute ns currentRoute updated
	where
		(currentNodeId, currentNodeMetrik, currentNodeRoute) = currentRoute
		updated = [if (metrik == -1 || currentNodeMetrik + neibourDistance < metrik) && neighbourId == nodeId
			then (nodeId, currentNodeMetrik + neibourDistance, currentNodeRoute ++ [currentNodeId])
			else (nodeId, metrik, route) | (nodeId, metrik, route) <- routes]
refreshRoute [] _ routes = routes

-- find current route to a node
currentRoute routes targetNode = head [(nodeId, metric, route) | (nodeId, metric, route) <- routes, nodeId == targetNode]


-- Berechung der kürzesten Pfade. Nimt Topologie Tabelle als input Parameter und gibt eine Liste von Trippeln (ID, Metrik, Route) zurück
dijkstra :: [(RouterId, [(RouterId, Distance)])] -> [ShortestPath] -> [ShortestPath]
dijkstra graph [] = dijkstra graph (setupRoute graph) -- einstieg: erstelle initiale Route tabledijkstra [] routes = routes -- basisfall: Routentabelle erstellt
dijkstra [] routes = routes -- basisfall: Routentabelle erstellt
dijkstra graph routes = dijkstra restGraph refreshRoutes  --- 
	where
		nextNodeId = nextNode graph routes
		currentNode = head [(nodeId, neighbours) | (nodeId, neighbours) <- graph, nodeId == nextNodeId]
		restGraph = [(nodeId, neighbours) | (nodeId, neighbours) <- graph, nodeId /= nextNodeId]			
		(_, neighbours) = currentNode
		refreshRoutes = refreshRoute neighbours (currentRoute routes nextNodeId) routes

