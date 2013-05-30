module OSPFkpsp11 where

import Prelude
import System.IO

-- Typendeklaration zur besseren Lesbarkeit der Funktionsdeklarationen

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

--hexStringToIntString :: String -> RouterId
---hexStringToInt string =

--- |Liest die Nachbarschaftstabelle aus einem File ein und gibt eine Liste mit Nachbarn und deren Eigenschaften zurück
--readNeighbourTable :: FilePath -> [(Address, Interface, State, Priority, Dead)]

--- |Liest ein LinkStateUpdate aus einem File ein und gibt die Topologie Tabelle (Graph) als Liste zurück
--readLinkStateUpdate :: FilePath -> Graph
---readLinkStateUpdate filePath = do
---	update <- readFile "linkstateupdates/linkstateupdate1.txt"

-- |Der graph wie er erstellt werden sollte
graphInput = [(1, [(2, 2), (3, 100), (4, 20)]),
			(2, [(5, 2), (6, 50), (1, 2)]),
			(3, [(4, 1), (6, 5), (1, 100)]),
			(4, [(1, 20), (3, 1), (6, 10)]),
			(5, [(2, 2), (6, 1)]),
			(6, [(4, 10), (3, 5), (2, 50), (5, 1)])]

-- |Das Resultat, das wir erwarten in unserem Anwendungsfall
resultExpected :: [ShortestPath]
resultExpected = [(1, 0, []),
					(2, 2, [1]),
					(3, 10, [1, 2, 5, 6]),
					(4, 11, [1, 2, 5, 6, 3]),
					(5, 4, [1, 2]),
					(6, 5, [1, 2, 5])]

-- |Initialisiere die Routes Tabelle anhand des Graphen. Die Metriken für jeden Pfad werden mit -1 initialisiert
setupRoute :: Graph -> [ShortestPath]
setupRoute ((a,_):graph) = (a,0,[]):[(fst g, -1, []) | g <- graph]

-- |Finde Route mit kleinster Metrik. -> Das zweite Argument ist ein Shortestpath, welcher möglich ist ((.....)) 
minRoute :: (RouterId, Metric) -> ShortestPath -> (RouterId, Metric)
minRoute (routeIdL, weightL) (routeIdR,weightR,_) = if weightL < weightR then (routeIdL, weightL) else (routeIdR, weightR)

-- |Gibt RouterId zurück, welcher noch nicht abgearbeitet ist und den aktuell kürzesten Pfad hat
nextNode :: Graph -> [ShortestPath] -> RouterId
nextNode graph routes = nextNodeId
	where
		graphIds = [nodeId | (nodeId,_) <- graph]
		possibleRoutes = [(routeId, weight, route) | (routeId, weight, route) <- routes, weight >= 0, elem routeId graphIds]
		nextNodeId = fst (foldl minRoute (999999, 10000) possibleRoutes)

updateShortestPaths :: [(RouterId, Distance)] -> ShortestPath -> [ShortestPath] -> [ShortestPath]
updateShortestPaths ((neighbourId, neibourDistance):ns) currentRoute routes = updateShortestPaths ns currentRoute updated
	where
		(currentNodeId, currentNodeMetrik, currentNodeRoute) = currentRoute
		updated = [if (metrik == -1 || currentNodeMetrik + neibourDistance < metrik) && neighbourId == nodeId
			then (nodeId, currentNodeMetrik + neibourDistance, currentNodeRoute ++ [currentNodeId])
			else (nodeId, metrik, route) | (nodeId, metrik, route) <- routes]
updateShortestPaths [] _ routes = routes

-- |Findet aktuellen kürzesten Pfad zu einem Router
currentRoute :: [ShortestPath] -> RouterId -> ShortestPath
currentRoute routes targetNode = head [(nodeId, metric, route) | (nodeId, metric, route) <- routes, nodeId == targetNode]

-- |Finde die Nachbarn im Graph für den aktuellen Router
neighboursOfCurrentRouter :: RouterId -> Graph -> [(RouterId, Distance)]
neighboursOfCurrentRouter routerId graph = head ([neighbours | (rid, neighbours) <- graph, rid == routerId])
-- neighboursOfCurrentRouter routerId graph = snd (head (filter (\(routerID, neighbours) -> routerID == routerId) graph))


-- |Berechung der kürzesten Pfade. Nimmt Topologie Tabelle als input Parameter und gibt eine Liste von Trippeln (ID, Metrik, Route) zurück
dijkstra :: Graph -> [ShortestPath] -> [ShortestPath]
dijkstra graph [] = dijkstra graph (setupRoute graph) -- einstieg: erstelle initiale Route tabledijkstra [] routes = routes -- basisfall: Routentabelle erstellt
dijkstra [] routes = routes -- basisfall: Routentabelle erstellt
dijkstra graph routes = dijkstra restGraph updateShortestPathss  --- 
	where
		nextNodeId = nextNode graph routes
		neighbours = neighboursOfCurrentRouter nextNodeId graph
		restGraph = [(nodeId, neighbours) | (nodeId, neighbours) <- graph, nodeId /= nextNodeId]			
		updateShortestPathss = updateShortestPaths neighbours (currentRoute routes nextNodeId) routes



main = do 
		print ((dijkstra graphInput []) == resultExpected)