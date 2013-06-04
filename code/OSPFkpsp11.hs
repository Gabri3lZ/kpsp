module OSPFkpsp11 where

import Prelude
import System.IO
import Data.Char

-- Typendeklaration zur besseren Lesbarkeit der Funktionsdeklarationen
-- |IP-Adresse eines Routers
type Address = String
-- |Das Interface des Routers
type Interface = String
-- |Priorität des Protokoll
type Priority = Int
-- |Timeout bis Route erneuert werden muss
type Dead = Int
-- |ID des Routers gemäss dem OSPF-Protokoll
type RouterId = String
-- |Die Distanz zwischen zwei Routern
type Distance = Int
-- |Das Gewicht des nach OSPF gewichteten Pfades (Aufsummierung der Distanzen auf Pfad)
type Metric = Int
-- |Liste mit den Hops, welche auf dem Pfad gemacht werden müssen
type Route = [RouterId]
-- |Beschreibt einen Kind Router bezogen zu seinem Eltern Router
type ChildNode = (RouterId, Distance)
-- |Beschreibung eines kürzesten Pfad zu einer Route
type ShortestPath = (RouterId, Metric, Route)
-- |Der Graph beschreibt die Netzwerktopologie
type Graph = [(RouterId, [ChildNode])]

-- Daten Deklarationen
-- |Status der Verbindung gemäss OSPF
data State = Down | Attempt | Init | TwoWay | ExStart | Exchange | Loading | Full deriving(Read, Show, Eq)


---------------------------------------------------------------------------------------------------------
-- KONSTANTENDEKLARATION

-- Längen und Offset Konstanten für die LinkStateUpdates in Anzahl Bytes
-- |Header Länge (in Byte) des Data Link Frames
dataLinkFrameHeaderLength = 14

-- |Header Länge (in Byte) des IP Packet
ipPacketHeaderLength = 20

-- |Header Länge (in Byte) des OSPF Packet
ospfPacketHeaderLength = 24
-- |Offset (in Byte) vom Anfang des OSPF Packet bis zum Message Type
messageTypeOffset = 1
-- |Länge (in Byte) des Message Types
messageTypeLength = 1
-- |Offset (in Byte) vom Anfang des OSPF Packet bis zur Router ID
routerIdOffset = 4
-- |Länge (in Byte) der Router ID
routerIdLength = 4


-- |Header Länge (in Byte) des Link State Update Packet
lsuHeaderLength = 4
-- |Offset (in Byte) vom Anfang des Link State Update Headers bis zur Anzahl Link State Advertisements
numberOfLsaOffset = 0
-- |Länge (in Byte) der Anzahl Link State Advertisements
numberOfLsaLength = 4

-- |Header Länge (in Byte) des Link State Advertisement Packet
lsaHeaderLength = 20
-- |Offset (in Byte) vom Anfang des Link State Advertisement Headers bis zum LSA-Type
lsaTypeOffset = 3
-- |Länge (in Byte) des LSA-Type
lsaTypeLength = 1
-- |Offset (in Byte) vom Anfang des Link State Advertisement Headers bis zur Link State Advertisement Länge
lsaLengthOffset = 18
-- |Länge (in Byte) der Link State Advertisement Länge
lsaLengthLength = 2

-- |Content Länge (in Byte) des Link State Advertisement Packet (ohne die einzelnen Links)
lsaContentLength = 4
-- |Offset (in Byte) vom Anfang des Link State Advertisement Contents bis zu den Anzahl Links
numberOfLinksOffset = 2
-- |Länge (in Byte) der Anzahl Links
numberOfLinksLength = 2

-- |Länge (in Bytes) eines Links
linkLength = 12
-- |Offset (in Byte) vom Anfang des Links bis zur ID des Child Routers
childRouterIdOffset = 0
-- |Länge (in Byte) der Child Router ID
childRouterIdLength = 4
-- |Offset (in Byte) vom Anfang des Links bis zur Metrik
metricOffset = 10
-- |Länge (in Byte) der Metrik
metricLength = 2

-- |Datei Pfade zu allen Link State Update Files die verwendet werden sollen
lsuFilePaths :: [FilePath]
lsuFilePaths = ["data/lsu1.txt", "data/lsu2.txt", "data/lsu3.txt", "data/lsu4.txt", "data/lsu5.txt"]


---------------------------------------------------------------------------------------------------------
-- Funktionen für das Parsen und Auswerten von Link State Update Files

-- |Nimt einen Hex-String entgegen und parst diesen in eine IP-Adresse
-- |Parameter 1: Hex-String (IP Adresse)
hexToIp :: String -> String
hexToIp (x:y:[]) = show ((digitToInt x) * 16 + digitToInt y)
hexToIp (x:y:zs) = show ((digitToInt x) * 16 + digitToInt y) ++ "." ++ hexToIp zs

-- |Nimt einen Hex-String entgegen und parst diesen in einen Int
-- |Parameter 1: Hex-String
hexToInt :: String -> Int
hexToInt [] = 0
hexToInt (x:xs) = (digitToInt x) * 16 ^ (length xs) + hexToInt xs

-- |Extrahiert aus einem Link State Update (Hex-String) den Message Type (4 für Link State Update)
-- |Parameter 1: Hex-String (Link State Update)
extractMessageType :: String -> Int
extractMessageType lsu = hexToInt (take (messageTypeLength*2)
	(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + messageTypeOffset*2) lsu))

-- |Extrahiert aus einem Link State Update (Hex-String) die Router Id des Senders
-- |Parameter 1: Hex-String (Link State Update)
extractRouterId :: String -> RouterId
extractRouterId lsu = hexToIp (take (routerIdLength*2)
	(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + routerIdOffset*2) lsu))

-- |Extrahiert aus einem Link State Update (Hex-String) die Anzahl Link State Advertisements, die in diesem Update enthalten sind
-- |Parameter 1: Hex-String (Link State Update)
extractNumberOfLsa :: String -> Int
extractNumberOfLsa lsu = hexToInt (take (numberOfLsaLength*2)
	(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + numberOfLsaOffset*2) lsu))

-- |Extrahiert aus einem Link State Update (Hex-String) die Längen der einzelnen Link State Advertisements
-- |Parameter 1: Hex-String (Link State Update)
-- |Parameter 2: Anzahl Link State Advertisements in diesem Link State Update
-- |Parameter 3: Aktuelles Link State Advertisement (am Anfang 1)
-- |Parameter 4: Akkumulator Liste für LSA-Lengths (Am Anfang leer)
extractLsaLengths :: String -> Int -> Int -> [Int] -> [Int]
extractLsaLengths lsu numOfLsa nthLsa lengths =
	if (numOfLsa == nthLsa)
		-- man ist beim letzten LSA angelangt
		-- letzten LSA noch analysieren und "LsaLength" hinten an die Liste anhängen
		-- Rekursion wird beendet
		then
			lengths ++ [hexToInt (take (lsaLengthLength*2)
			(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
			(sum lengths)*2 + lsaLengthOffset*2) lsu))]
		-- man ist noch nicht beim letzten LSA angelangt
		-- LSA analysieren und "LsaLength" hinten an die Liste anhängen
		-- rekursiver Aufruf mit neuer Liste und nächstem LSA als Parameter
		else
			extractLsaLengths lsu numOfLsa (nthLsa+1)
			(lengths ++ [hexToInt (take (lsaLengthLength*2)
			(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
			(sum lengths)*2 + lsaLengthOffset*2) lsu))])

-- |Extrahiert aus einem Link State Update (Hex-String) die LSA Types der einzelnen Link State Advertisements (1 für Router-LSA)
-- |Parameter 1: Hex-String (Link State Update)
-- |Parameter 2: Anzahl Link State Advertisements in diesem Link State Update
-- |Parameter 3: Aktuelles Link State Advertisement (am Anfang 1)
-- |Parameter 4: Liste mit allen Längen der einzelnen Link State Advertisements
-- |Parameter 5: Akkumulator Liste für LSA-Types (Am Anfang leer)
extractLsaTypes :: String -> Int -> Int -> [Int] -> [Int] -> [Int]
extractLsaTypes lsu numOfLsa nthLsa lengths types =
	if (numOfLsa == nthLsa)
		-- man ist beim letzten LSA angelangt
		-- letzten LSA noch analysieren und "LsaType" hinten an die Liste anhängen
		-- Rekursion wird beendet
		then
			types ++ [hexToInt (take (lsaTypeLength*2)
			(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
			(sum (take (nthLsa-1) lengths))*2 + lsaTypeOffset*2) lsu))]
		-- man ist noch nicht beim letzten LSA angelangt
		-- LSA analysieren und "LsaType" hinten an die Liste anhängen
		-- rekursiver Aufruf mit neuer Liste und nächstem LSA als Parameter
		else
			extractLsaTypes lsu numOfLsa (nthLsa+1) lengths
			(types ++ [hexToInt (take (lsaTypeLength*2)
			(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
			(sum (take (nthLsa-1) lengths))*2 + lsaTypeOffset*2) lsu))])

-- |Extrahiert aus einem Link State Update (Hex-String) die Anzahl Links, die in diesem Link State Advertisement enthalten sind
-- |Parameter 1: Hex-String (Link State Update)
-- |Parameter 2: Anzahl Link State Advertisements in diesem Link State Update
-- |Parameter 3: Aktuelles Link State Advertisement (am Anfang 1)
-- |Parameter 4: Liste mit allen Längen der einzelnen Link State Advertisements
-- |Parameter 5: Liste mit allen LSA-Typen der einzelnen Link State Advertisements
-- |Parameter 6: Akkumulator Liste für NumberOfLinks (Am Anfang leer)
extractNumbersOfLinks :: String -> Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
extractNumbersOfLinks lsu numOfLsa nthLsa lengths types numOfLinks =
	if (types!!(nthLsa-1) == 1)
		-- Nur Router-LSAs können interpretiert werden (1 = Router-LSA)
		then
			if (numOfLsa == nthLsa)
				-- man ist beim letzten LSA angelangt
				-- letzten LSA noch analysieren und "NumberOfLinks" hinten an die Liste anhängen
				-- Rekursion wird beendet
				then
					numOfLinks ++ [hexToInt (take (numberOfLinksLength*2)
					(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
					(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + numberOfLinksOffset*2) lsu))]
				-- man ist noch nicht beim letzten LSA angelangt
				-- LSA analysieren und "NumberOfLinks" hinten an die Liste anhängen
				-- rekursiver Aufruf mit neuer Liste und nächstem LSA als Parameter
				else
					extractNumbersOfLinks lsu numOfLsa (nthLsa+1) lengths types
					(numOfLinks ++ [hexToInt (take (numberOfLinksLength*2)
					(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
					(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + numberOfLinksOffset*2) lsu))])
		-- LSA ist kein Router-LSA
		else
			if (numOfLsa == nthLsa)
				-- man ist beim letzten LSA angelangt
				-- da aktuelles LSA kein Router-LSA ist, muss nichts mehr gemacht werden
				-- Rekursion wird beendet
				then numOfLinks
				-- man ist noch nicht beim letzten LSA angelangt
				-- rekursiver Aufruf mit nächstem LSA als Parameter
				else extractNumbersOfLinks lsu numOfLsa (nthLsa+1) lengths types numOfLinks

-- |Extrahiert aus einem Link State Update (Hex-String) die Router Id und entsprechenden Distanzen der Child Router
-- |Parameter 1: Hex-String (Link State Update)
-- |Parameter 2: Anzahl Link State Advertisements in diesem Link State Update
-- |Parameter 3: Aktuelles Link State Advertisement (am Anfang 1)
-- |Parameter 4: Liste mit allen Längen der einzelnen Link State Advertisements
-- |Parameter 5: Liste mit allen LSA-Typen der einzelnen Link State Advertisements
-- |Parameter 6: Liste mit den Anzahl Links pro Link State Advertisement (nur für Router-LSA)
-- |Parameter 7: Aktueller Link (am Anfang 1)
-- |Parameter 8: Akkumulator Liste für ChildNodes (Am Anfang leer)
extractChildNodes :: String -> Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> [ChildNode] -> [ChildNode]
extractChildNodes lsu numOfLsa nthLsa lengths types (n:numOfLinks) nthLink childNodes =
	if (types!!(nthLsa-1) == 1)
		-- Nur Router-LSAs können interpretiert werden (1 = Router-LSA)
		then
			if (numOfLsa == nthLsa)
				-- man ist beim letzten LSA angelangt
				then
					if (n == nthLink)
						-- man ist beim letzten Link angelengt
						-- letzten Link noch analysieren und ChildNode hinten an die Liste anhängen
						-- Rekursion wird beendet
						then
							childNodes ++ [(hexToIp (take (childRouterIdLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + childRouterIdOffset*2) lsu)),
							hexToInt (take (metricLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + metricOffset*2) lsu)))]
						-- man ist noch nicht beim letzten Link angelangt
						-- Link analysieren und ChildNode hinten an die Liste anhängen
						-- rekursiver Aufruf mit neuer Liste, dem selben LSA und nächsem Link als Parameter
						else
							extractChildNodes lsu numOfLsa nthLsa lengths types (n:numOfLinks) (nthLink+1)
							(childNodes ++ [(hexToIp (take (childRouterIdLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + childRouterIdOffset*2) lsu)),
							hexToInt (take (metricLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + metricOffset*2) lsu)))])
				-- man ist noch nicht beim letzten LSA angelangt
				else
					if (n == nthLink)
						-- man ist beim letzten Link angelengt
						-- letzten Link noch analysieren und ChildNode hinten an die Liste anhängen
						-- rekursiver Aufruf mit neuer Liste, nächstem LSA und erstem Link als Parameter
						then
							extractChildNodes lsu numOfLsa (nthLsa+1) lengths types numOfLinks 1
							(childNodes ++ [(hexToIp (take (childRouterIdLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + childRouterIdOffset*2) lsu)),
							hexToInt (take (metricLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + metricOffset*2) lsu)))])
						-- man ist noch nicht beim letzten Link angelangt
						-- Link analysieren und ChildNode hinten an die Liste anhängen
						-- rekursiver Aufruf mit neuer Liste, dem selben LSA und nächstem Link als Parameter
						else
							extractChildNodes lsu numOfLsa nthLsa lengths types (n:numOfLinks) (nthLink+1)
							(childNodes ++ [(hexToIp (take (childRouterIdLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + childRouterIdOffset*2) lsu)),
							hexToInt (take (metricLength*2)
							(drop (dataLinkFrameHeaderLength*2 + ipPacketHeaderLength*2 + ospfPacketHeaderLength*2 + lsuHeaderLength*2 +
							(sum (take (nthLsa-1) lengths))*2 + lsaHeaderLength*2 + lsaContentLength*2 + (nthLink-1)*linkLength*2 + metricOffset*2) lsu)))])
		-- LSA ist kein Router-LSA
		else
			if (numOfLsa == nthLsa)
				-- man ist beim letzten LSA angelangt
				-- da aktuelles LSA kein Router-LSA ist, muss nichts mehr gemacht werden
				-- Rekursion wird beendet
				then childNodes
				-- man ist noch nicht beim letzten LSA angelangt
				-- rekursiver Aufruf mit nächstem LSA und erstem Link als Parameter
				else extractChildNodes lsu numOfLsa (nthLsa+1) lengths types (n:numOfLinks) 1 childNodes
-- es sind keine Links mehr vorhanden die analysiert werden könnten
-- Rekursion wird beendet
extractChildNodes lsu numOfLsa nthLsa lengths types [] nthLink childNodes = childNodes

-- |Liest ein LinkStateUpdate aus einem File ein und gibt die Topologie Tabelle (Graph) als Liste zurück
-- |Parameter 1: Datei Pfad zum Link State Update File das eingelesen werden soll
readLinkStateUpdate :: FilePath -> IO (RouterId, [ChildNode])
readLinkStateUpdate filePath = do
	update <- readFile filePath
	let upperUpdate = map toUpper update
	let messageType = extractMessageType upperUpdate
	let routerId = if (messageType == 4)
		then extractRouterId upperUpdate
		else "0.0.0.0"
	let numberOfLsa = if (messageType == 4)
		then extractNumberOfLsa upperUpdate
		else 0
	let lsaLengths = if (numberOfLsa > 0)
		then extractLsaLengths upperUpdate numberOfLsa 1 []
		else []
	let lsaTypes = if (numberOfLsa > 0)
		then extractLsaTypes upperUpdate numberOfLsa 1 lsaLengths []
		else []
	let numbersOfLinks = if (lsaTypes /= [])
		then extractNumbersOfLinks upperUpdate numberOfLsa 1 lsaLengths lsaTypes []
		else []
	let childNodes = if (numbersOfLinks /= [])
		then extractChildNodes upperUpdate numberOfLsa 1 lsaLengths lsaTypes numbersOfLinks 1 []
		else []
	return (routerId, childNodes)

-- |Liest alle definierten Link State Updates ein und baut daraus den Topology Graph
buildTopoGraph :: IO [(RouterId, [ChildNode])]
buildTopoGraph = do
	lsu1 <- readLinkStateUpdate "data/lsu1.txt"
	lsu2 <- readLinkStateUpdate "data/lsu2.txt"
	lsu3 <- readLinkStateUpdate "data/lsu3.txt"
	lsu4 <- readLinkStateUpdate "data/lsu4.txt"
	lsu5 <- readLinkStateUpdate "data/lsu5.txt"
	lsu6 <- readLinkStateUpdate "data/lsu6.txt"
	let graph = [lsu1, lsu2, lsu3, lsu4, lsu5, lsu6]
	return graph

---------------------------------------------------------------------------------------------------------
-- Funktionen für den Shortest Path Algorithmus

-- |Initialisiere die Routes Tabelle anhand des Graphen. Die Metriken für jeden Pfad werden mit -1 initialisiert
setupRoute :: Graph -> [ShortestPath]
setupRoute ((a,_):graph) = (a,0,[]):[(fst g, -1, []) | g <- graph]


-- |Gibt RouterId zurück, welcher noch nicht abgearbeitet ist und den aktuell kürzesten Pfad hat
nextRouterId :: Graph -> [ShortestPath] -> RouterId
nextRouterId graph shortestpaths = nextRId
	where
		graphIds = [nodeId | (nodeId,_) <- graph]
		possibleRoutes = [(routeId, metric, route) | (routeId, metric, route) <- shortestpaths, metric >= 0, elem routeId graphIds]
		nextRId = fst (foldl sortShortestPath ("255.255.255.255", 10000) possibleRoutes)

-- |Vergleicht ein (RouterId, Metric)-Tupel mit einem ShortestPath und gibt das Element mit kleinerer Metrik zurück, als (RouterId, Metric)
-- |Wird verwendet um den Router mit dem aktuell kürzesten Pfad zu finden
sortShortestPath :: (RouterId, Metric) -> ShortestPath -> (RouterId, Metric)
sortShortestPath (routeIdL, metricL) (routeIdR,metricR,_) = if metricL < metricR then (routeIdL, metricL) else (routeIdR, metricR)


-- |Aktualisiert die Liste der kürzesten Pfade anhand des aktuell ausgewählten Router.
-- |Das erste Argument beschreibt die Distanz zu den Nachbarn des Routers
-- |Das zweite Argument ist der kürzeste Pfad zum aktuellen Router
-- |Das letzte Argument ist die Liste der aktuell kürzesten Pfade.
-- |Die Rückgabe ist die aktualisierte Shortestpath-Liste
updateShortestPaths :: [ChildNode] -> ShortestPath -> [ShortestPath] -> [ShortestPath]
updateShortestPaths ((neighbourId, neibourDistance):ns) currentRoute routes = updateShortestPaths ns currentRoute updated
	where
		(currentNodeId, currentNodeMetrik, currentNodeRoute) = currentRoute
		updated = [if (metrik == -1 || currentNodeMetrik + neibourDistance < metrik) && neighbourId == nodeId
			then (nodeId, currentNodeMetrik + neibourDistance, currentNodeRoute ++ [currentNodeId])
			else (nodeId, metrik, route) | (nodeId, metrik, route) <- routes]
updateShortestPaths [] _ routes = routes

-- |Gibt den aktuellen ShortestPath zu einem Router aus der Liste zurück
currentRoute :: [ShortestPath] -> RouterId -> ShortestPath
currentRoute routes targetNode = head [(nodeId, metric, route) | (nodeId, metric, route) <- routes, nodeId == targetNode]

-- |Finde die Nachbarn im Graph für den aktuellen Router
neighboursOfCurrentRouter :: RouterId -> Graph -> [ChildNode]
neighboursOfCurrentRouter routerId graph = head ([neighbours | (rid, neighbours) <- graph, rid == routerId])
-- neighboursOfCurrentRouter routerId graph = snd (head (filter (\(routerID, neighbours) -> routerID == routerId) graph))

-- |Berechung der kürzesten Pfade. Nimmt Topologie Tabelle als input Parameter und gibt eine Liste von Trippeln (ID, Metrik, Route) zurück
dijkstra :: Graph -> [ShortestPath] -> [ShortestPath]
dijkstra graph [] = dijkstra graph (setupRoute graph) -- einstieg: erstelle initiale Route tabledijkstra [] routes = routes -- basisfall: Routentabelle erstellt
dijkstra [] routes = routes -- basisfall: Routentabelle erstellt
dijkstra graph routes = dijkstra restGraph shortestPaths  --- iteration: Wende Algorithmus für jeden Router im Graphen an
	where
		nRid = nextRouterId graph routes -- id des nächsten zu bearbeitenden Routers
		neighbours = neighboursOfCurrentRouter nRid graph -- Die Nachbarn des nächsten Routers
		restGraph = [(routerId, neighbours) | (routerId, neighbours) <- graph, routerId /= nRid] -- Rest des Graph, alle noch nicht "markierten" (bearbeiteten) Router
		shortestPaths = updateShortestPaths neighbours (currentRoute routes nRid) routes -- aktualisierter kürzester Pfad

---------------------------------------------------------------------------------------------------------
-- Funktionen für die Ausgabe


-- |Besserer Ansatz für Ausgabefunktion
printRoutingTable :: [ShortestPath] -> IO()
printRoutingTable [] = putStrLn ""
printRoutingTable (sp:xs) = do
	putStrLn (prettifyShortestPath sp)
	printRoutingTable xs


-- |Dummy Methode
prettifyShortestPath :: ShortestPath -> String
prettifyShortestPath (routerId, metric, routes) = "RouterId: " ++ (show routerId) ++ " Metric: " ++ (show metric)


---------------------------------------------------------------------------------------------------------
-- Funktoinen für die Nachbarschaftstabelle

-- |Teilt einen String bei den Tabulatorenzeichen
splitStringOnTab :: [Char] -> [String]
splitStringOnTab x = splitIt [] [] x
	where
		splitIt accu curstring (x:xs) | x == '\t' = splitIt (accu ++ [curstring]) [] xs
								   | otherwise = splitIt accu (curstring ++ [x]) xs
		splitIt accu [] [] = accu
		splitIt accu curstring [] = splitIt (accu ++ [curstring]) [] []

-- |Parst eine Zeile in der Nachbarschaftstabelle
parseNeighbourTableLine :: [[Char]] -> (Address,Interface,State,RouterId,Priority,Dead)
-- Konvertieren auf den richtigen Typ. Vorsicht bei von [Char] abgeleiteten typen
parseNeighbourTableLine (a:i:s:r:p:d:[])  = (a, i, read s, r, read p, read d) :: (Address,Interface,State,RouterId,Priority,Dead)


-- |Liest die Nachbarschaftstabelle (in Tabellenform) aus einem File ein und parst diese in eine Liste mit Nachbarn und deren Eigenschaften
readNeighbourTable :: String -> [(Address,Interface,State,RouterId,Priority,Dead)]
readNeighbourTable fileContents = processNeighbourTable (tail (lines fileContents))  -- ignoriere erste zeile
	where
		processNeighbourTable xs  = [processLine x | x <- xs  ]
		processLine x = parseNeighbourTableLine (splitStringOnTab x)


-- |Das Programm
main = do
		neighboursTableContents <- readFile "data/neighboursTable.txt"
		expectedResultFileContents  <- readFile "data/expectedResult.txt"
		topoGraphInput <- buildTopoGraph

		let neighboursTable = readNeighbourTable neighboursTableContents
		let expectedResultInput  =  read expectedResultFileContents :: [ShortestPath]

		putStrLn "\n***** Topology Graph *****"
		putStrLn (show topoGraphInput)
		putStrLn "\n***** Routing Table *****"
		printRoutingTable expectedResultInput
		putStrLn "***** Test Result *****"
		print ((dijkstra topoGraphInput []) == expectedResultInput)