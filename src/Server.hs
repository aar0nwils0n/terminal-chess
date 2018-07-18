module Server where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GamePlay
import Board
import qualified Network.WebSockets as WS
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace (trace)
import Data.List (length)
import Data.Either (isRight)

type RoomsState = M.Map Text ([Text], PieceMap, Text)

type ClientInfo = (Text, WS.Connection)

type ClientMap = M.Map Text ClientInfo

type ServerState = (ClientMap, RoomsState)


newServerState :: ServerState
newServerState = let
    clientMap = M.empty
    roomsState = M.empty
    in (clientMap, roomsState)

addRoom :: Text -> ServerState -> ServerState
addRoom roomName s =
    let (clients, rooms) = s
        rooms' = M.insert roomName ([], initPieces, "white") rooms
        in (clients, rooms')

removeClient :: Text -> MVar ServerState -> IO ()
removeClient uuid state =
    modifyMVar_ state $ \s -> do
        let (clients, rooms) = s
            client = clients M.!? uuid
            clients' = M.delete uuid clients
            s' = (clients', rooms)
            rooms' = if (length $ M.elems clients') == 0 && (not $ isNothing client) then
                M.delete (fst $ fromJust client) rooms
                else rooms
            in return (clients', rooms')


addClient :: ClientInfo -> Text ->  ServerState -> ServerState
addClient info guid s =
    let (clients, rooms) = s
        clients' = M.insert guid info clients
        in (clients', rooms)


addClientToRoom :: Text -> Text -> ServerState -> ServerState
addClientToRoom uuid roomName s =
        let (clients, rooms) = s
            room = rooms M.!? roomName
            newState = case room of 
                Just room -> 
                    let
                        (roomClients, ps, color) = room
                        roomClients' = roomClients ++ [uuid]
                        rooms' = M.insert roomName (roomClients', ps, color) $ M.delete roomName rooms
                        client = clients M.!? uuid
                        clients' = case client of
                          Just client -> M.insert uuid (roomName, snd client) $ M.delete uuid clients
                          Nothing -> clients
                        in 
                            (clients', rooms')
                Nothing -> 
                    s
            in newState

validCommand :: Text -> Text -> ServerState -> Bool
validCommand cmd roomName s = 
    let
        (_, rooms) = s
        mayRoom = rooms M.!? roomName
        in case mayRoom of
            Just room ->
                let (_, pieces, color) = room
                    parsed = parseCommand (T.unpack cmd) pieces (T.unpack color)
                in case parsed of
                    Left _ -> True
                    Right _ -> False
            Nothing -> False
    
    
updateBoard :: Text -> Text -> ServerState -> ServerState
updateBoard cmd roomName s = 
        let (clients, rooms) = s
            mayRoom = rooms M.!? roomName
            state' = case mayRoom of 
                Just room ->
                    let (roomClients, pieces, color) = room
                        parsed = parseCommand (T.unpack cmd) pieces (T.unpack color)
                        newPieces = movePiece parsed pieces
                        color' = if color == "white" then "black" else "white"
                        rooms' = M.insert roomName (roomClients, newPieces, color') $ M.delete roomName rooms
                    in (clients, rooms')
                Nothing ->
                    (clients, rooms)
            in state'



broadcast :: Text -> [WS.Connection] -> IO ()
broadcast message clients = do
    forM_ clients $ \conn -> WS.sendTextData conn message

roomExists :: ServerState -> Text -> Bool
roomExists state roomName = 
    let (_, rooms) = state
        mayRoom = rooms M.!? roomName
    in case mayRoom of
        Just _ -> True
        Nothing -> False

clientCount :: ServerState -> Text -> Int
clientCount state roomName = 
    let (_, rooms) = state
        mayRoom = rooms M.!? roomName
    in case mayRoom of
        Just (clients, _, _) -> length clients
        Nothing -> 0



getPieces :: Text -> ServerState -> PieceMap
getPieces roomName state = 
    let (_, rooms) = state
        room = rooms M.!? roomName
        in case room of 
            Just room -> 
                let (_, ps, _) = room
                in ps
            Nothing ->
                initPieces

updateState :: MVar ServerState -> ServerState -> IO ()
updateState state newState = 
    modifyMVar_ state $ \s -> do
        return newState


application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn 
    state' <- readMVar state
    putStrLn "in app"
  
    case (msg :: Text) of
        _   | T.take 10 cmd == "start game" ->
                let roomName = T.drop 11 cmd in
                if roomExists state' roomName then
                    WS.sendTextData conn ("Room already exists" :: Text)
                    else
                        flip finally disconnect $ do
                            let stateWithRoom = addRoom roomName state'
                                stateWithClient = addClientToRoom uuid roomName $ addClient (roomName, conn) uuid stateWithRoom
                            updateState state stateWithClient
                            WS.sendTextData conn (T.concat [("Created Room: " :: Text), roomName])
                            talk conn state roomName
            | T.take 9 cmd == "join game" ->
                let roomName = T.drop 9 cmd in
                case (roomName) of
                    _ | clientCount state' roomName == 2 ->
                        do
                            WS.sendTextData conn ("Already two players in room" :: Text)
                    _ | roomExists state' roomName ->
                        flip finally disconnect $ do
                                do
                                    let stateWithAddedClient = addClient (roomName, conn) uuid $ addClientToRoom uuid roomName state'
                                    updateState state stateWithAddedClient
                                    WS.sendTextData conn (T.concat [("Joined Room: " :: Text), roomName])
                                    talk conn state roomName
                    | otherwise ->
                            do
                                WS.sendTextData conn ("Room does not exist" :: Text)

            where
                uuid = T.take 36 msg
                cmd = T.drop 36 msg
                disconnect = do
                    putStrLn "disconnecting"
                    removeClient uuid state


talk :: WS.Connection -> MVar ServerState -> Text -> IO ()
talk conn state roomName = forever $ do
    msg <- WS.receiveData conn
    s <- readMVar state
    let cmd = T.drop 36 msg
        valid = validCommand cmd roomName s
        state' = updateBoard cmd roomName s
        newMessage = if valid then 
            let ps = getPieces roomName state'
            in T.pack (printBoard ps)
            else ("Invalid move" :: Text)
    updateState state state'
    let (clients, _) = s
        filtered = filter (\x -> fst x == roomName ) (M.elems clients)
        conns = fmap (\x -> snd x) filtered
    broadcast newMessage conns