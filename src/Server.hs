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
import qualified Data.Map.Lazy as M
import Data.Maybe

type RoomsState = M.Map Text ([Text], PieceMap, Text)

type ClientInfo = (Text, WS.Connection)

type ClientMap = M.Map Text ClientInfo

type ServerState = (ClientMap, RoomsState)


newServerState :: ServerState
newServerState = let
    clientMap = M.empty
    roomsState = M.empty
    in (clientMap, roomsState)

addRoom :: Text -> MVar ServerState -> IO ()
addRoom roomName state =
    modifyMVar_ state $ \s -> do
    let clients = fst s
        rooms = M.insert roomName ([], initPieces, "white") $ snd s
        s = (clients, rooms)
        in return (clients, rooms)

removeClient :: Text -> MVar ServerState -> IO ()
removeClient uuid state =
    modifyMVar_ state $ \s -> do
    let (clients, rooms) = s
        clients' = M.delete uuid clients
        s' = (clients', rooms)
        in return (clients', rooms)


addClient :: ClientInfo -> Text ->  MVar ServerState -> IO ()
addClient info guid state =
    modifyMVar_ state $ \s -> do
    let (clients, rooms) = s
        clients' = M.insert guid info clients
        in return (clients, rooms)


addClientToRoom :: Text -> Text -> MVar ServerState -> IO ()
addClientToRoom uuid roomName state =
    modifyMVar_ state $ \s -> do
        let (clients, rooms) = s
            room = rooms M.! roomName
            (roomClients, ps, color) = room
            roomClients' = roomClients ++ [uuid]
            rooms' = M.insert roomName (roomClients', ps, color) $ M.delete roomName rooms
            client = clients M.! uuid
            client' = (roomName, snd client)
            clients' = M.insert uuid client' $ M.delete uuid clients
            in return (clients', rooms')
    
updateBoard :: Text -> Text -> MVar ServerState -> IO ()
updateBoard cmd roomName state = 
    modifyMVar_ state $ \s -> do
        let (clients, rooms) = s
            room = rooms M.! roomName
            (roomClients, pieces, color) = room
            parsed = parseCommand (T.unpack cmd) pieces (T.unpack color)
            newPieces = movePiece parsed pieces
            color' = if color == "white" then "black" else "white"
            rooms' = M.insert roomName (roomClients, newPieces, color') $ M.delete roomName rooms
            in return (clients, rooms')



broadcast :: Text -> [WS.Connection] -> IO ()
broadcast message clients = do
    TIO.putStrLn message
    forM_ clients $ \conn -> WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

getPieces :: Text -> ServerState -> PieceMap
getPieces roomName state = 
    let (_, rooms) = state
        room = rooms M.! roomName
        (_, ps, _) = room
        in ps


application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn 
    state' <- readMVar state
    case (msg :: Text) of
        _   | cmd == "start game" ->
                do
                    addRoom (T.drop 11 cmd) state
                    addClientToRoom uuid (T.drop 11 cmd) state
                    WS.sendTextData conn ("Created Room:" :: Text)
                    talk conn state roomName
            | cmd == "join game" ->
                do
                    addClientToRoom uuid (T.drop 12 cmd) state
                    WS.sendTextData conn ("Joined Room" :: Text)
                    talk conn state roomName

            | otherwise ->
                flip finally disconnect $ do
                    addClient ("", conn) uuid state
                    WS.sendTextData conn $ ("Welcome" :: Text)
            where
                uuid = T.take 36 msg
                cmd = T.drop 36 msg
                (clients, _) = state'
                (roomName, _) = fromJust $ M.lookup uuid clients
                disconnect = do
                    removeClient uuid state


talk :: WS.Connection -> MVar ServerState -> Text -> IO ()
talk conn state roomName = forever $ do
    msg <- WS.receiveData conn
    s <- readMVar state
    let cmd = T.drop 36 msg
    updateBoard cmd roomName state
    state'' <- readMVar state
    let ps = getPieces roomName state'' 
        boardText = T.pack (printBoard ps)
    WS.sendTextData conn boardText
    let (clients, _) = s
        filtered = filter (\x -> fst x == roomName ) (M.elems clients)
        conns = fmap (\x -> snd x) filtered
        in
        broadcast msg conns