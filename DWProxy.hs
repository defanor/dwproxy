{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import qualified Data.HashMap.Strict as HM
import qualified Database.SQLite.Simple as SQLite
import Data.Maybe (mapMaybe, fromJust)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Network.Socket.ByteString (sendAll, recv)
import Network.Socket hiding (send, recv)
import Control.Concurrent.Async (async, waitAnyCatchCancel)
import Control.Exception as E
import Control.Monad (when, forM_, forever)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.MVar
import Data.Either (rights)
import Control.Applicative
import Data.Aeson (FromJSON, decodeStrict)
import GHC.Generics (Generic)
import Data.Monoid


type RoomID = T.Text

data RoomInfo = RoomInfo { identifier :: T.Text, name :: T.Text }
  deriving (Show, Generic, FromJSON)

data Room = Room { roomID :: RoomID
                 , roomMapID :: Int
                 , roomX :: Int
                 , roomY :: Int
                 , roomShort :: T.Text
                 , roomType :: T.Text
                 } deriving (Show, Eq)

data Exit = Exit { exitFrom :: RoomID
                 , exitTo :: RoomID
                 , exitName :: T.Text }
  deriving (Show, Eq)

mapNames :: [T.Text]
mapNames =
  [ "Ankh-Morpork", "AM Assassins", "AM Buildings", "AM Cruets", "AM Docks"
  , "AM Guilds", "AM Isle of Gods", "Shades Maze", "Temple of Small Gods"
  , "AM Temples", "AM Thieves", "Unseen University", "AM Warriors"
  , "Pseudopolis Watch House", "Magpyr's Castle", "Bois", "Bes Pelargic"
  , "BP Buildings", "BP Estates", "BP Wizards", "Brown Islands"
  , "Death's Domain", "Djelibeybi", "IIL - DJB Wizards", "Ephebe"
  , "Ephebe Underdocks", "Genua", "Genua Sewers", "GRFLX Caves"
  , "Hashishim Caves", "Klatch Region", "Mano Rossa", "Monks of Cool"
  , "Netherworld", "Pumpkin Town", "Ramtops Regions", "Sto Lat"
  , "Academy of Artificers", "Cabbage Warehouse", "AoA Library"
  , "Sto Lat Sewers", "Sprite Caves", "Sto Plains Region"
  , "Uberwald Region", "UU Library", "Klatchian Farmsteads", "CFT Arena"
  , "PK Arena", "AM Post Office", "Ninja Guild", "The Travelling Shop"
  , "Slippery Hollow", "House of Magic - Creel", "Special Areas"
  , "Skund Wolf Trail"
  ]

instance SQLite.FromRow Room where
  fromRow = Room <$> SQLite.field <*> SQLite.field <*> SQLite.field
            <*> SQLite.field <*> SQLite.field <*> SQLite.field

instance SQLite.FromRow Exit where
  fromRow = Exit <$> SQLite.field <*> SQLite.field <*> SQLite.field

data PathFinder = PF { pfGraph :: Gr RoomID Int
                     , pfRoomToNode :: HM.HashMap RoomID Int
                     , pfNodeToRoom :: HM.HashMap Int RoomID
                     , pfExitMap :: HM.HashMap (RoomID, RoomID) T.Text
                     }

pathFinder :: [Room] -> [Exit] -> PathFinder
pathFinder rooms exits =
  let roomIDs = map roomID rooms
      roomToNode = HM.fromList (zip roomIDs [1..])
      nodes = zip [1..] roomIDs
      nodeToRoom = HM.fromList nodes
      mkEdge e = do
        from <- HM.lookup (exitFrom e) roomToNode
        to <- HM.lookup (exitTo e) roomToNode
        pure (from, to, 1)
      edges = mapMaybe mkEdge exits
      graph = mkGraph nodes edges :: Gr RoomID Int
      exitMap = HM.fromList $ map (\(Exit f t e) -> ((f, t), e)) exits
  in PF graph roomToNode nodeToRoom exitMap

findPath :: PathFinder -> RoomID -> RoomID -> Either T.Text T.Text
findPath pf from to =
  let route = sp (fromJust $ HM.lookup from $ pfRoomToNode pf)
              (fromJust $ HM.lookup to $ pfRoomToNode pf)
              (pfGraph pf)
  in case route of
    Just path@(_:_) -> Right $ T.intercalate ";" $
      mapMaybe (flip HM.lookup (pfExitMap pf)) $
      (\p -> zip p (tail p)) $ mapMaybe (flip HM.lookup $ pfNodeToRoom pf) path
    Just [] -> Right "look"
    Nothing -> Left "No route found"

mkServer :: IO Socket
mkServer = do
  addr <- head <$> getAddrInfo
    (Just $ defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream})
    Nothing
    (Just "2000")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 1
  pure sock

mkClient :: IO Socket
mkClient = do
  addr <- head <$> getAddrInfo
    (Just $ defaultHints { addrSocketType = Stream })
    (Just "discworld.starturtle.net")
    (Just "23")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  pure sock

showRoom :: Room -> BS.ByteString
showRoom r = TE.encodeUtf8 $ T.concat
  [ roomShort r, " ("
  , mapNames !! (roomMapID r - 1), ", "
  , T.pack (show $ roomX r), "x", T.pack (show $ roomY r)
  , ")"]

c2s :: Config -> BS.ByteString -> IO ()
c2s c leftover = do
  cmd <- parseWith (recv (cClient c) 4096) pCommand leftover
  case cmd of
    Fail d _ _ -> when (d /= "") $ do
      -- putStrLn $ "Client: " ++ show cmd
      sendAll (cServer c) $ BS.takeWhile (/= '\255') d
      c2s c $ BS.dropWhile (/= '\255') d
    Partial _ -> error "Unexpected partial result"
    Done d r -> do
      case r of
        TelnetSN s -> sendAll (cServer c) $ BS.concat ["\255\250", s, "\255\240"]
        Telnet "\254\201" -> do
          sendAll (cServer c) $ BS.concat
            [ "\255\253\201" -- enable GMCP
            , "\255\250\201" -- GMCP sub-negotiation
            , "core.hello { \"client\" : \"dwproxy\", \"version\" : \"0\" }"
            , "\255\240"
            , "\255\250\201"
            , "core.supports.set [ \"room.info\" ]"
            , "\255\240"
            ]
        Telnet other -> sendAll (cServer c) $ BS.cons '\255' other
        ShopSearch query -> do
          curRoom <- currentRoom <$> readMVar (connState c)
          shops <- SQLite.query (dbConn c)
            ("select room_id, room_short, item_name, sale_price from shop_items"
             <> " natural inner join rooms where item_name like ?")
            (SQLite.Only (concat ["%", query, "%"]))
            :: IO [(T.Text, T.Text, T.Text, T.Text)]
          -- cleanup the routes
          modifyMVar_ (connState c) $ \cst -> pure $ cst { routeOptions = [] }
          forM_ shops $ \(roomI, roomS, item, price) -> do
            let p = curRoom >>=
                    \cr -> either (const Nothing) Just (findPath (cPF c) cr roomI)
            n <- case p of
              Nothing -> pure " "
              Just p' -> modifyMVar (connState c) $ \cst ->
                pure (cst { routeOptions = routeOptions cst ++ [TE.encodeUtf8 p'] }
                     , show (length $ routeOptions cst))
            sendAll (cClient c) $ TE.encodeUtf8 $
              T.concat ["[", T.pack n, "] ", roomS, ": ", item, " for ", price, "\r\n"]
        SpeedWalk f t -> do
          cs <- readMVar (connState c)
          case (T.pack <$> f) <|> currentRoom cs of
            Nothing -> sendAll (cClient c)
              "No source location is given, and none detected"
            Just f' -> do
              fromRooms <- SQLite.query (dbConn c)
                "select * from rooms where room_short like ? or room_id = ?"
                ((T.concat ["%", f', "%"]), f')
              toRooms <- SQLite.query (dbConn c)
                "select * from rooms where room_short like ?"
                (SQLite.Only (concat ["%", t, "%"]))
              let routes = rights
                    [ (\p -> (showRoom from, showRoom to, p)) <$>
                    findPath (cPF c) (roomID from) (roomID to)
                    | from <- fromRooms, to <- toRooms ]
              case routes of
                [] -> sendAll (cClient c) "No routes found\r\n"
                [(_from, _to, p)] -> do
                  sendAll (cServer c) $ BS.concat
                    ["alias _speedwalk "
                    , TE.encodeUtf8 p
                    , "\r\n"
                    , "_speedwalk"
                    , "\r\n"
                    ]
                _ -> do
                  forM_ (zip ([0..] :: [Int]) routes) $ \(n, (from, to, _)) ->
                    sendAll (cClient c) $
                    BS.concat ["[", BS.pack (show n), "] "
                              , maybe "" (const $ from <> " to ") f
                              , to, "\r\n"]
                  modifyMVar_ (connState c) $ \cst -> pure $
                    cst { routeOptions = map (\(_,_,route) -> TE.encodeUtf8 route) routes }
                  pure ()
        RouteChoice n -> do
          routes <- routeOptions <$> readMVar (connState c)
          if length routes < n
            then sendAll (cClient c) "No such route"
            else sendAll (cServer c) $
                 BS.concat ["alias _speedwalk ", routes !! n, "\r\n"
                           , "_speedwalk", "\r\n"]
      c2s c d

s2c :: Config -> BS.ByteString -> IO ()
s2c c leftover = do
  cmd <- parseWith (recv (cServer c) 4096) pCommon leftover
  case cmd of
    Fail d _ _ -> when (d /= "") $ do
      -- putStrLn $ "Server: " ++ show cmd
      sendAll (cClient c) $ BS.takeWhile (/= '\255') d
      s2c c $ BS.dropWhile (/= '\255') d
    Partial _ -> error "Unexpected partial result"
    Done d r -> do
      case r of
        Telnet s -> sendAll (cClient c) $ BS.cons '\255' s
        TelnetSN s -> do
          case BS.stripPrefix "\201room.info " s >>= decodeStrict of
            Nothing -> pure ()
            jri -> do
              print jri
              modifyMVar_ (connState c) $ \cs -> pure cs { currentRoom = identifier <$> jri }
              sendAll (cClient c) $ BS.concat ["\255\250", s, "\255\240"]
        _ -> error "Parsed an unexpected server command"
      s2c c d

data Command = Telnet BS.ByteString
             | TelnetSN BS.ByteString
             | SpeedWalk (Maybe String) String
             | ShopSearch String
             | RouteChoice Int
  deriving (Show)

data ConnState = CS { routeOptions :: [BS.ByteString]
                    , currentRoom :: Maybe RoomID
                    } deriving (Show)

data Config = Config { cClient :: Socket
                     , cServer :: Socket
                     , dbConn :: SQLite.Connection
                     , connState :: MVar ConnState
                     , cPF :: PathFinder
                     }

pRouteChoice :: Parser Command
pRouteChoice = do
  _ <- "route "
  d <- decimal
  _ <- "\r\n"
  pure $ RouteChoice d

pSpeedWalk :: Parser Command
pSpeedWalk = do
  _ <- "speedwalk "
  from <- ("from " *> (Just <$> manyTill (notChar '\255') " to "))
    <|> ("to " *> pure Nothing)
  to <- manyTill (notChar '\255') "\r\n"
  pure $ SpeedWalk from to

pShopSearch :: Parser Command
pShopSearch = ShopSearch <$> ("shop " *> manyTill (notChar '\255') "\r\n")

pClientCommand :: Parser Command
pClientCommand = pSpeedWalk <|> pRouteChoice <|> pShopSearch

pTelnetSN :: Parser Command
pTelnetSN = do
  _ <- "\255\250"
  (TelnetSN . BS.pack) <$> manyTill anyChar "\255\240"

pTelnet :: Parser Command
pTelnet = do
  _ <- char '\255'
  c <- notChar '\250'
  Telnet <$> if c >= '\251' && c <= '\254'
    then do
      code <- anyChar
      pure $ BS.pack [c, code]
    else pure $ BS.pack [c]

pCommon :: Parser Command
pCommon = pTelnet <|> pTelnetSN

pCommand :: Parser Command
pCommand = pClientCommand <|> pCommon

main :: IO ()
main = do
  args <- getArgs
  case args of
    [quow] ->
      E.bracket
      (SQLite.open $ quow </> "maps" </> "_quowmap_database.db")
      (SQLite.close) $ \db -> do
      rooms <- SQLite.query_ db
        "select room_id, map_id, xpos, ypos, room_short, room_type from rooms"
      exits <- SQLite.query_ db "select room_id, connect_id, exit from room_exits"
      let pf = pathFinder rooms exits
      mv <- newMVar $ CS [] Nothing
      E.bracket mkServer close $ \serverSocket -> forever $ do
        putStrLn "Waiting for connections"
        E.bracket (fst <$> accept serverSocket) close $ \client ->
          E.bracket mkClient close $ \server -> do
          putStrLn "Connected to DW"
          let c = Config client server db mv pf
          c2s' <- async $ c2s c ""
          s2c' <- async $ s2c c ""
          r <- waitAnyCatchCancel [c2s', s2c']
          case r of
            (_, Left e) -> print e
            _ -> pure ()
    _ -> putStrLn "Usage: dwroute <quow plugins>"
  pure ()
