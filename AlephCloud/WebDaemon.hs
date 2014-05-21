{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module AlephCloud.WebDaemon
    ( runDaemon
    , runDaemonT
    , Config(..)
    , Section(..)
    ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Monoid
import Options.Applicative hiding (header)
import Web.Scotty (ScottyM)
import Web.Scotty.Trans
import Web.Scotty.TLS (scottyTTLS)
import System.Directory
import System.Environment
import Network.Wai

import AlephCloud.Logging

type ServiceName = String

data Opt = Opt
    { optPort            :: Int
    , optCertificateFile :: FilePath
    , optKeyFile         :: FilePath
    , optConfig          :: [FilePath]
    , optDisableTLS      :: Bool
    , optDebug           :: Bool
    } deriving (Show,Eq,Read)

parseOpt = Opt
    <$> option (long "port" <> short 'p' <> metavar "PORT" <> value 3000)
    <*> strOption (long "certificate" <> short 'x' <> metavar "CERTIFICATE" <> value "cert.pem")
    <*> strOption (long "key" <> short 'k' <> metavar "KEY" <> value "key.pem")
    <*> many (strOption (long "config" <> short 'c' <> metavar "CONFIG"))
    <*> switch (long "disable-tls" <> short 'd')
    <*> switch (long "debug")

newtype Config = Config [Section]
    deriving (Show,Eq)

data Section = Section
    { sectionName :: String
    , sectionKVs  :: [(String, String)]
    } deriving (Show,Eq)

parseConfig :: String -> Config
parseConfig = Config . reverse . toSections . foldl accSections ([], Nothing) . lines
  where toSections (l,Nothing) = l
        toSections (l,Just s)  = s : l

        -- a new section in the config file
        accSections (sections, mcurrent) ('[':sectNameE)
            | last sectNameE == ']' =
                let sectName = take (length sectNameE - 1) sectNameE
                 in case mcurrent of
                    Nothing      -> (sections, Just $ Section sectName [])
                    Just current -> (sectionFinalize current : sections, Just $ Section sectName []) 
            | otherwise             =
                (sections, mcurrent)
        -- a normal line without having any section defined yet
        accSections acc@(_, Nothing) _ = acc
        -- potentially a k-v line in an existing section
        accSections (sections, Just current) kvLine =
            case break (== '=') kvLine of
                (k,'=':v) -> (sections, Just $ sectionAppend current (strip k, strip v))
                (_,_)     -> (sections, Just current) -- not a k = v line
        -- append a key-value
        sectionAppend (Section n l) kv = Section n (kv:l)
        sectionFinalize (Section n l) = Section n $ reverse l

        strip s = dropSpaces $ reverse $ dropSpaces $ reverse s
          where dropSpaces = dropWhile (\c -> c == ' ' || c == '\t')

readConfigPath filepath = parseConfig <$> readFile filepath

checkFileExist :: String -> FilePath -> IO ()
checkFileExist name filepath = do
    e <- doesFileExist filepath
    unless e $ error (name ++ " file " ++ show filepath ++ " cannot be found")

readConfiguration :: [FilePath] -> ServiceName -> IO Config
readConfiguration extra serviceName = do
    home <- getEnv "HOME"
    readIfExist (extra ++ [hPath home,etcPath])
  where etcPath    = "/etc/" ++ sConf
        hPath home = home ++ "/" ++ sConf
        sConf      = serviceName ++ ".conf"

        readIfExist [] = return $ Config []
        readIfExist (path:paths) = do
            exists <- doesFileExist path
            if exists
                then readConfigPath path
                else readIfExist paths

runDaemonT :: (Monad m, MonadIO n)
           => (forall a. m a -> n a)
           -> (m Response -> IO Response)
           -> ServiceName
           -> (Config -> IO st)
           -> (st -> ScottyT t m ()) -> n ()
runDaemonT runM runToIO serviceName cfgToSt s = do
    args <- liftIO $ execParser (info parseOpt mempty)
    st   <- liftIO (readConfiguration (optConfig args) serviceName >>= cfgToSt)
    let routingOpts = routing (optDebug args) st
    if optDisableTLS args
        then scottyT (optPort args) runM runToIO routingOpts
        else do
                liftIO $ do
                    checkFileExist "certificate" (optCertificateFile args)
                    checkFileExist "key" (optKeyFile args)
                scottyTTLS (optPort args) (optKeyFile args) (optCertificateFile args) runM runToIO routingOpts
  where routing debug st = do
            --when debug $ middleware
            s st


runDaemon :: ServiceName -> (Config -> IO st) -> (st -> ScottyM ()) -> IO ()
runDaemon = runDaemonT id id
