module CIO
    ( ePutStrBl
    , ePutStr
    , vPutStrBl
    , vEPutStrBl
    , vEPutStr
    , putStrBl
    , ev
    , bol
    , eBOL
    , vBOL
    , tMessage
    , vMessage
    , vPutChar
    , vPutStr
    , printStdout
    , printStderr
    , printOutput
    , dotOutput
    , setStyle
    , addPrefixes
    ) where

import qualified Data.ByteString.Char8 as B ( length, unpack )
import Prelude hiding (putStr)
import Control.Monad.Trans ( MonadIO, liftIO )
import System.IO (hPutStr, stderr)
import qualified System.IO as IO
    ( putChar, putStr, stderr, hPutStrLn, hPutStr )
import System.Unix.Process ( Output(Stderr, Stdout) )

{-
type CIO = IO
class MonadIO m => CIO m
instance CIO IO
-}

{-
-- |Catch exceptions in a MonadIO action.
tryCIO :: CIO m => m a -> m (Either SomeException a)
tryCIO task =
    try' task
    where
      try' task = try task
          do result <- try task
             case result of
               Left e -> return (Left e)
               Right (a, _) -> return (Right a)
-}

ePutStrBl :: String -> IO ()
ePutStrBl s = IO.hPutStrLn IO.stderr s
ePutStr :: String -> IO ()
ePutStr s = IO.hPutStr IO.stderr s
vEPutStr n s = if n <= 0 then IO.hPutStr IO.stderr s else return ()
vPutStrBl :: Int -> String -> IO ()
vPutStrBl n s = if n <= 0 then putStrLn s else return ()
vEPutStrBl :: Int -> String -> IO ()
vEPutStrBl n s = if n <= 0 then ePutStrBl s else return ()
vPutChar n c = if n <= 0 then liftIO (IO.putChar c) else return ()
vPutStr n s = if n <= 0 then liftIO (IO.putStr s) else return ()
putStrBl s = liftIO (putStrLn s)
ev :: Int -> IO Int
ev n = return n
putStr :: String -> IO ()
putStr s = IO.putStr s
bol :: IO ()
bol = return ()
eBOL :: IO ()
eBOL = return ()
vBOL :: Int -> IO ()
vBOL _ = return ()
setStyle _style output = output
addPrefixes :: String -> String -> a -> a
addPrefixes _ _ style = style

-- |Print a message without forcing the command's output
tMessage :: String -> a -> IO a
tMessage message output = ePutStrBl message >> return output

-- |Print a message without forcing the command's output
vMessage :: Int -> String -> a -> IO a
vMessage v message output = vEPutStrBl v message >> return output

-- |Print stdout to stdout
printStdout :: [Output] -> IO [Output]
printStdout output =
    bol >> mapM print output
    where
      print x@(Stdout s) = putStr (B.unpack s) >> return x
      print x = return x

-- |Print stderr to stderr
printStderr :: [Output] -> IO [Output]
printStderr output =
    mapM print output
    where
      print x@(Stderr s) = hPutStr stderr (B.unpack s) >> return x
      print x = return x

-- |Print all the output to the appropriate output channel
printOutput :: [Output] -> IO [Output]
printOutput output =
    eBOL >> mapM print output
    where
      print x@(Stdout s) = putStr (B.unpack s) >> return x
      print x@(Stderr s) = hPutStr stderr (B.unpack s) >> return x
      print x = return x

-- |Print one dot to stderr for every COUNT characters of output.
dotOutput :: Int -> [Output] -> IO [Output]
dotOutput groupSize output =
    mapM (\ (count, elem) -> ePutStr (replicate count '.') >> return elem) pairs
    where
      pairs = zip (dots 0 (map length output)) output
      dots _ [] = []
      dots rem (count : more) =
          let (count', rem') = divMod (count + rem) groupSize in
          count' : dots rem' more
      length (Stdout s) = B.length s
      length (Stderr s) = B.length s
      length _ = 0

