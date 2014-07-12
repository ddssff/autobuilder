module DryRun 
    ( createDirectoryIfMissingDR
    , setFileModeDR
    , lazyCommandDR
    , removeFileDR
    , createLinkDR
    , prepareSymbolicLinkDR
    , writeFileDR
    , bWriteFileDR
    , renameFileDR
    , pgpSignFilesDR
    , removeRecursiveSafelyDR
    --, systemTaskDR
    , createSymbolicLinkIfMissingDR
    ) where

import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Posix.Files
import System.Unix.Directory
import System.Unix.Process
import Extra.TIO
import Debian.IO
import qualified System.IO as IO
import System.Exit
--import System.Time
import Extra.Files
import Extra.GPGSign

createSymbolicLinkIfMissingDR :: String -> FilePath -> AptIO ()
createSymbolicLinkIfMissingDR text path =
    dr' (tio (msgLn 0 ("(dry run) createSymbolicLinkIfMissing " ++ show text ++ " " ++ show path)))
        (io $ createSymbolicLinkIfMissing text path)

createDirectoryIfMissingDR flag path =
    dr' (tio (msgLn 0 ("(dry run) createDirectoryIfMissing " ++ show flag ++ " " ++ show path)))
        (do --ePut ("(running) createDirectoryIfMissing " ++ show flag ++ " " ++ show path)
          io $ createDirectoryIfMissing flag path)

setFileModeDR path mode =
    dr' (tio (msgLn 0 ("(dry run) setFileMode " ++ show path ++ " " ++ show mode)))
        (do --ePut ("(running) setFileMode " ++ show path ++ " " ++ show mode)
           io $ setFileMode path mode)

{-
lazyCommandDR False cmd input =
    do --ePut ("(running) lazyCommand " ++ show cmd ++ " input")
       lazyCommand cmd input
lazyCommandDR True cmd _ =
    do ePut ("(dry run) lazyCommand " ++ show cmd ++ " input")
       return [Result ExitSuccess]
-}

lazyCommandDR :: String -> [B.ByteString] -> AptIO [Output]
lazyCommandDR cmd input =
    dr' (do tio (msgLn 0 ("(dry run) lazyCommand " ++ show cmd ++ " input"))
            return [Result ExitSuccess])
        (io $ lazyCommand cmd input)

removeFileDR path =
    dr' (tio (msgLn 0 ("(dry run) removeFile " ++ show path))) (io $ removeFile path)

createLinkDR text path =
    dr' (tio (msgLn 0 ("(dry run) createLink " ++ show text ++ " " ++ show path))) (io $ createLink text path)

prepareSymbolicLinkDR name path =
    dr' (tio (msgLn 0 ("(dry run) prepareSymbolicLink " ++ show name ++ " " ++ show path)))
        (do --ePut ("(running) prepareSymbolicLink " ++ show name ++ " " ++ show path)
           io $ prepareSymbolicLink name path)

writeFileDR :: FilePath -> String -> AptIO ()
writeFileDR path text =
    dr' (tio (msgLn 0 ("(dry run) writeFile " ++ show path ++ " <text>"))) (io $ writeFile path text)

bWriteFileDR :: FilePath -> B.ByteString -> AptIO ()
bWriteFileDR path text =
    dr' (tio (msgLn 0 ("(dry run) writeFile " ++ show path ++ " <text>"))) (io $ B.writeFile path text)

renameFileDR old new =
    dr' (tio (msgLn 0 ("(dry run) renameFile " ++ show old ++ " " ++ show new)))
        ({- do ePut ("(running) renameFile " ++ show old ++ " " ++ show new) -}
           io $ renameFile old new)

pgpSignFilesDR :: FilePath -> PGPKey -> [FilePath] -> AptIO [Bool]
pgpSignFilesDR root key files =
    dr' (tio (msgLn 0 ("(dry run) pgpSignFiles " ++ root ++ " key " ++ show files) >>
              return (map (const True) files)))
        (do --ePut ("(running) pgpSignFiles " ++ show root ++ " key " ++ show files)
           io $ pgpSignFiles root key files)

removeRecursiveSafelyDR path =
    dr' (tio (msgLn 0 ("(dry run) removeRecursiveSafely " ++ show path)))
        (io $ removeRecursiveSafely path)

drMsg :: String -> AptIO a -> AptIO a -> AptIO a
drMsg msg fake real = dr' (do tio (msgLn 0 ("(dry run) " ++ msg)); fake) real

--systemTaskDR cmd = drMsg ("(dry run) " ++ cmd) sysOK (systemTask cmd)

--systemTaskDR_ cmd = drMsg ("(dry run) " ++ cmd) sysOK_ (systemTask_ cmd)

--sysOK = return ([], noTimeDiff)

--sysOK_ = return noTimeDiff
