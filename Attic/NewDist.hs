-- |Replacement for debpool.  
module Main where

import		 Prelude hiding (putStr, putStrLn, putChar)
import		 Control.Monad.Trans
import		 Extra.TIO
import		 Debian.Repo
import		 Debian.Config as Config
import		 Control.Monad
import		 Data.List
import		 Data.Maybe
import		 Extra.GPGSign
import		 Extra.Lock
import		 Debian.Version
import		 System.Console.GetOpt
import		 System.Environment
import		 System.Exit
import qualified System.IO as IO
import		 Text.Regex
import qualified Debian.AutoBuilder.Version as Version
import		 Extra.Email

appName = "newdist"
defaultRoot = "/var/www/" ++ appName
--defaultComponent = Component "main"
-- owner = "root"
-- group = "root"

optSpecs :: [ParamDescr]
optSpecs =
    [ Param ['v'] ["verbose"] ["Verbose"] (NoArg (Value "Verbose" "1"))
                 "Increase the amount of debugging output"
    , Param [] ["root"] ["Root"]	(ReqArg (Value "Root") "PATH")
		 "Specify the root directory of the repository"
    , Param [] ["section"] ["Section"]	(ReqArg (Value "Section") "PATH")
		 "Force uploads to the specified section"
    , Param [] ["expire"] ["Expire"]	(NoArg (Value "Expire" "yes"))
		 "Remove all packages trumped by newer versions from the package lists."
    , Param [] ["clean-up"] ["Clean-Up"]	(NoArg (Value "Clean-Up" "yes"))
		 "Move all unreferenced files in the repository to the removed directory."
{-  , Param ['n'] ["dry-run"] ["Dry-Run"]	(NoArg (Value "Dry-Run" "yes"))
		 "Test run, don't modify the repository." -}
    , Param [] ["remove"] ["Remove"]	(ReqArg (Value "Remove") "DIST,SECTION,PACKAGE=VERSION")
                 "remove a particular version of a package (may be repeated.)"
    , Param ['i'] ["install"] ["Install"]	(NoArg (Value "Install" "yes"))
                 ("Scan the incoming directory and install the packages found there.\n" ++
                  "This option is automatically true if no --remove arguments are given.")
    , Param [] ["create-release"] ["Create-Release"] (ReqArg (Value "Create-Release") "NAME")
		 ("Create any new releases and/or sections found in Distribution\n" ++
                  "and Section fields of the uploaded .changes files.")
    , Param [] ["create-alias"] ["Create-Alias"] (ReqArg (Value "Create-Alias") "ALIAS=RELEASE")
		 "Create an alias for an existing release"
    , Param [] ["create-section"] ["Create-Section"] (ReqArg (Value "Create-Section") "RELEASE,SECTION")
		 "Create a new section in the given release."
    , Param [] ["replace"] ["Replace"]	(NoArg (Value "Replace" "yes"))
                 ("Permit uploading of a package whose version is already present.\n" ++
                  "This is normally an error.")
    , Param [] ["notify-email"] ["Notify-Email"] (ReqArg (Value "Notify-Email") "USER@HOST, USER@HOST...")
                 "Email address to send notifications about success and failure of uploads."
    , Param [] ["sender-email"] ["Sender-Email"] (ReqArg (Value "Sender-Email") "USER@HOST")
                 "Sender address for notifications."
    , Param [] ["verify"] ["Verify"]	(NoArg (Value "Verify" "yes"))
                 "Verify the structure and contents of the repository."
    , Param [] ["reject-revision"] ["Reject-Revision"] (ReqArg (Value "Reject-Revision") "STRING")
                 ("Disallow uploads of packages with this revision string.  The\n" ++
                  "autobuilder gives 'dir' targets the revision string 'none', the\n" ++
                  "option 'Reject-Revision: none' can be used to prohibit uploads of\n" ++
                  "packages built from a 'dir' target.")
    , Param [] ["version"] ["Version"] (NoArg (Value "Version" "yes"))
                 "Print the version string and exit"
    , Param [] ["layout"] ["Layout"] (ReqArg (Value "Layout") "flat or pool")
                 "Specify a default layout"
    , Param [] ["no-sign"] ["No-Sign"] (NoArg (Value "No-Sign" "yes"))
                 "Do not attempt to cryptographically sign the repository."
    , Param [] ["sign"] ["Sign"] (NoArg (Value "Sign" "yes"))
                 "Cryptographically sign the repository even if nothing changed."
    , Param [] ["keyname"] ["Key-Name"] (ReqArg (Value "Key-Name") "STRING")
                 "Name of the pgp key with which to sign the repository."
{-
     , Param [] ["rebuild"] ["Rebuild"]	(NoArg (Value "Rebuild" "yes"))
     "(UNIMPLEMENTED) Reconstruct the package lists from scratch."
     , Param [] ["obsolete"] ["Obsolete"]	(NoArg (Value "Obsolete" "yes"))
     (My.consperse "\n"
      ["(UNIMPLEMENTED) Remove any package for which newer versions exist,",
       "remove any package which is not part of any dist."])
-}
    ]

{-
defaultStyles :: IOStyle
defaultStyles = (setFinish (Just "done.") .
                 setError (Just "failed.") .
                 setEcho False .
                 setElapsed False .
                 setDots IO.stdout 1024 '.' .
                 setDots IO.stderr 1024 ',') (Debian.IO.defStyle 0)
-}

defaultArchitectures = [Binary "i386", Binary "amd64"]

style :: [Flag] -> TStyle -> TStyle
style flags = foldl (.) id . map readStyle $ findValues flags "Style"

readStyle :: String -> TStyle -> TStyle
readStyle text =
    case (mapSnd tail . break (== '=')) text of
      --("Start", message) -> setStart . Just $ message
      --("Finish", message) -> setFinish . Just $ message
      --("Error", message) -> setError . Just $ message
      --("Output", "Indented") -> addPrefixes "" ""
      --("Output", "Dots") -> dotStyle IO.stdout . dotStyle IO.stderr
      --("Output", "Quiet") -> quietStyle IO.stderr . quietStyle IO.stdout
      --("Echo", flag) -> setEcho (readFlag flag)
      --("Elapsed", flag) -> setElapsed (readFlag flag)
      ("Verbosity", value) -> setVerbosity (read value)
      --("Indent", prefix) -> addPrefixes prefix prefix
      _ -> id
    where
      readFlag "yes" = True
      readFlag "no" = False
      readFlag "true" = True
      readFlag "false" = True
      readFlag text = error ("Unrecognized bool: " ++ text)
      mapSnd f (a, b) = (a, f b)

main :: IO ()
main =
    do args <- getArgs
       let verbosity = length $ filter (flip elem ["-v", "--verbose"]) args
       runTIO defStyle { verbosity = verbosity }
              (runAptIO
               (do -- Compute configuration options
                   let flags' = useMain (Config.seedFlags appName optSpecs args)
                   flags <- liftIO (computeConfig verbosity appName flags' nameFirstSection) >>= return . concat
                   lift (vPutStrBl 1 ("Flags:\n  " ++ concat (intersperse "\n  " (map show flags))))
                   let lockPath = outsidePath (root flags) ++ "/newdist.lock"
                   case findValues flags "Version" of
                     [] -> withLock lockPath (runFlags flags) >>=
                           either (\ e -> error $ "Failed to obtain lock " ++ lockPath ++ ":\n " ++ show e) (const . lift $ vBOL 0)
                     _ -> lift (putStrBl Version.autoBuilderVersion) >>
                          liftIO (exitWith ExitSuccess)))
    where
      nameFirstSection (flags : more) = nameSection "Main" flags : more
      nameFirstSection [] = []
      nameSection name flags = case any isName flags of
                                 False -> (Name name : flags)
                                 True -> flags
      isName (Name _) = True
      isName _ = False
      useMain flags =
          case any isUse flags of
            False -> (Use ["Main"] : flags)
            True -> flags
      isUse (Use _) = True
      isUse _ = False

runFlags :: CIO m => [Flag] -> AptIOT m ()
runFlags flags =
    do createReleases flags
       repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       -- Get a list of the Release objects in the repository at Root
       releases <- findReleases repo
       -- Get the Repository object, this will certainly be a singleton list.
       --let repos = nub $ map releaseRepo releases
       lift (deletePackages releases flags keyname)
       --vPutStrBl 1 IO.stderr $ "newdist " ++ show (root flags)
       --vPutStrBl 1 IO.stderr $ "signRepository=" ++ show signRepository
       --io $ exitWith ExitSuccess
       liftIO $ setRepositoryCompatibility repo
       when install ((scanIncoming False keyname repo) >>= 
                     \ (ok, errors) -> (liftIO (sendEmails senderAddr emailAddrs (map (successEmail repo) ok)) >>
                                        liftIO (sendEmails senderAddr emailAddrs (map (\ (changes, error) -> failureEmail changes error) errors)) >>
                                        lift (exitOnError (map snd errors))))
       when expire  $ lift (deleteTrumped keyname releases) >> return ()
       when cleanUp $ deleteGarbage repo >> return ()
       -- This flag says sign even if nothing changed
       when signRepository $ lift (signReleases keyname releases)
    where
{-
      findReleaseByName :: [Release] -> ReleaseName -> Maybe Release
      findReleaseByName releases dist =
          case filter (\ release -> releaseName release == dist) releases of
            [] -> Nothing
            [release] -> (Just release)
            _ -> error ("Multiple releases with name " ++ show dist)
-}
      --section = Component $ maybe "main" id $ findValue flags "Section"
      --runStyle = (cond Debian.IO.dryRun Debian.IO.realRun dryRun) . setVerbosity verbose . (style flags)
      --dryRun = findBool flags "Dry-Run"
      --verbose = maybe 0 read (findValue flags "Verbose")
      remove = findValues flags "Remove"
      emailAddrs :: [(String, String)]
      emailAddrs =
          catMaybes . map parseEmail . concat . map (splitRegex (mkRegex "[ \t]*,[ \t]*")) . findValues flags $ "Notify-Email"
      senderAddr :: (String, String)
      senderAddr = maybe ("autobuilder", "somewhere") id . maybe Nothing parseEmail . listToMaybe . findValues flags $ "Sender-Email"
      successEmail :: LocalRepository -> ChangesFile -> (String, [String])
      successEmail repo changesFile =
          let subject = ("newdist: " ++ changePackage changesFile ++ "-" ++ show (changeVersion changesFile) ++ 
                         " now available in " ++ releaseName' (changeRelease changesFile) ++
                         " (" ++ archName (changeArch changesFile) ++")")
              body = ("Repository " ++ envPath (repoRoot repo)) : [] : (lines $ show $ changeInfo changesFile) in
    	  (subject, body)
      failureEmail :: ChangesFile -> InstallResult -> (String, [String])
      failureEmail changesFile error =
          let subject = ("newdist failure: " ++ changePackage changesFile ++ "-" ++ show (changeVersion changesFile) ++ 
                         " failed to install in " ++ releaseName' (changeRelease changesFile))
              body = concat (map (lines . explainError) (resultToProblems error)) in
          (subject, body)
      --dists = map ReleaseName $ findValues flags "Create"
      expire = findBool flags "Expire"
      cleanUp = findBool flags "Clean-Up"
      install = findBool flags "Install" || (remove == [] && cleanUp == False)
      --replace = findBool flags "Replace"
      --verify = findBool flags "Verify"
      signRepository = findBool flags "Sign"
      keyname = 
          case (findValues flags "Key-Name", findBool flags "No-Sign") of
            (["none"], _) -> Nothing
            (_, True) -> Nothing
            ([], False) -> Just Extra.GPGSign.Default
            (x:[], False) -> Just (Extra.GPGSign.Key x)
            (_:_, False) -> error "Too many Key-Name values."
      parseEmail s = case break (== '@') s of
                       (user, ('@' : host)) -> Just (user, host)
                       _ -> Nothing


createReleases flags =
    do repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       releases <- findReleases repo
       mapM_ (createRelease repo (archList flags)) (map parseReleaseName . findValues flags $ "Create-Release")
       mapM_ (createAlias repo) (findValues flags "Create-Alias")
       mapM_ (createSectionOfRelease releases repo) (findValues flags "Create-Section")
    where
      createSectionOfRelease releases repo arg =
          case break (== ',') arg of
            (relName, ',' : sectName) ->
                case filter (\ release -> releaseName release == parseReleaseName relName) releases of
                  [release] -> createSection repo release (parseSection' sectName)
                  [] -> error $ "createReleases: Invalid release name: " ++ relName
                  _ -> error "Internal error 1"
            _ ->
                error $ "Invalid argument to --create-section: " ++ arg
      createSection :: CIO m => LocalRepository -> Release -> Section -> AptIOT m Release
      createSection repo release section =
          case filter ((==) section) (releaseComponents release) of
            [] -> prepareRelease repo (releaseName release) (releaseInfoAliases . releaseInfo $ release) 
                    (releaseComponents release ++ [section])  (releaseArchitectures release)
            _ -> return release

root flags = EnvPath (EnvRoot "") (case findValues flags "Root" of 
                                     [] -> defaultRoot
                                     [root] -> root
                                     roots -> error $ "Multiple roots: " ++ show roots)

archList flags = maybe defaultArchitectures (map Binary . words) $ findValue flags "Architectures"

layout flags =
    case findValue flags "Layout" of
      Just "pool" -> Pool
      Just "flat" -> Flat
      Just x -> error ("Unknown layout: " ++ x ++ "(use 'pool' or 'flat')")
      Nothing -> Pool

createRelease :: CIO m => LocalRepository -> [Arch] -> ReleaseName -> AptIOT m Release
createRelease repo archList name =
    do releases <- findReleases repo
       case filter (\release -> elem name (releaseName release : (releaseInfoAliases . releaseInfo) release)) releases of
         [] -> prepareRelease repo name [] [parseSection' "main"] archList
         [release] -> return release
         _ -> error "Internal error 2"

createAlias :: CIO m => LocalRepository -> String -> AptIOT m Release
createAlias repo arg =
    case break (== '=') arg of
      (relName, ('=' : alias)) ->
          do releases <- findReleases repo
             case filter ((==) (parseReleaseName relName) . releaseName) releases of
               [] -> error $ "Attempt to create alias in non-existant release: " ++ relName
               [release] -> 
                   case elem (parseReleaseName alias) ((releaseInfoAliases . releaseInfo) release) of
                     False -> prepareRelease repo (parseReleaseName relName) ((releaseInfoAliases . releaseInfo) release ++ [parseReleaseName alias])
                               (releaseComponents release) (releaseArchitectures release)
                     True -> return release
               _ -> error $ "Internal error 3"
      _ -> error $ "Invalid argument to --create-alias: " ++ arg

exitOnError :: CIO m => [InstallResult] -> m ()
exitOnError [] = return ()
exitOnError errors =
    do vPutStrBl 0 (showErrors errors)
       liftIO $ IO.hFlush IO.stderr
       liftIO $ exitWith (ExitFailure 1)

-- |Return the list of releases in the repository at root, creating
-- the ones in the dists list with the given components and
-- architectures.
getReleases root layout dists section archList =
    do repo <- prepareLocalRepository root layout
       existingReleases <- findReleases repo
       requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section archList) dists
       return $ mergeReleases (existingReleases ++ requiredReleases)

deletePackages releases flags keyname =
    do let toRemove = map (parsePackage releases) $ findValues flags "Remove"
       deleteSourcePackages keyname toRemove
    where             
      parsePackage :: [Release] -> String -> PackageIDLocal
      -- Parse a string in the form <dist>,<packagename>=<versionnumber>
      parsePackage releases s =
          case splitRegex (mkRegex "[,=]") s of
            [dist, component, name, version] ->
                maybe (error ("Can't find release: " ++ dist))
                      (\ release -> PackageID (PackageIndex release (parseSection' component) Source) name (parseDebianVersion version))
                      (findReleaseByName releases (parseReleaseName dist)) 
            x -> error ("Invalid remove spec: " ++ show x)
      findReleaseByName :: [Release] -> ReleaseName -> Maybe Release
      findReleaseByName releases dist =
          case filter (\ release -> releaseName release == dist) releases of
            [] -> Nothing
            [release] -> (Just release)
            _ -> error ("Multiple releases with name " ++ releaseName' dist)
