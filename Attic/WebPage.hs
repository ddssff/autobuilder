-- |Web interface to control autobuilder.
module Main where


import		 Control.Monad
import		 Control.Monad.Trans(lift, liftIO)	-- required despite warning
import		 Data.List
import		 Data.Maybe
import		 Debian.Control
import		 Debian.Repo.Slice ()
import 		 Debian.Repo.IO (AptIOT, runAptIO)
import		 Debian.Repo.Slice (sourceSlices, releaseSlices, parseNamedSliceList')
import		 Debian.Repo.Types (EnvRoot, Arch(Source, Binary),
                                    ReleaseName(ReleaseName), parseReleaseName, rootPath,
                                    SliceName(SliceName), Slice(sliceSource), NamedSliceList, 
                                    sliceList, sliceName, sliceListName, slices,
	 			    SourceType(Deb, DebSrc), DebSource(DebSource))
import		 Debian.Repo.Cache (cacheRootDir, archFiles)
import		 Debian.URI
import		 Extra.TIO
import qualified MyHtml
import qualified Network.CGI as CGI
import qualified Debian.AutoBuilder.ParamClass as P
--import qualified Debian.AutoBuilder.OldParams as O
import		 System.Directory
import		 System.Environment
import		 System.IO
import		 Text.Regex
import		 Text.XHtml.Transitional

main :: IO ()
main = CGI.runCGI (CGI.handleErrors $ cgiMain)

cgiMain :: CGI.CGI CGI.CGIResult
cgiMain = 
    do
      name <- CGI.scriptName
      inputs <- CGI.getInputs
      return (("SCRIPT_NAME", name) : inputs) >>= liftIO . runTIO defStyle . runAptIO . application >>= CGI.output

application :: CIO m => [(String,String)] -> AptIOT m String
application cgivars =
    do -- Use the same application name as the autobuilder so we
       -- see the same configuration files.
      paramSets <- O.params appName ["web-config"] undefined undefined
      case paramSets of
        (params : _) ->
            do html <-
                   case lookup "page" cgivars of
                     Nothing -> liftIO $ topPage params cgivars
                     --Just "params" -> liftIO $ flagPage params
                     Just "env" -> liftIO $ envPage
                     Just "dist" -> distPage params cgivars
                     Just "source-package" -> sourcePackagePage params cgivars
                     Just "binary-package" -> liftIO $ binaryPackagePage params cgivars
                     Just "apt-get-update" -> liftIO $ aptGetUpdate params cgivars
                     Just page -> liftIO $ errorPage params cgivars page
               return $ show $ concatHtml [heading params cgivars, html]

appName :: String
appName = "autobuilder"

aptGetUpdate :: P.RunClass p => p -> [(String,String)] -> IO Html
aptGetUpdate params cgivars =
    do
      let dist = parseReleaseName . fromJust . lookup "dist" $ cgivars
      let dir = P.cleanRootOfRelease params dist
      let cmd = "sudo chroot " ++ rootPath dir ++ " apt-get update"
      -- Can't do this until we convince Apache it is ok
      -- (output, code) <- My.processOutput cmd
      -- return $ pre (stringToHtml (cmd ++ "\n" ++ output ++ "\n" ++ show code))
      return $ stringToHtml cmd

heading :: P.ParamClass p => p -> [(String,String)] -> Html
heading params cgivars =
    let nav = td (concatHtml topNav) in
    let info = td (concatHtml [stringToHtml "Upload URI: ", stringToHtml (maybe "None" show (P.uploadURI params))])
               ! [strAttr "align" "right"] in
    (table . tr . concatHtml) [nav, info] ! [strAttr "width" "100%"]
    where
      topNav = [MyHtml.linkTo cgivars (stringToHtml "Top") []] ++ pageNav
      pageNav =
          case lookup "page" cgivars of
            Just "dist" ->
                case lookup "dist" cgivars of
                  Nothing -> []
                  Just dist ->
                      [stringToHtml " > ", linkToDist cgivars dist]
            _ -> []

topPage :: P.ParamClass p => p -> [(String,String)] -> IO Html
topPage params cgivars =
    do dists <- runTIO defStyle (runAptIO (distros params)) >>= return . map (sliceName . sliceListName)
       return (concatHtml
               [h3 (stringToHtml "Dists"),
                ulist (concatHtml (map (li . linkToDist cgivars . show) dists)),
	        h3 (MyHtml.linkTo cgivars (stringToHtml "Parameters") [("page", "params")]),
	        h3 (MyHtml.linkTo cgivars (stringToHtml "Environment") [("page", "env")])])

{-
flagPage :: P.ParamClass p => p -> IO Html
flagPage params =
    return (concatHtml 
            [h3 (stringToHtml "Parameters"),
             -- FIXME: format as html, replace newlines with <br>, etc
             ulist (concatHtml (map (li . stringToHtml . show) (Map.assocs (P.flags params))))])
-}

envPage :: IO Html
envPage =
    do
      env <- getEnvironment
      return (concatHtml 
              [h3 (stringToHtml "Environment"),
               -- FIXME: format as html, replace newlines with <br>, etc
               ulist (concatHtml (map (li . stringToHtml . show) env))])

{-
environment :: IO Html
environment =
    do
      env <- getEnvironment
      return $ concatHtml (map (\ (name, value) -> concatHtml [stringToHtml (name ++ "=" ++ value), br]) env)
-}

distPage :: CIO m => P.RunClass p => p -> [(String,String)] -> AptIOT m Html
distPage params cgivars =
    do distro <- distros params >>= return . find (isDist dist)
       case distro of
         Nothing ->
             error ("Unknown dist: " ++ sliceName dist)
         Just distro ->
             do (Control sourcePackages) <- sourcePackageInfo root distro
                return (form
                        (concatHtml
                         [input ! [strAttr "type" "submit",  strAttr "name" "page", strAttr "value" "apt-get-update"],
                          hidden "dist" (sliceName dist),
                          h3 (center (stringToHtml ("ReleaseName " ++ sliceName dist ++ " source packages"))),
		          sourcePackageTable sourcePackages,
                          h3 (stringToHtml ("ReleaseName " ++ sliceName dist ++ " - Sources")),
                          ulist (concatHtml (map (li . showSource params) (map sliceSource (slices . sliceList $ distro)))),
		          h3 (stringToHtml "Binary Package Lists"),
                          ulist (concatHtml (map (li . stringToHtml . ((rootPath root) ++))
                                             (concat (map (archFiles (Binary "i386"))
                                                      (map sliceSource (slices . sliceList $ distro)))))),
		          h3 (stringToHtml "Source Package Lists"),
                          ulist (concatHtml (map (li . stringToHtml . ((rootPath root) ++))
                                             (concat (map (archFiles Source)
                                                      (map sliceSource (slices . sliceList $ distro))))))]))
    where
      sourcePackageTable packages =
          let rows = map (tr . sourcePackageHtml) (sortBy cmpPackages packages) in
          table (concatHtml (tr (concatHtml (map (th . stringToHtml) ["name", "version", "binary"])) :
                             rows)) ! MyHtml.thinborder
      sourcePackageHtml info =
          concatHtml [(td . linkToSourcePackage . fromJust) (fieldValue "Package" info),
                      (td . stringToHtml . fromJust) (fieldValue "Version" info),
                      (td . concatHtml . intersperse br . map linkToBinaryPackage . splitCommaList . fromJust) (fieldValue "Binary" info)]
      linkToSourcePackage name =
          MyHtml.linkTo cgivars (stringToHtml name) [("page", "source-package"), ("package", name), ("dist", sliceName dist)]
      linkToBinaryPackage name =
          MyHtml.linkTo cgivars (stringToHtml name) [("page", "binary-package"), ("package", name), ("dist", sliceName dist)]
      splitCommaList s = splitRegex (mkRegex "[, ]+") s
      cmpPackages a b = compare (fieldValue "Package" a) (fieldValue "Package" b)
      dist = maybe (error "No dist name") SliceName (lookup "dist" cgivars)
      root = cacheRootDir (P.topDir params) (either (error . show) (ReleaseName . sliceName . sliceListName) (P.findSlice params dist))
      isDist dist distro = dist == sliceListName distro

sourcePackagePage :: CIO m => P.RunClass p => p -> [(String, String)] -> AptIOT m Html
sourcePackagePage params cgivars =
    do
      let package = fromJust (lookup "package" cgivars)
      let dist = maybe (error "No dist name") SliceName (lookup "dist" cgivars)
      let distro = either (error . show) id (P.findSlice params dist)
      let root = cacheRootDir (P.topDir params) (ReleaseName . sliceName . sliceListName $ distro)
      (Control control) <- sourcePackageInfo root distro
      let (Paragraph info) = fromJust (find (\ info -> fieldValue "Package" info == Just package) control)
      return $ concatHtml (intersperse br (map (stringToHtml . show) info))

binaryPackagePage :: P.ParamClass p => p -> [(String, String)] -> IO Html
binaryPackagePage _ cgivars =
    let package = fromJust (lookup "package" cgivars) in
    return $ stringToHtml ("binaryPackagePage: " ++ package)

sourcePackageInfo :: CIO m => EnvRoot -> NamedSliceList -> AptIOT m Control
sourcePackageInfo root distro =
    do
      lift (vPutStrBl 0 ("sourcePackageFiles: " ++ show sourcePackageFiles))
      filterM (liftIO . doesFileExist) sourcePackageFiles >>=
              mapM (liftIO . parseControlFromFile) >>=
              return . map (either (\ e -> error (show e)) id) >>=
              return . mergeControls
    where
      sourcePackageFiles = map ((rootPath root) ++) $ concat (map (archFiles Source) (map sliceSource . slices $ sourceSources))
      sourceSources = sourceSlices uploadSources
      uploadSources = releaseSlices dist (sliceList distro)
      dist = parseReleaseName (sliceName . sliceListName $ distro)

showSource :: P.ParamClass p => p -> DebSource -> Html
showSource params src@(DebSource DebSrc uri _)
    | Just uri == (P.uploadURI params) =
        concatHtml [stringToHtml "uploadable source: ", (stringToHtml . show) src]
showSource params src@(DebSource Deb uri _)
    | Just uri == (P.uploadURI params) =
        concatHtml [stringToHtml "uploadable binary: ", (stringToHtml . show) src]
showSource _ src = (stringToHtml . show) src

errorPage :: P.ParamClass p => p -> [(String,String)] -> String -> IO Html
errorPage _ _ page =
    return (concatHtml
            [h3 (stringToHtml ("Unknown page type: " ++ page))])

distros :: (CIO m, P.ParamClass p) => p -> AptIOT m [NamedSliceList]
distros params = mapM parseNamedSliceList' (P.sources params)

linkToDist :: [(String,String)] -> String -> Html
linkToDist cgivars dist =
    MyHtml.linkTo cgivars (stringToHtml dist) [("page", "dist"), ("dist", dist)]

uriHost :: URI -> Maybe String
uriHost uri = maybe Nothing (Just . uriRegName) (uriAuthority uri)
