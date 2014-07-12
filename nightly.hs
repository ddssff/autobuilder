#!/usr/bin/runhaskell

import Control.Monad
import System.Cmd
import System.Exit
import System.IO

logdir = "/home/david/darcs/autobuilder/logs"
dists = ["hardy", "sid", "intrepid", "lenny"]
arches = [("i386", "192.168.0.2"), ("amd64", "192.168.0.12")]

cmd dist arch host = 
    ("ssh -o 'PreferredAuthentications hostbased,publickey' root@" ++ host ++
     " autobuilder --use-repo-cache --flush-pool " ++ dist ++
     "-build --use all-targets --do-upload --do-newdist " ++
     "> " ++ logdir ++ "/`date +%F-%T-%Z`-" ++ dist ++ "-" ++ arch)
     -- logdir ++ "/" ++ date ++ "-" ++ dist ++ " 2>&1")

main =
    foldM f ExitSuccess cmds
    where
      f code@(ExitFailure _) _ = return code
      f ExitSuccess cmd = run cmd
      run s = hPutStrLn stderr ("# " ++ s) >> system s
      cmds = concatMap (\ dist -> map (\ (arch, host) -> cmd dist arch host) arches) dists
