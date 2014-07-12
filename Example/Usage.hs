module Usage ( usage ) where

import System.Console.GetOpt

-- Intermediate type used while formatting the usage info.
data DescrLine
    = Opt { long :: String, short :: String }
    | Text String

-- |Modified version of System.Console.GetOpt.usageInfo, avoids printing
-- such wide lines.
usage :: String		-- header
          -> [OptDescr a]	-- parameter descriptor
          -> String		-- nicely formatted decription of options
usage header params =
    unlines (header:table)
    where table = map fmtLine xs
          fmtLine (Text s) = "    " ++ s
          fmtLine (Opt {long = ls, short = ss}) =
              "  " ++
              flushLeft lsl ls ++ "  " ++
              flushLeft ssl ss
          xs = legend ++ concatMap fmtOpt params
          ssl = foldl max 0 (map ss xs)
          lsl = foldl max 0 (map ls xs)
          -- The length of a short option
          ss opt@(Opt _ _) = length (short opt)
          ss _ = 0
          -- The length of a long option
          ls opt@(Opt _ _) = length (long opt)
          ls _ = 0
          flushLeft n x = take n (x ++ repeat ' ')
          legend = [Opt {long = "Long option", short = "Short option"},
                    Opt {long = "-----------", short = "------------"}]

fmtOpt :: OptDescr a -> [DescrLine]
fmtOpt paramDescr@(Option sos los ad descr) =
   let ds = [Text ""] ++ map Text (lines descr) ++ [Text ""]
       ss = map (fmtShort ad) sos
       ls = map (fmtLong  ad) los in
   let n = max (length ls) (length ss) in
   let ss' = ss ++ replicate (n - length ss) ""
       ls' = ls ++ replicate (n - length ls) "" in
       -- ps' = ps ++ replicate (n - length ps) "" in
   map (\ (l, s) -> Opt {long = l, short = s}) (zip ls' ss') ++ ds

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _ ad) so = "-" ++ [so] ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ [so] ++ "[" ++ ad ++ "]"

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

fmtParam :: ArgDescr a -> String -> String
fmtParam (NoArg  _   ) po = po ++ ": Yes"
fmtParam (ReqArg _ ad) po = po ++ ": " ++ ad
fmtParam (OptArg _ ad) po = po ++ ": " ++ ad
