{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ( (<*>) )
import Data.Monoid ( (<>) )
import qualified Data.Text.Lazy.IO as T
import Options.Applicative ( Parser, execParser, long, help, switch, progDesc
                           , fullDesc, helper, header, info, short )

import Math.Automata.Simple ( subsetConstruction, faToDot, textToNFA
                            , flattenDFAStates )

type Config = Bool

parseConfig :: Parser Config
parseConfig = switch subset
  where
    subset =
      long "subsetConstruction"
      <> short 's'
      <> help "Whether to perform subset construction (determinisation)"

go :: Bool -> IO ()
go wantSubsetConstruction = do
    input <- T.getContents
    case textToNFA input of
        Left err -> error err
        Right nfa -> do
            let dotText =
                    -- We need to duplicate faToDot here, because we can't
                    -- abstract over the f parameter of FA (difference
                    -- between NFA/DFA).
                    if wantSubsetConstruction
                        then faToDot . flattenDFAStates $
                                 subsetConstruction nfa
                        else faToDot $ nfa
            T.putStr dotText

main :: IO ()
main = do
    wantSubsetConstruction <- execParser $ info (helper <*> parseConfig)
                                  (fullDesc <> progDesc desc <> header hdr)
    go wantSubsetConstruction
  where
    desc = "Read input and output DOT formatted NFA, optionally performing"
           ++ " determinisation."
    hdr = "NFA tools: TODO"
