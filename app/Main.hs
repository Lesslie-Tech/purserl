module Main where

import Prelude

import Command.Compile qualified as Compile
import Command.Graph qualified as Graph
import Command.Hierarchy qualified as Hierarchy
import Command.Ide qualified as Ide
import Control.Monad (join)
import Data.Foldable (fold)
import Options.Applicative qualified as Opts
import System.Environment (getArgs)
import System.IO qualified as IO
import Text.PrettyPrint.ANSI.Leijen qualified as Doc
import Version (versionString)


main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering
    join $ Opts.handleParseResult . execParserPure opts =<< getArgs
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "The PureScript compiler and tools"
    footerInfo  = Opts.footerDoc (Just footer)

    footer =
      mconcat
        [ para $
            "For help using each individual command, run `purs COMMAND --help`. " ++
            "For example, `purs compile --help` displays options specific to the `compile` command."
        , Doc.hardline
        , Doc.hardline
        , Doc.text $ "purs " ++ versionString
        ]

    para :: String -> Doc.Doc
    para = foldr (Doc.</>) Doc.empty . map Doc.text . words

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo (Opts.ShowHelpText Nothing) mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands =
      (Opts.subparser . fold)
        [ Opts.command "compile"
            (Opts.info Compile.command
              (Opts.progDesc "Compile PureScript source files"))
        , Opts.command "graph"
            (Opts.info Graph.command
              (Opts.progDesc "Module dependency graph"))
        , Opts.command "hierarchy"
            (Opts.info Hierarchy.command
              (Opts.progDesc "Generate a GraphViz directed graph of PureScript type classes"))
        , Opts.command "ide"
            (Opts.info Ide.command
              (Opts.progDesc "Start or query an IDE server process"))
        ]
