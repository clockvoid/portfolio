-- \|
--   Lucidaを使ってコンパイルしたHTMLを所定のフォルダに書き出すためのコマンドを生成する．
--   引数のパースには[optparser-applicative](https://hackage.haskell.org/package/optparse-applicative)を使った．
--   また，このプログラムは<https://github.com/jaspervdj/hakyll/blob/master/lib/Hakyll/Main.hs>を参考にして作成した．

import Html.Lib (compile, deleteAllFilesInDirectory, deployDirectory, pages, templatesDirectory)
import qualified Options.Applicative as OA
import System.Environment (getProgName)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import System.Process

-- | コマンドを表現する直積型
data Command
  = Clean
  | Build
  deriving (Eq, Show)

-- | 実際にマッチする文字列を探す関数
commandParser :: OA.Parser Command
commandParser = OA.subparser $ foldr ((<>) . produceCommand) mempty commands
  where
    produceCommand (text, command, description) = OA.command text (OA.info command description)

    -- \|
    --        コマンドを表現するトリプルのリスト．
    --        要素は(期待する引数 :: String, コマンド型のApplicative :: OA.Parser Command, コマンドの説明 :: OA.InfoMod String
    --
    commands =
      [ ( "clean",
          pure Clean,
          OA.fullDesc <> OA.progDesc "clean renderd HTML."
        ),
        ( "build",
          pure Build,
          OA.fullDesc <> OA.progDesc "build HTML and render to file."
        )
      ]

-- | optparser-applicativeの設定と表示する情報を付与する
defaultParser :: IO Command
defaultParser = OA.customExecParser defaultParserPrefs defaultParserInfo

-- | パースに失敗したらヘルプを表示する設定
defaultParserPrefs :: OA.ParserPrefs
defaultParserPrefs = OA.prefs OA.showHelpOnError

-- | 使用するパーサと，表示する情報を設定
defaultParserInfo :: OA.ParserInfo Command
defaultParserInfo = OA.info parser info
  where
    parser = OA.helper <*> commandParser
    info = OA.fullDesc <> OA.progDesc (progName ++ "- Static HTML compiler created with Lucida")

-- | Haskellでは$0にプログラムの名前が流れてこないため，unsafeな関数を使ってプログラムの名前を見る
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}

invokCommand :: IO ExitCode
invokCommand = do
  arg <- defaultParser
  case arg of
    Clean -> do
      deleteAllFilesInDirectory $ deployDirectory <> templatesDirectory
      deleteAllFilesInDirectory deployDirectory
    Build -> do
      putStrLn =<< readProcess "npm" ["install"] ""
      putStrLn =<< readProcess "cp" ["-r", "node_modules/@fortawesome/fontawesome-free/webfonts", "_site/"] ""
      compile deployDirectory pages

main :: IO ()
main = invokCommand >>= exitWith
