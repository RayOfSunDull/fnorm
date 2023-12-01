import qualified Data.Char as Character
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit

getFilename :: [String] -> Maybe String
getFilename [] = Maybe.Nothing
getFilename (s:_) = Maybe.Just s

parseWord :: String -> String -> String
parseWord buffer "" = reverse buffer
parseWord buffer (c:cs)
    | Character.isAlphaNum c || c == '.' = parseWord (c:buffer) cs
    | Character.isSpace c = parseSpace buffer cs
    | c == '-' = parseDash buffer cs
    | otherwise = parseWord buffer cs

parseSpace :: String -> String -> String
parseSpace buffer "" = reverse buffer
parseSpace buffer (c:cs)
    | Character.isAlphaNum c || c == '.' = parseWord (c:'-':buffer) cs
    | c == '-' = parseDash buffer cs
    | otherwise = parseSpace buffer cs

parseDash :: String -> String -> String
parseDash buffer "" = reverse buffer
parseDash buffer (c:cs)
    | Character.isAlphaNum c || c == '.' = parseWord (c:'_':buffer) cs
    | otherwise = parseDash buffer cs


normalizeFilename :: String -> String
normalizeFilename (c:cs)
    | Character.isAlphaNum c = parseWord [c] cs
    | otherwise = parseSpace "" cs


main = do
    args <- Environment.getArgs

    let maybeFilename = getFilename args
    Monad.when (Maybe.isNothing maybeFilename) (do
        putStrLn "Error: No filename provided."
        Exit.exitWith (Exit.ExitFailure 1))

    let filename = Maybe.fromJust maybeFilename
        normalizedFilename = normalizeFilename filename

    fileExists <- Directory.doesFileExist filename
    if fileExists
    then Directory.renameFile filename normalizedFilename
    else do
        directoryExists <- Directory.doesDirectoryExist filename
        if directoryExists
        then Directory.renameDirectory filename normalizedFilename
        else do
            putStrLn ("Error: " ++ filename ++ " is not a file or directory.")
            Exit.exitWith (Exit.ExitFailure 1)

    Exit.exitWith Exit.ExitSuccess
