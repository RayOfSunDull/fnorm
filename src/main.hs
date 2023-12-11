import qualified Data.Char as Character
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit

getFilename :: [String] -> Maybe String
getFilename [] = Maybe.Nothing
getFilename (s:_) = Maybe.Just s

isDash = \c -> c == '-' || c == '_'

parseWord :: String -> String -> String
parseWord buffer "" = reverse buffer
parseWord buffer (c:cs)
    | Character.isAlphaNum c || c == '.' = parseWord (c:buffer) cs
    | Character.isSpace c = parseSpace buffer cs
    | isDash c = parseDash buffer cs
    | otherwise = parseWord buffer cs

parseSpace :: String -> String -> String
parseSpace buffer "" = reverse buffer
parseSpace buffer (c:cs)
    | Character.isAlphaNum c || c == '.' = parseWord (c:'-':buffer) cs
    | isDash c = parseDash buffer cs
    | otherwise = parseSpace buffer cs

parseDash :: String -> String -> String
parseDash buffer "" = reverse buffer
parseDash buffer (c:cs)
    | Character.isAlphaNum c || c == '.' = parseWord (c:'_':buffer) cs
    | isDash c = parseDash buffer cs
    | otherwise = parseDash buffer cs


normalizeFilename :: String -> String
normalizeFilename (c:cs)
    | Character.isAlphaNum c = parseWord [c] cs
    | isDash c = parseDash "" cs
    | otherwise = parseSpace "" cs


normalizeSingleFile :: String -> IO String
normalizeSingleFile filename = do
    let normalizedFilename = normalizeFilename filename

    fileExists <- Directory.doesFileExist filename
    if fileExists then do
        Directory.renameFile filename normalizedFilename
        return ""
    else do
        directoryExists <- Directory.doesDirectoryExist filename
        if directoryExists then do
            Directory.renameDirectory filename normalizedFilename
            return ""
        else
            return $ "Error: " ++ filename ++ " is not a file or directory."

checkError :: String -> IO ()
checkError "" = Exit.exitWith Exit.ExitSuccess
checkError errorMessage = do
    putStrLn errorMessage
    Exit.exitWith (Exit.ExitFailure 1)

main = do
    args <- Environment.getArgs

    let maybeFilename = getFilename args
    Monad.when (Maybe.isNothing maybeFilename) (do
        putStrLn "Error: No filename provided."
        Exit.exitWith (Exit.ExitFailure 1))

    let filename = Maybe.fromJust maybeFilename
    output <- normalizeSingleFile filename

    checkError output
