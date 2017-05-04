import System.FilePath
import System.Directory
import System.Environment

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

rename :: FilePath -> String -> IO ()
rename a filePath = do
  renameFile filePath (filePath ++ a)


bulkRename :: FilePath -> String -> IO ()
bulkRename filePath suffix = do
  files <- getDirectoryContents filePath
  let correctedFilePath = map ((filePath ++ "/") ++ )
                  (filter (/= "..") (filter (/= ".") files))
  mapM (rename suffix) correctedFilePath
  return ()

main :: IO ()
main = do
  [filePath, suffix] <- getArgs
  bulkRename filePath suffix
{-
main :: IO ()
main = do
  input <- getLine
  putStrLn (reverse input)
-}
