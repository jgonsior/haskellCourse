import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

reverseList :: [a] -> [a]
reverseList []     = []
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


squash :: [String] -> String
squash [x]    = x
squash (x:xs) = x ++ (squash xs)

combineContent :: FilePath -> String -> IO ()
combineContent inputDirectory outputFile = do
  files <- getDirectoryContents inputDirectory
  let correctedFilePath = map ((inputDirectory ++ "/") ++ )
                  (filter (/= "..") (filter (/= ".") files))
  content <- mapM readFile correctedFilePath
  print content
  writeFile outputFile (squash content)
  return ()

combineContentWithHandle :: FilePath -> String -> IO ()
combineContentWithHandle inputDirectory outputFile = do
  files <- getDirectoryContents inputDirectory
  let correctedFilePath = map ((inputDirectory ++ "/") ++ )
                  (filter (/= "..") (filter (/= ".") files))
  withFile outputFile WriteMode (\handle -> do
    mapM (hPutStrLn handle) . ((mapM (readFile) correctedFilePath)))
    -- content <- (mapM (readFile) correctedFilePath)
    -- mapM (hPutStrLn handle) content)
  return ()

{-
main :: IO ()
main = do
  [filePath, suffix] <- getArgs
  bulkRename filePath suffix

main :: IO ()
main = do
  input <- getLine
  putStrLn (reverse input)
-}
