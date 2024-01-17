module Main where

import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist, getModificationTime)
import Control.Monad (filterM)
import System.FilePath (combine, takeFileName, normalise)
import System.IO (hPutStrLn, stderr)
import Data.Time (UTCTime, utctDay)
import Data.Time.Format (readTime, defaultTimeLocale)
import Matching (matches)

-- Получить содержимое данной папки
listContents :: FilePath -> IO [FilePath]
listContents path = do
    allContents <- getDirectoryContents path
    let realContents = filter (not . (`elem` [".", ".."])) allContents
    let fullContents = map (normalise . combine path) realContents
    return fullContents

-- Получить содержимое данной папки, её подпапок и.т.д.
listRecursiveContents :: FilePath -> IO [FilePath]
listRecursiveContents path = do
    contents <- listContents path
    subdirectories <- filterM doesDirectoryExist contents
    recursiveContentLists <- mapM listRecursiveContents subdirectories
    let recursiveContents = concat (contents : recursiveContentLists)
    return recursiveContents

-- Параметры
data Options = Options {
    pattern :: Maybe String
    ,dir :: Maybe FilePath
    ,recursive :: Bool
    ,time :: Maybe UTCTime }

-- Изначальные параметры
defaultOptions = Options {
    pattern = Nothing
    ,dir = Nothing
    ,recursive = False
    ,time = Nothing }

-- Преобразовать строку типа 12-12-2012 в дату
parseTime :: String -> UTCTime
parseTime = readTime defaultTimeLocale "%d-%m-%Y"

-- Считать следующую опцию из списка
-- Опция -path дублирует аргумент, отвечающий за директорию поиска, позволяя писать
-- запросы вида filesearch -path subdir -date 12-12-2012
updateOptions :: [String] -> Options -> Maybe Options
updateOptions ("-r":xs) options = updateOptions xs options { recursive = True }
updateOptions ("-date":t:xs) options = updateOptions xs options { time = Just (parseTime t) }
updateOptions ("-path":d:xs) options = updateOptions xs options { dir = Just d }
updateOptions (x:xs) options
    | pattern options == Nothing = updateOptions xs options { pattern = Just x }
    | dir options == Nothing = updateOptions xs options { dir = Just x }
updateOptions [] options = Just options
updateOptions _ _ = Nothing

-- Считать шаблон имени файла из консоли, если не было предоставлено
-- ни шаблона, ни даты
inputPattern :: Options -> IO Options
inputPattern options@Options { pattern = Nothing, time = Nothing } = do
    p <- getLine
    return options { pattern = Just p }
inputPattern options = return options

-- Вернуть функцию, выдающую содержимое папки
getListFunction :: Options -> FilePath -> IO [FilePath]
getListFunction Options { recursive = True } path = listRecursiveContents path >>= filterM doesFileExist
getListFunction _ path = listContents path >>= filterM doesFileExist

-- Вернуть папку для поиска
getSearchDir :: Options -> FilePath
getSearchDir Options { dir = Nothing } = "."
getSearchDir Options { dir = Just d } = d

-- Вернуть фунцию, фильтрирующую по имени
filterByPattern :: Maybe String -> FilePath -> Bool
filterByPattern Nothing _ = True
filterByPattern (Just pattern) path = matches pattern (takeFileName path)

-- Вернуть функцию, фильтрующую по времени изменения
filterByTime :: Maybe UTCTime -> FilePath -> IO Bool
filterByTime Nothing _ = return True
filterByTime (Just time) path = do
    modificationTime <- getModificationTime path
    return (utctDay modificationTime == utctDay time)

-- Вернуть фильтровочную функцию для поиска
getFilterFunction :: Options -> FilePath -> IO Bool
getFilterFunction Options { pattern = p, time = t } path = do
    timeMatched <- filterByTime t path
    return (filterByPattern p path && timeMatched) 

-- Вывести список файлов в консоль
output :: [FilePath] -> IO ()
output = mapM_ putStrLn

main = do
    args <- getArgs
    runWith (updateOptions args defaultOptions)
        where runWith Nothing = do
                    hPutStrLn stderr "Usage: filesearch [pattern [path]] [options]"
                    hPutStrLn stderr "-r: recursive\n-date: filter by change date\n-path: directory to search"
              runWith (Just options) = do
                    options <- inputPattern options
                    let listFunction = getListFunction options
                    let filterFunction = getFilterFunction options
                    contents <- listFunction (getSearchDir options)
                    matchedFiles <- filterM filterFunction contents
                    output matchedFiles

