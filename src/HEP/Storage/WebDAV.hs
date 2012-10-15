-- | 
--   Currently, id and password use only  .wgetrc 
--

module HEP.Storage.WebDAV ( 
  module HEP.Storage.WebDAV.Type, 
  fetchFile, 
  uploadFile, 
  checkUrl, 
  checkNdownloadFile
  ) where

import HEP.Storage.WebDAV.Type

import System.Directory
import System.Process
import System.FilePath

import Control.Applicative

-- | deprecated
fetchFile :: WebDAVConfig 
             -> WebDAVRemoteDir 
             -> FilePath         -- ^ remote file name
             -> IO ()   
fetchFile = downloadFile

-- | 
data URLtype = LocalURL FilePath
             | GlobalURL String
             deriving (Show)

-- | 
checkUrl :: String -> Maybe URLtype
checkUrl str = 
  if length str > 6 
  then let (method,path)= splitAt 7 str 
       in case method of 
            "file://" -> Just (LocalURL path)
            "http://" -> Just (GlobalURL str)
            _ -> Nothing
  else Nothing
                   
-- | 
downloadFile :: WebDAVConfig 
             -> WebDAVRemoteDir 
             -> FilePath         -- ^ remote file name
             -> IO ()   
downloadFile wdavc rdir filename = do
  let r_url = checkUrl (webdav_baseurl wdavc)
  case r_url of
    Nothing -> error ("no such url : " ++ webdav_baseurl wdavc)
    Just (LocalURL path) -> do  
      let remotepath = path </> webdav_remotedir rdir </> filename
          (_,remotefile) = splitFileName remotepath 
      currdir <- getCurrentDirectory 
      putStrLn $ "copy " ++ remotepath ++ " to " ++ (currdir </> remotefile)
      copyFile remotepath (currdir </> remotefile)
      return ()
    Just (GlobalURL url) -> do  
      readProcess (webdav_path_wget wdavc) 
                  [ url </> webdav_remotedir rdir 
                        </> filename ]
                  "" 
      return ()

-- | 
checkNdownloadFile :: WebDAVConfig 
                      -> WebDAVRemoteDir
                      -> FilePath
                      -> IO Bool 
checkNdownloadFile wdavc rdir filename = do
  let r_url = checkUrl (webdav_baseurl wdavc)
  case r_url of
    Nothing -> return False -- error ("no such url : " ++ webdav_baseurl wdavc)
    Just (LocalURL path) -> do  
      let remotepath = path </> webdav_remotedir rdir </> filename
          (_,remotefile) = splitFileName remotepath 
      currdir <- getCurrentDirectory 
      putStrLn $ "copy " ++ remotepath ++ " to " ++ (currdir </> remotefile)
      b <- doesFileExist remotepath 
      if b then do {copyFile remotepath (currdir </> remotefile); return True}
           else return False
    Just (GlobalURL url) -> do  
      putStrLn "downloading --- " 
      system $ (webdav_path_wget wdavc) ++ " " ++ (url </> webdav_remotedir rdir </> filename)   
      let (_,newfile) = splitFileName filename
      return =<< doesFileExist newfile

  
-- | 
uploadFile :: WebDAVConfig
              -> WebDAVRemoteDir
              -> FilePath          -- ^ local file name
              -> IO Bool 
uploadFile wdavc rdir filepath = do
  let r_url = checkUrl (webdav_baseurl wdavc)
  case r_url of
    Nothing -> error ("no such url : " ++ webdav_baseurl wdavc)
    Just (LocalURL path) -> do  
      let remotedir = path </> webdav_remotedir rdir 
          (_,localfile) = splitFileName filepath 
      putStrLn $ "copy " ++ filepath ++ " to " ++ (remotedir</>localfile)
      b <- (&&) <$> doesFileExist filepath <*> doesDirectoryExist remotedir
      if b 
        then do
          copyFile filepath (remotedir</>localfile)
          return True
        else
          return False
    Just (GlobalURL _url) -> do  
      let scriptstr = mkCadaverScript wdavc rdir filepath Upload
      putStrLn scriptstr 
      result <- readProcess (webdav_path_cadaver wdavc) [] scriptstr
      putStrLn result 
      return True

-- |   
mkCadaverScript :: WebDAVConfig
                   -> WebDAVRemoteDir
                   -> FilePath 
                   -> WebDAVCommand
                   -> String 
mkCadaverScript _wdavc _rdir _filepath Download = undefined 
mkCadaverScript wdavc rdir filepath Upload = 
  let (dirpath,filename) = (takeDirectory filepath, takeFileName filepath) 
  in  "open " ++ (webdav_baseurl wdavc </> webdav_remotedir rdir) ++ "\n"
      ++ "lcd " ++ dirpath ++ "\n"  
      ++ "put " ++ filename ++ "\n"
