-- | 
--   Currently, id and password use only  .wgetrc 
--

module HEP.Storage.WebDAV ( 
  module HEP.Storage.WebDAV.Type, 
  fetchFile, 
  uploadFile
  ) where

import HEP.Storage.WebDAV.Type

import System.Directory
import System.Process
import System.FilePath


-- | deprecated

fetchFile :: WebDAVConfig 
             -> WebDAVRemoteDir 
             -> FilePath         -- ^ remote file name
             -> IO ()   
fetchFile = downloadFile

data URLtype = LocalURL FilePath
             | GlobalURL String

checkUrl :: String -> Maybe URLtype
checkUrl str = 
  if length str > 6 
  then let (method,path)= splitAt 6 str 
       in case method of 
            "file://" -> Just (LocalURL path)
            "http://" -> Just (GlobalURL str)
            _ -> Nothing
  else Nothing
                   


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

uploadFile :: WebDAVConfig
              -> WebDAVRemoteDir
              -> FilePath          -- ^ local file name
              -> IO () 
uploadFile wdavc rdir filepath = do
  let r_url = checkUrl (webdav_baseurl wdavc)
  case r_url of
    Nothing -> error ("no such url : " ++ webdav_baseurl wdavc)
    Just (LocalURL path) -> do  
      let remotedir = path </> webdav_remotedir rdir 
          (_,localfile) = splitFileName filepath 
      putStrLn $ "copy " ++ filepath ++ " to " ++ (remotedir</>localfile)
      copyFile filepath (remotedir</>localfile)
      return ()
    Just (GlobalURL _url) -> do  
      let scriptstr = mkCadaverScript wdavc rdir filepath Upload
      putStrLn scriptstr 
      result <- readProcess (webdav_path_cadaver wdavc) [] scriptstr
      putStrLn result 
      return () 
  
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
