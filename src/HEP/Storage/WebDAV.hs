-- | 
--   Currently, id and password use only  .wgetrc 
--

module HEP.Storage.WebDAV ( 
  module HEP.Storage.WebDAV.Type, 
  fetchFile, 
  uploadFile
  ) where

import HEP.Storage.WebDAV.Type

import System.Process
import System.FilePath

fetchFile :: WebDAVConfig 
             -> WebDAVRemoteDir 
             -> FilePath         -- ^ remote file name
             -> IO ()   
fetchFile wdavc rdir filename = do 
  readProcess (webdav_path_wget wdavc) 
              [ webdav_baseurl wdavc </> webdav_remotedir rdir 
                </> filename ]
              "" 
  return ()

uploadFile :: WebDAVConfig
              -> WebDAVRemoteDir
              -> FilePath          -- ^ local file name
              -> IO () 
uploadFile wdavc rdir filepath = do 
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
mkCadaverScript wdavc rdir filepath Download = undefined 
mkCadaverScript wdavc rdir filepath Upload = 
  let (dirpath,filename) = (takeDirectory filepath, takeFileName filepath) 
  in  "open " ++ (webdav_baseurl wdavc </> webdav_remotedir rdir) ++ "\n"
      ++ "lcd " ++ dirpath ++ "\n"  
      ++ "put " ++ filename ++ "\n"
