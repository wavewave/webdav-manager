module HEP.Storage.WebDAV ( 
  module HEP.Storage.WebDAV.Type, 
  fetchFile
  ) where

import HEP.Storage.WebDAV.Type

import System.Process
import System.FilePath ((</>))

fetchFile :: WebDAVConfig 
             -> WebDAVRemoteDir 
             -> FilePath         -- ^ remote file name
             -> IO ()   
fetchFile wdavc rdir filename = do 
  readProcess (webdav_path_wget wdavc) 
              [ "--user=" ++ webdav_id wdavc
              , "--password=" ++ webdav_passwd wdavc 
              , webdav_baseurl wdavc </> webdav_remotedir rdir 
                </> filename ]
              "" 
  return ()
