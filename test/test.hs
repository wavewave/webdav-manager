module Main where

import HEP.Storage.WebDAV

wdav = WebDAVConfig { 
  webdav_path_wget = "/usr/bin/wget", 
  webdav_path_cadaver = "/usr/bin/cadaver", 
  webdav_baseurl = "http://susy.physics.lsa.umich.edu:8080/mc"
  }
   
rdir = WebDAVRemoteDir { 
  webdav_remotedir = "test/iwtest"
  }

-- test = fetchFile

main = do 
  putStrLn "haha"
  
  uploadFile wdav rdir "/home/wavewave/nfs/prog/webdav-manager/test/test.hs" 
  
  