module Main where

import System.Environment
--
import HEP.Storage.WebDAV



 
rdir = WebDAVRemoteDir { 
  webdav_remotedir = "curltest"
  }

-- test = fetchFile

main = do
  args <- getArgs 
  let url = args !! 0  
      wdav = WebDAVConfig url 
  putStrLn "curl download/upload test"  
  i <- getLine 
  p <- getLine 
  downloadFile wdav rdir (CredDigest i p)
    "ADMXQLD211MST50000.0MG300.0MSQ100.0_ttbar_LHC7ATLAS_NoMatch_NoCut_Cone0.4_WithTau_Set1_pgs_events.lhco.gz" 

  
  