-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Storage.WebDAV.Curl
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- WebDAV IO operation using cURL program
-- 
-----------------------------------------------------------------------------

module HEP.Storage.WebDAV.CURL ( 
  downloadFile

) where

import Control.Applicative
import System.Directory
import System.Process
import System.FilePath
-- 
import HEP.Storage.WebDAV.Type
import HEP.Storage.WebDAV.Util

(#) :: a -> ( a -> b ) -> b 
x # f = f x 

infixr 9 #

getCredentialOption :: Credential -> [String] 
getCredentialOption (CredDigest _ _) = ["--digest","-K","-"]
getCredentialOption _ = []

getCredentialStdin :: Credential -> String 
getCredentialStdin (CredDigest i p) = "-u " ++ i ++ ":" ++ p
getCredentialStdin _ = []
-- | 
downloadFile :: WebDAVConfig 
             -> WebDAVRemoteDir 
             -> Credential 
             -> FilePath         -- ^ remote file name
             -> IO ()   
downloadFile wdavc rdir cr filename = 
  checkUrl (webdav_baseurl wdavc) #
    maybe (error ("no such url : " ++ webdav_baseurl wdavc)) ( \r_url -> 
      case r_url of 
        LocalURL path -> do 
          let remotepath = path </> webdav_remotedir rdir </> filename
              (_,remotefile) = splitFileName remotepath 
          currdir <- getCurrentDirectory 
          putStrLn $ "copy " ++ remotepath ++ " to " ++ (currdir </> remotefile)
          copyFile remotepath (currdir </> remotefile)
          return ()
        GlobalURL url -> do
          let arg = url </> webdav_remotedir rdir </> filename
          print arg
          readProcess "curl"
                      (getCredentialOption cr
                       ++ [ "-o", filename,  arg ])
                      (getCredentialStdin cr)
          return ()
    )
{-

-- | 
checkNdownloadFile :: WebDAVConfig 
                      -> WebDAVRemoteDir
                      -> FilePath
                      -> IO Bool 
checkNdownloadFile wdavc rdir filename = do
  checkUrl (webdav_baseurl wdavc) #
    maybe (return False) $ \r_url -> 
      case r_url of 
        LocalURL path -> do  
          let remotepath = path </> webdav_remotedir rdir </> filename
              (_,remotefile) = splitFileName remotepath 
          currdir <- getCurrentDirectory 
          putStrLn $ "copy " ++ remotepath ++ " to " ++ (currdir </> remotefile)
          b <- doesFileExist remotepath 
          if b then do {copyFile remotepath (currdir </> remotefile); return True}
               else return False
        GlobalURL url -> do  
          putStrLn "downloading --- " 
          system $ (webdav_path_wget wdavc) ++ " " ++ (url </> webdav_remotedir rdir </> filename)   
          let (_,newfile) = splitFileName filename
          return =<< doesFileExist newfile


-}

{-  
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

-}
