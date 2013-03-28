{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Storage.WebDAV.CURL
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
  downloadFile, 
  uploadFile, 
  doesFileExistInDAV
) where

import Control.Applicative ((<$>),(<*>))
import Data.Attoparsec.Char8 
import qualified Data.ByteString.Char8 as B 
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
             -> FilePath         -- ^ remote file name
             -> IO Bool 
downloadFile wdavc rdir filename = 
  checkUrl (webdav_baseurl wdavc) #
    maybe (return False) ( \r_url -> 
      case r_url of 
        LocalURL path -> do 
          let remotepath = path </> webdav_remotedir rdir </> filename
              (_,remotefile) = splitFileName remotepath 
          currdir <- getCurrentDirectory 
          putStrLn $ "copy " ++ remotepath ++ " to " ++ (currdir </> remotefile)
          copyFile remotepath (currdir </> remotefile)
          b <- doesFileExist remotepath 
          if b then do {copyFile remotepath (currdir </> remotefile); return True}
               else return False
        GlobalURL urlroot -> do
          let cr = webdav_credential wdavc
          let fullurl = urlroot </> webdav_remotedir rdir </> filename
          readProcess "curl"
                      (getCredentialOption cr
                       ++ [ "-o", filename,  fullurl])
                      (getCredentialStdin cr)
          return =<< doesFileExist filename 
    )

  
-- | 
uploadFile :: WebDAVConfig
              -> WebDAVRemoteDir
              -> FilePath          -- ^ local file name
              -> IO Bool 
uploadFile wdavc rdir filepath = do
  checkUrl (webdav_baseurl wdavc) #
    maybe (return False) ( \r_url -> 
      case r_url of 
        LocalURL path -> do  
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
        GlobalURL urlroot -> do  
          b <- doesFileExist filepath 
          if b 
            then do 
              let cr = webdav_credential wdavc
                  fullurl = urlroot </> webdav_remotedir rdir ++ "/"
              result <- readProcess "curl" 
                          (getCredentialOption cr
                           ++ ["-T",filepath,fullurl] 
                          ) 
                          (getCredentialStdin cr)
              putStrLn result 
              return True
            else return False 
    )


-- | check whether a file exists in webdav server using PROPFIND HTTP method 
doesFileExistInDAV :: WebDAVConfig -> WebDAVRemoteDir -> FilePath -> IO Bool 
doesFileExistInDAV wdavconfig wdavrdir filename = do 
    case checkUrl (webdav_baseurl wdavconfig) of 
      Nothing -> return False
      Just r_url -> case r_url of  
        LocalURL _ -> return True -- return False 
        GlobalURL urlroot -> do 
          let cr = webdav_credential wdavconfig 
          let fullurl = urlroot </> webdav_remotedir wdavrdir </> filename
          result <- readProcess "curl" 
                      (["-I","-X","PROPFIND"]
                       ++ getCredentialOption cr
                       ++ [fullurl]) 
                      (getCredentialStdin cr) 
          let e = parseOnly p_result (B.pack result) 
          case e of 
            Left _ -> return False
            Right (c1,c2) -> return ((c1,c2) == (401,207))
  where p_result :: Parser (Int,Int)
        p_result = (,) <$> p_http_status <*> p_http_status 
        p_http_status :: Parser Int 
        p_http_status = do 
          manyTill anyChar (try (string "HTTP/1.1")) 
          skipSpace
          read <$> manyTill anyChar (try (char ' '))


{-
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
