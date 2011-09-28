{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module HEP.Storage.WebDAV.Type where

import Data.Typeable
import Data.Data

data WebDAVConfig = WebDAVConfig {
        webdav_path_wget :: String,
        webdav_path_cadaver :: String, 
        webdav_baseurl   :: String
        } deriving (Show, Typeable, Data)

newtype WebDAVRemoteDir = WebDAVRemoteDir {
    webdav_remotedir :: FilePath
  } deriving (Show, Typeable, Data)

data WebDAVCommand = Download | Upload
  deriving (Show, Typeable, Data)

newtype WebDAVServer = WebDAVServer { 
  webdav_server_url :: String
} deriving (Show,Typeable,Data)

data WebDAVClient = WebDAVClient { 
  webdav_client_wget :: FilePath, 
  webdav_client_cadaver :: FilePath
} deriving (Show,Typeable,Data)

