module HEP.Storage.WebDAV.Type where

data WebDAVConfig = WebDAVConfig {
        webdav_path_wget :: String,
        webdav_baseurl   :: String
        } deriving Show

data WebDAVRemoteDir = WebDAVRemoteDir {
    webdav_remotedir :: FilePath
  }


