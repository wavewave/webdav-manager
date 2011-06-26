module HEP.Storage.WebDAV.Type where

data WebDAVConfig = WebDAVConfig {
        webdav_path_wget :: String,
        webdav_path_cadaver :: String, 
        webdav_baseurl   :: String
        } deriving Show

data WebDAVRemoteDir = WebDAVRemoteDir {
    webdav_remotedir :: FilePath
  } deriving Show

data WebDAVCommand = Download | Upload

data WebDAVServer = WebDAVServer { 
  webdav_server_url :: String
}

data WebDAVClient = WebDAVClient { 
  webdav_client_wget :: FilePath, 
  webdav_client_cadaver :: FilePath
}

