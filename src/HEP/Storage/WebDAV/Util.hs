-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Storage.WebDAV.Util
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Utility functions for WebDAV operations
-- 
-----------------------------------------------------------------------------

module HEP.Storage.WebDAV.Util where 

import HEP.Storage.WebDAV.Type 

-- | 
checkUrl :: String -> Maybe URLtype
checkUrl str = 
  if length str > 6 
  then let (method,path)= splitAt 7 str 
       in case method of 
            "file://" -> Just (LocalURL path)
            "http://" -> Just (GlobalURL str)
            _ -> Nothing
  else Nothing
                   

