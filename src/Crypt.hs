{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypt where

-- import Data.ByteString
import Data.ByteString.Char8
import Crypto.Hash
import Crypto.Hash.Algorithms

blake :: ByteString -> Digest Blake2b_512
blake = hash

encode :: HashAlgorithm a => String -> Digest a
encode x = hash $ pack x

encodeToString :: String -> String
encodeToString x = "id" ++ (show $ (encode x :: Digest Blake2b_512))
