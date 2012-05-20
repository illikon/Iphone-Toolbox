module Iphone where

import System.USB

appleVendorId = 0x05ac
iphone4GsmProductId  = 0x1297
iphone4SGmsProductId = 0x129c 

iphone4 :: (Int -> Int -> Bool)
iphone4 vendorId productId = vendorId == appleVendorId && (productId == iphone4GsmProductId || productId == iphone4SGmsProductId)

findDevice :: (Int -> Int -> Bool) -> [Device] -> Maybe Device 
findDevice _ [] = Nothing
findDevice f (dev:xs) = let desc      = deviceDesc dev
                            vendorId  = fromEnum $ deviceVendorId desc
                            productId = fromEnum $ deviceProductId desc
                        in if f vendorId productId then Just dev 
                           else findDevice f xs
 
main :: IO ()
main = do
  ctx  <- newCtx
  devs <- getDevices ctx
  putStrLn $ show $ findDevice iphone4 devs