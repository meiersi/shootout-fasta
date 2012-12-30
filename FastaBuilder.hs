{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-  The Computer Language Benchmarks Game

    http://shootout.alioth.debian.org/

    contributed by Bryan O'Sullivan

    Modified by Simon Meier. This version exercises the bytestring builder and
    highlights its rather high cost for building results byte-by-byte. The
    problem is that most time is spent in thunk creation and buffer boundary
    checking, as the buffer might be full and builder execution must be
    suspended. This cost is better amortized once more space of the buffer is
    filled at once, e.g., when decimally encoding integers.

    This version takes about 3.5x the time of the C version.
-}

module Main (main) where

import System.Environment
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8   as LC
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Unsafe as B
import Data.Monoid

main :: IO ()
main = do
    n <- getArgs >>= readIO.head

    writeAlu ">ONE Homo sapiens alu" (L.take (fromIntegral n*2) (L.cycle alu))
    LC.putStr $ runGen 42 $
      make ">TWO IUB ambiguity codes" (n*3) iub <>
      make ">THREE Homo sapiens frequency" (n*5) homosapiens

writeAlu :: B.ByteString -> L.ByteString -> IO ()
writeAlu name s0 = BC.putStrLn name >> go s0
 where go s
         | L.null t = LC.putStrLn h >> return ()
         | otherwise = LC.putStrLn h >> go t
         where (h,t) = LC.splitAt 60 s

newtype Gen = Gen ((Int -> B.Builder) -> (Int -> B.Builder))

runGen :: Int -> Gen -> L.ByteString
runGen seed (Gen g) = B.toLazyByteString $ g (const mempty) seed

instance Monoid Gen where
  mempty           = Gen id
  Gen g1 `mappend` Gen g2 = Gen (g1 . g2)

make :: B.ByteString -> Int -> [(Char, Float)] -> Gen
make name n0 tbl =
    Gen $ \cont seed0 ->
      let
        cr      = B.char8 '\n'
        modulus = 139968
        convert = scanl1 (\(_,p) (c,q) -> (c,p+q))
        fill ((c,p):cps) !j =
          let !k = min modulus (floor (fromIntegral modulus * p + 1))
          in BC.replicate (fromIntegral (k - j)) c : fill cps k
        fill _ _ = []
        lookupTable = BC.concat $ fill (convert tbl) 0

        make' n i seed
            | n < n0 =
                let seed' = (seed * 3877 + 29573) `rem` modulus
                    !w    = lookupTable `B.unsafeIndex` seed'
                in
                  if i == 0
                    then cr <> B.word8 w <> make' (n+1) (60-1) seed'
                    else       B.word8 w <> make' (n+1) (i -1) seed'
            | otherwise = cr <> cont seed
      in
        B.byteString name <> make' 0 (0::Int) seed0


alu :: L.ByteString
alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub, homosapiens :: [(Char, Float)]

iub = [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
      ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
      ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens = [('a',0.3029549426680),('c',0.1979883004921)
              ,('g',0.1975473066391),('t',0.3015094502008)]
