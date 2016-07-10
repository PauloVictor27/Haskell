import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

getReg = do
 c <- G.getWord8
 f <- G.getWord32be
 return (c, f)
 
getRegs = do
 empty <- G.isEmpty
 if empty then return []
 else do {r <- getReg; rs <- getRegs; return (r:rs);}
 
leitura arq = do
 bs <- L.readFile arq
 let rs = G.runGet getRegs bs
 printRegs rs
 
printRegs [] = return ()
printRegs (r:rs) = do
 printReg r
 printRegs rs
 
printReg (c, f) = putStrLn ((show(I.w2c c))++"-"++show f)
