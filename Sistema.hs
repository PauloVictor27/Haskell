import System.Environment

fat 0 = 1
fat n = n*fat(n-1)

main = do
  args <- getArgs
  let n = read(head args)
  putStr(show(fat n ) 
