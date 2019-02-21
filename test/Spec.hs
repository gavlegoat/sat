import Parser
import Transformation

main :: IO ()
main = do
  let f1 = parseFormula "(a & b) | (c & d)"
  print f1
  print (tseitin f1)
  let f2 = parseFormula "(a -> b) | (b -> a)"
  print f2
  print (tseitin f2)
