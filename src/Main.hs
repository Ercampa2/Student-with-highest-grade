module Main where

import Data.List
import System.Process
import Text.Printf

class ConvertibleToString a where
  toString :: a -> String

data Estudante = Estudante String Int Float

instance Eq Estudante where
  (Estudante _ _ nota) == (Estudante _ _ nota2) = nota == nota2

instance Ord Estudante where
  compare (Estudante _ _ nota) (Estudante _ _ nota2) = compare nota nota2

instance ConvertibleToString Estudante where
  toString (Estudante nome mat nota) = printf "Nome: %s, Matricula: %s, Nota: %s" nome (show mat) (show nota)

ordenaNotas :: [Estudante] -> [Estudante]
ordenaNotas list = reverse (sort list)

maiorNota :: [Estudante] -> Estudante
maiorNota = maximum

stringEstudante :: [Estudante] -> [String]
stringEstudante = map toString

-------------------------------------------------
main :: IO ()
main = do
  _ <- system "reset"
  let notas = [Estudante "En" 1 7.9, Estudante "Le" 2 8, Estudante "Ko" 3 7, Estudante "Da" 4 8.5]
  putStrLn "Maior nota:"
  print (toString (maiorNota notas))

-- putStrLn "Ordem descrescente:"
-- putStr (unlines (stringEstudante (ordenaNotas notas)))