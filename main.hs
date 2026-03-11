module Main where
import Itens
import BancoDeDados
import Menus
import Users
import Emprestimos

main :: IO ()
main = do 
    menu clearDB