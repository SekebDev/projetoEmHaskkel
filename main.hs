module Main where
import Itens
import BancoDeDados (carregarSistema)
import Menus
import Users
import Emprestimos

main :: IO ()
main = do 
    dbInicial <- carregarSistema
    menu dbInicial