module Listagem where
import Tipos

-- listar usuários cadastrados
imprimirUsuarios :: [Usuario] -> IO ()
imprimirUsuarios [] = putStrLn "-----------------------------------\n"
imprimirUsuarios (x:xs) = do
    putStrLn $ "Nome: " ++ nome x ++ " | Matrícula: " ++ matricula x ++ " | E-mail: " ++ email x
    imprimirUsuarios xs

-- listar itens cadastrados
listarItens :: [Item] -> IO ()
listarItens [] = putStrLn "-----------------------------------\n"
listarItens (x:xs) = do
    putStrLn $ "Nome: " ++ titulo x ++ " | Autor: " ++ autor x ++ " | Ano: " ++ show (ano x) ++ " | Código Único: " ++ codigoUnico x ++ " | Tipo de Mídia: " ++ show (tipoMidia x)
    listarItens xs
