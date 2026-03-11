module Itens where
import Tipos

-- verifica se o código já existe na lista de itens
codigoExiste :: String -> [Item] -> Bool
codigoExiste _ [] = False
codigoExiste cod (i:is) 
    | codigoUnico i == cod = True
    | otherwise            = codigoExiste cod is

-- valida o ano do item (entre 1900 e 2026)
anoValido :: Int -> Bool
anoValido a = a <= 1900 || a >= 2026

-- cadastra um item no banco de dados, verificando se o código é único e se o ano é válido
cadastrarItem :: Item -> DB -> Either String DB
cadastrarItem novoItem db
    | codigoExiste (codigoUnico novoItem) (itens db) = 
        Left ("Erro: codigo \"" ++ codigoUnico novoItem ++ "\" ja cadastrado.")
    | anoValido (ano novoItem) = 
        Left ("Erro: ano \"" ++ show (ano novoItem) ++ "\" invalido para itens.")
    | otherwise = Right (db { itens = novoItem : itens db })

-- remove um item do banco de dados, verificando se o código existe
removerItem :: String -> DB -> Either String DB
removerItem cod db
    | not (codigoExiste cod (itens db)) = 
        Left ("Erro: codigo \"" ++ cod ++ "\" nao encontrado.")
    | otherwise = Right (db { itens = filter (\item -> codigoUnico item /= cod) (itens db) })
