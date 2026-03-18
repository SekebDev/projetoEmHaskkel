module Edicao where
import Tipos

-- funções auxiliares para edição de itens
trocarTituloDoItem :: String -> String -> [Item] -> [Item]
trocarTituloDoItem _   _          []     = []
trocarTituloDoItem cod novoTitulo (i:is)
    | codigoUnico i == cod = i { titulo = novoTitulo } : is
    | otherwise            = i : trocarTituloDoItem cod novoTitulo is

trocarAutorDoItem :: String -> String -> [Item] -> [Item]
trocarAutorDoItem cod novoAutor =
    map (\item -> if codigoUnico item == cod
                    then item { autor = novoAutor }
                    else item)

trocarAnoDoItem :: String -> Int -> [Item] -> [Item]
trocarAnoDoItem cod novoAno =
    map (\item -> if codigoUnico item == cod
                    then item { ano = novoAno }
                    else item)

-- funções auxiliares para edição de usuários
trocarNomeDoUsuario :: String -> String -> [Usuario] -> [Usuario]
trocarNomeDoUsuario mat novoNome =
    map (\u -> if matricula u == mat
                 then u { nome = novoNome }
                 else u)

trocarEmailDoUsuario :: String -> String -> [Usuario] -> [Usuario]
trocarEmailDoUsuario mat novoEmail =
    map (\u -> if matricula u == mat
                 then u { email = novoEmail }
                 else u)