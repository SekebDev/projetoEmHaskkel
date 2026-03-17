module Busca where
import Tipos
import Data.Char (toLower, toUpper)
import Data.List (sortBy)
import Itens (codigoExiste)

-- algoritmo recursivo para busca binária por código único.
buscaItemBinaria :: String -> [Item] -> Maybe Item
buscaItemBinaria _ [] = Nothing
buscaItemBinaria cod lista
    | codigoLista == cod' = Just item
    | cod' > codigoLista = buscaItemBinaria cod (drop (metade +1) lista)
    | otherwise = buscaItemBinaria cod (take metade lista)
        where
            cod' = read cod :: Int
            item = lista !! metade
            codigoLista = read $ codigoUnico item
            metade = length lista `div` 2

-- Busca um usuário específico pela matrícula.
buscarUsuarioPorMatricula :: String -> [Usuario] -> Maybe Usuario
buscarUsuarioPorMatricula _ [] = Nothing
buscarUsuarioPorMatricula mat (u:us)
    | matricula u == mat = Just u
    | otherwise          = buscarUsuarioPorMatricula mat us

-- Busca por Título (Case-insensitive)
buscarPorTitulo :: String -> [Item] -> [Item]
buscarPorTitulo termo = filter (contemString termo . titulo)

--  Busca por Autor/Diretor (Case-insensitive)
buscarPorAutor :: String -> [Item] -> [Item]
buscarPorAutor termo = filter (contemString termo . autor)

--  Busca Combinada (Múltiplos campos)
buscaCombinada :: String -> String -> [Item] -> [Item]
buscaCombinada termoTit termoAut = buscarPorAutor termoAut . buscarPorTitulo termoTit

--  Ordenação de Listas
ordenarItens :: String -> String -> [Item] -> [Item]
ordenarItens criterio ordem lista =
    let ordenado = sortBy (compararItens criterio) lista
    in case ordem of
        "asc"  -> ordenado
        "desc" -> reverse ordenado


--  Verificação de Duplicatas em Filas de Espera
verificarDuplicataFila :: String -> [String] -> Bool
verificarDuplicataFila _ [] = False
verificarDuplicataFila matricula (m:ms)
    | matricula == m = True
    | otherwise = verificarDuplicataFila matricula ms

-- Compara dois itens para fins de ordenação baseada em um campo.
-- Auxilia a função de ordenação
compararItens :: String -> Item -> Item -> Ordering
compararItens criterio item1 item2 =
    case criterio of
        "titulo" -> compare (titulo item1) (titulo item2)
        "autor"  -> compare (autor item1) (autor item2)
        "ano"    -> compare (ano item1) (ano item2)

-- Verificação de Disponibilidade (Interdependente)
-- Percorre a lista de empréstimos e verifica se um código de item está ocupado.
verificarItemEmprestado :: String -> [Emprestimo] -> Bool
verificarItemEmprestado _ [] = False
verificarItemEmprestado codItem (e:es)
    -- Se o código bater e estiver ocupado, retorna True imediatamente (Sucesso)
    | codItem == codItemEmprestimo e && (status e == Emprestado || status e == Atrasado) = True
    -- Se não bater OU se bater mas estiver 'Devolvido', continua procurando no resto da lista
    | otherwise = verificarItemEmprestado codItem es

-- Utilidade: Contém String (Case-insensitive)
contemString :: String -> String -> Bool
contemString sub str = buscar (map toLower sub) (map toLower str)
  where
    tamanhoSub = length sub
    -- Função auxiliar que já recebe tudo em minúsculo para evitar reprocessar
    buscar _ [] = False
    buscar s frase
        | length frase < tamanhoSub = False
        | take tamanhoSub frase == s = True
        | otherwise = buscar s (tail frase)

-- Filtra o inventário para somente um tipo específico de itens
filtrarPorCategoria :: TipoMidia -> [Item] -> [Item]
filtrarPorCategoria tipo = filter (\x -> tipoMidia x == tipo)