module Relatorios where
import Tipos
import Busca
import Emprestimos (estaEmprestado)
import Data.List (sortBy, group, sort)
import Data.Ord (comparing, Down(..))

itensDisponiveis :: DB -> [Item]
itensDisponiveis db =
  filter (\item -> not (estaEmprestado (codigoUnico item) (emprestimos db))) (itens db)

listarAtrasados :: String -> DB -> [Emprestimo]
listarAtrasados dataAtual db =
  filter (\e -> dataDevolucao e < dataAtual) (emprestimos db)

percentualOcupacao :: DB -> Float
percentualOcupacao db =
  let totalItens = length (itens db)
      totalEmprestados = length (emprestimos db)
  in if totalItens == 0
     then 0
     else (fromIntegral totalEmprestados / fromIntegral totalItens) * 100

contarPorCategoria :: TipoMidia -> DB -> Int
contarPorCategoria tipo db =
  length (filtrarPorCategoria tipo (itens db))

rankingItens :: DB -> [(Item, Int)]
rankingItens db =
  let codigos = map codItemEmprestimo (emprestimos db)
      agrupados = group (sort codigos)
      contagem = map (\g -> (head g, length g)) agrupados
      ordenado = sortBy (comparing (Down . snd)) contagem
  in map (\(cod, qtd) ->
            case buscaItemBinaria cod (itens db) of
              Just item -> (item, qtd)
              Nothing -> error "Item não encontrado"
        ) ordenado

usuariosInadimplentes :: DB -> [Usuario]
usuariosInadimplentes db =
  let atrasados = filter (\e -> status e == Atrasado) (emprestimos db)
      matriculas = map matriculaUsuarioEmprestimo atrasados
  in map (\mat ->
            case buscarUsuarioPorMatricula mat (usuarios db) of
              Just u -> u
              Nothing -> error "Usuário não encontrado"
        ) matriculas
