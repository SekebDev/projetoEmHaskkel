module Listagem where
import Tipos
import Relatorios

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


-- listar empréstimos ativos
imprimirEmprestimos :: [Emprestimo] -> IO ()
imprimirEmprestimos [] = putStrLn "-----------------------------------\n"
imprimirEmprestimos (x:xs) = do
    putStrLn $ "Código: " ++ codItemEmprestimo x ++ " | Usuário: " ++ matriculaUsuarioEmprestimo x ++ " | Data: " ++ dataRetirada x
    imprimirEmprestimos xs


exibirDashboard :: String -> DB -> IO ()
exibirDashboard dataAtual db = do
  let totalItens = length (itens db)
  let ocupacao = percentualOcupacao db
  let atrasados = listarAtrasados dataAtual db

  putStrLn "\n===== DASHBOARD ====="
  putStrLn ("Total de itens: " ++ show totalItens)
  putStrLn ("Percentual de ocupação: " ++ show ocupacao ++ "%")
  putStrLn ("Empréstimos atrasados: " ++ show (length atrasados))

imprimirRanking :: DB -> IO ()
imprimirRanking db = do
  let ranking = rankingItens db
  putStrLn "\n===== RANKING ====="
  mapM_ (\(pos, (item, qtd)) ->
          putStrLn (show pos ++ "º - " ++ titulo item ++ " - " ++ show qtd ++ " locações")
        ) (zip [1..] ranking)