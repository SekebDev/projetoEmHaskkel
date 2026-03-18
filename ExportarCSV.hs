module ExportarCSV where
import Tipos

exportarParaCSV :: DB -> IO ()
exportarParaCSV db = do
    usersExportarParaCSV db
    itemExportarParaCSV db
    emprestimosExportarParaCSV db
    esperaExportarParaCSV db

-- ================= USUARIOS =================

usuarioParaCSV :: Usuario -> String
usuarioParaCSV user =
    nome user ++ "," ++ matricula user ++ "," ++ email user 

usersExportarParaCSV :: DB -> IO ()
usersExportarParaCSV db = do
    let listaUsuarios = usuarios db
    let linhas = map usuarioParaCSV listaUsuarios
    let conteudo = unlines linhas
    appendFile "usuarios.csv" conteudo

-- ================= ITENS =================

itemParaCSV :: Item -> String
itemParaCSV item =
    titulo item ++ "," ++
    autor item ++ "," ++
    show (ano item) ++ "," ++
    codigoUnico item ++ "," ++
    show (tipoMidia item)

itemExportarParaCSV :: DB -> IO ()
itemExportarParaCSV db = do
    let listaItens = itens db
    let linhas = map itemParaCSV listaItens
    let conteudo = unlines linhas
    appendFile "itens.csv" conteudo

-- ================= EMPRESTIMOS =================

emprestimoParaCSV :: Emprestimo -> String
emprestimoParaCSV emp =
    codItemEmprestimo emp ++ "," ++
    matriculaUsuarioEmprestimo emp ++ "," ++
    dataRetirada emp ++ "," ++
    dataDevolucao emp ++ "," ++
    show (status emp)

emprestimosExportarParaCSV :: DB -> IO ()
emprestimosExportarParaCSV db = do
    let listaEmprestimos = emprestimos db
    let linhas = map emprestimoParaCSV listaEmprestimos
    let conteudo = unlines linhas
    appendFile "emprestimos.csv" conteudo

-- ================= FILA DE ESPERA =================

filaParaCSV :: FilaEspera -> String
filaParaCSV fila =
    codItemFila fila ++ "," ++
    unwords (usuariosFila fila)

esperaExportarParaCSV :: DB -> IO ()
esperaExportarParaCSV db = do
    let listaEspera = esperas db
    let linhas = map filaParaCSV listaEspera
    let conteudo = unlines linhas
    appendFile "espera.csv" conteudo