module BancoDeDados (clearDB,carregarSistema,salvarSistema) where
import Tipos
import System.Directory (doesFileExist)


-- devolve um banco de dados vazio, sem usuários ou itens
clearDB :: DB
clearDB = DB {
    usuarios = [],
    itens = [],
    emprestimos = [],
    esperas = []
}

-- Função para Carregar o Banco de Dados ao iniciar o programa
carregarSistema :: IO DB
carregarSistema = do
    existe <- doesFileExist "biblioteca.txt"
    if existe
    then do
        conteudo <- readFile "biblioteca.txt"
        -- Tenta converter a String do arquivo de volta para o tipo DB
        return (read conteudo :: DB)
    else do
        putStrLn "Nenhum banco de dados encontrado. Iniciando um sistema vazio."
        return clearDB

-- Função interativa para Salvar e Sair
salvarSistema :: DB -> IO ()
salvarSistema db = do
    existe <- doesFileExist "biblioteca.txt"
    
    if existe 
    then do
        putStrLn "\nArquivo biblioteca.txt ja existe. Deseja sobrescreve-lo? (S/N)"
        resp <- getLine
        if resp == "S" || resp == "s"
        then do
            -- Sobrescreve o arquivo com os dados atuais
            writeFile "biblioteca.txt" (show db)
            putStrLn "Ate a proxima."
        else 
            putStrLn "Ate a proxima."
    else do
        putStrLn "\narquivo biblioteca.txt nao existe. Deseja criar um novo? (S/N)"
        resp <- getLine
        if resp == "S" || resp == "s"
        then do
            -- Cria o arquivo e escreve os dados
            writeFile "biblioteca.txt" (show db)
            putStrLn "Ate a proxima."
        else 
            putStrLn "Ate a proxima."
