module Menus where
import Tipos
import Itens
import BancoDeDados
import Users
import Emprestimos
import Listagem 
import Control.Monad.RWS (MonadState(put))
import Busca
import System.Directory (doesFileExist)
import Data.List (isInfixOf)


-- função para exibir o menu principal
menu :: DB -> IO ()
menu db = do
    putStrLn "\n==================================="
    putStrLn "Sistema de Mídias - Menu Principal"
    putStrLn "==================================="
    putStrLn "1 - Cadastro de Itens"
    putStrLn "2 - Cadastro de Usúarios"
    putStrLn "3 - Empréstimos e Devolucões"
    putStrLn "4 - Busca e Listagem Avançcada"
    putStrLn "5 - Relatáorios e Estatísticas"
    putStrLn "6 - Edicão de Dados"
    putStrLn "7 - Exportacão/Importacão de Dados"
    putStrLn "8 - Auditoria e Histórico"
    putStrLn "0 - Salvar e Sair"
    putStr "Digite uma opção: "

    opcao <- getLine

    case opcao of
        "1" -> do
            novoDb <- menuItens db
            menu novoDb
        "2" -> do
            novoDb <- menuUsers db
            menu novoDb
        "3" -> do
            novoDb <- menuEmprestimos db
            menu novoDb
        "4" -> do
            novoDb <- menuBusca db
            menu novoDb
        "5" -> do
            putStrLn "\n-> Indo para o submenu de cadastro..."
            menu db
        "6" -> do
            putStrLn "\n-> Indo para o submenu de cadastro..."
            menu db
        "7" -> do
            putStrLn "\n-> Indo para o submenu de cadastro..."
            menu db
        "8" -> do
            putStrLn "\n-> Indo para o submenu de cadastro..."
            menu db
        "0" -> do
            salvarSistema db
        _   -> do
            putStrLn "\nOpção inválida! Tente novamente."
            menu db

-- função auxiliar para ler uma linha de input com um texto
prompt :: String -> IO String
prompt texto = do
    putStr texto
    getLine

-- função auxiliar para converter String em um TipoItem
obterTipo :: String -> Maybe TipoMidia
obterTipo entrada = case entrada of
    "1" -> Just Livro
    "2" -> Just Filme
    "3" -> Just Jogo
    _   -> Nothing

menuUsers :: DB -> IO DB
menuUsers db = do
    putStrLn "\n====================="
    putStrLn "  Cadastro de Usuários  "
    putStrLn "====================="
    putStrLn "1 - Adicionar novo usuário"
    putStrLn "2 - Listar usuárioss"
    putStrLn "3 - Voltar ao menu principal"
    putStr "Digite uma opção: "
    opcao <- getLine

    case opcao of
        "1" -> do
            nome <- prompt "Nome: "
            matricula <- prompt "Matrícula: "
            email <- prompt "E-mail: "
            
            let novoUser = Usuario nome matricula email
            let novodb = cadastrarUsuario novoUser db
            
            case novodb of
                Left err -> do
                    putStrLn ("\n" ++ err)
                    -- volta ao submenu com o banco de dados antigo
                    menuUsers db
                Right novodb -> do
                    putStrLn "\nUsuário cadastrado com sucesso!"
                    -- volta ao submenu com o banco de dados atualizado
                    menuUsers novodb

        "2" -> do
            putStrLn "\nLista de usuários:"
            putStrLn "-----------------------------------"
            imprimirUsuarios (usuarios db)
            menuUsers db
        "3" -> do
            return db 

        _ -> do
            putStrLn "\nOpção inválida."
            menuUsers db




menuItens :: DB -> IO DB
menuItens db = do
    putStrLn "\n====================="
    putStrLn "  Cadastro de Itens  "
    putStrLn "====================="
    putStrLn "1 - Adicionar novo item"
    putStrLn "2 - Remover item"
    putStrLn "3 - Listar itens cadastrados"
    putStrLn "4 - Voltar ao menu principal"
    putStr "Digite uma opcao: "
    opcao <- getLine

    case opcao of
        "1" -> do
            tit <- prompt "Titulo: "
            aut <- prompt "Autor/Diretor/Criador: "
            ano <- prompt "Ano de publicacao: "
            cod <- prompt "Codigo unico: "
            
            termoTipo <- prompt "Tipo de midia (1-Livro, 2-Filme, 3-Jogo): "
            
            case obterTipo termoTipo of
                Just tipoEscolhido -> do
                    -- Converte a string do ano para Inteiro (com cuidado)
                    let anoInt = read ano :: Int
                    let novoItem = Item tit aut anoInt cod tipoEscolhido
                    
                    case cadastrarItem novoItem db of
                        Left erro -> do
                            putStrLn $ "\nErro: " ++ erro
                            menuItens db -- repete o menu sem alterar
                        Right novoDb -> do
                            putStrLn "\nItem cadastrado com sucesso!"
                            menuItens novoDb -- volta com o banco atualizado!
                
                Nothing -> do
                    putStrLn "\nOpção de tipo de mídia inválida! Tente novamente."
                    menuItens db
        "2" -> do
            cod <- prompt "Codigo do item a ser removido: "

            let dbSemOItem = removerItem cod db
            
            case dbSemOItem of
                Left erro -> do
                    putStrLn $ "\n" ++ erro
                    menuItens db
                Right novoDb -> do
                    putStrLn "\nItem removido com sucesso!"
                    menuItens novoDb

        "3" -> do
            putStrLn "\nLista de itens:"
            putStrLn "-----------------------------------"
            listarItens (itens db)
            menuItens db
        "4" -> do
            return db -- volta para o menu principal devolvendo o banco de dados

        _ -> do
            putStrLn "\nOpcao invalida."
            menuItens db

fazerEmprestimo :: String -> String -> String -> DB -> IO DB
fazerEmprestimo codItem matriculaUser dataRetirada db = do
    case registrarEmprestimo codItem matriculaUser dataRetirada db of 
        Left aviso -> do
            putStrLn $ "\n" ++ aviso
            if "ja esta emprestado" `isInfixOf` aviso && not ("JA ESTA" `isInfixOf` aviso) then do
                resposta <- getLine
                if resposta == "S" || resposta == "s" then do
                    let novasEsperas = adicionarEspera codItem matriculaUser (esperas db)
                    let dbComEspera = db { esperas = novasEsperas }
                    putStrLn "\nVoce foi adicionado a lista de espera com sucesso!"
                    return dbComEspera 
                else do
                    putStrLn "\nOperacao cancelada."
                    return db 
            else do

                return db 
                
        Right novoDb -> do
            putStrLn "\nEmprestimo realizado com sucesso!"
            return novoDb -- Devolve o DB novo com o empréstimo registrado

menuEmprestimos :: DB -> IO DB
menuEmprestimos db = do
    putStrLn "\n====================="
    putStrLn "  Empréstimos e Devoluções  "
    putStrLn "====================="
    putStrLn "1 - Registrar empréstimos"
    putStrLn "2 - Registrar devolução"
    putStrLn "3 - Visualizar empréstimos ativos"
    putStrLn "4 - Renovar empréstimo"
    putStrLn "5 - Empréstimos/devoluções em lote"
    putStrLn "6 - Voltar ao menu principal"
    putStr "Digite uma opcao: "
    opcao <- getLine

    case opcao of
        "1" -> do
            codItem <- prompt "Codigo do Item: "
            matriculaUser <- prompt "Matricula do Usuario: "
            dataRetirada <- prompt "Data de Retirada (ex: 2026-03-16): "

            case verificarItemParaEmprestimo codItem matriculaUser db of
                Left erro -> do
                    putStrLn $ "\n" ++ erro
                    menuEmprestimos db -- Volta com erro
                Right _ -> do                    
                    dbNovo <- fazerEmprestimo codItem matriculaUser dataRetirada db
                    menuEmprestimos dbNovo -- Volta para o menu com o banco atualizado!
        "2" -> do
            codItem <- prompt "Codigo do item a ser devolvido: "
            dataDev <- prompt "Data da devolucao (ex: 2026-03-16): "
            
            case registrarDevolucao codItem dataDev db of
                Left erro -> do
                    putStrLn $ "\n" ++ erro
                    menuEmprestimos db
                Right novoDb -> do
                    putStrLn "\nDevolucao registrada com sucesso!"
                    menuEmprestimos novoDb

        "3" -> do
            putStrLn "\n=== Emprestimos Ativos ==="
            let ativos = pegarEmprestimosAtivos (emprestimos db)
            
            if null ativos
            then putStrLn "Nenhum item emprestado no momento."
            else imprimirEmprestimos ativos
            
            menuEmprestimos db
        "4" -> do
            codItem <- prompt "Codigo do item a ser renovado: "
            novaData <- prompt "Nova data de devolucao (ex: 2026-04-16): "
            
            case renovarEmprestimo codItem novaData db of
                Left erro -> do
                    putStrLn $ "\n" ++ erro
                    menuEmprestimos db
                Right novoDb -> do
                    putStrLn "\nEmprestimo renovado com sucesso!"
                    menuEmprestimos novoDb
        "5" -> do
            putStrLn "\n=== Emprestimo em Lote ==="
            matUser <- prompt "Matricula do Usuario: "
            dataAtual <- prompt "Data de Retirada (ex: 2026-03-17): "
            
            putStrLn "Digite os codigos dos itens separados por espaco (ex: 101 102 103): "
            entradaCodigos <- getLine
            
            -- Transforma a string "101 102" na lista ["101", "102"]
            let listaDeCodigos = words entradaCodigos
            
            novoDb <- emprestimoEmLote listaDeCodigos matUser dataAtual db
            
            menuEmprestimos novoDb

        "6" -> do
            return db

-- | Submenu: Busca e Listagem Avançada
menuBusca :: DB -> IO DB
menuBusca db = do
    putStrLn "\n========================="
    putStrLn " Busca e Listagem Avançada "
    putStrLn "========================="
    putStrLn "1 - Buscar por título"
    putStrLn "2 - Buscar por autor/diretor"
    putStrLn "3 - Busca combinada (título e autor)"
    putStrLn "4 - Filtrar por categoria"
    putStrLn "5 - Ordenar resultados"
    putStrLn "6 - Voltar ao menu principal"
    putStr "Escolha uma opção: "
    
    opcao <- getLine
    case opcao of
        "1" -> do
            termo <- prompt "Digite o título: "
            let resultados = buscarPorTitulo termo (itens db)
            listarItens resultados
            menuBusca db
        
        "2" -> do
            termo <- prompt "Digite o autor/diretor: "
            let resultados = buscarPorAutor termo (itens db)
            listarItens resultados
            menuBusca db
        
        "3" -> do
            termo1 <- prompt "Digite o titulo: "
            termo2 <- prompt "Digite autor/diretor: "
            let resultados = buscaCombinada termo1 termo2 (itens db)
            listarItens resultados
            menuBusca db
        
        "4" -> do
            termo <- prompt "Tipo de midia (1-Livro, 2-Filme, 3-Jogo): "
            case obterTipo termo of
                Just tipo -> do
                    let resultados = filtrarPorCategoria tipo (itens db)
                    listarItens resultados
                    menuBusca db
                Nothing -> do
                    putStrLn "Opção inválida! Tente novamente."
                    menuBusca db
            

        "5" -> do
            c <- prompt "Escolha o critério (1 - Autor, 2 - Titulo, 3 - Ano): "
            let criterio' = case c of
                                "1" -> "autor"
                                "2" -> "titulo"
                                "3" -> "ano"
                                _   -> "" -- Usamos vazio para indicar erro
            
            if criterio' == "" 
                then do
                    putStrLn "Opção inválida! Tente novamente."
                    menuBusca db
                else do
                    o <- prompt "Escolha a ordem (1 - Ascendente, 2 - Descendente): "
                    let ordem' = if o == "2" then "desc" else "asc"
                    let resultados = ordenarItens criterio' ordem' (itens db)
                    listarItens resultados
                    menuBusca db
            
        "6" -> return db
        _   -> do
            putStrLn "Opção inválida."
            menuBusca db