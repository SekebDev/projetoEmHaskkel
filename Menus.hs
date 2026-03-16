module Menus where
import Tipos
import Itens
import BancoDeDados
import Users
import Emprestimos
import Listagem 
import Control.Monad.RWS (MonadState(put))



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
            putStrLn "\n-> Indo para o submenu de cadastro..."
            menu db
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
        "9" -> do
            putStrLn "\n-> Indo para o submenu de cadastro..."
            menu db

            -- sai do loop  
        "0" -> do

            putStrLn "\nAté a próxima."
        _   -> do
            putStrLn "\nOpção inválida! Tente novamente."
            menu db

-- função auxiliar para ler uma linha de input com um texto
prompt :: String -> IO String
prompt texto = do
    putStr texto
    getLine



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
            
            putStrLn "Tipo de midia (1-Livro, 2-Filme, 3-Jogo): "
            tipoStr <- getLine
            
            -- Converte a string do ano para Inteiro
            let anoInt = read ano :: Int
            
            -- converte a string digitada para TipoMidia
            let tipoEscolhido = case tipoStr of
                    "1" -> Livro
                    "2" -> Filme
                    "3" -> Jogo
                    _   -> Livro -- assume livro caso invalido
            
            -- cria o novo item
            let novoItem = Item tit aut anoInt cod tipoEscolhido
            
            -- cadastra no banco de dados
            case cadastrarItem novoItem db of
                Left erro -> do
                    putStrLn $ "\n" ++ erro
                    menuItens db -- repete o menu sem alterar
                Right novoDb -> do
                    putStrLn "\nItem cadastrado com sucesso!"
                    menuItens novoDb -- volta com o banco atualizado!
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
fazerEmprestimo codItem matriculaUser dataRetirada db = 
    case registrarEmprestimo codItem matriculaUser dataRetirada db of
        Left aviso -> do
            putStrLn $ "\n" ++ aviso
            resposta <- getLine
                               
            if resposta == "S" || resposta == "s" then do
                -- atualizamos a lista de esperas
                let novasEsperas = adicionarEspera codItem matriculaUser (esperas db)
                let dbComEspera = db { esperas = novasEsperas }
                putStrLn "\nVoce foi adicionado a lista de espera com sucesso!"
                menuEmprestimos dbComEspera
            else do
                putStrLn "\nOperacao cancelada."
                menuEmprestimos db
                                    
            -- sucesso no empréstimo
        Right novoDb -> do
            putStrLn "\nEmprestimo realizado com sucesso!"
            menuEmprestimos novoDb

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
                    menuEmprestimos db
                Right _ -> do                    
                    dbNovo <- fazerEmprestimo codItem matriculaUser dataRetirada db
                    menuEmprestimos dbNovo
        "6" -> do
            return db
                        