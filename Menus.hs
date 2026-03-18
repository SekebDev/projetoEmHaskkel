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
import Data.Maybe (isJust, fromJust)
import Data.Char (toLower,toUpper)
import Relatorios
import ImportarCSV
import ExportarCSV
import Logs
import Edicao 

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
            novoDb <- menuRelatorios db
            menu novoDb
        "6" -> do
            novoDb <- menuEdicao db
            menu novoDb
        "7" -> do
            novoDb <- menuExportacaoImportacao db
            menu novoDb
        "8" -> do
            menuAuditoria
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
                    let log = criarLog CadastroUsuario ("Tentativa de cadastro do usuário " ++ nome) (Erro err)
                    arquivarLog log
                    -- volta ao submenu com o banco de dados antigo
                    menuUsers db
                Right novodb -> do
                    putStrLn "\nUsuário cadastrado com sucesso!"
                    let log = criarLog CadastroUsuario ("Usuário " ++ nome ++ " cadastrado") Sucesso
                    arquivarLog log
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
                            let log = criarLog CadastroItem ("Tentativa de cadastro do item \"" ++ tit ++ "\"") (Erro erro)
                            arquivarLog log
                            menuItens db -- repete o menu sem alterar
                        Right novoDb -> do
                            putStrLn "\nItem cadastrado com sucesso!"
                            let log = criarLog CadastroItem ("Item \"" ++ tit ++ "\" cadastrado") Sucesso
                            arquivarLog log
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
                    let log = criarLog CadastroItem ("Tentativa de remoção do item \"" ++ cod ++ "\"") (Erro erro)
                    arquivarLog log
                    menuItens db
                Right novoDb -> do
                    putStrLn "\nItem removido com sucesso!"
                    let log = criarLog CadastroItem ("Item com código \"" ++ cod ++ "\" removido") Sucesso
                    arquivarLog log
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
                    let log = criarLog Logs.Emprestimo ("Usuario " ++ matriculaUser ++ " adicionado a fila de espera") Sucesso
                    arquivarLog log
                    return dbComEspera 
                else do
                    putStrLn "\nOperacao cancelada."
                    let log = criarLog Logs.Emprestimo ("Usuario nao quis se adicionar a fila de espera") (Erro aviso)
                    arquivarLog log
                    return db 
            else do
                let log = criarLog Logs.Emprestimo ("Erro durante emprestimo do item " ++ codItem) (Erro aviso)
                arquivarLog log
                return db 
                
        Right novoDb -> do
            putStrLn "\nEmprestimo realizado com sucesso!"
            let log = criarLog Logs.Emprestimo ("Empréstimo do item \"" ++ codItem ++ "\" para usuário \"" ++ matriculaUser ++ "\" realizado") Sucesso
            arquivarLog log
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
                    let log = criarLog Logs.Emprestimo ("Tentativa de empréstimo do item \"" ++ codItem ++ "\"") (Erro erro)
                    arquivarLog log
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
                    let log = criarLog Devolucao ("Tentativa de devolução do item \"" ++ codItem ++ "\"") (Erro erro)
                    arquivarLog log
                    menuEmprestimos db
                Right novoDb -> do
                    putStrLn "\nDevolucao registrada com sucesso!"
                    let log = criarLog Devolucao ("Devolução do item \"" ++ codItem ++ "\" registrada") Sucesso
                    arquivarLog log
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
                    let log = criarLog Logs.Emprestimo ("Tentativa de renovação do empréstimo \"" ++ codItem ++ "\"") (Erro erro)
                    arquivarLog log
                    menuEmprestimos db
                Right novoDb -> do
                    putStrLn "\nEmprestimo renovado com sucesso!"
                    let log = criarLog Logs.Emprestimo ("Empréstimo do item \"" ++ codItem ++ "\" renovado") Sucesso
                    arquivarLog log
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





menuRelatorios :: DB -> IO DB
menuRelatorios db = do
  putStrLn "\n===== MENU DE RELATÓRIOS ====="
  putStrLn "1 - Dashboard"
  putStrLn "2 - Ranking de Itens"
  putStrLn "3 - Itens Disponíveis"
  putStrLn "4 - Usuários Inadimplentes"
  putStrLn "0 - Voltar"

  op <- prompt "Escolha uma opção: "

  case op of
    "1" -> do
      dataAtual <- prompt "Digite a data atual: "
      exibirDashboard dataAtual db
      menuRelatorios db

    "2" -> do
      imprimirRanking db
      menuRelatorios db

    "3" -> do
      let disponiveis = itensDisponiveis db
      listarItens disponiveis
      menuRelatorios db

    "4" -> do
      let devedores = usuariosInadimplentes db
      imprimirUsuarios devedores
      menuRelatorios db

    "0" -> do
        return db

    _ -> do
      putStrLn "Opção inválida!"
      menuRelatorios db

menuExportacaoImportacao :: DB -> IO DB
menuExportacaoImportacao db = do
  putStrLn "\n========================="
  putStrLn "Exportacao/Importacao"
  putStrLn "========================="
  putStrLn "1 - Exportar dados para CSV"
  putStrLn "2 - Importar dados de CSV"
  putStrLn "3 - Voltar ao menu principal"
  putStr "Digite uma opção: "

  opcao <- getLine

  case opcao of
    "1" -> do
      putStrLn "\nExportando dados para CSV..."
      exportarParaCSV db
      putStrLn "Dados exportados com sucesso!"
      menuExportacaoImportacao db

    "2" -> do
      putStrLn "\nImportando dados de CSV..."
      novoDb <- importarDeCSV
      putStrLn "Dados importados com sucesso!"
      return novoDb

    "3" -> do
      return db

    _ -> do
      putStrLn "\nOpção inválida! Tente novamente."
      menuExportacaoImportacao db

menuAuditoria :: IO ()
menuAuditoria = do
  putStrLn "\n========================="
  putStrLn "Auditoria e Histórico"
  putStrLn "========================="
  putStrLn "1 - Exibir log de operações"
  putStrLn "2 - Exibir histórico de alterações"
  putStrLn "3 - Voltar ao menu principal"
  putStr "Digite uma opção: "

  opcao <- getLine

  case opcao of
    "1" -> do
      putStrLn "\n===== LOG DE OPERAÇÕES ====="
      imprimirLogs
      menuAuditoria

    "2" -> do
      putStrLn "\nFuncionalidade em desenvolvimento."
      menuAuditoria

    "3" -> do
      return ()

    _ -> do
      putStrLn "\nOpção inválida! Tente novamente."
      menuAuditoria

menuEdicao :: DB -> IO DB
menuEdicao db = do
    putStrLn "\n====================="
    putStrLn "  Edição de Dados  "
    putStrLn "====================="
    putStrLn "  1 - Editar item"
    putStrLn "  2 - Editar usuário"
    putStrLn "  3 - Voltar ao menu principal"
    putStr   "Digite uma opção: "

    opcao <- getLine
    case opcao of
        "1" -> editarItem db
        "2" -> editarUsuario db
        "3" -> return db
        _   -> do
            putStrLn "Opção não reconhecida. Tenta 1, 2 ou 3!"
            menuEdicao db



-- busca o item pelo código e deixa editar
editarItem :: DB -> IO DB
editarItem db = do
    putStr "\nInforme o codigo do item: "
    cod <- getLine

    case buscaItemBinaria cod (itens db) of
        Nothing -> do
            putStrLn $ "Não encontrei nenhum item com o código \"" ++ cod ++ "\"."
            putStrLn   "Confere se digitou certo e tente de novo."
            let log = criarLog CadastroItem ("Tentativa de edição do item \"" ++ cod ++ "\"") (Erro "Item não encontrado")
            arquivarLog log
            menuEdicao db

        Just itemEncontrado -> do
            putStrLn $ "\nItem encontrado: \"" ++ titulo itemEncontrado ++ "\""
            putStrLn   "\nDados atuais:"
            putStrLn $ "  Título : " ++ titulo itemEncontrado
            putStrLn $ "  Autor  : " ++ autor  itemEncontrado
            putStrLn $ "  Ano    : " ++ show (ano itemEncontrado)
            putStrLn   "\nEscolha o campo para editar:"
            putStrLn   "  1 - Título"
            putStrLn   "  2 - Autor / Diretor / Criador"
            putStrLn   "  3 - Ano de lançamento"
            putStrLn   "  4 - Voltar sem alterar nada"

            escolha <- getLine
            case escolha of
                "1" -> do
                    novoTitulo <- prompt "Novo título: "
                    confirmarESalvar db
                        (db { itens = trocarTituloDoItem cod novoTitulo (itens db) })
                        ("Título alterado para \"" ++ novoTitulo ++ "\".")

                "2" -> do
                    novoAutor <- prompt "Novo autor / diretor / criador: "
                    confirmarESalvar db
                        (db { itens = trocarAutorDoItem cod novoAutor (itens db) })
                        ("Autor alterado para \"" ++ novoAutor ++ "\".")

                "3" -> do
                    anoStr <- prompt "Novo ano de lançamento (1900–2026): "
                    let anoInformado = read anoStr :: Int
                    if anoInformado < 1900 || anoInformado > 2026
                        then do
                            putStrLn "Ano fora do intervalo permitido (1900–2026). Vamos tentar de novo?"
                            let log = criarLog CadastroItem ("Tentativa de edição - ano inválido \"" ++ anoStr ++ "\"") (Erro "Ano fora do intervalo")
                            arquivarLog log
                            editarItem db
                        else
                            confirmarESalvar db
                                (db { itens = trocarAnoDoItem cod anoInformado (itens db) })
                                ("Ano alterado para " ++ show anoInformado ++ ".")

                _ -> menuEdicao db

-- edita usuário
editarUsuario :: DB -> IO DB
editarUsuario db = do
    putStr "\nInforme a matricula do usuario: "
    mat <- getLine

    case buscarUsuarioPorMatricula mat (usuarios db) of
        Nothing -> do
            putStrLn $ "Não achei nenhum usuário com a matrícula \"" ++ mat ++ "\"."
            putStrLn   "Verifique se a matrícula está correta."
            let log = criarLog CadastroUsuario ("Tentativa de edição do usuário \"" ++ mat ++ "\"") (Erro "Usuário não encontrado")
            arquivarLog log
            menuEdicao db

        Just usuarioEncontrado -> do
            putStrLn $ "\nUsuario encontrado: " ++ nome usuarioEncontrado
            putStrLn   "\nDados atuais:"
            putStrLn $ "  Nome   : " ++ nome  usuarioEncontrado
            putStrLn $ "  E-mail : " ++ email usuarioEncontrado
            putStrLn   "\nEscolha o campo para editar:"
            putStrLn   "  1 - Nome"
            putStrLn   "  2 - E-mail"
            putStrLn   "  3 - Voltar sem alterar nada"

            escolha <- getLine
            case escolha of
                "1" -> do
                    novoNome <- prompt "Novo nome: "
                    confirmarESalvar db
                        (db { usuarios = trocarNomeDoUsuario mat novoNome (usuarios db) })
                        ("Nome alterado para \"" ++ novoNome ++ "\".")

                "2" -> do
                    novoEmail <- prompt "Novo e-mail: "
                    if '@' `elem` novoEmail
                        then confirmarESalvar db
                                (db { usuarios = trocarEmailDoUsuario mat novoEmail (usuarios db) })
                                ("E-mail atualizado para \"" ++ novoEmail ++ "\".")
                        else do
                            putStrLn "Esse e-mail parece incompleto — faltou o @. Tenta de novo!"
                            let log = criarLog CadastroUsuario ("Tentativa de alteração de e-mail com formato inválido") (Erro "E-mail sem @")
                            arquivarLog log
                            editarUsuario db

                _ -> menuEdicao db

-- pergunta se salva antes de efetivar mudança
confirmarESalvar :: DB -> DB -> String -> IO DB
confirmarESalvar dbOriginal dbAtualizado mensagemSucesso = do
    putStr "\nConfirma edição? (S/N): "
    resposta <- getLine

    case map toUpper resposta of
        s | s `elem` ["S", "SIM"] -> do
                putStrLn $ "Sucesso! " ++ mensagemSucesso ++ " ..."
                let log = criarLog CadastroItem mensagemSucesso Sucesso
                arquivarLog log
                menuEdicao dbAtualizado
          | otherwise -> do
                putStrLn "Tudo bem, nada foi alterado. Voltando ao menu."
                menuEdicao dbOriginal
