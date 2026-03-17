module Emprestimos where
import Itens (codigoExiste) 
import Tipos

estaEmprestado :: String -> [Emprestimo] -> Bool
estaEmprestado _ [] = False
estaEmprestado cod xs = any (\x -> codItemEmprestimo x == cod && (status x == Emprestado || status x == Atrasado)) xs

verificarItemParaEmprestimo :: String -> String -> DB -> Either String Bool
verificarItemParaEmprestimo codItem matriculaUser db 
    -- verifica se o item existe no banco de dados
    | not (codigoExiste codItem (itens db)) = Left $ "codigo do item \"" ++ codItem ++ "\" nao encontrado."
    -- verifica se o usuário existe no banco de dados
    | not (any (\u -> matricula u == matriculaUser) (usuarios db)) = Left $ "matricula \"" ++ matriculaUser ++ "\" nao encontrada."
    | otherwise = Right True

-- conta quantos empréstimos com status 'Atrasado' uma matrícula possui
contarAtrasosUsuario :: String -> [Emprestimo] -> Int
contarAtrasosUsuario mat listaEmprestimos = 
    length $ filter (\emp -> matriculaUsuarioEmprestimo emp == mat && status emp == Atrasado) listaEmprestimos

-- verifica se a matrícula já está dentro da fila de espera de um código específico
usuarioNaEspera :: String -> String -> [FilaEspera] -> Bool
usuarioNaEspera _ _ [] = False
usuarioNaEspera codItem matUser (f:fs)
    | codItemFila f == codItem = matUser `elem` usuariosFila f 
    | otherwise = usuarioNaEspera codItem matUser fs

-- regista um empréstimo
registrarEmprestimo :: String -> String -> String -> DB -> Either String DB
registrarEmprestimo codItem matriculaUser dataAtual db
    -- O usuário tem empréstimos atrasados
    | contarAtrasosUsuario matriculaUser (emprestimos db) > 0 =
        let qtd = contarAtrasosUsuario matriculaUser (emprestimos db)
        in Left $ "Atencao: Voce possui " ++ show qtd ++ " emprestimos atrasados.\nRegularize sua situacao para novos emprestimos."
    -- O item está emprestado E o usuário JÁ ESTÁ na lista de espera
    | estaEmprestado codItem (emprestimos db) && usuarioNaEspera codItem matriculaUser (esperas db) =
        Left $ "Aviso: O item " ++ codItem ++ " ja esta emprestado e voce JA ESTA na lista de espera."
    -- O item está emprestado (e o usuário não está na fila)
    | estaEmprestado codItem (emprestimos db) =
        Left $ "Aviso: O item " ++ codItem ++ " ja esta emprestado.\nDeseja entrar na lista de espera? (S/N)"

    -- se passar por todas as verificações
    | otherwise = 
        let novoEmprestimo = Emprestimo { 
            codItemEmprestimo = codItem, 
            matriculaUsuarioEmprestimo = matriculaUser, 
            dataRetirada = dataAtual, 
            dataDevolucao = "data_prevista_aqui", 
            status = Emprestado 
        }
        in Right (db { emprestimos = novoEmprestimo : emprestimos db })


-- verifica se um usuário já está na fila de espera para o item
jaEstaNaFila :: String -> [String] -> Bool
jaEstaNaFila _ [] = False
jaEstaNaFila matricula (x:xs)
    | matricula == x = True
    | otherwise = jaEstaNaFila matricula xs

adicionarEspera :: String -> String -> [FilaEspera] -> [FilaEspera]
-- caso de não existir nenhuma fila para o item
adicionarEspera codItem matriculaUser [] = [FilaEspera { codItemFila = codItem, usuariosFila = [matriculaUser] }] 
-- caso recursivo percorre a lista de filas procurando o item correspondente
adicionarEspera codItem matriculaUser (fila:filas)
    | codItemFila fila == codItem =
        -- usuario ja esta na fila, entao nao adiciona novamente
        if jaEstaNaFila matriculaUser (usuariosFila fila) then fila : filas
        -- caso contrario, adiciona o usuario ao final da fila
        else fila { usuariosFila = usuariosFila fila ++ [matriculaUser] } : filas
    
    -- caso recursivo continua procurando o item na lista de filas
    | otherwise = fila : adicionarEspera codItem matriculaUser filas



-- Regista a devolução atualizando o status do empréstimo ativo
registrarDevolucao :: String -> String -> DB -> Either String DB
registrarDevolucao codItem dataDev db
    -- Valida se o item esta emprestado?
    | not (estaEmprestado codItem (emprestimos db)) = 
        Left ("Erro: O item " ++ codItem ++ " nao esta emprestado no momento.")
    | otherwise = 
        let listaAtualizada = map (\emp -> 
                -- Se for o item certo e o status não for Devolvido, atualiza o registro
                if codItemEmprestimo emp == codItem && status emp /= Devolvido then emp { status = Devolvido, dataDevolucao = dataDev }
                -- Se for outro item ou já estiver devolvido, mantém intacto
                else emp
              ) (emprestimos db)
        in Right (db { emprestimos = listaAtualizada })

pegarEmprestimosAtivos :: [Emprestimo] -> [Emprestimo]
pegarEmprestimosAtivos lista = filter (\emp -> status emp /= Devolvido) lista


-- Função auxiliar pura para verificar se existe fila de espera para um código
temFilaDeEspera :: String -> [FilaEspera] -> Bool
temFilaDeEspera _ [] = False
temFilaDeEspera cod (f:fs)
    | codItemFila f == cod && not (null (usuariosFila f)) = True
    | otherwise = temFilaDeEspera cod fs

-- Função pura para renovar o empréstimo
renovarEmprestimo :: String -> String -> DB -> Either String DB
renovarEmprestimo codItem novaData db
    -- Regra 1: O item precisa estar emprestado
    | not (estaEmprestado codItem (emprestimos db)) = 
        Left ("Erro: O item " ++ codItem ++ " nao esta emprestado no momento.")
    -- Regra 2: Não pode haver ninguém na fila de espera
    | temFilaDeEspera codItem (esperas db) = 
        Left ("Aviso: Nao e possivel renovar. Ha usuarios na fila de espera para este item.")
        
    -- Atualiza a data do empréstimo ativo usando map
    | otherwise = 
        let listaAtualizada = map (\emp -> 
                if codItemEmprestimo emp == codItem && status emp == Emprestado
                then emp { dataDevolucao = novaData }
                else emp
              ) (emprestimos db)
        in Right (db { emprestimos = listaAtualizada })

-- Filtra todos os empréstimos (ativos ou devolvidos) de uma matrícula específica
historicoUsuario :: String -> DB -> [Emprestimo]
historicoUsuario mat db = filter (\emp -> matriculaUsuarioEmprestimo emp == mat) (emprestimos db)


emprestimoEmLote :: [String] -> String -> String -> DB -> IO DB
emprestimoEmLote [] _ _ db = do
    putStrLn "\nProcessamento em lote concluido!"
    return db
emprestimoEmLote (cod:cods) mat dataAtual db = do
    case registrarEmprestimo cod mat dataAtual db of
        Left erro -> do
            -- Se deu erro (ex: já emprestado)
            putStrLn $ "-> [Erro no item " ++ cod ++ "]: " ++ erro
            emprestimoEmLote cods mat dataAtual db
            
        Right novoDb -> do
            -- Se deu certo
            putStrLn $ "-> [Sucesso]: Item " ++ cod ++ " emprestado!"
            emprestimoEmLote cods mat dataAtual novoDb