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

-- regista um empréstimo
registrarEmprestimo :: String -> String -> String -> DB -> Either String DB
registrarEmprestimo codItem matriculaUser dataAtual db
    -- verifica se esta emprestado
    | estaEmprestado codItem (emprestimos db) =
        Left $ "Aviso: O item " ++ codItem ++ " ja esta emprestado.\nDeseja entrar na lista de espera? (S/N)"
        
    -- WIP: outras verificações como se o usuário tem empréstimos atrasados, ou se já está na fila de espera para esse item

    -- se passar pelas verificaçoes cria o empréstimo e adiciona ao DB
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