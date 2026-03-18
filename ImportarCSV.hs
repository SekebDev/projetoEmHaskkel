module ImportarCSV where
import Tipos


importarDeCSV :: IO DB
importarDeCSV = do
    u <- lerUsuarios
    i <- lerItens
    e <- lerEmprestimos
    f <- lerFilas
    return DB {
        usuarios = u,
        itens = i,
        emprestimos = e,
        esperas = f
    }

lerUsuarios :: IO [Usuario]
lerUsuarios = do
    conteudo <- readFile "usuarios.csv"
    let linhas = lines conteudo
    return (map csvParaUsuario linhas)

csvParaUsuario :: String -> Usuario
csvParaUsuario linha =
    let [n, m, e] = splitCSV linha
    in Usuario n m e


lerItens :: IO [Item]
lerItens = do
    conteudo <- readFile "itens.csv"
    let linhas = lines conteudo
    return (map csvParaItem linhas)

csvParaItem :: String -> Item
csvParaItem linha =
    let [t, a, anoStr, cod, tipoStr] = splitCSV linha
    in Item t a (read anoStr) cod (read tipoStr)


lerEmprestimos :: IO [Emprestimo]
lerEmprestimos = do
    conteudo <- readFile "emprestimos.csv"
    let linhas = lines conteudo
    return (map csvParaEmprestimo linhas)

csvParaEmprestimo :: String -> Emprestimo
csvParaEmprestimo linha =
    let [cod, mat, dr, dd, st] = splitCSV linha
    in Emprestimo cod mat dr dd (read st)


lerFilas :: IO [FilaEspera]
lerFilas = do
    conteudo <- readFile "espera.csv"
    let linhas = lines conteudo
    return (map csvParaFila linhas)

csvParaFila :: String -> FilaEspera
csvParaFila linha =
    let (cod:resto) = splitCSV linha
        usuariosLista = words (unwords resto)
    in FilaEspera cod usuariosLista

--Função auxilar
splitCSV :: String -> [String]
splitCSV [] = []
splitCSV s =
    let (campo, resto) = break (== ',') s
    in campo : case resto of
        [] -> []
        (_:xs) -> splitCSV xs

stringParaStatus :: String -> StatusEmprestimo
stringParaStatus "Emprestado" = Emprestado
stringParaStatus "Devolvido"  = Devolvido
stringParaStatus "Atrasado"   = Atrasado
stringParaStatus _ = error "Status inválido"