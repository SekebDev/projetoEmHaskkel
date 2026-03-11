module Users (cadastrarUsuario) where
import Tipos


-- verifica se a matrícula já existe na lista
matriculaExiste :: String -> [Usuario] -> Bool
matriculaExiste _ [] = False
matriculaExiste mat (u:us) 
    | matricula u == mat = True
    | otherwise          = matriculaExiste mat us

-- verifica se o e-mail é valido (contém '@' e '.')
emailValido :: String -> Bool
emailValido emailUser = elem '@' emailUser && elem '.' emailUser



-- recebe um usuário e o banco de dados, e devolve o banco de dados atualizado com o novo usuário
cadastrarUsuario :: Usuario -> DB -> Either String DB
cadastrarUsuario novoUser db
    | matriculaExiste (matricula novoUser) (usuarios db) = Left 
        ("Erro: matricula \"" ++ matricula novoUser ++ "\" ja cadastrada.")
    | not (emailValido (email novoUser)) =  
        Left ("Erro: e-mail \"" ++ email novoUser ++ "\" esta mal formatado.")
    | otherwise = Right (db { usuarios = novoUser : usuarios db })