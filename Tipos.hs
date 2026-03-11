module Tipos(Usuario(..), Item(..), DB(..), TipoMidia(..)) where

-- tipo para os Usuários
data Usuario = Usuario {
    nome      :: String,
    matricula :: String,
    email     :: String
} deriving (Show, Eq)


-- tipos de mídia disponíveis
data TipoMidia = Livro | Filme | Jogo deriving (Show, Eq)

-- tipo para Livros, Filmes e Jogos
data Item = Item {
    titulo      :: String,
    autor       :: String,
    ano         :: Int,
    codigoUnico :: String,
    tipoMidia   :: TipoMidia
} deriving (Show, Eq)

-- tipo para os Empréstimos
data Emprestimo = Emprestimo {
}


-- tipo para o Banco de Dados
data DB = DB {
    usuarios :: [Usuario],
    itens    :: [Item]
} deriving (Show)