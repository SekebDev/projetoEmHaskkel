module BancoDeDados (clearDB) where
import Tipos


-- devolve um banco de dados vazio, sem usuários ou itens
clearDB :: DB
clearDB = DB {
    usuarios = [],
    itens = [],
    emprestimos = [],
    esperas = []
}
