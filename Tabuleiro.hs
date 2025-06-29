module Tabuleiro where

import Posicao
import Control.Monad (guard) -- Permite que o fluxo prossiga se o retorno for True e gerando Nothing se for False
-- Usado em funções com muitas validações, como por exemplo a moverPeca e atualizarCasa

-- Define novos tipos de dados
data Peca = PecaJogador1 | PecaJogador2 | DamaJogador1 | DamaJogador2 | Semicapturada Peca
    deriving (Eq, Show)

data Casa = Vazia | Ocupada Peca
    deriving (Eq, Show)

-- Define tipo de dado Jogador
data Jogador = Jogador1 | Jogador2
    deriving (Eq, Show)

-- Types para tornar o código mais legível
type Linha = [Casa]
type Tabuleiro = [Linha]

-- Verifica se uma peça pertence a um jogador
pecaPertenceAoJogador :: Peca -> Jogador -> Bool
pecaPertenceAoJogador PecaJogador1 Jogador1 = True
pecaPertenceAoJogador DamaJogador1 Jogador1 = True
pecaPertenceAoJogador PecaJogador2 Jogador2 = True
pecaPertenceAoJogador DamaJogador2 Jogador2 = True
pecaPertenceAoJogador _ _ = False

-- Verifica se o Jogador está sem peças
jogadorSemPecas :: Tabuleiro -> Jogador -> Bool
jogadorSemPecas tab jogador =
    null [()
         | linha <- tab
         , Ocupada peca <- linha
         , pecaPertenceAoJogador peca jogador
         ]

-- Retorna as posições das peças do Jogador
posicoesDoJogador :: Tabuleiro -> Jogador -> [(Int, Char)]
posicoesDoJogador tab jogador = 
    [ (8 - li, toEnum (fromEnum 'A' + ci)) 
    | (li, linha) <- zip [0..] tab
    , (ci, casa) <- zip [0..] linha
    , Ocupada peca <- [casa]
    , pecaPertenceAoJogador peca jogador
    ]

-- Alterna entre jogadores
trocarJogador :: Jogador -> Jogador
trocarJogador Jogador1 = Jogador2
trocarJogador Jogador2 = Jogador1

-- Colore o nome do Jogador 
nomeJogadorColorido :: Jogador -> String
nomeJogadorColorido Jogador1 = corTexto "\x1b[34;1m" "Jogador 1"  -- Azul
nomeJogadorColorido Jogador2 = corTexto "\x1b[33;1m" "Jogador 2"  -- Amarelo

-- Checa se a peça que será capturada é uma adversaria ou nao
ehPecaAdversaria :: Peca -> Peca -> Bool
ehPecaAdversaria PecaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria DamaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria PecaJogador2  p = p == PecaJogador1 || p == DamaJogador1
ehPecaAdversaria DamaJogador2  p = p == PecaJogador1 || p == DamaJogador1

-- Verifica se é Dama
ehDama :: Peca -> Bool
ehDama DamaJogador1 = True
ehDama DamaJogador2 = True
ehDama _            = False

-- Realiza a promoção para Dama
promoverParaDama :: (Int, Peca) -> Peca
promoverParaDama (0, PecaJogador1) = DamaJogador1
promoverParaDama (7, PecaJogador2) = DamaJogador2
promoverParaDama (_, peca)         = peca

-- Verifica se realizará a promoção para Dama
avaliarPromocaoParaDama :: (Int, Char) -> Peca -> Peca
avaliarPromocaoParaDama destino peca =
    case linhaParaIndice (fst destino) of
        Just linhaIndex -> promoverParaDama (linhaIndex, peca)
        Nothing -> peca

-- Remove as peças semicapturadas ao final de uma jogada
removerSemicapturadas :: Tabuleiro -> Tabuleiro
removerSemicapturadas =
    map (map remover)
  where
    remover (Ocupada (Semicapturada _)) = Vazia
    remover outra = outra

-- Obtem casa usando índices internos (linha, coluna) no Tabuleiro
obterCasaPorIndice :: Tabuleiro -> (Int, Int) -> Maybe Casa
obterCasaPorIndice tab (linhaIndex, colIndex)
  | linhaIndex >= 0 && linhaIndex < 8 && colIndex >= 0 && colIndex < 8 = Just ((tab !! linhaIndex) !! colIndex)
  | otherwise = Nothing

-- Obtem casa usando índices da interface (linha, coluna) no Tabuleiro
obterCasa :: Tabuleiro -> (Int, Char) -> Maybe Casa
obterCasa tab pos@(linha, coluna) =
    if ehPosicaoValidaInterface pos
       then do
           linhaIndex <- linhaParaIndice linha
           colIndex <- colunaParaIndice coluna
           Just ((tab !! linhaIndex) !! colIndex) -- Acessa a linha do tabuleiro depois a casa da linha
       else Nothing

-- Obtem a peça em uma casa do tabuleiro
obterPeca :: Tabuleiro -> (Int, Char) -> Maybe Peca
obterPeca tab pos = case obterCasa tab pos of
    Just (Ocupada peca) -> Just peca
    _ -> Nothing

-- Substitui um elemento de uma lista por outro 
substituirNaLista :: [a] -> Int -> a -> [a]
substituirNaLista lista idx novoElemento =
    take idx lista ++ [novoElemento] ++ drop (idx + 1) lista

-- Atualiza a casa do tabuleiro para vazia ou ocupada, a depender do caso
atualizarCasa :: Tabuleiro -> (Int, Char) -> Casa -> Maybe Tabuleiro
atualizarCasa tab (linha, coluna) novaCasa = do
    linhaIndex <- linhaParaIndice linha
    colIndex <- colunaParaIndice coluna
    guard (ehPosicaoValida (linhaIndex, colIndex))
    let linhaNova = substituirNaLista (tab !! linhaIndex) colIndex novaCasa
        tabuleiroNovo = substituirNaLista tab linhaIndex linhaNova
    return tabuleiroNovo

-- Verifica se o caminho entre duas posições está livre (exceto as extremidades)
caminhoEhLivre :: Tabuleiro -> (Int, Int) -> (Int, Int) -> Bool
caminhoEhLivre tab (linhaIndexOrig, colIndexOrig) (linhaIndexDest, colIndexDest) =
  let deltaLinha = if linhaIndexDest > linhaIndexOrig then 1 else -1
      deltaColuna = if colIndexDest > colIndexOrig then 1 else -1
      -- Gera a lista de posições entre origem e destino, excluindo origem e destino
      posicoesEntre = zip -- Monta as coordenadas usando as listas de todas os indices de linhas e colunas entre as posicoes
        [linhaIndexOrig + deltaLinha, linhaIndexOrig + 2 * deltaLinha .. linhaIndexDest - deltaLinha]
        [colIndexOrig + deltaColuna, colIndexOrig + 2 * deltaColuna .. colIndexDest - deltaColuna]
  in all (\pos -> obterCasaPorIndice tab pos == Just Vazia) posicoesEntre -- Checa se todos os elementos da lista cumprem a condição

-- Gera o tabuleiro inicial da partida
tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ linhaJogador2 i | i <- [0..2] ] ++ -- Posiciona as peças do jogador 2
    replicate 2 linhaVazia ++ -- Deixa as duas linhas do meio vazias
    [ linhaJogador1 i | i <- [0..2] ] -- Posiciona as peças do jogador 1
    where
        linhaJogador2 i = [if odd (i + j) then Ocupada PecaJogador2 else Vazia | j <- [0..7]]
        linhaJogador1 i = [if even (i + j) then Ocupada PecaJogador1 else Vazia | j <- [0..7]]
        linhaVazia = replicate 8 Vazia

-- Define cores e plano de fundo usando código ANSI
corTexto :: String -> String -> String
corTexto cor s = cor ++ s ++ "\x1b[0m"

bgPreto :: String -> String
bgPreto s = "\x1b[40m" ++ s ++ "\x1b[0m"

bgBranco :: String -> String
bgBranco s = "\x1b[47m" ++ s ++ "\x1b[0m"

-- Exibe a casa do tabuleiro
mostrarCasa :: Int -> (Int, Casa) -> String
mostrarCasa linhaIndex (colIndex, casa) =
    let bg = if even (linhaIndex + colIndex) then bgBranco else bgPreto -- Define a cor branca ou preta para a Casa
        texto = case casa of
            Vazia -> "   "
            Ocupada (Semicapturada DamaJogador1) -> corTexto "\x1b[37;1m" " D "
            Ocupada (Semicapturada DamaJogador2) -> corTexto "\x1b[37;1m" " D "
            Ocupada (Semicapturada _)            -> corTexto "\x1b[37;1m" " o " -- Branco
            Ocupada PecaJogador1   -> corTexto "\x1b[34;1m" " o " -- Azul
            Ocupada PecaJogador2   -> corTexto "\x1b[33;1m" " o " -- Amarelo
            Ocupada DamaJogador1   -> corTexto "\x1b[34;1m" " D "
            Ocupada DamaJogador2   -> corTexto "\x1b[33;1m" " D "
    in bg texto ++ "|" -- Define o separador lateral das casas

-- Exibe a linha do tabuleiro
mostrarLinha :: Int -> Linha -> String
mostrarLinha numeroLinha linha =
    " " ++ show numeroLinha ++ " |" ++ concatMap (mostrarCasa numeroLinha) (zip [0..] linha)
    -- Pega a função mostrarCasa com o primeiro argumento fixado (linhaIndex) e aplica ela a cada elemento da lista gerada por zip [0..] linha.

-- Exibe o tabuleiro no terminal
mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   +---+---+---+---+---+---+---+---+" -- Desenha a borda superior do tabuleiro
    mapM_ putStrLn $ zipWith mostrarLinha [8,7..1] tab -- Percorre as linhas do tabuleiro, as exibindo numerando-as
    putStrLn "   +---+---+---+---+---+---+---+---+" -- Desenha a borda inferior do tabuleiro
    putStrLn "     A   B   C   D   E   F   G   H" -- Escreve os nomes das colunas no canto inferior
