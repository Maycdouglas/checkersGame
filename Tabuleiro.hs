module Tabuleiro where

import Control.Monad (guard) -- função guard da biblioteca Control.Monad
-- usado em condições, permitindo que o fluxo prossiga se o retorno for True e gerando Nothing se for False
-- usado em funções com muitas validações, como por exemplo a moverPeca e atualizarCasa

data Peca = PecaJogador1 | PecaJogador2 | DamaJogador1 | DamaJogador2
    deriving (Eq, Show)
-- usa o Eq para poder comparar se dois jogadores são iguais ou diferentes
-- usa o Show para poder converter o texto para String

data Casa = Vazia | Ocupada Peca
    deriving (Eq, Show)
-- usa o Eq para poder comparar se dois jogadores são iguais ou diferentes
-- usa o Show para poder converter o texto para String

type Linha = [Casa]
type Tabuleiro = [Linha]

-- Função para checar se a peça que será capturada é uma adversaria ou nao
ehPecaAdversaria :: Peca -> Peca -> Bool
ehPecaAdversaria PecaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria DamaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria PecaJogador2  p = p == PecaJogador1 || p == DamaJogador1
ehPecaAdversaria DamaJogador2  p = p == PecaJogador1 || p == DamaJogador1

-- Função que gera o tabuleiro inicial da partida
tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ linhaJogador2 i | i <- [0..2] ] ++ -- posiciona as peças do jogador 2
    replicate 2 linhaVazia ++ -- deixa as duas linhas do meio vazias
    [ linhaJogador1 i | i <- [0..2] ] -- posiciona as peças do jogador 1
  where
    linhaJogador2 i = [if (i + j) `mod` 2 == 1 then Ocupada PecaJogador2 else Vazia | j <- [0..7]]
    linhaJogador1 i = [if (i + j) `mod` 2 == 0 then Ocupada PecaJogador1 else Vazia | j <- [0..7]]
    linhaVazia = replicate 8 Vazia

-- Função que exibe o tabuleiro no terminal
mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗" -- Desenha a borda superior do tabuleiro
    mapM_ putStrLn $ zipWith mostrarLinha [8,7..1] tab -- percorre as linhas do tabuleiro mostrando as linhas e colocando a numeracao de cada uma
    putStrLn "   ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝" -- Desenha a borda inferior do tabuleiro
    putStrLn "     A   B   C   D   E   F   G   H" -- Escreve os nomes das colunas no canto inferior

-- Função auxiliar para mostrar a linha do tabuleiro
mostrarLinha :: Int -> Linha -> String
mostrarLinha numeroLinha linha =
    " " ++ show numeroLinha ++ " ║" ++ concatMap (mostrarCasa numeroLinha) (zip [0..] linha) -- aqui tem aplicação parcial. ainda não entendi isso direito
    -- Pegue a função mostrarCasa com o primeiro argumento fixado (linhaIndex) e aplique ela a cada elemento da lista gerada por zip [0..] linha.

-- Função auxliar para mostrar a casa do tabuleiro
mostrarCasa :: Int -> (Int, Casa) -> String
mostrarCasa linhaIndex (colIndex, casa) =
    let bg = if (linhaIndex + colIndex) `mod` 2 == 0 then bgBranco else bgPreto
        texto = case casa of
                    Vazia -> "   "
                    Ocupada PecaJogador1   -> corTexto "\x1b[34;1m" " o "
                    Ocupada PecaJogador2   -> corTexto "\x1b[33;1m" " o "
                    Ocupada DamaJogador1   -> corTexto "\x1b[34;1m" " D "
                    Ocupada DamaJogador2   -> corTexto "\x1b[33;1m" " D "
    in bg texto ++ "║"

-- Cores e plano de fundo
corTexto :: String -> String -> String
corTexto cor s = cor ++ s ++ "\x1b[0m"

bgPreto :: String -> String
bgPreto s = "\x1b[40m" ++ s ++ "\x1b[0m"

bgBranco :: String -> String
bgBranco s = "\x1b[47m" ++ s ++ "\x1b[0m"

posicaoValida :: (Int, Int) -> Bool
posicaoValida (linha, coluna) =
    linha >= 0 && linha < 8 &&
    coluna >= 0 && coluna < 8 &&
    ((linha + coluna) `mod` 2 == 1)  -- somente casas pretas


-- Converte coluna de Char para índice Int (A=0, B=1, ..., H=7)
colunaParaIndice :: Char -> Maybe Int
colunaParaIndice c
    | c >= 'A' && c <= 'H' = Just (fromEnum c - fromEnum 'A')
    | otherwise = Nothing

-- Converte linha (1..8) para índice interno (0..7) invertendo o eixo vertical
linhaParaIndice :: Int -> Maybe Int
linhaParaIndice n
    | n >= 1 && n <= 8 = Just (8 - n)
    | otherwise = Nothing

-- Valida posição dada em formato de interface (linha, coluna) 
posicaoValidaInterface :: (Int, Char) -> Bool
posicaoValidaInterface (l, c) = case (linhaParaIndice l, colunaParaIndice c) of
    (Just li, Just ci) -> posicaoValida (li, ci)
    _ -> False

-- Função para consultar o conteúdo de uma posição válido no tabuleiro
obterCasa :: Tabuleiro -> (Int, Char) -> Maybe Casa
obterCasa tab pos@(l, c) =
    if posicaoValidaInterface pos
       then do
           li <- linhaParaIndice l
           ci <- colunaParaIndice c
           Just ((tab !! li) !! ci)
       else Nothing

atualizarCasa :: Tabuleiro -> (Int, Char) -> Casa -> Maybe Tabuleiro
atualizarCasa tab pos@(l, c) novaCasa = do
    li <- linhaParaIndice l
    ci <- colunaParaIndice c
    -- Verifica se a posição é válida (dentro do tabuleiro e em casa preta)
    guard (posicaoValida (li, ci))
    -- Atualiza a linha desejada
    let linhaAntiga = tab !! li
        linhaNova = take ci linhaAntiga ++ [novaCasa] ++ drop (ci + 1) linhaAntiga
    -- Atualiza o tabuleiro
    let tabuleiroNovo = take li tab ++ [linhaNova] ++ drop (li + 1) tab
    return tabuleiroNovo

moverPeca :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
moverPeca tab origem destino = do
  -- Verifica se as duas posições são válidas
  guard (posicaoValidaInterface origem)
  guard (posicaoValidaInterface destino)

  -- Pega a casa da origem
  casaOrigem <- obterCasa tab origem

  -- Verifica se tem peça na origem
  case casaOrigem of
    Vazia -> Nothing
    Ocupada peca -> do
      -- Verifica se a casa destino está vazia
      casaDestino <- obterCasa tab destino
      guard (casaDestino == Vazia)

      -- Atualiza o tabuleiro: tirar da origem
      tab1 <- atualizarCasa tab origem Vazia
      -- Atualiza o tabuleiro: colocar na destino
      tab2 <- atualizarCasa tab1 destino (Ocupada peca)

      return tab2

testaMovimento :: Tabuleiro -> (Int, Char) -> (Int, Char) -> IO ()
testaMovimento tab origem destino =
  maybe (putStrLn "Movimento inválido") mostrarTabuleiro (moverPeca tab origem destino)

movimentoSimplesValido :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Bool
movimentoSimplesValido tab origem destino = 
    case (linhaParaIndice (fst origem), colunaParaIndice (snd origem),
          linhaParaIndice (fst destino), colunaParaIndice (snd destino)) of
        (Just liOrig, Just ciOrig, Just liDest, Just ciDest) ->
            let 
                deltaLinha = liDest - liOrig
                deltaColuna = ciDest - ciOrig
                casaOrigem = obterCasa tab origem
                casaDestino = obterCasa tab destino
            in
                case casaOrigem of
                    Just (Ocupada peca) ->
                        casaDestino == Just Vazia &&
                        abs deltaColuna == 1 &&
                        case peca of
                            PecaJogador1  -> deltaLinha == -1 -- sobe
                            PecaJogador2  -> deltaLinha == 1  -- desce
                            DamaJogador1  -> abs deltaLinha == 1 -- sobe ou desce
                            DamaJogador2  -> abs deltaLinha == 1 -- sobe ou desce
                    _ -> False
        _ -> False

movimentoCapturaValido :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Bool
movimentoCapturaValido tab origem destino =
    case (linhaParaIndice (fst origem), colunaParaIndice (snd origem),
          linhaParaIndice (fst destino), colunaParaIndice (snd destino)) of
        (Just liOrig, Just ciOrig, Just liDest, Just ciDest) ->
            let 
                deltaLinha = liDest - liOrig
                deltaColuna = ciDest - ciOrig

                -- Posição da peça no meio
                meioLinha = liOrig + deltaLinha `div` 2
                meioColuna = ciOrig + deltaColuna `div` 2

                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char)

                casaOrigem = obterCasa tab origem
                casaMeio   = obterCasa tab posMeio
                casaDestino = obterCasa tab destino
            in
                case casaOrigem of
                    Just (Ocupada pecaOrigem) ->
                        abs deltaLinha == 2 && abs deltaColuna == 2 &&
                        casaDestino == Just Vazia &&
                        case casaMeio of
                            Just (Ocupada pecaMeio) -> ehPecaAdversaria pecaOrigem pecaMeio
                            _ -> False
                    _ -> False
        _ -> False

capturarPeca :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
capturarPeca tab origem destino = do
    -- Verifica se a captura é válida
    guard (movimentoCapturaValido tab origem destino)

    casaOrigem <- obterCasa tab origem
    case casaOrigem of
        Vazia -> Nothing
        Ocupada peca -> do
            liOrig <- linhaParaIndice (fst origem)
            ciOrig <- colunaParaIndice (snd origem)
            liDest <- linhaParaIndice (fst destino)
            ciDest <- colunaParaIndice (snd destino)

            -- Calcula a posição da peça capturada
            let meioLinha = liOrig + (liDest - liOrig) `div` 2
                meioColuna = ciOrig + (ciDest - ciOrig) `div` 2
                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char)

            -- Remove a peça capturada
            tab1 <- atualizarCasa tab posMeio Vazia
            -- Move a peça de origem para destino
            tab2 <- atualizarCasa tab1 origem Vazia
            tab3 <- atualizarCasa tab2 destino (Ocupada peca)

            return tab3
