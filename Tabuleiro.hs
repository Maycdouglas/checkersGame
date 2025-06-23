module Tabuleiro where

import Control.Monad (guard)

data Peca = PecaJogador | PecaMaquina | DamaJogador | DamaMaquina
    deriving (Eq, Show)

pecaAdversaria :: Peca -> Peca -> Bool
pecaAdversaria PecaJogador  p = p == PecaMaquina || p == DamaMaquina
pecaAdversaria DamaJogador  p = p == PecaMaquina || p == DamaMaquina
pecaAdversaria PecaMaquina  p = p == PecaJogador || p == DamaJogador
pecaAdversaria DamaMaquina  p = p == PecaJogador || p == DamaJogador
    

data Casa = Vazia | Ocupada Peca
    deriving (Eq, Show)

type Linha = [Casa]
type Tabuleiro = [Linha]

tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ linhaMaquina i | i <- [0..2] ] ++
    replicate 2 linhaVazia ++
    [ linhaJogador i | i <- [0..2] ]
  where
    linhaMaquina i = [if (i + j) `mod` 2 == 1 then Ocupada PecaMaquina else Vazia | j <- [0..7]]
    linhaJogador i = [if (i + j) `mod` 2 == 0 then Ocupada PecaJogador else Vazia | j <- [0..7]]
    linhaVazia = replicate 8 Vazia

mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗"
    mapM_ putStrLn $ zipWith mostrarLinha [8,7..1] tab
    putStrLn "   ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝"
    putStrLn "     A   B   C   D   E   F   G   H"
  where
    mostrarLinha i linha =
        " " ++ show i ++ " ║" ++ concatMap (mostrarCasa i) (zip [0..] linha)

    mostrarCasa linhaIndex (colIndex, casa) =
        let bg = if (linhaIndex + colIndex) `mod` 2 == 0 then bgBranco else bgPreto
            texto = case casa of
                        Vazia -> "   "
                        Ocupada PecaJogador   -> corTexto "\x1b[34m" " o "
                        Ocupada PecaMaquina   -> corTexto "\x1b[31m" " o "
                        Ocupada DamaJogador   -> corTexto "\x1b[94m" " D "
                        Ocupada DamaMaquina   -> corTexto "\x1b[91m" " D "
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
                            PecaJogador  -> deltaLinha == -1 -- sobe
                            PecaMaquina  -> deltaLinha == 1  -- desce
                            DamaJogador  -> abs deltaLinha == 1 -- sobe ou desce
                            DamaMaquina  -> abs deltaLinha == 1 -- sobe ou desce
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
                            Just (Ocupada pecaMeio) -> pecaAdversaria pecaOrigem pecaMeio
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
