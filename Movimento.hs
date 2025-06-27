{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Movimento where

import Tabuleiro
import Posicao
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Debug.Trace (trace)

-- Move peça de acordo se é uma dama ou uma peça comum
moverPeca :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
moverPeca tab origem destino = do
    guard (ehPosicaoValidaInterface origem)
    guard (ehPosicaoValidaInterface destino)

    casaOrigem <- obterCasa tab origem
    case casaOrigem of
        Vazia -> Nothing
        Ocupada peca ->
            if ehDama peca
                then moverDama tab origem destino peca
                else moverPecaComum tab origem destino peca

-- Move peça comum
moverPecaComum :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Peca -> Maybe Tabuleiro
moverPecaComum tab origem destino peca = do
    guard (movimentoSimplesValido tab origem destino)
    casaDestino <- obterCasa tab destino
    guard (casaDestino == Vazia)

    let novaPeca = avaliarPromocaoParaDama destino peca -- Avalia se deve promover a peça para dama
    tab1 <- atualizarCasa tab origem Vazia -- Atualiza o tabuleiro: tirar da origem
    tab2 <- atualizarCasa tab1 destino (Ocupada novaPeca)  -- Atualiza o tabuleiro: colocar na destino
    return tab2

--Move Dama
moverDama :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Peca -> Maybe Tabuleiro
moverDama tab origem destino dama = do
    -- Verifica se o destino está vazio
    casaDestino <- obterCasa tab destino
    guard (casaDestino == Vazia)

    -- Converte posições para índices
    liOrig <- linhaParaIndice (fst origem)
    ciOrig <- colunaParaIndice (snd origem)
    liDest <- linhaParaIndice (fst destino)
    ciDest <- colunaParaIndice (snd destino)

    -- Verifica se é uma diagonal válida
    guard (abs (liDest - liOrig) == abs (ciDest - ciOrig))

    -- Verifica se caminho está livre
    guard (caminhoLivre tab (liOrig, ciOrig) (liDest, ciDest))

    -- Atualiza o tabuleiro
    tab1 <- atualizarCasa tab origem Vazia -- Atualiza o tabuleiro: tirar da origem
    tab2 <- atualizarCasa tab1 destino (Ocupada dama)  -- Atualiza o tabuleiro: colocar na destino

    return tab2

-- Atualiza a casa do tabuleiro para vazia ou ocupada, a depender do caso
atualizarCasa :: Tabuleiro -> (Int, Char) -> Casa -> Maybe Tabuleiro
atualizarCasa tab (l, c) novaCasa = do
    li <- linhaParaIndice l
    ci <- colunaParaIndice c
    guard (ehPosicaoValida (li, ci))
    let linhaNova = substituirNaLista (tab !! li) ci novaCasa
        tabuleiroNovo = substituirNaLista tab li linhaNova
    return tabuleiroNovo

-- Função para consultar o conteúdo de uma posição válido no tabuleiro
obterCasa :: Tabuleiro -> (Int, Char) -> Maybe Casa
obterCasa tab pos@(l, c) =
    if ehPosicaoValidaInterface pos
       then do
           li <- linhaParaIndice l
           ci <- colunaParaIndice c
           Just ((tab !! li) !! ci) -- Acessa a linha do tabuleiro depois a casa da linha
       else Nothing

-- Função para substituir um elemento de uma lista por outro 
substituirNaLista :: [a] -> Int -> a -> [a]
substituirNaLista lista idx novoElemento =
    take idx lista ++ [novoElemento] ++ drop (idx + 1) lista

-- verifica se um movimento simples é válido (sem captura de peças)
movimentoSimplesValido :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Bool
movimentoSimplesValido tab origem destino = 
    case (linhaParaIndice (fst origem), colunaParaIndice (snd origem), -- fst e snd são funcoes haskell que retornam os elementos da tupla
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

-- Verifica se uma captura simples é válida (não funciona para captura de damas)
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

                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char) -- converte para numero a coluna depois soma e volta para letra

                casaOrigem = obterCasa tab origem
                casaMeio   = obterCasa tab posMeio
                casaDestino = obterCasa tab destino
            in
                case casaOrigem of
                    Just (Ocupada pecaOrigem) ->
                        abs deltaLinha == 2 && abs deltaColuna == 2 &&
                        casaDestino == Just Vazia &&
                        case casaMeio of
                            Just (Ocupada pecaMeio) -> ehPecaAdversaria pecaOrigem pecaMeio -- retorno positivo
                            _ -> False
                    _ -> False
        _ -> False

-- Verifica se uma captura por dama é válida
movimentoCapturaDamaValido :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Bool
movimentoCapturaDamaValido tab origem destino =
    case (obterCasa tab origem, obterCasa tab destino) of
        (Just (Ocupada peca), Just Vazia)
            | ehDama peca ->
                case (linhaParaIndice (fst origem), colunaParaIndice (snd origem),
                      linhaParaIndice (fst destino), colunaParaIndice (snd destino)) of
                    (Just liOrig, Just ciOrig, Just liDest, Just ciDest) ->
                        let
                            -- Verifica se está na diagonal
                            deltaLinha = liDest - liOrig
                            deltaColuna = ciDest - ciOrig
                            emDiagonal = abs deltaLinha == abs deltaColuna

                            -- Gera todas as posições entre origem e destino (excluindo extremos)
                            direcaoLinha = if deltaLinha > 0 then 1 else -1
                            direcaoColuna = if deltaColuna > 0 then 1 else -1
                            posicoesEntre = zip
                                [liOrig + direcaoLinha, liOrig + 2 * direcaoLinha .. liDest - direcaoLinha]
                                [ciOrig + direcaoColuna, ciOrig + 2 * direcaoColuna .. ciDest - direcaoColuna]

                            casasEntre = map (obterCasaPorIndice tab) posicoesEntre
                            ocupadas = filter (/= Just Vazia) casasEntre
                        in
                            emDiagonal &&
                            length ocupadas == 1 &&
                            case head ocupadas of
                                Just (Ocupada pecaAlvo) -> ehPecaAdversaria peca pecaAlvo
                                _ -> False
                    _ -> False
        _ -> False

capturarPecaComPos :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe (Tabuleiro, (Int, Char))
capturarPecaComPos tab origem destino = do
    novoTab <- capturarPeca tab origem destino
    return (novoTab, destino)

capturarPeca :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
capturarPeca tab origem destino = do
    casaOrigem <- obterCasa tab origem
    case casaOrigem of
        Ocupada peca
            | ehDama peca && movimentoCapturaDamaValido tab origem destino ->
                capturarDama tab origem destino
            | not (ehDama peca) && movimentoCapturaValido tab origem destino ->
                capturarPecaSimples tab origem destino
        _ -> Nothing

-- Função para capturar uma peça de forma simples (não funciona para dama)
capturarPecaSimples :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
capturarPecaSimples tab origem destino = do
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
                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char) -- converte para numero a coluna depois soma e volta para letra

            -- Em vez de remover, marca como semicapturada
            casaMeio <- obterCasa tab posMeio
            novaCasaMeio <- case casaMeio of
                Ocupada p -> Just (Ocupada (Semicapturada p))
                _ -> Nothing

            -- Remove a peça capturada
            tab1 <- atualizarCasa tab posMeio novaCasaMeio
            -- Move a peça de origem para destino
            tab2 <- atualizarCasa tab1 origem Vazia

            -- Avalia se deve promover a peça para dama
            let novaPeca = avaliarPromocaoParaDama destino peca

            tab3 <- atualizarCasa tab2 destino (Ocupada peca)

            return tab3

-- Captura por uma dama
capturarDama :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
capturarDama tab origem destino = do
    -- Verifica se a captura é válida
    guard (movimentoCapturaDamaValido tab origem destino)

    -- Obtém a peça de origem (dama)
    casaOrigem <- obterCasa tab origem
    peca <- case casaOrigem of
        Ocupada p -> Just p
        _ -> Nothing

    -- Converte posições para índices
    liOrig <- linhaParaIndice (fst origem)
    ciOrig <- colunaParaIndice (snd origem)
    liDest <- linhaParaIndice (fst destino)
    ciDest <- colunaParaIndice (snd destino)

    -- Calcula direção do movimento
    let deltaLinha = if liDest > liOrig then 1 else -1
        deltaColuna = if ciDest > ciOrig then 1 else -1

        -- Gera as posições entre origem e destino (excluindo extremos)
        posicoesEntre = zip
            [liOrig + deltaLinha, liOrig + 2 * deltaLinha .. liDest - deltaLinha]
            [ciOrig + deltaColuna, ciOrig + 2 * deltaColuna .. ciDest - deltaColuna]

        -- Encontra a posição da peça a ser capturada
        pecaCapturadaIdx = head [ (li, ci) | (li, ci) <- posicoesEntre,
                                             obterCasaPorIndice tab (li, ci) /= Just Vazia ]

    -- Converte o índice da peça capturada para interface
    let posCaptura = (8 - fst pecaCapturadaIdx, toEnum (fromEnum 'A' + snd pecaCapturadaIdx) :: Char)

    -- Em vez de remover, marca como semicapturada
    casaCapturada <- obterCasa tab posCaptura
    novaCasaCaptura <- case casaCapturada of
        Ocupada p -> Just (Ocupada (Semicapturada p))
        _ -> Nothing

    -- Remove peça capturada
    tab1 <- atualizarCasa tab posCaptura novaCasaCaptura
    -- Remove dama da origem
    tab2 <- atualizarCasa tab1 origem Vazia
    -- Coloca dama no destino
    tab3 <- atualizarCasa tab2 destino (Ocupada peca)

    return tab3

-- Verifica se o caminho entre duas posições está livre (exceto as extremidades)
caminhoLivre :: Tabuleiro -> (Int, Int) -> (Int, Int) -> Bool
caminhoLivre tab (liOrig, ciOrig) (liDest, ciDest) =
  let deltaLinha = if liDest > liOrig then 1 else -1
      deltaColuna = if ciDest > ciOrig then 1 else -1
      -- gera a lista de posições entre origem e destino, excluindo origem e destino
      posicoesEntre = zip -- monta as coordenadas usando as listas de todas os indices de linhas e colunas entre as posicoes
        [liOrig + deltaLinha, liOrig + 2 * deltaLinha .. liDest - deltaLinha] --
        [ciOrig + deltaColuna, ciOrig + 2 * deltaColuna .. ciDest - deltaColuna]
  in all (\pos -> obterCasaPorIndice tab pos == Just Vazia) posicoesEntre -- função que checa se todos os elementos da lista cumprem a condição

-- Função auxiliar para obter casa usando índices (linha,coluna) no Tabuleiro
obterCasaPorIndice :: Tabuleiro -> (Int, Int) -> Maybe Casa
obterCasaPorIndice tab (li, ci)
  | li >= 0 && li < 8 && ci >= 0 && ci < 8 = Just ((tab !! li) !! ci)
  | otherwise = Nothing

-- wrapper público: chama o helper com visited contendo apenas a origem
sequenciasCapturaSimples :: Tabuleiro -> (Int, Char) -> [[(Int, Char)]]
sequenciasCapturaSimples tab origem =
  buscar tab origem [origem]
  where
    -- buscar tabAtual posAtual visited retorna todas as sequências a partir de posAtual
    buscar :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> [[(Int, Char)]]
    buscar tabAtual posAtual visited =
      let
        -- direções de captura simples
        direcoes = [(-2,-2),(-2,2),(2,-2),(2,2)]

        -- para cada direção, tenta achar um destino válido
        capturasPossiveis =
          do
            (dl,dc) <- direcoes
            liOrig   <- maybeToList $ linhaParaIndice (fst posAtual)
            ciOrig   <- maybeToList $ colunaParaIndice (snd posAtual)
            let liDest = liOrig + dl
                ciDest = ciOrig + dc
                destino = (8 - liDest, toEnum (fromEnum 'A' + ciDest))
            guard (ehPosicaoValidaInterface destino)
            guard (movimentoCapturaValido tabAtual posAtual destino)
            guard (destino `notElem` visited)  -- **bloqueio do ciclo**
            return destino

      in case capturasPossiveis of
           []  -> [[]]  -- não tem mais capturas: sequência acabou
           dsts -> concatMap (\destino ->
                     case simularCapturaSimples tabAtual posAtual destino of
                       Just tabNovo ->
                         -- na recursão, adicionamos destino em visited
                         map (destino :) (buscar tabNovo destino (destino:visited))
                       Nothing -> []
                   ) dsts

-- Função auxiliar que simula uma captura simples sem modificar o tabuleiro original
simularCapturaSimples :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
simularCapturaSimples tab origem destino = do
    guard (movimentoCapturaValido tab origem destino)
    casaOrigem <- obterCasa tab origem
    case casaOrigem of
        Ocupada peca -> do
            liOrig <- linhaParaIndice (fst origem)
            ciOrig <- colunaParaIndice (snd origem)
            liDest <- linhaParaIndice (fst destino)
            ciDest <- colunaParaIndice (snd destino)

            let meioLinha = liOrig + (liDest - liOrig) `div` 2
                meioColuna = ciOrig + (ciDest - ciOrig) `div` 2
                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char)

            tab1 <- atualizarCasa tab posMeio Vazia
            tab2 <- atualizarCasa tab origem Vazia
            tab3 <- atualizarCasa tab2 destino (Ocupada peca)

            return tab3
        _ -> Nothing

removerSemicapturadas :: Tabuleiro -> Tabuleiro
removerSemicapturadas =
    map (map remover)
  where
    remover (Ocupada (Semicapturada _)) = Vazia
    remover outra = outra
