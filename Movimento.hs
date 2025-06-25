{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Movimento where

import Tabuleiro
import Posicao
import Control.Monad (guard)

-- Realiza a movimentação simples de uma peça
moverPeca :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
moverPeca tab origem destino = do
  -- Verifica se as duas posições são válidas
  guard (ehPosicaoValidaInterface origem)
  guard (ehPosicaoValidaInterface destino)

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

-- Função para capturar uma peça de forma simples (não funciona para dama)
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
                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char) -- converte para numero a coluna depois soma e volta para letra

            -- por aqui que vai entrar função para impedir que capture somente uma peça ao invés de várias

            -- Remove a peça capturada
            tab1 <- atualizarCasa tab posMeio Vazia
            -- Move a peça de origem para destino
            tab2 <- atualizarCasa tab1 origem Vazia
            tab3 <- atualizarCasa tab2 destino (Ocupada peca)

            return tab3

-- Função para checar se a peça que será capturada é uma adversaria ou nao
ehPecaAdversaria :: Peca -> Peca -> Bool
ehPecaAdversaria PecaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria DamaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria PecaJogador2  p = p == PecaJogador1 || p == DamaJogador1
ehPecaAdversaria DamaJogador2  p = p == PecaJogador1 || p == DamaJogador1