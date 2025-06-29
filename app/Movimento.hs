-- Desenvolvido por: Maycon Douglas Henrique da Silva Gomes - 202065570C

module Movimento where

import Tabuleiro
import Posicao
import Control.Monad (guard) -- Permite que o fluxo prossiga se o retorno for True e gerando Nothing se for False
-- Usado em funções com muitas validações, como por exemplo a moverPeca e atualizarCasa
import Data.Maybe (maybeToList, fromJust)

-- Verifica se um movimento simples é válido (sem captura de peças)
movimentoSimplesValido :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Bool
movimentoSimplesValido tab origem destino = 
    case (linhaParaIndice (fst origem), colunaParaIndice (snd origem),
          linhaParaIndice (fst destino), colunaParaIndice (snd destino)) of
        (Just linhaIndexOrig, Just colIndexOrig, Just linhaIndexDest, Just colIndexDest) ->
            let 
                deltaLinha = linhaIndexDest - linhaIndexOrig
                deltaColuna = colIndexDest - colIndexOrig
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

-- Move peça comum
moverPecaComum :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Peca -> Maybe Tabuleiro
moverPecaComum tab origem destino peca = do
    guard (movimentoSimplesValido tab origem destino) -- Verifica se é possível realizar o movimento simples

    let novaPeca = avaliarPromocaoParaDama destino peca -- Avalia se deve promover a peça para dama
    tabSemOrigem <- atualizarCasa tab origem Vazia -- Atualiza o tabuleiro: tirar da origem
    tabComDestino <- atualizarCasa tabSemOrigem destino (Ocupada novaPeca)  -- Atualiza o tabuleiro: colocar na destino
    return tabComDestino

-- Move Dama
moverDama :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Peca -> Maybe Tabuleiro
moverDama tab origem destino dama = do
    -- Verifica se o destino está vazio
    casaDestino <- obterCasa tab destino
    guard (casaDestino == Vazia)

    -- Converte posições para índices
    linhaIndexOrig <- linhaParaIndice (fst origem)
    colIndexOrig <- colunaParaIndice (snd origem)
    linhaIndexDest <- linhaParaIndice (fst destino)
    colIndexDest <- colunaParaIndice (snd destino)

    -- Verifica se é uma diagonal válida
    guard (abs (linhaIndexDest - linhaIndexOrig) == abs (colIndexDest - colIndexOrig)) -- A diferença absoluta entre as linhas deve ser igual a das colunas

    -- Verifica se caminho está livre
    guard (caminhoEhLivre tab (linhaIndexOrig, colIndexOrig) (linhaIndexDest, colIndexDest))

    -- Atualiza o tabuleiro
    tabSemOrigem <- atualizarCasa tab origem Vazia -- Atualiza o tabuleiro: tirar da origem
    tabComDestino <- atualizarCasa tabSemOrigem destino (Ocupada dama)  -- Atualiza o tabuleiro: colocar na destino

    return tabComDestino

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

-- Verifica se uma captura simples é válida
movimentoCapturaValido :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Bool
movimentoCapturaValido tab origem destino =
    case (linhaParaIndice (fst origem), colunaParaIndice (snd origem),
          linhaParaIndice (fst destino), colunaParaIndice (snd destino)) of
        (Just linhaIndexOrig, Just colIndexOrig, Just linhaIndexDest, Just colIndexDest) ->
            let 
                deltaLinha = linhaIndexDest - linhaIndexOrig
                deltaColuna = colIndexDest - colIndexOrig

                -- Calcula posição da peça no meio
                meioLinha = linhaIndexOrig + deltaLinha `div` 2
                meioColuna = colIndexOrig + deltaColuna `div` 2

                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char) -- Converte para a coluna para número, depois soma e volta para letra

                casaOrigem = obterCasa tab origem
                casaMeio   = obterCasa tab posMeio
                casaDestino = obterCasa tab destino
            in
                case casaOrigem of
                    Just (Ocupada pecaOrigem) ->
                        abs deltaLinha == 2 && abs deltaColuna == 2 &&
                        casaDestino == Just Vazia &&
                        case casaMeio of
                            Just (Ocupada pecaMeio) -> ehPecaAdversaria pecaOrigem pecaMeio -- Retorna que captura simples é possível
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
                    (Just linhaIndexOrig, Just colIndexOrig, Just linhaIndexDest, Just colIndexDest) ->
                        let
                            -- Verifica se está na diagonal
                            deltaLinha = linhaIndexDest - linhaIndexOrig
                            deltaColuna = colIndexDest - colIndexOrig
                            emDiagonal = abs deltaLinha == abs deltaColuna

                            -- Gera todas as posições entre origem e destino (excluindo extremos)
                            direcaoLinha = if deltaLinha > 0 then 1 else -1
                            direcaoColuna = if deltaColuna > 0 then 1 else -1
                            posicoesEntre = zip
                                [linhaIndexOrig + direcaoLinha, linhaIndexOrig + 2 * direcaoLinha .. linhaIndexDest - direcaoLinha]
                                [colIndexOrig + direcaoColuna, colIndexOrig + 2 * direcaoColuna .. colIndexDest - direcaoColuna]

                            casasEntre = map (obterCasaPorIndice tab) posicoesEntre -- Obtem todas as casas das posicoes da diagonal
                            ocupadas = filter (/= Just Vazia) casasEntre -- Filtra para ter apenas as casas ocupadas
                        in
                            emDiagonal &&
                            length ocupadas == 1 &&
                            case head ocupadas of
                                Just (Ocupada pecaAlvo) -> ehPecaAdversaria peca pecaAlvo
                                _ -> False
                    _ -> False
        _ -> False

-- Captura peça
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

-- Captura peça e também retorna a posição destino - Usado pela máquina
capturarPecaComPos :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe (Tabuleiro, (Int, Char))
capturarPecaComPos tab origem destino = do
    novoTab <- capturarPeca tab origem destino
    return (novoTab, destino)

-- Captura uma peça de forma simples
capturarPecaSimples :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
capturarPecaSimples tab origem destino = do
    -- Verifica se a captura é válida
    guard (movimentoCapturaValido tab origem destino)

    casaOrigem <- obterCasa tab origem
    case casaOrigem of
        Vazia -> Nothing
        Ocupada peca -> do
            -- Converte posições para índices
            linhaIndexOrig <- linhaParaIndice (fst origem)
            colIndexOrig <- colunaParaIndice (snd origem)
            linhaIndexDest <- linhaParaIndice (fst destino)
            colIndexDest <- colunaParaIndice (snd destino)

            -- Calcula a posição da peça capturada
            let meioLinha = linhaIndexOrig + (linhaIndexDest - linhaIndexOrig) `div` 2
                meioColuna = colIndexOrig + (colIndexDest - colIndexOrig) `div` 2
                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char) -- Converte para numero a coluna depois soma e volta para letra

            -- Em vez de remover, marca como semicapturada
            casaMeio <- obterCasa tab posMeio
            novaCasaMeio <- case casaMeio of
                Ocupada p -> Just (Ocupada (Semicapturada p))
                _ -> Nothing
            
            tabSemicapturada <- atualizarCasa tab posMeio novaCasaMeio -- Atualiza o tabuleiro: colocar peça semicapturada
            tabSemicapturadaSemOrigem <- atualizarCasa tabSemicapturada origem Vazia -- Atualiza o tabuleiro: tirar da origem
            let novaPeca = avaliarPromocaoParaDama destino peca -- Avalia se deve promover a peça para dama
            tabSemicapturadaComDestino <- atualizarCasa tabSemicapturadaSemOrigem destino (Ocupada peca) -- Atualiza o tabuleiro: colocar na destino

            return tabSemicapturadaComDestino

-- Captura uma peça sob regras da Dama
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
    linhaIndexOrig <- linhaParaIndice (fst origem)
    colIndexOrig <- colunaParaIndice (snd origem)
    linhaIndexDest <- linhaParaIndice (fst destino)
    colIndexDest <- colunaParaIndice (snd destino)

    -- Calcula direção do movimento
    let deltaLinha = if linhaIndexDest > linhaIndexOrig then 1 else -1
        deltaColuna = if colIndexDest > colIndexOrig then 1 else -1

        -- Gera as posições entre origem e destino (excluindo extremos)
        posicoesEntre = zip
            [linhaIndexOrig + deltaLinha, linhaIndexOrig + 2 * deltaLinha .. linhaIndexDest - deltaLinha]
            [colIndexOrig + deltaColuna, colIndexOrig + 2 * deltaColuna .. colIndexDest - deltaColuna]

        -- Encontra a posição da peça a ser capturada
        pecaCapturadaIndex = head [ (li, ci) | (li, ci) <- posicoesEntre,
                                             obterCasaPorIndice tab (li, ci) /= Just Vazia ]

    -- Converte o índice da peça capturada para interface
    let posCaptura = (8 - fst pecaCapturadaIndex, toEnum (fromEnum 'A' + snd pecaCapturadaIndex) :: Char) 

    -- Em vez de remover, marca como semicapturada
    casaCapturada <- obterCasa tab posCaptura
    novaCasaCaptura <- case casaCapturada of
        Ocupada p -> Just (Ocupada (Semicapturada p))
        _ -> Nothing

    tabSemicapturada <- atualizarCasa tab posCaptura novaCasaCaptura -- Atualiza o tabuleiro: colocar peça semicapturada
    tabSemicapturadaSemOrigem <- atualizarCasa tabSemicapturada origem Vazia -- Atualiza o tabuleiro: tirar da origem
    tabSemicapturadaComDestino <- atualizarCasa tabSemicapturadaSemOrigem destino (Ocupada peca) -- Atualiza o tabuleiro: colocar na destino

    return tabSemicapturadaComDestino

-- Simula uma captura simples sem modificar o tabuleiro original
simularCapturaSimples :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
simularCapturaSimples tab origem destino = do
    guard (movimentoCapturaValido tab origem destino)
    casaOrigem <- obterCasa tab origem
    case casaOrigem of
        Ocupada peca -> do
            -- Converte posições para índices
            linhaIndexOrig <- linhaParaIndice (fst origem)
            colIndexOrig <- colunaParaIndice (snd origem)
            linhaIndexDest <- linhaParaIndice (fst destino)
            colIndexDest <- colunaParaIndice (snd destino)

            let meioLinha = linhaIndexOrig + (linhaIndexDest - linhaIndexOrig) `div` 2
                meioColuna = colIndexOrig + (colIndexDest - colIndexOrig) `div` 2
                posMeio = (8 - meioLinha, toEnum (fromEnum 'A' + meioColuna) :: Char)

            -- Simula a captura
            tabSimuladoPecaCapturada <- atualizarCasa tab posMeio Vazia -- Atualiza o tabuleiro simulado: tirar peça capturada
            tabSimuladoSemOrigem <- atualizarCasa tabSimuladoPecaCapturada origem Vazia -- Atualiza o tabuleiro simulado: tirar da origem
            tabSimuladoComDestino <- atualizarCasa tabSimuladoSemOrigem destino (Ocupada peca) -- Atualiza o tabuleiro simulado: colocar na destino

            return tabSimuladoComDestino
        _ -> Nothing

-- Gera sequência de captura simples, quando possível
sequenciasCapturaSimples :: Tabuleiro -> (Int, Char) -> [[(Int, Char)]]
sequenciasCapturaSimples tab origem =
    buscar tab origem [origem]
    where
        -- Retorna todas as sequências a partir da posição atual
        buscar :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> [[(Int, Char)]]
        buscar tabAtual posAtual visited =
            let
                -- Direções de captura simples
                direcoes = [(-2,-2),(-2,2),(2,-2),(2,2)]

                -- Para cada direção, tenta achar um destino válido
                capturasPossiveis =
                    do
                        (direcaoLinha,direcaoColuna) <- direcoes
                        -- maybeToList transforma o dado tipo Maybe para uma lista de um elemento ou vazia
                        linhaIndexOrig   <- maybeToList $ linhaParaIndice (fst posAtual) 
                        colIndexOrig   <- maybeToList $ colunaParaIndice (snd posAtual)
                        let linhaIndexDest = linhaIndexOrig + direcaoLinha
                            colIndexDest = colIndexOrig + direcaoColuna
                            destino = (8 - linhaIndexDest, toEnum (fromEnum 'A' + colIndexDest))
                        guard (ehPosicaoValidaInterface destino)
                        guard (movimentoCapturaValido tabAtual posAtual destino)
                        guard (destino `notElem` visited) -- Verifica se o destino já foi visitado
                        return destino
            in case capturasPossiveis of
                []  -> [[]]  -- Não tem mais capturas: sequência acabou
                dsts -> concatMap (\destino ->
                            case simularCapturaSimples tabAtual posAtual destino of
                            Just tabNovo ->
                                -- Na recursão, o destino é adicionado em visited
                                map (destino :) (buscar tabNovo destino (destino:visited))
                            Nothing -> []
                        ) dsts

-- Gera sequência de capturas
sequenciasCaptura :: Tabuleiro -> (Int, Char) -> [[(Int, Char)]]
sequenciasCaptura tab origem =
    case obterCasa tab origem of
        Just (Ocupada peca)
         | ehDama peca -> sequenciasCapturaDama tab origem
         | otherwise   -> sequenciasCapturaSimples tab origem
        _ -> []

-- Simula uma captura por Dama sem modificar o tabuleiro original
simularCapturaDama :: Tabuleiro -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
simularCapturaDama tab origem destino = do
    -- Verifica se a captura é válida
    guard (movimentoCapturaDamaValido tab origem destino)

    -- Obtém a peça de origem (deve ser dama)
    casaOrigem <- obterCasa tab origem
    peca <- case casaOrigem of
        Ocupada p -> Just p
        _ -> Nothing

    -- Converte posições para índices
    linhaIndexOrig <- linhaParaIndice (fst origem)
    colIndexOrig <- colunaParaIndice (snd origem)
    linhaIndexDest <- linhaParaIndice (fst destino)
    colIndexDest <- colunaParaIndice (snd destino)

    let
        -- Direções do movimento
        deltaLinha = if linhaIndexDest > linhaIndexOrig then 1 else -1
        deltaColuna = if colIndexDest > colIndexOrig then 1 else -1

        -- Caminho percorrido pela dama, excluindo origem e destino
        posicoesIntermediarias =
            zip [linhaIndexOrig + deltaLinha, linhaIndexOrig + 2 * deltaLinha .. linhaIndexDest - deltaLinha]
                [colIndexOrig + deltaColuna, colIndexOrig + 2 * deltaColuna .. colIndexDest - deltaColuna]

        -- Encontra a posição da peça a ser capturada
        capturavel = [ (li, ci)
                     | (li, ci) <- posicoesIntermediarias
                     , obterCasaPorIndice tab (li, ci) /= Just Vazia
                     ]

    -- Se não há exatamente uma peça no caminho, a captura não é válida
    (linhaIndexMeio, colIndexMeio) <- case capturavel of
        [única] -> Just única
        _       -> Nothing

    -- Converte a posição intermediária para (Int, Char)
    let posMeio = (8 - linhaIndexMeio, toEnum (fromEnum 'A' + colIndexMeio) :: Char)

    -- Simula a captura
    tabSimuladoPecaCapturada <- atualizarCasa tab posMeio Vazia -- Atualiza o tabuleiro simulado: tirar peça capturada
    tabSimuladoSemOrigem <- atualizarCasa tabSimuladoPecaCapturada origem Vazia -- Atualiza o tabuleiro simulado: tirar da origem
    tabSimuladoComDestino <- atualizarCasa tabSimuladoSemOrigem destino (Ocupada peca) -- Atualiza o tabuleiro simulado: colocar na destino

    return tabSimuladoComDestino

-- Simula Captura com Dama - Versão que recebe a posição da peça capturada
simularCapturaDamaComCapturada :: Tabuleiro -> (Int, Char) -> (Int, Char) -> (Int, Char) -> Maybe Tabuleiro
simularCapturaDamaComCapturada tab origem destino posMeio = do
    casaOrigem <- obterCasa tab origem
    peca <- case casaOrigem of
        Ocupada p -> Just p
        _ -> Nothing

    -- Simula a captura
    tabSimuladoPecaCapturada <- atualizarCasa tab posMeio Vazia -- Atualiza o tabuleiro simulado: tirar peça capturada
    tabSimuladoSemOrigem <- atualizarCasa tabSimuladoPecaCapturada origem Vazia -- Atualiza o tabuleiro simulado: tirar da origem
    tabSimuladoComDestino <- atualizarCasa tabSimuladoSemOrigem destino (Ocupada peca) -- Atualiza o tabuleiro simulado: colocar na destino

    return tabSimuladoComDestino

-- Retorna pares (destino final, posição da peça capturada)
destinosCapturaDama :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> [(Int, Char)] -> (Int, Int) -> [((Int, Char), (Int, Char))]
destinosCapturaDama tab (l, c) visitados capturados (dl, dc) = go (li + dl) (ci + dc) False Nothing []
  where
    li = fromJust (linhaParaIndice l)
    ci = fromJust (colunaParaIndice c)

    go x y encontrouAdversaria posAdversaria acc
      | not (ehPosicaoValida (x, y)) = acc
      | otherwise =
          let pos = (8 - x, toEnum (fromEnum 'A' + y) :: Char)
          in case obterCasa tab pos of
              Just Vazia ->
                if encontrouAdversaria && pos `notElem` visitados
                   then go (x + dl) (y + dc) encontrouAdversaria posAdversaria (((pos, fromJust posAdversaria) : acc))
                   else go (x + dl) (y + dc) encontrouAdversaria posAdversaria acc
              Just (Ocupada p)
                | not encontrouAdversaria
                  && ehPecaAdversaria (fromJust (obterPeca tab (l, c))) p
                  && pos `notElem` capturados ->
                      go (x + dl) (y + dc) True (Just pos) acc
                | otherwise -> acc
              _ -> acc

-- Gera sequência de captura por Dama, quando possível
sequenciasCapturaDama :: Tabuleiro -> (Int, Char) -> [[(Int, Char)]]
sequenciasCapturaDama tab origem = buscar tab origem [origem] []
    where
        buscar :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> [(Int, Char)] -> [[(Int, Char)]]
        buscar tabAtual posAtual visitados capturados =
            let
                direcoes = [(-1,-1), (-1,1), (1,-1), (1,1)]  -- Diagonais
                capturasPossiveis = concatMap (destinosCapturaDama tabAtual posAtual visitados capturados) direcoes
            in case capturasPossiveis of
                [] -> [[]]
                ds -> concatMap (\(destino, posCapturada) ->
                            case simularCapturaDamaComCapturada tabAtual posAtual destino posCapturada of
                            Just novoTab ->
                                map (destino :) (buscar novoTab destino (destino:visitados) (posCapturada:capturados))
                            Nothing -> []
                        ) ds

-- Gera as melhores capturas possíveis na jogada
melhoresCapturas :: Tabuleiro -> Jogador -> Maybe [((Int, Char), [(Int, Char)])]
melhoresCapturas tab jogador =
    let
        todasPosicoes = posicoesDoJogador tab jogador
        todasCapturas =
            [ (origem, sequencia)
            | origem <- todasPosicoes
            , sequencia <- sequenciasCaptura tab origem
            , not (null sequencia)
            ]
    in
        if null todasCapturas
            then Nothing
            else
                let maxLen = maximum (map (length . snd) todasCapturas)
                in Just $ filter (\(_, sequencia) -> length sequencia == maxLen) todasCapturas

-- Executa loop para capturas sequenciais
loopCapturasSequenciais :: Tabuleiro -> (Int, Char) -> Jogador -> IO Tabuleiro
loopCapturasSequenciais tab pos jogador = do
    let capturas = sequenciasCaptura tab pos
    case capturas of
        [] -> do
            putStrLn "Nenhuma captura possível a partir daqui."
            return tab
        _ -> do
            let melhores = filter (\sequencia -> length sequencia == maximum (map length capturas)) capturas
            case melhores of
                [] -> return tab
                (melhorSeq:_) -> fazerCapturas tab pos melhorSeq

-- Realiza capturas
fazerCapturas :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
fazerCapturas t ultimaPos [] = do -- Momento quando não há mais capturas a se fazer
    case obterCasa t ultimaPos of
        Just (Ocupada peca) -> do
            let novaPeca = avaliarPromocaoParaDama ultimaPos peca -- Verifica se deve promover a dama após último movimento da sequência
            case atualizarCasa t ultimaPos (Ocupada novaPeca) of
                Just t1 -> return (removerSemicapturadas t1)
                Nothing -> return (removerSemicapturadas t) -- Ao final, limpa as semicapturadas
        _ -> return (removerSemicapturadas t)
fazerCapturas t atual (prox:resto) = do 
    case capturarPecaComPos t atual prox of
        Just (novoTab, novaPos) -> do
            mostrarTabuleiro novoTab
            putStrLn $ "Captura para " ++ show novaPos ++ " realizada!"
            fazerCapturas novoTab novaPos resto
        Nothing -> do
            putStrLn $ "Erro ao capturar para " ++ show prox
            return t