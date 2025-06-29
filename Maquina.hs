module Maquina where

import Tabuleiro
import Movimento
import Posicao
import Data.List (maximumBy, maximum)
import Data.Ord (comparing)
import Data.Maybe (fromJust, fromMaybe)

-- Função principal da IA: retorna o tabuleiro atualizado após o movimento da máquina
-- Executa a jogada da IA
jogadaIa :: Tabuleiro -> Jogador -> IO Tabuleiro
jogadaIa tab jogador = do
    let capturas = fromMaybe [] (melhoresCapturas tab jogador)
    if null capturas
        then do
            let movimentosSimples = movimentosSimplesValidos tab jogador
            if null movimentosSimples
                then do
                    putStrLn "IA: sem jogadas possíveis."
                    return tab
                else do
                    let (origem, destino) = escolherMelhorMovimentoSimples tab jogador movimentosSimples
                    putStrLn $ "IA: movendo de " ++ show origem ++ " para " ++ show destino
                    case moverPeca tab origem destino of
                        Just tabNovo -> return tabNovo
                        Nothing -> do
                            putStrLn "IA: erro ao mover peça."
                            return tab
        else do
            let (origem, melhorSeq) = escolherMelhorCaptura tab jogador capturas
            putStrLn $ "IA: capturando de " ++ show origem ++ " seguindo " ++ show melhorSeq
            tabFinal <- executarSequenciaCapturas tab origem melhorSeq
            return tabFinal

-- Escolhe a melhor captura entre as melhores capturas segundo heurística
escolherMelhorCaptura :: Tabuleiro -> Jogador -> [((Int, Char), [(Int, Char)])] -> ((Int, Char), [(Int, Char)])
escolherMelhorCaptura tab jogador capturas =
    maximumBy (comparing (mobilidadePosicaoFinal tab jogador)) capturas

-- Avalia a "mobilidade" (quantidade de movimentos possíveis) da peça após a sequência de capturas
mobilidadePosicaoFinal :: Tabuleiro -> Jogador -> ((Int, Char), [(Int, Char)]) -> Int
mobilidadePosicaoFinal tab jogador (origem, seq) =
    let posFinal = if null seq then origem else last seq
        tabSimulado = simularSequenciaCaptura tab origem seq
    in case tabSimulado of
        Just tabNovo -> length $ sequenciasCaptura tabNovo posFinal -- número de sequências de captura disponíveis na nova posição
        Nothing -> 0

-- Simula o resultado de uma sequência de captura sem modificar o tabuleiro original
simularSequenciaCaptura :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> Maybe Tabuleiro
simularSequenciaCaptura tab origem [] = Just tab
simularSequenciaCaptura tab origem (d:ds) =
    case capturarPecaComPos tab origem d of
        Just (tabNovo, novaPos) -> simularSequenciaCaptura tabNovo d ds
        Nothing -> Nothing

-- Executa a sequência de capturas
executarSequenciaCapturas :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
executarSequenciaCapturas tab origem [] = return tab
executarSequenciaCapturas tab origem (destino:resto) = do
    case capturarPecaComPos tab origem destino of
        Just (novoTab, novaPos) -> do
            mostrarTabuleiro novoTab  -- Exibe o tabuleiro após a captura com peça branca
            executarCapturasRec novoTab novaPos resto
        Nothing -> return tab  -- erro na primeira captura

  where
    executarCapturasRec :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
    executarCapturasRec t ultimaPos [] = do
        -- Quando acabou a sequência de capturas:
        case obterCasa t ultimaPos of
            Just (Ocupada peca) -> do
                let novaPeca = avaliarPromocaoParaDama ultimaPos peca
                case atualizarCasa t ultimaPos (Ocupada novaPeca) of
                    Just t1 -> return (removerSemicapturadas t1)
                    Nothing -> return (removerSemicapturadas t)
            _ -> return (removerSemicapturadas t)

    executarCapturasRec t atual (prox:resto') = do
        case capturarPecaComPos t atual prox of
            Just (novoTab, novaPos) -> do
                mostrarTabuleiro novoTab  -- Mostra tabuleiro com peça branca
                executarCapturasRec novoTab novaPos resto'
            Nothing -> return t  -- erro numa das capturas subsequentes

-- Obtem todos movimentos simples válidos (origem,destino)
movimentosSimplesValidos :: Tabuleiro -> Jogador -> [((Int, Char), (Int, Char))]
movimentosSimplesValidos tab jogador =
    concatMap movimentosDaPeca (posicoesDoJogador tab jogador)
  where
    movimentosDaPeca :: (Int, Char) -> [((Int, Char), (Int, Char))]
    movimentosDaPeca origem =
        case obterCasa tab origem of
            Just (Ocupada peca) ->
                if ehDama peca
                    then destinosDama origem
                    else destinosPecaComum peca origem
            _ -> []

    -- Movimentos possíveis para peças comuns (diagonal para frente)
    destinosPecaComum :: Peca -> (Int, Char) -> [((Int, Char), (Int, Char))]
    destinosPecaComum peca (l, c) =
        let direcoes = case peca of
                PecaJogador1 -> [(1, -1), (1, 1)]    -- desce
                PecaJogador2 -> [(-1, -1), (-1, 1)]  -- sobe
                _ -> []
            possiveis = [ (l + dl, toEnum (fromEnum c + dc)) | (dl, dc) <- direcoes ]
        in [ ((l, c), destino)
           | destino <- possiveis
           , ehPosicaoValidaInterface destino
           , obterCasa tab destino == Just Vazia
           ]

    -- Movimentos possíveis para damas (em qualquer diagonal, até encontrar obstáculo)
    destinosDama :: (Int, Char) -> [((Int, Char), (Int, Char))]
    destinosDama (l, c) =
        let direcoes = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
            li = fromJust (linhaParaIndice l)
            ci = fromJust (colunaParaIndice c)
        in concatMap (caminharDirecao li ci) direcoes

    caminharDirecao :: Int -> Int -> (Int, Int) -> [((Int, Char), (Int, Char))]
    caminharDirecao li ci (dl, dc) =
        let posicoes = tail $ zip [li, li+dl ..] [ci, ci+dc ..]
        in takeWhile ehValidaELivre posicoes >>= \ (x, y) ->
            let destino = (8 - x, toEnum (fromEnum 'A' + y))
            in [((8 - li, toEnum (fromEnum 'A' + ci)), destino)]

    ehValidaELivre :: (Int, Int) -> Bool
    ehValidaELivre (x, y)
        | not (ehPosicaoValida (x, y)) = False
        | otherwise =
            let pos = (8 - x, toEnum (fromEnum 'A' + y))
            in obterCasa tab pos == Just Vazia

-- Escolhe o melhor movimento simples segundo heurística (exemplo: maior mobilidade no destino)
escolherMelhorMovimentoSimples :: Tabuleiro -> Jogador -> [((Int, Char), (Int, Char))] -> ((Int, Char), (Int, Char))
escolherMelhorMovimentoSimples tab jogador movimentos =
    maximumBy (comparing (avaliarMovimento tab jogador)) movimentos

-- Atribui peso/pontuação a um movimento
avaliarMovimento :: Tabuleiro -> Jogador -> ((Int, Char), (Int, Char)) -> Int
avaliarMovimento tab jogador (origem, destino) =
    let base = length (sequenciasCaptura tab destino)
        -- Adiciona peso para estar na última linha (chance de promover a peça)
        linhaDestino = fst destino
        pesoPromocao = case jogador of
            Jogador1 -> if linhaDestino == 8 then 3 else 0
            Jogador2 -> if linhaDestino == 1 then 3 else 0
        -- Soma a quantidade de casas livres ao redor da nova posição
        mobilidade = length $ movimentosAdjacentesLivres tab destino
    in base * 10 + pesoPromocao + mobilidade

-- Verifica movimentos adjacentes livres 
movimentosAdjacentesLivres :: Tabuleiro -> (Int, Char) -> [(Int, Char)]
movimentosAdjacentesLivres tab (l, c) =
    let direcoes = [(-1,-1), (-1,1), (1,-1), (1,1)]
        vizinhos = [(l + dl, toEnum (fromEnum c + dc)) | (dl, dc) <- direcoes]
    in filter (\pos -> ehPosicaoValidaInterface pos && obterCasa tab pos == Just Vazia) vizinhos
