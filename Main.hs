module Main where -- Define um módulo Main

import Tabuleiro -- importa o módulo Tabuleiro criado por mim
import Movimento
import Posicao
import Data.List (maximumBy, intercalate)
import Data.Ord (comparing)


-- Tipo para identificar o jogador atual
-- Cria um tipo algébrico com apenas dois valores possíveis
data Jogador = Jogador1 | Jogador2
    deriving (Eq, Show)
-- usa o Eq para poder comparar se dois jogadores são iguais ou diferentes
-- usa o Show para poder converter o texto para String

-- Função para verificar se uma peça pertence a um jogador
pecaPertenceAoJogador :: Peca -> Jogador -> Bool
pecaPertenceAoJogador PecaJogador1 Jogador1 = True
pecaPertenceAoJogador DamaJogador1 Jogador1 = True
pecaPertenceAoJogador PecaJogador2 Jogador2 = True
pecaPertenceAoJogador DamaJogador2 Jogador2 = True
pecaPertenceAoJogador _ _ = False

-- Loop principal do jogo

-- Controla o turno dos jogadores
-- Lê as posições de origem e destino
-- Verifica se o movimento é válido (simples ou de captura)
-- Verifica se a peça escolhida é do jogador atual
-- Atualiza o tabuleiro e troca o jogador

--LOOP COM CAPTURA DE PEÇAS FUNCIONANDO e TURNOS
loopJogo :: Tabuleiro -> Jogador -> IO ()
loopJogo tab jogadorAtual = do
    putStrLn $ "\nTurno do " ++ nomeJogadorColorido jogadorAtual
    mostrarTabuleiro tab

    --NOVO: Exibir sugestão de melhor jogada
    case melhorCaptura tab jogadorAtual of
        Just (origem, caminho@(_:_)) -> do
            putStrLn "Melhor sequência de captura disponível:"
            putStrLn $ "  Origem: " ++ show origem
            putStrLn $ "  Sequência: " ++ intercalate " -> " (map show caminho)
        _ -> return ()

    putStrLn "Digite posição origem (ex: 6B): "
    origemStr <- getLine
    putStrLn "Digite posição destino (ex: 5A): "
    destinoStr <- getLine

    -- Verifica se o usuário inseriu dados que representam alguma casa do tabuleiro corretamente
    case (lerPosicao origemStr, lerPosicao destinoStr) of
        (Just origem, Just destino) -> do
            -- Verifica se na origem existe peça e a quem pertence
            if not (ehPosicaoValidaInterface origem) -- veriicacao para quando o usuário tenta usar uma casa branca como origem
                then do
                    putStrLn "Casa inválida! Escolha apenas casas pretas do tabuleiro."
                    loopJogo tab jogadorAtual
                else
                    case obterCasa tab origem of
                        Just (Ocupada peca)
                            | pecaPertenceAoJogador peca jogadorAtual -> do
                                case capturarPeca tab origem destino of
                                    Just tabNovo -> do
                                        putStrLn "Captura realizada!"
                                        loopJogo tabNovo (trocarJogador jogadorAtual)
                                    Nothing ->
                                        case moverPeca tab origem destino of
                                            Just tabNovo -> loopJogo tabNovo (trocarJogador jogadorAtual)
                                            Nothing -> do
                                                putStrLn "Movimento inválido! Tente novamente."
                                                loopJogo tab jogadorAtual
                        Just Vazia -> do
                            putStrLn "Não há peça na posição de origem. Tente novamente."
                            loopJogo tab jogadorAtual    
                        _ -> do
                            putStrLn "Essa peça não pertence a você! Escolha uma peça sua."
                            loopJogo tab jogadorAtual
        _ -> do
            putStrLn "Entrada inválida! Tente novamente."
            loopJogo tab jogadorAtual

-- Função para colorir o 
nomeJogadorColorido :: Jogador -> String
nomeJogadorColorido Jogador1 = corTexto "\x1b[34;1m" "Jogador 1"  -- Azul
nomeJogadorColorido Jogador2 = corTexto "\x1b[33;1m" "Jogador 2"  -- Amarelo


-- Função para alternar entre jogadores no LoopJogo
trocarJogador :: Jogador -> Jogador
trocarJogador Jogador1 = Jogador2
trocarJogador Jogador2 = Jogador1

-- Menu inicial do jogo
main:: IO() -- declara a função main como uma ação de input e output(IO)
main = do
    putStrLn "====================="
    putStrLn "    JOGO DE DAMAS    "
    putStrLn "====================="
    putStrLn "Escoha o modo de jogo:"
    putStrLn "1 - Jogador vs Máquina"
    putStrLn "2 - Máquina vs Máquina"
    putStrLn "3 - Sair"
    putStrLn "Opção: "
    option <- getLine -- captura a opção do usuário como uma String
    case option of
        "1" -> escolherInicio True -- True pq é jogadorVsMaquina
        "2" -> escolherInicio False -- False pq é maquinaVsMaquina
        "3" -> putStrLn "Saindo do jogo..."
        _   -> do 
            putStrLn "Opção inválida, tente novamente."
            main -- executa essa funcao até o usuário escolher uma opção válida

-- Função para escolher quem começa o jogo
escolherInicio :: Bool -> IO ()
escolherInicio jogadorVsMaquina = do
    putStrLn "Quem começa o Jogo?"
    putStrLn "1 - Jogador (ou Máquina 1)"
    putStrLn "2 - Máquina (ou máquina 2)"
    putStr "Opção: "
    inicio <- getLine
    case inicio of
        "1" -> iniciarJogo jogadorVsMaquina True
        "2" -> iniciarJogo jogadorVsMaquina False
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            escolherInicio jogadorVsMaquina

-- Função que inicializa o jogo
iniciarJogo :: Bool -> Bool -> IO ()
iniciarJogo jogadorVsMaquina jogadorComeca = do
    putStrLn $ "\nModo de jogo: " ++
        if jogadorVsMaquina then "Jogador vs Máquina" else "Máquina vs Máquina"
    putStrLn $ "Quem começa: " ++ 
        if jogadorComeca then "Jogador (ou Máquina 1)" else "Máquina (ou Máquina 2)"
    loopJogo tabuleiroInicial (if jogadorComeca then Jogador1 else Jogador2)

posicoesDoJogador :: Tabuleiro -> Jogador -> [(Int, Char)]
posicoesDoJogador tab jogador = 
    [ (8 - li, toEnum (fromEnum 'A' + ci)) 
    | (li, linha) <- zip [0..] tab
    , (ci, casa) <- zip [0..] linha
    , Ocupada peca <- [casa]
    , pecaPertenceAoJogador peca jogador
    ]

melhorCaptura :: Tabuleiro -> Jogador -> Maybe ((Int, Char), [(Int, Char)])
melhorCaptura tab jogador =
    let
        todasPosicoes = posicoesDoJogador tab jogador

        -- Para cada posição, obtém todas as sequências de captura possíveis
        todasCapturas = 
            [ (origem, seq) 
            | origem <- todasPosicoes
            , seq <- sequenciasCapturaSimples tab origem
            , not (null seq)
            ]

    in if null todasCapturas
        then Nothing
        else Just $ maximumBy (comparing (length . snd)) todasCapturas

