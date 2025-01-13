-- André Luiz Casarim Leite 201865566C
-- Aaron Ramires da Fonseca 202465134A

import System.Random (randomRIO)

type Estado = [Int]

-- Função para calcular o XOR
xor :: Int -> Int -> Int
xor 0 b = b
xor a 0 = a
xor a b
    | even a && even b = 0
    | odd a && odd b = 0
    | otherwise = 1

-- Gera um número ímpar aleatório entre 1 e 7
geraPalitosAleatorios :: IO Int
geraPalitosAleatorios = do
    n <- randomRIO (1, 7)
    if odd n then return n else geraPalitosAleatorios

-- Gera o estado inicial com um número aleatório de fileiras (>= 2)
geraEstadoInicial :: IO Estado
geraEstadoInicial = do
    numFileiras <- randomRIO (2, 10 :: Int)
    mapM (\_ -> geraPalitosAleatorios) [1..numFileiras]

-- Exibe o estado do jogo no terminal
exibeEstado :: Estado -> IO ()
exibeEstado estado = mapM_ (\(i, n) -> putStrLn $ show i ++ ": " ++ replicate n '|') (zip [1..] estado)

-- Modifica o estado removendo a quantidade de palitos da fileira especificada
modificaEstado :: Estado -> Int -> Int -> Estado
modificaEstado estado fileira qtd =
    let (antes, x:depois) = splitAt (fileira - 1) estado
    in antes ++ [x - qtd] ++ depois

-- Jogada do jogador
jogadaJogador :: Estado -> IO Estado
jogadaJogador estado = do
    putStrLn "Escolha uma fileira e a quantidade de palitos para remover:"
    putStrLn "Digite o número da fileira:"
    fileira <- readLn
    putStrLn "Digite a quantidade de palitos para remover:"
    qtd <- readLn
    if fileira < 1 || fileira > length estado || qtd < 1 || qtd > (estado !! (fileira - 1))
        then do
            putStrLn "Jogada inválida. Tente novamente."
            jogadaJogador estado
        else return (modificaEstado estado fileira qtd)

-- Escolhe uma linha aleatória não vazia
escolheLinhaAleatoria :: Estado -> IO Int
escolheLinhaAleatoria estado = do
    let linhasValidas = [i | (i, n) <- zip [1..] estado, n > 0]
    randomRIO (0, length linhasValidas - 1) >>= \idx -> return (linhasValidas !! idx)

-- Jogada do computador no nível fácil: remove 1 palito da primeira fileira não vazia
jogadaComputadorFacil :: Estado -> IO Estado
jogadaComputadorFacil estado = do
    fileira <- escolheLinhaAleatoria estado
    return $ modificaEstado estado fileira 1


-- Jogada do computador no nível difícil: utiliza a estratégia vencedora
jogadaComputadorDificil :: Estado -> IO Estado
jogadaComputadorDificil estado = do
    let xorTotal = foldr xor 0 estado
    if xorTotal == 0
        then jogadaComputadorFacil estado  -- Sem jogada perfeita, faz uma jogada fácil
        else do
            let jogadasPossiveis = [(i, estado !! (i - 1) - (estado !! (i - 1) `xor` xorTotal))
                                   | i <- [1..length estado], 
                                     (estado !! (i - 1) `xor` xorTotal) < estado !! (i - 1)] -- calcula as jogadas posiveis
            let (fileira, qtd) = head jogadasPossiveis
            return $ modificaEstado estado fileira qtd

-- Loop principal do jogo
jogo :: Estado -> Bool -> Int -> IO ()
jogo estado vezDoJogador nivel = do
    exibeEstado estado
    if all (== 0) estado
        then putStrLn $ if vezDoJogador then "Computador venceu!" else "Você venceu!"
        else do
            novoEstado <- if vezDoJogador
                          then jogadaJogador estado
                          else if nivel == 1
                               then jogadaComputadorFacil estado
                               else jogadaComputadorDificil estado
            jogo novoEstado (not vezDoJogador) nivel

-- Função principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao Jogo dos Palitinhos!"
    estadoInicial <- geraEstadoInicial
    putStrLn "Escolha o nível de dificuldade (1 - Fácil, 2 - Difícil):"
    nivel <- readLn
    if nivel == 1 || nivel == 2
        then jogo estadoInicial True nivel  -- Jogador sempre começa
        else putStrLn "Nível inválido. Tente novamente."