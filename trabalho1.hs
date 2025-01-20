-- André Luiz Casarim Leite 201865566C
-- Aaron Ramires da Fonseca 202465134A

import System.Random (randomRIO)

type Estado = [Int]

-- Gera um número ímpar aleatório entre 1 e 7
geraPalitosAleatorios :: IO Int
geraPalitosAleatorios = do
    n <- randomRIO (1, 7)
    if odd n then return n else geraPalitosAleatorios

-- Gera o estado inicial com um número aleatório de fileiras (>= 2)
geraEstadoInicial :: IO Estado
geraEstadoInicial = do
    numFileiras <- randomRIO (2, 7 :: Int)
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
    idx <- randomRIO (0, length linhasValidas - 1)
    return (linhasValidas !! idx)


-- Jogada do computador no nível fácil: remove 1 palito da primeira fileira não vazia
jogadaComputadorFacil :: Estado -> IO Estado
jogadaComputadorFacil estado = do
    putStrLn "Vez do computador:"
    linha <- escolheLinhaAleatoria estado
    let maxPalitos = estado !! (linha - 1)
    quantidade <- randomRIO (1, maxPalitos)
    let novoEstado = modificaEstado estado linha quantidade
    putStrLn $ "Computador escolheu a linha " ++ show linha ++ " e retirou " ++ show quantidade ++ " palito(s)."
    return novoEstado

xor :: Int -> Int -> Int
xor x y = x `xor'` y
  where
    xor' 0 0 = 0
    xor' 0 1 = 1
    xor' 1 0 = 1
    xor' 1 1 = 0
    xor' a b = let (qa, ra) = a `divMod` 2
                   (qb, rb) = b `divMod` 2
               in (ra `xor'` rb) + 2 * (qa `xor'` qb)

-- Jogada do computador no nível difícil: utiliza a estratégia vencedora
jogadaComputadorDificil :: [Int] -> IO [Int]
jogadaComputadorDificil fileiras = do
    putStrLn "Vez do computador:"
    let somaBinaria = foldr1 xor fileiras
    if somaBinaria == 0
        then jogadaComputadorFacil fileiras -- Jogue qualquer coisa, pois não há estratégia vencedora no momento
        else do
            let (fileira, novaQuantidade) = encontrarJogadaEstrategica fileiras somaBinaria
            let quantidadeRemovida = fileiras !! (fileira - 1) - novaQuantidade
            let novaFileiras = modificaEstado fileiras fileira quantidadeRemovida
            putStrLn $ "Computador removeu " ++ show quantidadeRemovida ++ " palito(s) da fileira " ++ show fileira
            return novaFileiras

-- Encontra a jogada estratégica que deixa a soma binária com todos os dígitos pares
encontrarJogadaEstrategica :: [Int] -> Int -> (Int, Int)
encontrarJogadaEstrategica fileiras somaBinaria =
    head [(i + 1, novaQuantidade) | (i, quantidade) <- zip [0..] fileiras,
                                    let novaQuantidade = quantidade `xor` somaBinaria,
                                    novaQuantidade >= 0 && novaQuantidade <= quantidade]

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