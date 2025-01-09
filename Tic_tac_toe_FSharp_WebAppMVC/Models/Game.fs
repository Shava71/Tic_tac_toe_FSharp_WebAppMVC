namespace Tic_tac_toe_FSharp_WebAppMVC.Models

open System

type Player = X | O

type GameState = 
    | InProgress
    | PlayerXWins
    | PlayerOWins
    | Draw

type GameMode =
    | PlayerVsPlayer = 0
    | PlayerVsAI = 1

type GameModeState() =
    let mutable mode = GameMode.PlayerVsPlayer  // 0 = PlayerVsPlayer, 1 = PlayerVsAI
    let mutable difficulty = 0 // Easy_start
    member this.GetMode() = mode
    member this.SetMode(newMode: GameMode) = mode <- newMode
    member this.GetDifficulty() = difficulty
    member this.SetDifficulty(newDifficulty: int) = difficulty <- newDifficulty

type GameModel(gameModeState: GameModeState) =
    let mutable board: Player option array array =
        Array.init 5 (fun _ -> Array.init 5 (fun _ -> None))
    let mutable currentPlayer = X
    let mutable gameState = InProgress

    // Методы, связанные с текущим состоянием игры
    member this.GetBoard() = board
    member this.SetBoard(newBoard) = board <- newBoard
    member this.GetCurrentPlayer() = currentPlayer
    member this.SetCurrentPlayer(newCurrentPlayer: Player) = currentPlayer <- newCurrentPlayer
    member this.GetGameState() = gameState
    member this.SetGameState(newGameState: GameState) = gameState <- newGameState
    
    member this.SwitchPlayer() =
        currentPlayer <- 
            match currentPlayer with
            | Player.X -> Player.O
            | Player.O -> Player.X
    
    member this.MakeMove(x: int, y: int) =
        let makeMoveOnBoard (board: Player option array array) =
            board.[x].[y] <- Some currentPlayer
            board
    
        let checkWinOrDraw board =
            if this.CheckWin(x, y, board) then
                Some (if currentPlayer = Player.X then PlayerXWins else PlayerOWins)
            elif this.CheckDraw(board) then
                Some Draw
            else
                None
    
        let switchPlayer currentPlayer =
            match currentPlayer with
            | Player.X -> Player.O
            | Player.O -> Player.X
    
        // Основная логика без изменяемых состояний
        if x >= 0 && x < 5 && y >= 0 && y < 5 then
            if board.[x].[y].IsNone && gameState = InProgress then
                let newBoard = makeMoveOnBoard board
                let newGameState = checkWinOrDraw newBoard |> Option.defaultValue InProgress
                let nextPlayer = 
                    if newGameState = InProgress then switchPlayer currentPlayer
                    else currentPlayer
    
                // Возвращаем новое состояние игры
                (newBoard, newGameState, nextPlayer)
            else
                (board, gameState, currentPlayer)
        else
            (board, gameState, currentPlayer)
    
    member this.CheckWin(x: int, y: int, board: Player option array array) =
        let player = board.[x].[y]
        
        // Проверка по горизонтали
        let rowWin = Array.forall (fun i -> board.[x].[i] = player) [|0..4|]
        
        // Проверка по вертикали
        let colWin = Array.forall (fun i -> board.[i].[y] = player) [|0..4|]
        
        // Проверка по диагонали (слева направо)
        let diag1Win = Array.forall (fun i -> board.[i].[i] = player) [|0..4|]
        
        // Проверка по диагонали (справа налево)
        let diag2Win = Array.forall (fun i -> board.[i].[4 - i] = player) [|0..4|]

        rowWin || colWin || diag1Win || diag2Win
  
    // Проверка на ничью
    member this.CheckDraw(board: Player option array array) =
        Array.forall (fun i -> Array.forall (fun j -> board.[i].[j].IsSome) [|0..4|]) [|0..4|]

    // Сброс игры
    member this.ResetGame() =
        board <- Array.init 5 (fun _ -> Array.init 5 (fun _ -> None))
        currentPlayer <- X
        gameState <- InProgress

    // Hard
    member this.EvaluateBoard_Hard(tempboard: Player option array array) =
        // Простая эвристическая оценка
        let scoreLine (line: Player option array) =
            let xCount = Array.length (Array.filter ((=) (Some Player.X)) line)
            let oCount = Array.length (Array.filter ((=) (Some Player.O)) line)
            match xCount, oCount with
            | 5, 0 -> -1000 // Player X побеждает
            | 0, 5 -> 1000  // Player O побеждает
            | 4, 0 -> -1000
            | 3, 0 -> -100   // X на пути к победе
            | x, 0 -> x * -10 // X доминирует
            | 0, y -> y * 10  // O доминирует
            | _ -> 0          // Смешанный ряд, не имеет смысла
    
        let rowsScore = Array.sumBy (fun x -> scoreLine tempboard.[x]) [|0..4|]
        let colsScore = Array.sumBy (fun y -> scoreLine (Array.map (fun i -> tempboard.[i].[y]) [|0..4|])) [|0..4|]
        let diag1 = Array.map (fun i -> tempboard.[i].[i]) [|0..4|]
        let diag2 = Array.map (fun i -> tempboard.[i].[4 - i]) [|0..4|]
        rowsScore + colsScore + scoreLine diag1 + scoreLine diag2

    // middle
    member this.EvaluateBoard_Middle(tempboard: Player option array array) =
        let scoreLine (line: Player option array) =
            let xCount = Array.length (Array.filter ((=) (Some Player.X)) line)
            let oCount = Array.length (Array.filter ((=) (Some Player.O)) line)
            match xCount, oCount with
                   | 5, 0 -> 0 // Победа Player X
                   | 0, 5 -> 1000  // Победа Player O
                   | 4, 0 -> -10  // Почти выиграл X
                   | 0, 4 -> 200   // Почти выиграл O
                   | 3, 0 -> 10   // X на пути к победе
                   | 0, 3 -> 500    // O на пути к победе
                   | _ -> 0         // Линия не полезна
    
        let rowsScore = Array.sumBy (fun x -> scoreLine tempboard.[x]) [|0..4|]
        let colsScore = Array.sumBy (fun y -> scoreLine (Array.map (fun i -> tempboard.[i].[y]) [|0..4|])) [|0..4|]
        let diag1 = Array.map (fun i -> tempboard.[i].[i]) [|0..4|]
        let diag2 = Array.map (fun i -> tempboard.[i].[4 - i]) [|0..4|]
        rowsScore + colsScore + scoreLine diag1 + scoreLine diag2
        
    // Lite
    member this.EvaluateBoard_Lite(tempboard: Player option array array) =
        // Простая эвристическая оценка
        let scoreLine (line: Player option array) =
            let xCount = Array.length (Array.filter ((=) (Some Player.X)) line)
            let oCount = Array.length (Array.filter ((=) (Some Player.O)) line)
            match xCount, oCount with
            | 5, 0 -> 0 // Player X побеждает
            | 0, 5 -> 1000  // Player O побеждает
            | x, 0 -> 0 // X доминирует
            | 0, y -> y * 10  // O доминирует
            | _ -> 0          // Смешанный ряд, не имеет смысла
    
        let rowsScore = Array.sumBy (fun x -> scoreLine tempboard.[x]) [|0..4|]
        let colsScore = Array.sumBy (fun y -> scoreLine (Array.map (fun i -> tempboard.[i].[y]) [|0..4|])) [|0..4|]
        let diag1 = Array.map (fun i -> tempboard.[i].[i]) [|0..4|]
        let diag2 = Array.map (fun i -> tempboard.[i].[4 - i]) [|0..4|]
        rowsScore + colsScore + scoreLine diag1 + scoreLine diag2 
    
    
    member this.EvaluateBoard(tempboard: Player option array array) =
        match gameModeState.GetDifficulty() with
        | 0 -> this.EvaluateBoard_Lite(tempboard)
        | 1 -> this.EvaluateBoard_Middle(tempboard)
        | 2 -> this.EvaluateBoard_Hard(tempboard)
      
    member this.Minimax(tempboard: Player option array array, depth: int, isMaximizing: bool, alpha: int, beta: int, maxDepth: int) =
    // Оценка доски
        let evalBoard = this.EvaluateBoard(tempboard)
        printfn "%d" evalBoard

        // Базовый случай: достижение глубины или конец игры
        if depth >= maxDepth || gameState <> GameState.InProgress then
            evalBoard
        else
            // Генерация всех возможных ходов
            let possibleMoves = 
                [|0..4|] 
                |> Array.collect (fun i ->
                    [|0..4|] 
                    |> Array.choose (fun j -> 
                        if board.[i].[j].IsNone then Some (i, j) else None
                    )
                )

            // Рекурсивная функция для оценки возможных ходов
            let rec evaluateMoves moves alpha beta bestScore =
                match moves with
                | [] -> bestScore // Все ходы рассмотрены
                | (i, j)::rest ->
                    // Копирование доски и применение хода
                    let newBoard = Array.map (fun row -> Array.copy row) tempboard
                    newBoard.[i].[j] <- Some (if isMaximizing then Player.O else Player.X)

                    // Рекурсивный вызов `Minimax` для оценки хода
                    let score = this.Minimax(newBoard, depth + 1, not isMaximizing, alpha, beta, maxDepth)

                    // Вычисляем новый лучший счёт и обновляем альфа/бета
                    let newBestScore =
                        if isMaximizing then max bestScore score
                        else min bestScore score
                    
                    let (newAlpha, newBeta) =
                        if isMaximizing then (max alpha newBestScore, beta)
                        else (alpha, min beta newBestScore)

                    // Если отсечение возможно, завершаем
                    if newAlpha >= newBeta then newBestScore
                    else evaluateMoves rest newAlpha newBeta newBestScore

            // Инициализация рекурсивной функции для всех возможных ходов
            evaluateMoves (List.ofArray possibleMoves) alpha beta (if isMaximizing then Int32.MinValue else Int32.MaxValue)
    
    member this.MakeAIMove() =
        printfn "Current difficulty: %d" (gameModeState.GetDifficulty())
        
        let evaluateMove i j =
            if board.[i].[j].IsNone then
                // Сделать ход (копирование доски, изменяя клетку)
                let newBoard = Array.map (fun row -> Array.copy row) board
                newBoard.[i].[j] <- Some Player.O

                // Рекурсивно посчитать оценку для этой доски
                this.Minimax(newBoard, 0, false, Int32.MinValue, Int32.MaxValue, 3)
            else
                Int32.MinValue

        // Генерация всех возможных ходов и оценка каждого хода
        let possibleMoves = 
            [|0..4|] 
            |> Array.collect (fun i ->
                [|0..4|] 
                |> Array.choose (fun j -> 
                    if board.[i].[j].IsNone then Some (i, j) else None
                )
            )

        // Оценка всех ходов и выбор наилучшего
        let move, _ =
            possibleMoves
            |> Array.fold (fun (bestMove, bestScore) (i, j) ->
                let score = evaluateMove i j
                if score > bestScore then
                    ((i, j), score)
                else
                    (bestMove, bestScore)
            ) ((0, 0), Int32.MinValue)

        // Применение выбранного хода
        let newBoard = Array.map (fun row -> Array.copy row) board
        let x, y = move
        newBoard.[x].[y] <- Some Player.O

        // Определение нового состояния игры и следующего игрока
        let newGameState = 
            if this.CheckWin(x, y, newBoard) then PlayerOWins
            elif this.CheckDraw(newBoard) then Draw
            else InProgress

        let nextPlayer =
            if newGameState = InProgress then Player.X
            else currentPlayer

        // Возвращаем новое состояние доски, игры и следующего игрока
        newBoard, newGameState, nextPlayer
    
    // member this.MakeAIMove() =
    //     printfn "Current difficulty: %d" (gameModeState.GetDifficulty())
    //     
    //     let evaluateMove i j =
    //         if board.[i].[j].IsNone then
    //             // Сделать ход (копирование доски, изменяя клетку)
    //             let newBoard = Array.map (fun row -> Array.copy row) board
    //             newBoard.[i].[j] <- Some Player.O
    //
    //             // Рекурсивно посчитать оценку для этой доски
    //             this.Minimax(newBoard,0, false, Int32.MinValue, Int32.MaxValue, 3)
    //         else
    //             Int32.MinValue
    //
    //     // Генерация всех возможных ходов и оценка каждого хода
    //     let possibleMoves = 
    //         [|0..4|] 
    //         |> Array.collect (fun i ->
    //             [|0..4|] 
    //             |> Array.choose (fun j -> 
    //                 if board.[i].[j].IsNone then Some (i, j) else None
    //             )
    //         )
    //
    //     // Оценка всех ходов и выбор наилучшего
    //     let move, bestScore =
    //         possibleMoves
    //         |> Array.fold (fun (bestMove, bestScore) (i, j) ->
    //             let score = evaluateMove i j
    //             if score > bestScore then
    //                 ((i, j), score)
    //             else
    //                 (bestMove, bestScore)
    //         ) ((0, 0), Int32.MinValue)
    //
    //     let x, y = move
    //     board.[x].[y] <- Some Player.O
    //     if this.CheckWin(x, y) then
    //         gameState <- PlayerOWins
    //     elif this.CheckDraw() then
    //         gameState <- Draw
    //     else
    //         this.SwitchPlayer()