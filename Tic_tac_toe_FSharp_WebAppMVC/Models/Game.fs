namespace Tic_tac_toe_FSharp_WebAppMVC.Models

open System

type Player = X | O

type GameState = 
    | InProgress
    | PlayerXWins
    | PlayerOWins
    | Draw

type GameModeState() =
    let mutable mode = 0  // 0 = PlayerVsPlayer, 1 = PlayerVsAI
    let mutable difficulty = 0 //Easy_start
    member this.GetMode() = mode
    member this.SetMode(newMode: int) = mode <- newMode
    member this.GetDifficulty() = difficulty
    member this.SetDifficulty(newDifficulty: int) = difficulty <- newDifficulty

type GameModel(gameModeState: GameModeState) =
    let mutable board: Player option array array =
        Array.init 5 (fun _ -> Array.init 5 (fun _ -> None))
    let mutable currentPlayer = X
    let mutable gameState = InProgress

    // Методы, связанные с текущим состоянием игры
    member this.GetBoard() = board
    member this.GetCurrentPlayer() = currentPlayer
    member this.GetGameState() = gameState
    
    member this.SwitchPlayer() =
        currentPlayer <- 
            match currentPlayer with
            | Player.X -> Player.O
            | Player.O -> Player.X

    member this.MakeMove(x: int, y: int) =
        if x >= 0 && x < 5 && y >= 0 && y < 5 then // Добавляем проверку границ
            if board.[x].[y].IsNone && gameState = InProgress then
                board.[x].[y] <- Some currentPlayer
                if this.CheckWin(x, y) then
                    gameState <- if currentPlayer = Player.X then PlayerXWins else PlayerOWins
                elif this.CheckDraw() then
                    gameState <- Draw
                else
                    this.SwitchPlayer()
        else
            ()
    
    member this.CheckWin(x: int, y: int) =
        let player = board.[x].[y]
        
        // Проверка по горизонтали
        let rowWin = [|0..4|] |> Array.forall (fun i -> board.[x].[i] = player)
        
        // Проверка по вертикали
        let colWin = [|0..4|] |> Array.forall (fun i -> board.[i].[y] = player)
        
        // Проверка по диагонали (слева направо)
        let diag1Win = [|0..4|] |> Array.forall (fun i -> board.[i].[i] = player)
        
        // Проверка по диагонали (справа налево)
        let diag2Win = [|0..4|] |> Array.forall (fun i -> board.[i].[4 - i] = player)

        rowWin || colWin || diag1Win || diag2Win
  
    // Проверка на ничью
    member this.CheckDraw() =
        [|0..4|] |> Array.forall (fun i -> [|0..4|] |> Array.forall (fun j -> board.[i].[j].IsSome))

    // Сброс игры
    member this.ResetGame() =
        board <- Array.init 5 (fun _ -> Array.init 5 (fun _ -> None))
        currentPlayer <- X
        gameState <- InProgress
    
    
    member this.gameSituation() =
        if this.CheckWin(0, 0) then
            if currentPlayer = X then PlayerXWins else PlayerOWins
        else if this.CheckDraw() then
            Draw
        else 
            InProgress
    
    
    // Hard
    member this.EvaluateBoard_Hard() =
        // Простая эвристическая оценка
        let scoreLine (line: Player option array) =
            let xCount = line |> Array.filter ((=) (Some Player.X)) |> Array.length
            let oCount = line |> Array.filter ((=) (Some Player.O)) |> Array.length
            match xCount, oCount with
            | 5, 0 -> -1000 // Player X побеждает
            | 0, 5 -> 1000  // Player O побеждает
            | x, 0 -> x * -10 // X доминирует
            | 0, y -> y * 10  // O доминирует
            | _ -> 0          // Смешанный ряд, не имеет смысла
    
        let rowsScore = [|0..4|] |> Array.sumBy (fun x -> scoreLine board.[x])
        let colsScore = [|0..4|] |> Array.sumBy (fun y -> scoreLine [| for x in 0..4 -> board.[x].[y] |])
        let diag1 = [| for i in 0..4 -> board.[i].[i] |]
        let diag2 = [| for i in 0..4 -> board.[i].[4 - i] |]
        rowsScore + colsScore + scoreLine diag1 + scoreLine diag2   
   
    // middle
    member this.EvaluateBoard_Middle() =
        let playerWins player =
            let winCheck line = line |> Array.forall ((=) (Some player))
            let checkLines player =
                let rows = [|0..4|] |> Array.exists (fun i -> winCheck board.[i])
                let cols = [|0..4|] |> Array.exists (fun i -> winCheck [| for j in 0..4 -> board.[j].[i] |])
                let diag1 = winCheck [| for i in 0..4 -> board.[i].[i] |]
                let diag2 = winCheck [| for i in 0..4 -> board.[i].[4 - i] |]
                rows || cols || diag1 || diag2
            if checkLines player then 1000 else 0
    
        let scoreX = playerWins Player.X
        let scoreO = playerWins Player.O
    
        scoreO - scoreX
    
    // Lite
    member this.EvaluateBoard_Lite() =
        // Простая эвристическая оценка
        let scoreLine (line: Player option array) =
            let xCount = line |> Array.filter ((=) (Some Player.X)) |> Array.length
            let oCount = line |> Array.filter ((=) (Some Player.O)) |> Array.length
            match xCount, oCount with
            | 5, 0 -> 0 // Player X побеждает
            | 0, 5 -> 1000  // Player O побеждает
            | x, 0 -> 0 // X доминирует
            | 0, y -> y * 10  // O доминирует
            | _ -> 0          // Смешанный ряд, не имеет смысла
    
        let rowsScore = [|0..4|] |> Array.sumBy (fun x -> scoreLine board.[x])
        let colsScore = [|0..4|] |> Array.sumBy (fun y -> scoreLine [| for x in 0..4 -> board.[x].[y] |])
        let diag1 = [| for i in 0..4 -> board.[i].[i] |]
        let diag2 = [| for i in 0..4 -> board.[i].[4 - i] |]
        rowsScore + colsScore + scoreLine diag1 + scoreLine diag2 
    
    
    member this.EvaluateBoard() =
        match gameModeState.GetDifficulty() with
        | 0 -> this.EvaluateBoard_Lite()
        | 1 -> this.EvaluateBoard_Middle()
        | 2 -> this.EvaluateBoard_Hard()
        | _ -> 0
    
    member this.Minimax(depth: int, isMaximizing: bool, alpha: int, beta: int, maxDepth: int) =
        let mutable currentAlpha = alpha
        let mutable currentBeta = beta

        if depth >= maxDepth || gameState <> GameState.InProgress then
            this.EvaluateBoard() // Прекращаем на определённой глубине
        else
            let mutable bestScore =
                if isMaximizing then Int32.MinValue else Int32.MaxValue

            let mutable shouldBreak = false // Флаг для остановки поиска

            for i in 0..4 do
                for j in 0..4 do
                    if not shouldBreak && board.[i].[j].IsNone then
                        // Сделать ход
                        board.[i].[j] <- Some (if isMaximizing then Player.O else Player.X)

                        let score = this.Minimax(depth + 1, not isMaximizing, currentAlpha, currentBeta, maxDepth)
                        board.[i].[j] <- None

                        if isMaximizing then
                            bestScore <- max bestScore score
                            currentAlpha <- max currentAlpha score
                        else
                            bestScore <- min bestScore score
                            currentBeta <- min currentBeta score

                        // Проверка на отсечение
                        if currentAlpha >= currentBeta then
                            shouldBreak <- true // Устанавливаем флаг, чтобы прервать оба цикла

            bestScore
           
    // // Логика для хода ИИ
    // member this.MakeAIMove() =
    //     // Пример простой стратегии: выбираем первую доступную клетку
    //     let mutable madeMove = false
    //     for i in 0..4 do
    //         for j in 0..4 do
    //             if not madeMove && board.[i].[j].IsNone then
    //                 board.[i].[j] <- Some currentPlayer
    //                 if this.CheckWin(i, j) then
    //                     gameState <- PlayerOWins // ИИ всегда будет игроком O в этом примере
    //                 elif this.CheckDraw() then
    //                     gameState <- Draw
    //                 else
    //                     this.SwitchPlayer()
    //                 madeMove <- true
    member this.MakeAIMove() =
        let mutable bestScore = Int32.MinValue
        let mutable move = (0, 0)

        for i in 0..4 do
            for j in 0..4 do
                if board.[i].[j].IsNone then
                    board.[i].[j] <- Some Player.O
                    let score = this.Minimax(0, false, Int32.MinValue, Int32.MaxValue, 1)
                    board.[i].[j] <- None
                    if score > bestScore then
                        bestScore <- score
                        move <- (i, j)

        let x, y = move
        board.[x].[y] <- Some Player.O
        if this.CheckWin(x, y) then
            gameState <- PlayerOWins
        elif this.CheckDraw() then
            gameState <- Draw
        else
            this.SwitchPlayer()
        