namespace Tic_tac_toe_FSharp_WebAppMVC.Controllers

open System
open System.Diagnostics
open System.Text
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open Tic_tac_toe_FSharp_WebAppMVC.Models

// Define the possible game modes
type GameMode =
    | PlayerVsPlayer = 0
    | PlayerVsAI = 1

type HomeController (logger : ILogger<HomeController>, game : GameModel, gameModeState: GameModeState) =
    inherit Controller()

    // static let game = new GameModel()
    let mutable gameMode = gameModeState.GetMode() // Default mode to Player vs Player

    // Рендеринг главной страницы
    member this.Index () =
        let board = game.GetBoard()
        let currentPlayer = game.GetCurrentPlayer()
        let gameState = game.GetGameState()
        
        // Начало формирования HTML-разметки
        let sb = StringBuilder()

        // Формируем форму для выбора режима игры
        sb.AppendLine($"""
        <form method="post" action="/Home/SetGameMode">
            <label for="playerVsPlayer">Player vs Player</label>
            <input type="radio" id="playerVsPlayer" name="mode" value="0" {if gameMode = 0 then "checked" else ""}>
            <label for="playerVsAI">Player vs AI</label>
            <input type="radio" id="playerVsAI" name="mode" value="1" {if gameMode = 1 then "checked" else ""}>
            <button type="submit">Set Mode</button>
        </form>
        """) |> ignore
        sb.AppendLine($"""
        <form method="post" action="/Home/SetDifficulty">
            <label for="difficulty">Choose AI Difficulty:</label>
            <select id="difficulty" name="difficulty">
                <option value="0" {if gameModeState.GetDifficulty() = 0 then "selected" else ""}>Easy</option>
                <option value="1" {if gameModeState.GetDifficulty() = 1 then "selected" else ""}>Medium</option>
                <option value="2" {if gameModeState.GetDifficulty() = 2 then "selected" else ""}>Hard</option>
            </select>
            <button type="submit">Set Difficulty</button>
        </form>
        """) |> ignore

        // Формируем HTML для отображения игры с использованием таблицы
        sb.AppendLine("<h2>Game Board</h2>")
        
        sb.AppendLine("<table border=\"1\" style=\"width: auto; text-align: center; border-collapse: collapse; margin: auto; \">")  // border-collapse для аккуратной таблицы

        for i in 0..4 do
            sb.AppendLine("<tr>") // Начало новой строки
            for j in 0..4 do
                // Получаем символ, который нужно отобразить в ячейке
                let (cell, color) = 
                    match board.[i].[j] with
                    | Some Player.X -> ("X", "color: blue;")
                    | Some Player.O -> ("O", "color: red;")
                    | None -> ("-", "")  // Для пустых клеток используем "-"
                
                // Используем строковую интерполяцию для формирования строки
                sb.AppendLine $"""
                <td style="width: 60px; height: 60px; padding: 0; margin: 0; text-align: center; vertical-align: middle; border: 1px solid black;">
                    <form method="post" action="/Home/MakeMove" style="margin: 0; padding: 0;">
                        <input type="hidden" name="x" value="%d{i}" />
                        <input type="hidden" name="y" value="%d{j}" />
                        <button type="submit" style="
                            width: 60px; 
                            height: 60px; 
                            font-size: 16px; 
                            padding: 0; 
                            margin: 0;
                            border: none;
                            background: transparent;
                            text-align: center;
                            line-height: 60px;
                            %s{color}">%s{cell}</button>
                    </form>
                </td>
            """ |> ignore
                
                printf "Board: %A " board.[i].[j]
            sb.AppendLine("</tr>") // Конец строки
            printfn ""
        sb.AppendLine("</table>")
        // Статус игры и текущий игрок
        sb.AppendFormat("<p>Current Player: {0}</p>", if currentPlayer = Player.X then "X" else "O")
        sb.AppendFormat("<p>Game State: {0}</p>", 
            match gameState with
            | GameState.InProgress -> "In Progress"
            | GameState.PlayerXWins -> "Player X Wins"
            | GameState.PlayerOWins -> "Player O Wins"
            | GameState.Draw -> "Draw"
        ) |> ignore

        // Кнопка для сброса игры
        sb.AppendLine("""
            <form method="post" action="/Home/ResetGame">
                <button type="submit">Reset Game</button>
            </form>
        """)

        // Возвращаем сформированную страницу
        this.Content(sb.ToString(), "text/html")

    // Выбор режима игры
    [<HttpPost>]
    member this.SetGameMode(mode: int) =
        match mode with
        | 0 | 1 -> gameModeState.SetMode(mode)
        | _ -> ()
        game.ResetGame()
        this.RedirectToAction("Index")
    
    
 
    [<HttpPost>]
    member this.MakeMove(x: int, y: int) =
        printfn "Making move at position (%d, %d)" x y
        
        if game.GetGameState() = GameState.InProgress && gameModeState.GetMode() = 0 then
            // Режим PlayerVsPlayer
            game.MakeMove(x, y)
        
        elif game.GetGameState() = GameState.InProgress && gameModeState.GetMode() = 1 then
            // Режим PlayerVsAI
            // if game.GetCurrentPlayer() = Player.X then
            //     game.MakeMove(x, y)
            // if game.GetGameState() = GameState.InProgress then
            //     game.MakeAIMove()
            lock game (fun () ->
            if game.GetCurrentPlayer() = Player.X then
                game.MakeMove(x, y)
            if game.GetGameState() = InProgress then
                game.MakeAIMove())

        this.RedirectToAction("Index")
   
    // Сброс игры
    member this.ResetGame() =
        game.ResetGame()
        this.RedirectToAction("Index")
        
    [<HttpPost>]
    member this.SetDifficulty(difficulty: int) =
        if difficulty >= 0 && difficulty <= 2 then
            gameModeState.SetDifficulty(difficulty)
        this.RedirectToAction("ResetGame")
    
    member this.Privacy () =
        this.View()

    [<ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)>]
    member this.Error () =
        let reqId = 
            if isNull Activity.Current then
                this.HttpContext.TraceIdentifier
            else
                Activity.Current.Id

        this.View({ RequestId = reqId })