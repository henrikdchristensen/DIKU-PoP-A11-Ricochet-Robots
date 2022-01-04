let g = RobotsLib.Game()
printfn "Game over. You finished in %i moves. Best score is set by %s moves" (g.Play()) (g.ReadHighScore().Value)