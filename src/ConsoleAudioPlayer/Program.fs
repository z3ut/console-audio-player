open System
open AduioPlayer

[<EntryPoint>]
let main argv =
    let audioPlayer = new AudioPlayer()
    audioPlayer.Run()

    0
