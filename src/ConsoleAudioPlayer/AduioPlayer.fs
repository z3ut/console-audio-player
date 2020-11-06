module AduioPlayer

open System
open System.Text
open Terminal.Gui
open NStack
open NAudio.Wave
open System.IO
open System.Collections.Generic
open System.Timers

let ustr (x: string) = ustring.Make(x)

type AudioPlayer() as self =
    let mutable playlistList : ListView = null
    let mutable playPauseButton : Button = null
    let mutable volumePercentLabel : Label = null
    let mutable trackNameLabel : Label = null
    let mutable playTimeLabel : Label = null

    let mutable playlist : string[] = [| |]
    let mutable trackExtensions = [ "mp3"; "flac" ]
    let mutable playingIndex = 0
    let mutable isPlaying = false
    let mutable isTrackInited = false
    let mutable currentVolume = 1.0f
    let volumeStep = 0.05f

    let mutable outputDevice : WaveOutEvent = null
    let mutable audioFile : AudioFileReader = null
    let playTimeTimer = new Timer(1000.0)
    
    let mutable playbackStoppedDelegate = new EventHandler<StoppedEventArgs>(
        fun obj args -> self.OnPlaybackStopped(args))

    do
        playTimeTimer.Elapsed.Add (fun args -> self.SetCurrentPlayTime())
        Console.OutputEncoding <- Encoding.UTF8
        Console.Title <- "Audio Player"
        self.InitializeInterface()

    member this.Run() =
        Application.Run()

    member private this.InitializeInterface() =
        Application.Init()

        let menu = new MenuBar([|
            new MenuBarItem(ustr "_File", [|
                new MenuItem(ustr "_Open", ustr "", fun _ -> this.OpenFiles())
                new MenuItem(ustr "_Quit", ustr "", fun _ -> Application.RequestStop())
            |])
        |])

        let playlistWindow = new Window(ustr "Playlist",
            X = Pos.At 0,
            Y = Pos.At 1,
            Width = Dim.Fill(),
            Height = Dim.Fill() - Dim.Sized 4)


        playlistList <- new ListView(X = Pos.At 0,
            Y = Pos.At 0,
            Width = Dim.Fill(),
            Height = Dim.Fill())
        playlistList.add_OpenSelectedItem(fun e -> this.StartPlaying(e.Item))

        playlistWindow.Add playlistList

        let controlWindows = new Window(ustr "Control",
            X = Pos.At 0,
            Y = Pos.AnchorEnd(4),
            Width = Dim.Fill(),
            Height = Dim.Sized 4)

        let previousButton = new Button(ustr "< _U",
            X = Pos.At 0,
            Y = Pos.At 1)
        previousButton.add_Clicked(fun _ -> this.PlayPrevious())

        playPauseButton <- new Button(ustr "Play ",
            X = Pos.Right(previousButton) + Pos.op_Implicit 1,
            Y = Pos.At 1)
        playPauseButton.add_Clicked(fun _ -> this.TogglePause())
        
        let nextButton = new Button(ustr "> _I",
            X = Pos.Right(playPauseButton) + Pos.op_Implicit 1,
            Y = Pos.At 1)
        nextButton.add_Clicked(fun _ -> this.PlayNext())

        let decreaseTimeButton = new Button(ustr "-10s _7",
            X = Pos.Right(nextButton) + Pos.op_Implicit 1,
            Y = Pos.At 1)
        decreaseTimeButton.add_Clicked(fun _ -> this.Seek(-10))

        let increaseTimeButton = new Button(ustr "+10s _8",
            X = Pos.Right(decreaseTimeButton) + Pos.op_Implicit 1,
            Y = Pos.At 1)
        increaseTimeButton.add_Clicked(fun _ -> this.Seek(10))

        let increaseVolume = new Button(ustr "+ _0",
            X = Pos.AnchorEnd() - Pos.op_Implicit 7,
            Y = Pos.At 1)
        increaseVolume.add_Clicked(fun _ -> this.ChangeVolume(volumeStep))

        volumePercentLabel <- new Label(ustr "100%",
            X = Pos.Left(increaseVolume) - Pos.op_Implicit 5,
            Y = Pos.At 1,
            Height = Dim.op_Implicit 1,
            Width = Dim.op_Implicit 4)

        let decreaseVolume = new Button(ustr "- _9",
            X = Pos.Left(volumePercentLabel) - Pos.op_Implicit 8,
            Y = Pos.At 1)
        decreaseVolume.add_Clicked(fun _ -> this.ChangeVolume(-volumeStep))

        let volumeLabel = new Label(ustr "Volume",
            X = Pos.Left(decreaseVolume) - Pos.op_Implicit 7,
            Y = Pos.At 1,
            Height = Dim.op_Implicit 1,
            Width = Dim.op_Implicit 6)

        let playTimeLabelWidth = 17
        trackNameLabel <- new Label(ustr "",
            X = Pos.At 0,
            Y = Pos.At 0,
            Height = Dim.op_Implicit 1,
            Width = Dim.Fill() - Dim.op_Implicit playTimeLabelWidth)

        playTimeLabel <- new Label(ustr "",
            X = Pos.AnchorEnd(playTimeLabelWidth),
            Y = Pos.At 0,
            Height = Dim.op_Implicit 1,
            Width = Dim.op_Implicit playTimeLabelWidth)

        controlWindows.Add(previousButton, playPauseButton, nextButton,
            decreaseTimeButton, increaseTimeButton,
            volumeLabel, decreaseVolume, volumePercentLabel, increaseVolume,
            trackNameLabel, playTimeLabel)

        Application.Top.Add(menu, playlistWindow, controlWindows)


    member private this.OpenFiles() =
        let openDialog = new OpenDialog(ustr "Open playlist",
            ustr "Select folder or files")
        openDialog.AllowsMultipleSelection <- true
        openDialog.CanChooseDirectories <- true
        Application.Run(openDialog)
        if openDialog.FilePaths <> null && openDialog.FilePaths.Count > 0 then
            this.StopPlaying()

            playingIndex <- 0
            playlistList.SelectedItem <- 0

            playlist <-
                openDialog.FilePaths
                |> Seq.map (fun path ->
                    if File.Exists path then seq { path }
                    else Directory.EnumerateFiles(path, "*", SearchOption.AllDirectories))
                |> Seq.concat
                |> Seq.filter (fun path ->
                    List.exists (fun (ext: string) -> path.ToLower().EndsWith(ext)) trackExtensions)
                |> Seq.toArray

            playlistList.SetSource(playlist
                |> Array.map (fun path -> this.FormatTrackListName(path)))

    member private this.FormatTrackListName(path: string) =
        let fileName = Path.GetFileName(path)
        if playingIndex < playlist.Length && playlist.[playingIndex] = path
        then (if isPlaying then " > " else " # ") + fileName
        else "   " + fileName

    member private this.UpdateListTrackName(index: int) =
        if index < playlist.Length then
            playlistList.Source.ToList().[index] <-
                this.FormatTrackListName(playlist.[index])

    member private this.StartPlaying(trackIndex: int) =
        isPlaying <- true
        isTrackInited <- true
        let previouslySelectedIndex = playingIndex
        playingIndex <- trackIndex
        playlistList.SelectedItem <- playingIndex
        this.UpdateListTrackName playingIndex
        this.UpdateListTrackName previouslySelectedIndex
        trackNameLabel.Text <- playlist.[playingIndex] |> Path.GetFileName |> ustr
        
        this.DisposeAudio()

        outputDevice <- new WaveOutEvent()
        outputDevice.PlaybackStopped.AddHandler playbackStoppedDelegate
        outputDevice.Volume <- currentVolume

        audioFile <- new AudioFileReader(playlist.[playingIndex])
        outputDevice.Init audioFile

        this.StartPlaying()
        this.SetCurrentPlayTime()

    member private this.StartPlaying() =
        if playingIndex < playlist.Length then
            isPlaying <- true
            playPauseButton.Text <- ustr "Pause"
            this.UpdateListTrackName(playingIndex)
            if outputDevice <> null then outputDevice.Play()
            playTimeTimer.Start()

    member private this.StopPlaying() =
        isPlaying <- false
        playPauseButton.Text <- ustr "Play "
        this.UpdateListTrackName(playingIndex)
        if outputDevice <> null then outputDevice.Pause()
        playTimeTimer.Stop()
        this.SetCurrentPlayTime()

    member private this.TogglePause() =
        if isPlaying then
            this.StopPlaying()
        elif isTrackInited then
            this.StartPlaying()
        elif playlist.Length > 0 then
            this.StartPlaying playlistList.SelectedItem

    member private this.PlayPrevious() =
        let nextIndex = playingIndex - 1
        if 0 <= nextIndex then
            this.StartPlaying nextIndex

    member private this.PlayNext() =
        let nextIndex = playingIndex + 1
        if nextIndex < playlist.Length then
            this.StartPlaying nextIndex
        
    member private this.Seek(offset: int) =
        if audioFile <> null then
            let position = audioFile.Position + int64(offset * audioFile.WaveFormat.AverageBytesPerSecond)
            let adj = position % int64(audioFile.WaveFormat.BlockAlign)
            let newPos = Math.Min(audioFile.Length, position - adj) |> max 0L
            audioFile.Position <- newPos
            this.SetCurrentPlayTime()

    member private this.ChangeVolume(step: float32) =
        currentVolume <- currentVolume + step |> min 1.0f |> max 0.0f
        if outputDevice <> null then outputDevice.Volume <- currentVolume
        volumePercentLabel.Text <- ustr (Math.Round(float(currentVolume * 100.0f)).ToString() + "%")

    member private this.DisposeAudio() =
        if outputDevice <> null then
            outputDevice.PlaybackStopped.RemoveHandler playbackStoppedDelegate
            outputDevice.Dispose()
            outputDevice <- null

        if audioFile <> null then
            audioFile.Dispose()
            audioFile <- null

    member private this.OnPlaybackStopped(args: StoppedEventArgs) =
        if outputDevice.PlaybackState = PlaybackState.Stopped then
            let nextIndex = playingIndex + 1
            if nextIndex < playlist.Length then
                this.StartPlaying nextIndex
            else
                isTrackInited <- false
                this.StopPlaying()
                this.DisposeAudio()

    member private this.SetCurrentPlayTime() =
        if audioFile <> null then
            let timeFormat = @"hh\:mm\:ss"
            playTimeLabel.Text <- ustr (audioFile.CurrentTime.ToString(timeFormat) +
                "/" + audioFile.TotalTime.ToString(timeFormat))
