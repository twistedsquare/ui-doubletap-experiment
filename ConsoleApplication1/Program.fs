open System
open System.Drawing
open System.IO
open System.Windows.Forms
open System.Threading
open System.Threading.Tasks
open XInputDotNetPure
open MicroLibrary
let mainForm = new Form(Width = 1024, Height = 768, Text = "Gamepad Test")

let l = new Label()
l.AutoSize <- true
l.Font <- new Font("Arial", float32 36.0, FontStyle.Bold)
l.Text <- "Hello!"
// I thought this should centre the label, but it doesn't:
l.Anchor <- AnchorStyles.None

mainForm.Controls.Add(l)

// Keeps track of state during a double-press of a button
type DoubleState =
 | None // Not pressed at all yet.
 | FirstDown of int64 // elapsed micros to first press.  Pressed once, not released
 | Between of int64 // ditto.  Pressed once and released (waiting for second press)

// Displaying: either a delay with orienting prompt, or which button to ask the user to press
type Display =
 | Delay of int64 //millis.  Show delay
 | X // Press X
 | A // Press A
 | DoubleA // Double tap A

type State =
 | PrePractice // Before practice run
 | PreReal // Before actual trial
 | Running of List<Display> * MicroStopwatch * DoubleState * List<string * int64 * int64> * (List<string * int64 * int64> -> State)
     // remaining list, time spent on item, data, next, function for afterwards
 | FinishedWriting of List<string * int64 * int64> // Finished, now write this data to CSV
 | Finished // Totally finished

let rand = new System.Random()
let shuffle cards =    
    cards 
        |> List.map (fun c -> (rand.Next(), c))
        |> List.sortBy fst
        |> List.map snd

let practiceButtons = shuffle (List.concat [for i in 1 .. 3 -> [X; A; DoubleA] ])
let buttons = shuffle (List.concat [for i in 1 .. 30 -> [X; A; DoubleA] ])
// Delays are 500 to 2500 milliseconds
let addDelays(bs) = List.concat (List.map (fun d -> [Delay (int64(500) + int64(rand.Next(2000))); d]) bs)

// printState now a misnomer; processes state according to gamepad state, updates progress accordingly
let printState(state : State ref, progress : IProgress<string>) =
     let padState = GamePad.GetState(PlayerIndex.One)
     if padState.IsConnected then
       match !state with
         | PreReal | PrePractice as p -> do 
                          // We are waiting for them to press B to continue:
                          let preReal = match p with | PreReal -> true; | _ -> false
                          if padState.Buttons.B = ButtonState.Pressed then
                            let watch = new MicroStopwatch()
                            watch.Start()
                            state := Running (addDelays(if preReal then buttons else practiceButtons), watch, None, [], if preReal then FinishedWriting else fun x -> PreReal)
                          else
                             progress.Report("Press B to start " + if preReal then "" else " practice")
         | Finished -> progress.Report("Finished\nClose Both Windows")
         | FinishedWriting data -> do
             let lines = data |> List.rev |> List.map (fun (l, t0, t1) -> l + "," + t0.ToString() + "," + t1.ToString())
             File.WriteAllLines("results.csv", lines)
             state := Finished
         | Running ([], _, _, data, next) -> state := next data
         | Running (displays, watch, ds, data, next) -> do
             let cur = displays.Head
             match cur with
               | A -> progress.Report("A")
                      if padState.Buttons.A = ButtonState.Pressed then
                        watch.Stop()
                        let newData = ("A", watch.ElapsedMicroseconds, int64 0) :: data
                        watch.Reset()
                        watch.Start()
                        state := Running (displays.Tail, watch, None, newData, next)
               | X -> progress.Report("X")
                      if padState.Buttons.X = ButtonState.Pressed then
                        watch.Stop()
                        let newData = ("X", watch.ElapsedMicroseconds, int64 0) :: data
                        watch.Reset()
                        watch.Start()
                        state := Running (displays.Tail, watch, None, newData, next)
               | DoubleA -> progress.Report("A A")
                            match ds with
                                 | None -> if padState.Buttons.A = ButtonState.Pressed then
                                             state := Running (displays, watch, FirstDown watch.ElapsedMicroseconds, data, next)
                                 | FirstDown t -> if padState.Buttons.A = ButtonState.Released then
                                                    state := Running (displays, watch, Between t, data, next)
                                 | Between t -> if padState.Buttons.A = ButtonState.Pressed then
                                                    watch.Stop()
                                                    let newData = ("AA", watch.ElapsedMicroseconds, t) :: data
                                                    watch.Reset()
                                                    watch.Start()
                                                    state := Running (displays.Tail, watch, None, newData, next)
               | Delay target ->
                      progress.Report("-")
                      // Make sure we don't progress until both buttons are released:
                      if (watch.ElapsedMilliseconds > target) && (padState.Buttons.A = ButtonState.Released) && (padState.Buttons.X = ButtonState.Released) then 
                        watch.Stop()
                        watch.Reset()
                        watch.Start()
                        state := Running (displays.Tail, watch, None, data, next)
     else
       progress.Report("Please connect gamepad")

let runTimer(progress) = 
  let t = new MicroTimer()
  let s = ref PrePractice
  t.Interval <- 100L
  t.MicroTimerElapsed.Add(fun x -> printState(s, progress))
  t.Start()
  true

[<STAThread>]
do
 let progress = new Progress<string>(fun s -> l.Text <- s)
 ignore(Task.Factory.StartNew<bool>((fun () -> runTimer(progress)),  TaskCreationOptions.LongRunning))
 Application.Run(mainForm)