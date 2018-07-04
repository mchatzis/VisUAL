# Specification  for Editor gui
## Editor contents format
### CodeRef () contains editor contents
Raw text format has line endings " \r\n", example below
```
        mov r7, #5
        mov r8, r0
Label   Mov R3, R4
        B   Label
```
Will be displayed as a string:
```
"       mov r7, #5\r\n      mov r8, r0\r\nLabel   Mov R3, R4\r\n        B   Label"
```

## Running the full code
Current:
* Run editor
    - Will not compile due to Fable regex match/group collection issue. Lengthy fix as requires rewriting parts of emulator not to use Regex
    - Code to send editor contents and interface with emulator will work as updating from a test datapath works and calling the run function in pure F# works

Plan:   
Give the format shown above, possible return data:

 * Succsessful build, set as current state, the resulting datapath and number of cycles, all good :)
 * Bad build, set current state datapath, number of cycles, and error bool/as option? + error message


## Step forward and Step back (unimplemented)

Plan:
* We ask for each step in turn.
* If the line is invalid, set the state as that error and the previous datapath
* Else if it is valid save new datapaths and its corresponding cylce number
* Set current state as the new datapath & cycle number
* If we step back, set current state as the previous datapath and cycle number

I will be a good idea to store in memory all the datapaths and cycle numbers as they
are generated, this is only needed if a step is called, not for running the whole thing at once
___
Buttons/Widgets/Features
===

Implemented Features:







## *File management* [1]
Current state:
* New file
    - Clears editor contents
* Open file
    - Reads txt file in local directory with filename set in input box, and imports to editor
* Save file
    - Writes txt file in local directory with filename set in input box with contents of editor. Creates file if doesn't exist
## Register Display [1]
* Registers and flag interfaces
    - Registers and flags read and update correctly from a fake datapath
## Flags [1]
Update.fs
```fsharp
//------------------------------------------------
let flagToggle (id:string) =
    let el = Ref.flag id
    el.innerHTML <- sprintf "%i" (match el.innerHTML with
                                    | "1" -> 0
                                    | "0" -> 1
                                    | _ -> 0)
//------------------------------------------------
```

Renderer.fs
```fsharp

let flagToggle (dest:string) =
(Ref.flag dest).addEventListener_click(fun _ ->
    Browser.console.log ("flag "+ dest + " changed") |> ignore
    Update.flagToggle dest

//inside let init()
    ["N";"Z";"C";"V"] |> List.map flagToggle |> ignore
)
```

## Current/Total instructions/cycles [1]
* Current Instruction/Cycle
    - GUI elements exist. Code to read unwritten due to Fable issue impeding interface

index.html
```html
    </div>
    <div id="cycles" class="btn-group pull-right footer-margin">
        <button class="btn btn-cyc btn-long">Cycles: </button>
        <button class="btn btn-cyc-con btn-long">0</button>
    </div>
```
photon.css
```css
.btn-cyc, .btn-cyc:active {
   background-color: #6d6c6d;
   border: 1px solid #c2c0c2;
 }

 .btn-cyc:focus, .btn-cyc:active {
   z-index: 3;
   border: 1px solid #c2c0c2;
 }

 .btn-cyc-con, .btn-reg-con:active {
   background-color: #fff;
   border: 1px solid #c2c0c2;
 }

 .btn-cyc-con:focus, .btn-reg-con:active {
   z-index: 3;
   border: 1px solid #c2c0c2;
 }


 .btn-long {
     padding: 2px 12px
 }
```
## Dec/Bin/Hex [1]
* Bin/Hex/Dec viewing of register contents
    - Button to cycle through Binary/Hex/Dec representations of register value
    - Issue: Negative values cause a problem when represented in binary
        - when converting to binary representations (since there is no built in type for this in f#) the negative sign is not properly delt with in my algorithm. 
Renderer.fs

```fsharp
///helper for Binary, decimal hex representation button
let BinDecHex reg =
    (Ref.bdh reg).addEventListener_click(fun _ ->
        Browser.console.log ("Register R" + (reg|>string) + " new bdh")
        Update.BinDecHex reg
        Update.register reg ((Ref.register reg).innerHTML |> int)
    )

[0..15] |> List.map BinDecHex |> ignore
```
Update.fs
```fsharp
///helper for int to binary string
let rec intToBinary i =
    match i with
    | 0 | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (intToBinary (i / 2)) + bit


let register (id: int) (value: int) =
    let bdh = Ref.bdh id//get register bhd state
    let el = Ref.register id
    el.setAttribute("style", "background: #fbbc05")
    el.innerHTML <- match bdh.innerHTML with            // output correct format from bhd state
                            | "Hex" ->  sprintf "0x%X" value
                            | "Bin" ->  "0b" + (intToBinary value) //deal with sign error here
                            | "Dec" -> sprintf "%i" value  
                            | _ ->     sprintf "0x%X"  value // default to hex

let BinDecHex (id: int) =
    let bdh = Ref.bdh id
    bdh.innerHTML <- match bdh.innerHTML with
                        | "Hex" ->  "Bin"
                        | "Bin" ->  "Dec"
                        | "Dec" -> "Hex"  
                        | _ ->     "Hex" // default to hex
```
## Reset [1]
* Reset button
    - All registers and flags are set to 0 in the current state, this is then reflected in GUI 
## Initialising Register values [Unfinished]
* Initialising registers/flags state
    - All registers and flags can be set by user in GUI
    - This changes the current state and would be very useful for testing
    - Currently this state is not passed to emulator 
    - More protection to when this is accessible is needed
    - No current checking valid values entered
    - Plans to store/import preset states for testing not implemented
index.html
```html
<td>
        Click on registers to update with below value.
        <div><input id="input" type="text" value = "x"></input></div>
</td>
```
Renderer.fs

```fsharp
let regButton reg =
    (Ref.register reg).addEventListener_click(fun _ ->
            Browser.console.log (regLog reg) |> ignore
            Ref.input.value |> int |> Update.register reg
        )
let init() =
//other funcs
[0..15] |> List.map regButton |> ignore
//other funcs
init()
```
# Unimplemented
## *Tools*
#### Memory Map [2]
#### Symbol Map [2]
#### Literal rotation animation?
#### Auto-Indent?
## *On-Screen Widgets*
#### Register History [3]
#### Branch animation [2]
#### Deal with white space [3]
### *Error Handling* [1]
Currently not implemented
* Error Handling
    - Displaying Error line and a message 
## *Settings*
#### Keyboard Shortcuts?
#### Font size
#### Colour Scheme
## *Misc*
#### Window Resizing?

