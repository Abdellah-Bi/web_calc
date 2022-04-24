namespace abdoCalc

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    


    [<SPAEntryPoint>]
    let Main () =
        let mutable (onum:double), (num:double), op = 0., 0., None

        let display = input [attr.``type`` "text"; attr.value "0"] []

        let updateDisplay () =
            let display = display :?> Elt
            display.Value <- string num

        let D n =
            num <- 10. * num + n
            updateDisplay ()

        let C () =
            num <- 0.
            updateDisplay()

        let AC () =
            num  <- 0.
            onum <- 0.
            op   <- None
            updateDisplay ()

        let N () =
            num <- - num
            updateDisplay ()

        let E () =
            match op with
            | None ->
                ()
            | Some f ->
                num <- f onum num
                op  <- None
                updateDisplay ()

        let O o () =
            match op with
            | None ->
                ()
            | Some f ->
                num <- f onum num
                updateDisplay ()
            onum <- num
            num  <- 0.
            op   <- Some o

        let btn caption action =
            button [on.click (fun _ _ -> action ())] [text caption]

        let digit n =
            btn (string n) (fun () -> D n)

        let calculator =
            div [] [
                display
                br [] []
                div [] [
                    digit 7.; digit 8.; digit 9.; btn "/" (O ( / ))
                    br [] []
                    digit 4.; digit 5.; digit 6.; btn "*" (O ( * ))
                    br [] []
                    digit 1.; digit 2.; digit 3.; btn "-" (O ( - ))
                    br [] []
                    digit 0.; btn "C" C; btn "AC" AC; btn "+" (O ( + ));
                    br [] []
                    btn "+/-" N; btn "=" E
                ]
            ]
       
        IndexTemplate.Main()
            .Main(
                calculator
                )
            .Doc()
        |> Doc.RunById "main"


