// Partial tests for expression only
module MiscTests
    open Expecto
    open Expressions

    let tab = ["THING3" , 5u ; "LABEL" , 8u ; "Lab2test" , 4u] |> Map.ofList

    [<Tests>]
    let parseTest=
        let eqTest name (inp,out) = 
            testCase name <| fun () ->
                Expect.equal inp out (sprintf "Equal Test %A" name)
        let errTest name inp = 
            testCase name <| fun () ->
                Expect.isError inp  (sprintf "Error Test %A" name)            
        Expecto.Tests.testList "Tests"
               [       //More specific, useful labels will be added
               //Including Categorising tests/errors to help work out any unmatched conditions
                eqTest "EQ:Test 1"    ((expParse None "0x8000"),(Ok 32768u ));
                eqTest "EQ:Test 2"    ((expParse None "0b100"),(Ok 4u ));
                eqTest "EQ:Test 3"    ((expParse None "&8000"),(Ok 32768u ));        
                eqTest "EQ:Test 4"    ((expParse None "1020"),(Ok 1020u )); 
                eqTest "EQ:Test 5"    ((expParse None "-4"),(Ok 4294967292u )); 
                eqTest "EQ:Test 6"    ((expParse None "16711680"),(Ok 16711680u )); 
                eqTest "EQ:Test 7"    ((expParse None "4+6"),(Ok 10u ));
                eqTest "EQ:Test 8"    ((expParse None "4*6"),(Ok 24u ));
                eqTest "EQ:Test 9"    ((expParse None "4-6"),(Ok 4294967294u ));
                eqTest "EQ:Test 10"   ((expParse None "-5*6"),(Ok 4294967266u )); 
                eqTest "EQ:Test 11"   ((expParse None "0x8*3-42+16-0b11+&18"),(Ok 19u )); 
                eqTest "EQ:Test 12"   ((expParse None "&8"),(Ok 8u ));
                eqTest "EQ:Test 13"   ((expParse (Some tab) "0x8000"),(Ok 32768u ));
                eqTest "EQ:Test 14"   ((expParse (Some tab) "THING3"),(Ok 5u ));
                eqTest "EQ:Test 15"   ((expParse (Some tab) "LABEL"),(Ok 8u ));
                eqTest "EQ:Test 16"   ((expParse (Some tab) "Lab2test"),(Ok 4u ));
                eqTest "EQ:Test 17"   ((expParse (Some tab) "8-3+94+THING3+3*LABEL*Lab2test"),(Ok 3424u )); 
                eqTest "EQ:Test 18"   ((expParse (Some tab) "-8-0b11+94+THING3+&3*LABEL*Lab2test"),(Ok 2912u )); 
                eqTest "EQ:Test 19"   ((expParse (Some tab) "1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19"),(Ok 190u )); //Uncertain on Visual/Arms max expression items, no current limit
                //Error test
                errTest "ERR:Test 1"   (expParse None " ");
                errTest "ERR:Test 2"   (expParse None "00x8000");
                errTest "ERR:Test 3"   (expParse None "0label");
                errTest "ERR:Test 4"   (expParse None "LABEL");
                errTest "ERR:Test 5"   (expParse (Some tab) "THING");
                errTest "ERR:Test 6"   (expParse None "257");
                errTest "ERR:Test 7"   (expParse None "0x257");
                errTest "ERR:Test 8"   (expParse None "--1");
                errTest "ERR:Test 9"   (expParse None "4++5");
                errTest "ERR:Test 10"  (expParse None "6*-5"); //
                errTest "ERR:Test 11"  (expParse None "1* 6");
                errTest "ERR:Test 12"  (expParse None "*16");
                errTest "ERR:Test 13"  (expParse None "16-");
                errTest "ERR:Test 14"  (expParse None " 0x8000"); //Problem: When parsing DCD lists Visual allows whitespace between words. Better matching needed to cover
                errTest "ERR:Test 15"  (expParse (Some tab) "THING33");
                errTest "ERR:Test 16"  (expParse (Some tab) "THING3+THIN");
                errTest "ERR:Test 17"  (expParse (Some tab) "THING3+label");
                errTest "ERR:Test 18"  (expParse None "&8&9");
                errTest "ERR:Test 19"  (expParse None ".")
                // errTest "ERR:Test 18"  (expParse (Some tab) "36/"); //Passed as 36u. Tighter Matching on non-alphanumeric characters

                ]


    let testsWithExpecto() = runTestsInAssembly defaultConfig [||]