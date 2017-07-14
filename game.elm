import GraphicSVG exposing(..)
import List
import Tuple exposing (first)
import String
import Maybe

type Msg = Tick Float GetKeyState

         | StartGame
         | Instruct
         | SubmitAnswer Answer Answer
         | Reset

type GameState = MainMenu
               | InstructMe
               | InGame
               | EndOfGame
               | Failure

type Answer = A | B | C | D

main =
    gameApp Tick {   model = init
                 ,   view = view
                 ,   update = update
                 }

--- MODEL ---

init = { state = MainMenu
       , levels = [ level1,
                    level2,
                    level3,
                    level4,
                    level5,
                    level6,
                    level7
                  ]
       , chances = 2
       , time = 0  -- This is specifically ANIMATION time.

         -- Below are a set of variables that aren't used in the template but
         -- maybe you can figure out how to use them? You can add more too!
       , score = 0
       , timelimit = 10
       , highscore = 0
       , current = 0
       }

--- VIEW ---

view model = case model.state of
                MainMenu -> collage 1000 500 (menuView model)
                InGame   -> collage 1000 500 (levelView (List.head model.levels) model.time model.chances model.timelimit model.score)
                EndOfGame   -> collage 1000 500 (endView model model.score)
                Failure  -> collage 1000 500 (failView model)
                InstructMe -> collage 1000 500 (logangInstructionsView model)

menuView model = [rect 10 1000
                    |> filled black
                    |> move (-400, 0)
                        , rect 10 1000
                          |> filled black
                          |> move (400, 0)
                        , rect 800 10
                            |> filled black
                            |> move (0, 245)
                        , rect 800 10
                            |> filled black
                            |> move (0, -245)
                        , text "Logang"
                            |> size 144
                            |> centered
                            |> fixedwidth
                            |> filled (changingColour (abs (sin (model.time/4))+2))
                            |> move(0,100)
                            |> rotate (degrees (sin (model.time/4)))
                            |> scale (0.5*(abs (sin (model.time/4))+1))
                         , group [ circle 100
                            |> filled green
                            |> move (-120,-50)
                         , text "START"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (-120,-65)
                            |> rotate (degrees (tan (model.time/25)))
                         ] |> notifyMouseDown StartGame
                         , group [ circle 100
                            |> filled orange
                            |> move (120,-50)
                         , text "INSTRUCTIONS"
                            |> size 25
                            |> centered
                            |> filled white
                            |> move (120,-57)
                            |> rotate (degrees -1*(tan (model.time/25)))
                         ] |> notifyMouseDown Instruct
                 ]

changingColour t = (rgb (abs(69*cos(t))) (abs(200*cos(t))) (abs(120*cos(t))))

logangInstructionsView model = [text "How to play: "
                            |> size 25
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move (0,100)
                            , text "Try to identify the logo and select the correct"
                            |> size 20
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move (0,80)
                            , text "company it belongs to, within the given time."
                            |> size 20
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move (0,60)

                            , text "Correct answers will be rewarded with points, while"
                            |> size 20
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move (0,40)

                            , text "incorrect choices will result in a deduction of points."
                            |> size 20
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move (0,20)

                         , group [circle 100
                            |> filled green
                            |> move (0, -150)
                         , text "Main Menu"
                            |> size 35
                            |> centered
                            |> filled white
                            |> move (0,-165)
                            |> rotate (degrees (tan (model.time/25)))
                         ] |> notifyMouseDown Reset
                  ]
endView model score = [text "You Win!"
                            |> size 144
                            |> centered
                            |> fixedwidth
                            |> filled white
                            |> addOutline (solid 2) green
                            |> move(0,150)
                         , text ("Final Score: " ++ (toString (ceiling score)))
                            |> size 50
                            |> centered
                            |> filled black
                            |> move (0,-200)
                         , group [circle 100
                            |> filled green
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                            |> rotate (degrees (tan (model.time/25)))
                         ] |> notifyMouseDown Reset
                 ]
failView model = [ text "Take this L!"
                            |> size 130
                            |> centered
                            |> fixedwidth
                            |> filled white
                            |> addOutline (solid 2) darkRed
                            |> move(0,150)
                         , group [ circle 100
                            |> filled darkRed
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                            |> rotate (degrees (tan (model.time/25)))
                         ] |> notifyMouseDown Reset
                 ]

levelView level t chances timelimit score= case level of
                                 Nothing -> []
                                 Just lev ->  [ group (lev.image t)
                                            , option A lev.optionA
                                                |> move (-150,-120)
                                                |> notifyMouseDown (SubmitAnswer A lev.answer)
                                            , option B lev.optionB
                                                |> move (-150,-160)
                                                |> notifyMouseDown (SubmitAnswer B lev.answer)
                                            , option C lev.optionC
                                                |> move (150,-120)
                                                |> notifyMouseDown (SubmitAnswer C lev.answer)
                                            , option D lev.optionD
                                                |> move (150,-160)
                                                |> notifyMouseDown (SubmitAnswer D lev.answer)
                                            , text "Extra chances"
                                                |> filled red
                                                |> move (200,100)
                                            , group (displayChances chances)
                                                |> move (200,150)
                                            , text ("Time: " ++ (toString (ceiling timelimit)))
                                                |> size 50
                                                |> centered
                                                |> filled black
                                                |> move (-250, 125)
                                            , text ("Score: " ++ (toString (ceiling score)))
                                                |> size 50
                                                |> centered
                                                |> filled black
                                                |> move (-250, 175)
                                           ]

displayChances chances = case chances of
                            0 -> []
                            _ -> [heart red
                                    |> scale (0.3  )
                                    |> move (0 +  chances * 100,0) ] ++ (displayChances (chances - 1))

option ans tex = group [ rectangle 200 30
                            |> filled blue
                       , text ((toString ans) ++ ": " ++ tex)
                            |> size 20
                            |> filled white
                            |> move (-90,-7) ]


level1 = { image = level1_image
         , optionA = "Bose"
         , optionB = "Beats"
         , optionC = "Bon Jovi"
         , optionD = "Blessed"
         , answer = B
         }
logo2 = group[ circle 60 |> filled (rgb  228 31 61)
                 , circle 25 |> outlined (solid 13) white
                 , rectangle 65 12.8 |> filled white |> rotate (degrees 90)|> move (-25,28)
                 ]
level1_image t = [logo2 |>rotate(degrees(360*sin(t/20))) |>scale (abs(sin (t/5))/2 +1)]





level2 = { image = level2_image
         , optionA = "Samsung"
         , optionB = "Apple"
         , optionC = "Android"
         , optionD = "CSS Robot"
         , answer = C
         }

head = circle 60
       |> filled (rgb 152 211 84)
       |> move (0, 160)
       |> scaleX 1.2
body = roundedRect 144 155 20
       |> filled (rgb 152 211 84)
       |> move (0, 80)
mouth = rect 150 10
        |> filled white
        |> move (0, 150)
arm1 = group [roundedRect 40 130 20
       |> filled (rgb 152 211 84)
       |> move (0, -50)]
arm2 = group [roundedRect 40 130 20
       |> filled (rgb 152 211 84)
       |> move (0, -50)]
eye1 = circle 10
       |> filled white
       |> move (-30, 185)
eye2 = circle 10
       |> filled white
       |> move (30, 185)
leg1 = roundedRect 40 130 20
       |> filled (rgb 152 211 84)
       |> move (-30, 10)
leg2 = roundedRect 40 130 20
       |> filled (rgb 152 211 84)
       |> move (30, 10)
antenna1 = roundedRect 8 50 5
           |> filled (rgb 152 211 84)
           |> move (50, 220)
           |> rotate (degrees -30)
antenna2 = roundedRect 8 50 5
           |> filled (rgb 152 211 84)
           |> move (-50, 220)
           |> rotate (degrees 30)



level2_image t = [group[group[head
                  , antenna1 |> rotate (degrees -5*(sin (t/2)))
                  , antenna2 |> rotate (degrees 5*(sin (t/2)))
                  , eye1
                  , eye2] |> move (0, 5*(sin (t/2)))
                  , body
                  , mouth
                  , arm1 |> move (-102,130)|> rotate (degrees -15*(sin (t/2)))
                  , arm2 |> move (102,130)|> rotate (degrees 15*(sin (t/2)))
                  , leg1
                  , leg2
                  ] |> rotate (0.5*(sin (t/20)))]

level3 = { image = level3_image
         , optionA = "Volkswagen"
         , optionB = "BYD"
         , optionC = "Mercedes Benz"
         , optionD = "BMW"
         , answer = D
         }
logo = group [
  circle 60 |> filled black
  ,circle 57 |> outlined (solid 1.5) white
  ,circle 37|> filled white
  ,polygon [(0,0),(0,33),(-33,0)]|> filled (rgb  0 131 195)|>move(-0.5,0.5)
  ,curve (0,0) [Pull (23,16) (46,0)]|> filled (rgb  0 131 195)|> rotate (degrees 45)|>move (-33.5,0.5)
  ,polygon [(0,0),(0,33),(-33,0)]|> filled (rgb  0 131 195)|> rotate (degrees 180)|>move(0.5,-0.5)
  ,curve (0,0) [Pull (23,16) (46,0)]|> filled (rgb  0 131 195)|> rotate (degrees 225)|>move (33.5,-0.5)
  ]

level3_image t = [logo |> rotate(degrees(180*sin(t/20)))]

level4 = { image = level4_image
         , optionA = "GUCCI"
         , optionB = "CARODAUR"
         , optionC = "CHANEL"
         , optionD = "CHAMPION"
         , answer = C
         }
circles = group [circle 33 |> outlined (solid 10) black |> move (-20, 0)
                ,circle 33 |> outlined (solid 10) black |> move (20, 0)
                ]

shaping = group [rect 20 20 |> filled white |> move (50, 10) |> rotate (degrees 30)
                ,rect 20 20 |> filled white |> move (50, -10) |> rotate (degrees 150)
                ,rect 20 20 |> filled white |> move (-50, 10) |> rotate (degrees 150)
                ,rect 20 20 |> filled white |> move (-50, -10) |> rotate (degrees 30)
                ,rect 10 10 |> filled white |> move (55, 0)
                ,rect 10 10 |> filled white |> move (-55, 0)
                   ]

level4_image t = [circles |> rotate(degrees (10*tan(t/25)))
                 ,shaping |> rotate(degrees (-10*tan(t/25)))]

level5 = { image = level5_image
         , optionA = "BEST PURCHASE"
         , optionB = "PERFECT BUY"
         , optionC = "BEST BUY"
         , optionD = "GOOD BUY"
         , answer = C
         }
body2 = group [rect 60 90 |> filled yellow |> rotate (degrees 90)
              ,rect 60 90 |> outlined (solid 1) black |> rotate (degrees 90)
              ,circle 3 |> filled darkBlue |> move (-35, 0)
              ,circle 3 |> outlined (solid 1) black |> move (-35, 0)
              ]

shaping2 = group [rect 50 35 |> filled darkBlue |> rotate (degrees 35) |> move (-50, 30)
                ,rect 50 35 |> filled darkBlue |> rotate (degrees 145) |> move (-50, -30)
                ,rect 32 1 |> filled black |> rotate (degrees 35) |> move (-32, 21)
                ,rect 32 1 |> filled black |> rotate (degrees 145) |> move (-32, -21)
              ]


level5_image t = [ rect 200 200 |> filled darkBlue
                 ,body2 |> rotate (degrees 355) |> rotate(degrees (10*sin(t/2)))
                 ,shaping2 |> rotate (degrees 355) |> rotate(degrees (10*sin(t/2))) ]

heart c = group [circle 50
            |> filled c
            |> move (0,50)
         ,
          circle 50
            |> filled c
            |> move (50,0)
         ,
          square 100
            |> filled c ] |> rotate (degrees 45)

level6 = { image = level6_image
          , optionA = "Shazam"
          , optionB = "InSync"
          , optionC = "Solitaire"
          , optionD = "Mine Sweeper"
          , answer = B
                     }


rectangleOne t = rect 25 35 |> filled white |> makeTransparent 0.9 |> move (0, 0) |> move ( 5*sin(t/30), 5*sin(t/30) )
rectangleTwo = rect 25 35 |> filled white |> makeTransparent 0.6 |> move (0, 0)
rectangleThree t = rect 25 35 |> filled white |> makeTransparent 0.3 |> move (0, 0) |> move ( -5*sin(t/30), -5*sin(t/30) )

level6_image t = [group[roundedRect 60 60 10 |> filled (rgb 63 181 255)
             ,rectangleOne t
             ,rectangleTwo
             ,rectangleThree t] |> scale 4 |> move (0,50)
             ]

level7 = { image = level7_image
          , optionA = "Droptop"
          , optionB = "Hotpop"
          , optionC = "Hotbox"
          , optionD = "Crockpot"
          , answer = A
                    }

bonus = text "Bonus Round:" |> size 50 |> centered |>filled black |> move (0, 50)
rain  = text "Raindrop" |> size 50 |> centered |>filled black


level7_image t = [group[bonus, rain] |> rotate (degrees (sin t))]
--- UPDATE ---

update msg model = case msg of
                        Tick t _ -> { model | state = if model.state == InGame && model.levels == []
                                                            then EndOfGame else if model.state == InGame && model.timelimit <=0
                                                            then Failure
                                                            else model.state
                                    ,         time = model.time + 1
                                    ,         timelimit =  if model.state == InGame
                                                                then model.timelimit - 0.05
                                                                else model.timelimit
                                    }
                        StartGame -> { model | state = InGame}
                        Instruct -> {model | state = InstructMe}
                        SubmitAnswer ans1 ans2 -> if ans1 == ans2
                                                    then nextLevel model

                                                    else wrongAnswer model

                        Reset -> init

nextLevel model = {model | levels = Maybe.withDefault [] (List.tail model.levels) , time = 0 , timelimit = 10 , score = model.score + 10 + model.timelimit}

wrongAnswer model = case model.chances of
                        0 -> {model | state = Failure}
                        _ -> {model | chances = model.chances - 1 , score = 0}
