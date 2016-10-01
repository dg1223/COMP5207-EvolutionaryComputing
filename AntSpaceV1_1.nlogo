;;;;;;;;;;;;;; Notes:  ;;;;;;;;;;;;;;;;;;;;
;; This version has a different implementation for returning ants than the antSpace-4 model did
;;   in this one, it uses the same algorithm as in the 'AntSpace-3-10min' model.  
;;    this also fixes the problem with ants moving in the wrong direction when turning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


globals [antOutColor distanceTraveled screenArea showAnts? showFood? numAnts numDead clock tanhVal homeX root2  maxPher colonyFood numAtHome fstRun? numPatches amtFood antsHome amtFoodToPlot rand3 ]

breeds [food ants ]

patches-own [foodRandVar food? foodVar totalFood pheromone]
ants-own [ home? leftYet? carrying-food? whenToLeave goingOut? move? C-inv test1 test2 ] ;pher-conc leftProb aheadProb rightProb ]  ;; "...Prob" var's are for prob of movement; C is avg of node concentrations (see sole et al)
food-own [foodQuality]

to Setup
    ca
;    no-display
    set-default-shape ants "ant"

    set screenArea (screen-edge-x * screen-edge-y * 4) ;; The 'screen-edge' command just give the 0 to edge width, so this has to be doubled
                                                       ;;  the 'screen-width' command is not as accurate for some reason
    set showFood? true     ;; 'showFood?' has to start out false, so Food gets hidden when 'ToggleFood' runs
    set showAnts? true     ;; Same reason as 'showFood?' has to be false

    set fstRun? true
    set numDead 0
  ;; Intialize variables
    set root2 1.41421356  ;; This is the distance ants move forward to go to diagonal grid squares (makes the world a Grid)
    set colonyFood 0
    set clock 0
    reset-timer
    set numAtHome 0               ;; Set Number of ants at home
    set antOutColor white

;    set allowMove? false    ;; Set whether the ants make the 'P_m' calculation every step or if they always try and move
;    set amtPherToRemove 0.0025
;    set amtFood 0
;    set amtFoodToPlot 50
;    set emptyNodeWeight 47
;    set amtPherToDrop 100

    ;; Initialize position of colony:
    set homeX 0
  SetupAnts
  SetupPlots
 
end

to TestStuff
create-custom-ants 2 ;; antsPerStep        ;; Creates "antsPerStep" new ants at Home position
[
       set carrying-food? false
       set heading 0
       setxy homeX homeY
       set move? false
       set home? True
       set goingOut? True
       set leftYet? false
       set color antOutColor
] 
   set antsHome (count ants)
   set numAnts count ants

end

to Go
;    no-display
    if clock < stopAt
      [
        if fstRun?
        [reset-timer
         set fstRun? false]
    
        setupAnts
        MoveAnts
        UpdatePatches
    
        set clock (clock + 1)

        ask patch-at homex homey
          [set pcolor red]

        PlotAreaCovered
        PlotWidth
      ]
end


;;;;;;;;;;;Toggle PROCEDURES;;;;;;;;;;;;;

to ToggleFood
    ifelse showFood?
      [ask food [hideturtle] 
        set showFood? false        ]
      [ask food [showturtle]     
        set showFood? true        ]

end

to ToggleAntColor
    ifelse antOutColor = white
      [ set antOutColor black 
        ask ants [set color black ]
      ]
      [ set antOutColor white 
        ask ants [set color white ]
      ]

end

to ToggleAnts

    ifelse showAnts?
      [  ask ants [ht]
         set showAnts? false    ]
      [  ask ants [st]
         set showAnts? true    ]

end


;;;;;;;;;;;SETUP PROCEDURES;;;;;;;;;;;;;



to makeFood
    locals [rand]
    set amtFood (screenArea * (%TotalArea / 100))
    create-food (amtFood)  ;; Makes as many food agents as there are going to be nodes of food
    ask food
      [ set foodQuality patchQuality  ; Gives each food piece an amount
        set amtFood (amtFood + foodQuality)
      ]
    ask food
      [
        set rand random-int-or-float 100
        ifelse rand <= 25      ;; This whole thing distributes the food agents evenly around the world.
            [ set xcor (random-int-or-float screen-size-x)
              set ycor (random-int-or-float screen-size-y)
            ]
            [ ifelse (rand > 25) and (rand <= 50)
                [ set xcor (random-int-or-float screen-size-x) * -1
                  set ycor (random-int-or-float screen-size-y) 
                ]
                [ ifelse (rand > 50) and (rand <= 75)
                    [ set xcor (random-int-or-float screen-size-x) * -1
                      set Ycor (random-int-or-float screen-size-y) * -1 
                    ]
  
                    [ set xcor (random-int-or-float screen-size-x)
                      set Ycor (random-int-or-float screen-size-y) * -1 
                    ]
                ]
            ]
       set color green    ;; Makes sure all food is same color
     ]
;        set amtFood (count food)

end


to cloneFood
    type "Note: you are making "
    type screenArea * (%TotalArea / 100) * patchQuality
    Print " pieces of food.  "
    Print "This may take a looong time (>1 minute)"
    print " but the program probably hasn't crashed.  "
    Print "Try hitting the 'halt' button if you get tired of waiting."
    ask food
      [
        hatch (patchQuality - 1)

        [set color green ]   ;; Makes sure all patches are same color

      ]
    Print "done"
    Print " "
    Print " "
    Print " "
    Print " "

end


;;;;;;;;;;;MISC PROCEDURES;;;;;;;;;;;;;

to UpdatePatches

  ask patches    ;remove Pheromone...
    [
    if (pheromone < 0) [set pheromone 0]        ;; Make sure pheromone stays above zero

    if (pheromone > 0)
        [
        set pheromone (pheromone - amtPherToRemove)
        ifelse showPher?
          [ set pcolor scale-color yellow pheromone 0 (maxPher-Return)]    ; Scales color of pher-Out path between 0 and the Max values for pher-Out
          [ set pcolor black    ]
        ]
    ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Plotting;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to SetupPlots
  set-current-plot "% area covered"
  clear-plot
  set-plot-pen-color blue 
  set-plot-y-range 0 15
  set-plot-x-range 0 300
  auto-plot-on 

  set-current-plot "Swarm Width"
  clear-plot
  set-plot-pen-color blue 
;  set xWidth screen-edge-x * 2
  set-plot-y-range 0 20
  set-plot-x-range 0 350
  auto-plot-on 


end

to PlotAreaCovered
  locals [ amtOfPheromone]

  set-current-plot "% area covered"
  ppd   

  set-plot-pen-color blue
  if pheromone-amt > 0
    [  plot (pheromone-amt / screenArea) * 100  ]


end 

to PlotWidth

  set-current-plot "Swarm Width"
  ppd   
  set-plot-pen-color blue
  plot swarm-width

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Ant Procedures;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to SetupAnts

    ask ants    ;; Kill ants that didn't make it out in the last time step
      [
        if not leftYet? 
          [ die]
      ]

   create-custom-ants antsPerStep ;; antsPerStep        ;; Creates "antsPerStep" new ants at Home position
[
       set carrying-food? false
       set heading 0
       setxy homeX homeY
       set move? false
       set home? True
       set goingOut? True
       set leftYet? false
       set color black
] 
   set antsHome (count ants)
   set numAnts count ants

end


to MoveAnts
  locals [ xPos yPos xHome yHome ]
  ask ants
   [
      if not (leftYet?)              ;; Sends ants on their way
        [   ;set leftYet? true
        ]
      ifelse home?
        [atHome]
   
        [
          ifelse (abs(xcor) > screen-edge-x - 1.5) or (abs(ycor) > screen-edge-y - 1.5)     ;; Kills ants that fall over the edge
            [ die
              print "died!"
               set numDead (numDead + 1)]

            [ifelse carrying-food?  ;If ant has food, it is going home
               [ set color red
                ;; Check to see if ant is back home:
                if distancexy-nowrap homeX homeY < 2
                  [ set xCor homeX
                    set yCor homeY
                    set home? true
                  ]
                PickNextPatchHome      ;; Else, decide where to move to next    
               ]
           ;; Else, check to see if ant is ready to leave nest or already out and then 'LookForFood'
               [ LookForFood]
           ]
      ]
    ]
   

end


to LookForFood ;;Ant Procedure
    if count food-here > 0
    [ ask food-here
        [ ;print "my foodQuality is " 
        ]  ;print FoodQuality ]
    ]


  ifelse (count food-here > 0)
   ;; If ant finds food:
   [ set amtFood amtFood - 1
     ask food-here
       [ set foodQuality (foodQuality - 1)
         if foodQuality < 1
           [ die ]
       ]
     set carrying-food? true     ;; pick up food
     set heading 180             ;; and turn around
     PickNextPatchHome           ;;Start heading home
   ]

   ;; If ant doesn't find food:
   [
     PickNextPatch                                  ;; Decide which patch to go to

   ]  ;; End of 'ifelse' statement

end

to PickNextPatch ;;Ant decides which grid-point to go to next
    locals [oldX newX sumOfProbs localRand ahead scent-left scent-right tanhValue P_m ltProb aheadProb rtProb rand2]    ;; declare local variables
   
    set ahead pher-conc                       ;; Get 'pher-conc' for node directly in front
    rt 45                                     ;; Turn to Front, Right node (node C)
    set scent-right pher-conc
    lt 90
    set scent-left pher-conc
    rt 45                        ;; Turn back to facing straight ahead


  ;; Ant decides if it even wants to move:

  ifelse allowMove?    ;; The 'allowMove" switch decides if ant chooses a P_m for movement or if it just moves at each step
    [
     ;; Sole et al's equation for Prob. of movement (P_m) is [ 1/2 (1 + tanh[(sum of pher. in next 3 patches) / 100) - 1])]
     ;; tanh(x) = (e^x - e^-x) / (e^x + e^-x)
     ;; where e^x => (exp(((scent-left + ahead + scent-right)/50%Prob)-1))

;      ifelse gridShape = 1  ;; (if ants are on a square grid, calculates tanh using 'ahead' scent as well)
;        [ set tanhValue ( ( 1 / 2 ) * ( 1 + ( ( ( exp ( ( ( scent-left + ahead + scent-right ) / FiftyPercProb ) - 1 ) ) - ( 1 / ( exp ( ( ( scent-left + ahead + scent-right ) / FiftyPercProb ) - 1 ) ) ) ) / ( ( exp ( ( ( scent-left + ahead + scent-right ) / FiftyPercProb ) - 1 ) ) + ( 1 / ( exp ( ( ( scent-left + ahead + scent-right ) / FiftyPercProb ) - 1 )))))))]
 ;       [ set tanhValue ( ( 1 / 2 ) * ( 1 + ( ( ( exp ( ( ( scent-left + scent-right ) / FiftyPercProb ) - 1 ) ) - ( 1 / ( exp ( ( ( scent-left + scent-right ) / FiftyPercProb ) - 1 ) ) ) ) / ( ( exp ( ( ( scent-left + scent-right ) / FiftyPercProb ) - 1 ) ) + ( 1 / ( exp ( ( ( scent-left + scent-right ) / FiftyPercProb ) - 1 )))))))]

  ;    set P_m (( (1 + tanhValue) / 2) )


     set oldX ( ( ( scent-left + (ahead * gridShape) + scent-right ) / FiftyPercProb ) - 1 ) ;; Using the "* gridShape) adjusts for if the grid is a diamond (gridshape = 0) or a Square (GS = 1)
     if (oldX > 500)   ;; e^>500 gives an error because the number is too big;  note that this does change the shape of the response curve
         [set oldX 500]
     set newX exp(oldX)
     set tanhValue ( (newX - (1 / newX) ) / (newX + (1 / newX) ) )
     set P_m (1 + tanhValue) / 2




      set localRand ((random-int-or-float 100) / 100)
 
      if (localRand < P_m)
        [ set move? true
          layPheromone  ;; If ant decides to move, it LaysPheromone -- (note: ant lays pher. whether it is able to move or not)
        ]
    ]
    [ set move? true]    ;; If "allowMove?" is false, "Move?" is automatically set to true so ants always move at each step
  ;; end "ifelse allowMove"

   if (move?)  ;; If the ant decides to move, calculate which patch it will move to and move it:
     [;; First calculate movement probabilities:
       ifelse gridShape = 1   ;; i.e. gridShap =1 for Sqaure, 0 for Diamond -- In diamond shape, only Left and Right probabilities are used
         [ 
           set C-inv ( 1 / ( (emptyNodeWeightOut + scent-left) ^ 2 + (emptyNodeWeightOut + ahead) ^ 2 + (emptyNodeWeightOut + scent-right) ^ 2 ) ) ;; Note this is the inverse of the C term in the Sole equation

          ;; Compute probabilities for choosing each node:
             set ltProb (C-inv * (emptyNodeWeightOut + scent-left) ^ 2 )
             set rtProb (C-inv * (emptyNodeWeightOut + scent-right) ^ 2 )
             set aheadProb (1 - ltProb - rtProb)
         ]
         [ 
           set C-inv ( 1 / ( (emptyNodeWeightOut + scent-left) ^ 2 + (emptyNodeWeightOut + scent-right) ^ 2 ) ) ;; (This is the inverse of the C term in the Sole equation)
          ;; Compute probs:
           set ltProb (C-inv * (emptyNodeWeightOut + scent-left) ^ 2 )
           set rtProb (C-inv * (emptyNodeWeightOut + scent-right) ^ 2 )
          ;; Normalize:
           set sumOfProbs ltProb + rtProb
           set ltProb (ltProb / sumOfProbs)
           set rtProb (rtProb / sumOfProbs)
           set aheadProb 0
        ] ;; end of "ifelse gridShape = 1"

       set rand2 ((random-int-or-float 100) / 100)

       ;; Then choose patch to move to if gridshape is Diamond (ant will only choose NW or NE): 
       ifelse gridshape = 0
         [
           ifelse (rand2 < ltProb)                   ;; If left is the chosen direction:
             [ set heading 0
               lt 45
               ;;  Check if square is full: 
               ifelse (count turtles-at dx dy > numAntsPerPatch) 
                 [ rt 90
                   ifelse (count turtles-at dx dy > numAntsPerPatch)
                     [ fd 0]
                     [ jump root2
                       set heading 0 ]
                 ]
                 [ jump root2
                   set heading 0]
                
             ]
            ;; Otherwise go to front right patch
             [
               set heading 0    ;; Point ant in the right direction 
               rt 45
               ;;  Check if square is full: 
               ifelse (count turtles-at dx dy > numAntsPerPatch) 
                 [ lt 90
                   ifelse (count turtles-at dx dy > numAntsPerPatch)
                     [ jump 0]
                     [ jump root2
                       set heading 0 ]
                 ]
                 [ jump root2
                   set heading 0]     ;; Point ant back 'north'
              ]
         ] ;; end movement for "gridShape = 0"

         [ ;; Start movement for "gridshape = 1" (square):
           ifelse (rand2 >= ltProb) and (rand2 <= (ltProb + aheadProb))  ;; If 'ahead' patch is chosen, go forward
             [
               set heading 0        ;; Point 'north'
                 ;;  Check if square is full: 
                 ifelse (count turtles-at dx dy > numAntsPerPatch) 
                   [ jump 0 ]
                   [ jump 1 ]   ;; Move to next square
             ]
             [ ;; If left is the chosen direction:
               ifelse (rand2 < ltProb) 
                 [ set heading 0
                   lt 45
                   ;;  Check if square is full: 
                   ifelse (count turtles-at dx dy > numAntsPerPatch) 
                     [ jump 0 ]
                     [ jump root2
                       set heading 0]
                 ]
                 [;; Otherwise go to front right patch:
                   set heading 0    ;; Point ant in the right direction 
                   rt 45
                   ;;  Check if square is full: 
                   ifelse (count turtles-at dx dy > numAntsPerPatch) 
                     [ jump 0 ]
                     [ jump root2
                       set heading 0]     ;; Point ant back 'north'
                 ]  ;; End of "ifelse (rand2 < ltProb)"
             ]  ;; End of "Ifelse (rand2 >= ltProb) and (rand2 <= (ltProb + aheadProb)) "
       ] ;; End of movement for "gridshape = 1" (square)"
   
    ]  ;; end of "if Move?"

    set move? false  ;; This assures that the ants have to make the move? decision again on the next loop

end

to PickNextPatchHome
    locals [newX oldX ahead scent-left scent-right tanhValue P_m ltProb aheadProb rtProb frst second whichs rand2 sumOfProbs localRandom ]    ;; declare local variables 

    set heading 180
    set ahead pher-conc                       ;; Get 'pher-conc' for node directly in front

    set heading 225                                     ;; Turn to Front, Right node (node C)
    set scent-right pher-conc
    set heading 135
    set scent-left pher-conc
    set heading 180              ;; Turn back to facing straight ahead


;; Ant decides if it even wants to move:
;;     Note, currently I'm running the simulation without having the ants calculate P_m (i.e. if they want to move), 
;;     they just move on each time-step, this makes things run faster.  
;;     I should figure out if it has an impact on the patterns / behaviors produced

  ifelse allowMove?
    [
     ;; Sole et al's equation for Prob. of movement (P_m) is [ 1/2 (1 + tanh[(sum of pher. in next 3 patches) / 100) - 1])]
     ;; tanh(x) = (e^x - e^-x) / (e^x + e^-x)
     ;; where e^x => (exp(((scent-left + ahead + scent-right)/100)-1))
    
;      ifelse gridShape = 1  ;; (if ants are on a square grid, calculates tanh using 'ahead' scent as well)
;        [ set tanhValue ( ( 1 / 2 ) * ( 1 + ( ( ( exp ( ( ( scent-left + ahead + scent-right ) / 100 ) - 1 ) ) - ( 1 / ( exp ( ( ( scent-left + ahead + scent-right ) / 100 ) - 1 ) ) ) ) / ( ( exp ( ( ( scent-left + ahead + scent-right ) / 100 ) - 1 ) ) + ( 1 / ( exp ( ( ( scent-left + ahead + scent-right ) / 100 ) - 1 )))))))]
 ;       [ set tanhValue ( ( 1 / 2 ) * ( 1 + ( ( ( exp ( ( ( scent-left + scent-right ) / 100 ) - 1 ) ) - ( 1 / ( exp ( ( ( scent-left + scent-right ) / 100 ) - 1 ) ) ) ) / ( ( exp ( ( ( scent-left + scent-right ) / 100 ) - 1 ) ) + ( 1 / ( exp ( ( ( scent-left + scent-right ) / 100 ) - 1 )))))))]

;      set P_m (( (1 + tanhValue) / 2) )
 

     set oldX ( ( ( scent-left + (ahead * gridShape) + scent-right ) / FiftyPercProb ) - 1 ) ;; Using the "* gridShape) adjusts for if the grid is a diamond (gridshape = 0) or a Square (GS = 1)
     if (oldX > 500)   ;; e^>500 gives an error because the number is too big;  note that this does change the shape of the response curve
         [set oldX 500]
     set newX exp(oldX)
     set tanhValue ( (newX - (1 / newX) ) / (newX + (1 / newX) ) )
     set P_m (1 + tanhValue) / 2

     set localRandom ((random-int-or-float 100) / 100)

      if (localRandom < P_m)
          [ set move? true
            layPheromone     ;; If ant decides to move, it LaysPheromone -- (note: ant lays pher. whether it is able to move or not)
          ]
    ]
    [ set move? true]  ;; end of "ifelse allowMove"


   if (move?)  ;; If the ant decides to move, calculate which patch it will move to and move it:
     [
      ;; Calculate probability of movement for each patch:
       ifelse gridShape = 1   ;; i.e. gridShape =1 for Sqaure, 0 for Diamond -- In diamond shape, only Left and Right probabilities are used
         [
           set C-inv ( 1 / ( (emptyNodeWeightIn + scent-left) ^ 2 + (emptyNodeWeightIn + ahead) ^ 2 + (emptyNodeWeightIn + scent-right) ^ 2 ) ) ;; Note this is the inverse of the C term in the Sole equation

          ;; Compute probabilities for choosing each node:
           set ltProb (C-inv * (emptyNodeWeightIn + scent-left) ^ 2 )
           set rtProb (C-inv * (emptyNodeWeightIn + scent-right) ^ 2 )
           set aheadProb (1 - ltProb - rtProb)

         ]
         [
           set C-inv ( 1 / ( (emptyNodeWeightIn + scent-left) ^ 2 + (emptyNodeWeightIn + scent-right) ^ 2 ) ) ;; (This is the inverse of the C term in the Sole equation)
          ;; Compute probs:
           set ltProb (C-inv * (emptyNodeWeightIn + scent-left) ^ 2 )
           set rtProb (C-inv * (emptyNodeWeightIn + scent-right) ^ 2 )
          ;; Normalize:
           set sumOfProbs ltProb + rtProb
           set ltProb (ltProb / sumOfProbs)
           set rtProb (rtProb / sumOfProbs)
           set aheadProb 0
         ] ;; end of "ifelse gridShape = 1"

       if requirePher?  ;; The 'requirePher' toggle sets whether or not returning ants will ever go to patches without pheromone
         [
          if scent-left < (amtPherToDrop * .5)    ;; (has to be less than original amount because pher. always evaporates and even new trail will be less than amtToDrop after one step
            [ set ltProb 0 ]
          if scent-right < (amtPherToDrop * .5)
            [ set rtProb 0 ]
          if ahead < (amtPherToDrop * .5)
            [ set aheadProb 0 ]
         ;; Add probabilities for re-normaliation, below:
          ifelse gridShape = 1  
             [ set sumOfProbs (ltProb + rtProb + aheadProb) ]
             [ set sumOfProbs (ltProb + rtProb) ]

         ;; Have to make sure sumOfProbs <> 0 before re-normalization -- (avoids lost ants and divide-by-0 crash)
          ifelse (sumOfProbs = 0) and (xCor < 0)  ;; If there is no pheromone in any direction and ant is to west of colony it moves east
            [  set heading 135
               jump root2
             set heading 180
             set move? false  ]  ;; 'move?' is set to false so ant doesn't move again in regular move loop after this one
            [  ifelse (sumofProbs = 0) and (xCor > 0)  ;; No pheromone with ant to East of colony, ant moves west
               [  set heading 225
                  jump root2
                  set heading 180
                  set move? false  ]
               [  ifelse (sumOfProbs = 0)             ;; No pheromone and ant is due north of colony, ant doesn't move          
                    [ set heading 180
                      jump 1
                      set move? false]
                    [ set ltProb (ltProb / sumOfProbs) ;; If there is pheromone somewhere, renormalize probabilities
                      set rtProb (rtProb / sumOfProbs)
                      set aheadProb (aheadProb / sumOfProbs)]
               ]
            ];; end "ifelse (sumofProbs = 0)"
         ]  ;; end of "if requirePher?"

      set localRandom ((random-int-or-float 100) / 100)

      if move?  ;; This If statement is so the ants that just moved above won't move again
        [ ;; Then choose patch to move to if gridshape is Diamond (ant will only choose NW or NE): 
          ifelse gridshape = 0  ;; (Grid is a diamond)
            [
              ifelse (localRandom < ltProb)                   ;; If left is the chosen direction, move SE:
                [ set heading 135
                 ;;  Check if square is full: 
                  ifelse (count turtles-at dx dy > numAntsPerPatch) 
                    [ ;; If full, check other patch, if that's full, don't move
                      set heading 225
                      ifelse (count turtles-at dx dy > numAntsPerPatch)
                        [ fd 0 ]
                        [ jump root2
                          set heading 180 ]    
                    ]
                    [ jump root2  ;; Else go forward to next grid-point
                      set heading 180]
             ]
            ;; Otherwise choose rtProb and go to SW patch
             [
                 set heading 225    ;; Point ant in the right direction 
                ;;  Check if square is full: 
                  ifelse (count turtles-at dx dy > numAntsPerPatch) 
                    [ ;; If full, check other patch, if that's full, don't move
                      set heading 135
                      ifelse (count turtles-at dx dy > numAntsPerPatch)
                        [ jump 0 ]
                        [ jump root2
                          set heading 180 ]    
                    ]
                   [ jump root2
                     set heading 180]     ;; Point ant back 'north'
               ]
           ] ;; end movement for "gridShape = 0"

           [ ;; Start movement for "gridshape = 1" (square):
             ifelse (localRandom >= ltProb) and (localRandom <= (ltProb + aheadProb))  ;; If 'ahead' patch is chosen, go forward
               [
                 set heading 180        ;; Point 'South'
                  ;;  Check if square is full: 
                   ifelse (count turtles-at dx dy > numAntsPerPatch) 
                     [ fd 0 ]
                     [ jump 1 ]   ;; Move to next square
               ]
               [ ;; If left is the chosen direction:
                 ifelse (localRandom < ltProb) 
                   [ set heading 135
                    ;;  Check if square is full: 
                     ifelse (count turtles-at dx dy > numAntsPerPatch) 
                       [ fd 0 ]
                       [ jump root2
                         set heading 180]
                   ]
                   [;; Otherwise go to front right patch:
                     set heading 225    ;; Point ant in the right direction 
                     ;;  Check if square is full: 
                     ifelse (count turtles-at dx dy > numAntsPerPatch) 
                       [ fd 0 ]
                       [ jump root2
                         set heading 180]     ;; Point ant back 'north'
                   ]  ;; End of "ifelse (localRandom < ltProb)"
               ]  ;; End of "Ifelse localRandom >= ltProb) and (rand2 <= (ltProb + aheadProb)) "
           ] ;; End of movement for "gridshape = 1" (square)"
        ] ;; end of 'ifMove' (second one)
    ]  ;; end of "if Move?" (first one)

    set move? false  ;; This assures that the ants have to make the "move?" decision again on the next loop
end


to LayPheromone
     ifelse (goingOut?)                ;; If ant is going out Increase 'pheromone' and move forward
                        
        [
            if (pheromone < maxPher-Out)
            [set pheromone (pheromone + amtPherToDrop)]
        ]
        [                              ;; Else, increae Pheromone with 'returning' parameters
            if (pheromone < maxPher-Return)  
            [set pheromone (pheromone + 10)] ;; '10' is the amt that Deneubourg et al use for returning ants
        ]

end


to atHome
   set color antOutColor
   set home? false 
   if carrying-Food?
     [ set carrying-Food? false
       set colonyFood (colonyFood + 1)  ]
 
   set heading 0
   set leftYet? true
end 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Reporters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pher-conc  ;; reports what the strength of nest-scent right in front of the ant is
  report pheromone-of patch-at dx dy
end


to-report pheromone-amt
    locals [countPher]
    set countPher 0
    ask patches
      [
        if pheromone > 0
        [ set countPher countPher + 1 ]
      ]

    report countPher

end

to-report max-distance
    report abs((max values-from ants [yCor] ) - homeY)
end

to-report swarm-width
    report (max values-from ants [xCor] + abs(min values-from ants [xCor]) )

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Things to Talk to Fred about   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;    1) Time scale --> If ants move 1 cm / time-step (my assumption), their test of 1000 steps is only ~10 meters at best 
;;        
;;    
;;    3) In Sole et al, if the node is full, the ant doesn't move -- this seems very different than if it spills out to and adjacent node
;;
;;    4) Sole et al's patterns are generated by an unlimited # of ants coming out of the nest, and then falling off the edge of the grid @ 200
;;        -- Does this bias the results somehow?
;;
;;
;;
@#$#@#$#@
GRAPHICS-WINDOW
467
10
660
445
45
100
2.011
1
10
1
1
1
0

CC-WINDOW
5
580
975
675
Command Center

BUTTON
131
365
207
398
GO
GO
T
1
T
OBSERVER
T
NIL

BUTTON
7
365
62
398
Setup
Setup
NIL
1
T
OBSERVER
T
NIL

SLIDER
6
304
179
337
%TotalArea
%TotalArea
0
100
5
1
1
NIL

MONITOR
372
343
450
392
Food
amtFood
1
1

SLIDER
6
148
179
181
amtPherToDrop
amtPherToDrop
0
100
12.0
0.1
1
NIL

SLIDER
179
148
359
181
amtPherToRemove
amtPherToRemove
0
0.5
0.035
0.0050
1
NIL

SLIDER
6
115
179
148
maxPher-Out
maxPher-Out
0
1000
1000
5
1
NIL

SLIDER
179
115
359
148
maxPher-Return
maxPher-Return
0
800
300
5
1
NIL

MONITOR
378
90
436
139
Ant Time
clock
1
1

SLIDER
6
181
179
214
homeY
homeY
-200
100
-84
1
1
NIL

MONITOR
378
139
436
188
Ants
count ants
0
1

MONITOR
378
188
436
237
# dead
numDead
0
1

SLIDER
179
181
359
214
emptyNodeweightOut
emptyNodeweightOut
0.5
100
5.0
0.5
1
NIL

MONITOR
378
42
436
91
Runtime
timer
0
1

MONITOR
366
240
449
289
%FoodFound
(colonyFood / amtFood) * 100
2
1

BUTTON
62
365
130
398
Make Food
MakeFood
NIL
1
T
OBSERVER
T
NIL

SLIDER
179
304
359
337
patchQuality
patchQuality
1
400
400
1
1
NIL

TEXTBOX
4
285
134
303
Food Parameters:

TEXTBOX
4
97
94
115
Ant Parameters

MONITOR
380
397
443
446
colFood
colonyFood
0
1

SLIDER
179
214
359
247
emptyNodeWeightIn
emptyNodeWeightIn
0
100
5.0
0.5
1
NIL

SWITCH
7
397
130
430
allowMove?
allowMove?
0
1
-1000

SWITCH
7
462
130
495
requirePher?
requirePher?
0
1
-1000

SWITCH
7
430
130
463
showPher?
showPher?
0
1
-1000

BUTTON
130
398
207
431
Toggle Ants
ToggleAnts
NIL
1
T
OBSERVER
T
NIL

BUTTON
130
431
207
464
ToggleFood
ToggleFood
NIL
1
T
OBSERVER
T
NIL

SLIDER
6
214
179
247
antsPerStep
antsPerStep
0
100
11
1
1
NIL

SLIDER
25
60
168
93
gridShape
gridShape
0
1
1
1
1
NIL

TEXTBOX
20
76
181
94
Diamond                    Square

TEXTBOX
5
42
95
60
Grid Shape:

SLIDER
182
59
352
92
stopAt
stopAt
0
1500
500
50
1
NIL

SLIDER
179
246
359
279
numAntsPerPatch
numAntsPerPatch
0
100
5
1
1
NIL

PLOT
663
182
913
380
% area covered
 
 
0.0
350.0
0.0
20.0
true
false

TEXTBOX
6
338
363
356
____________________________________________________

MONITOR
860
184
910
233
% area covered
(pheromone-amt / screenArea) * 100
2
1

SLIDER
6
246
179
279
FiftyPercProb
FiftyPercProb
1
500
1
1
1
NIL

MONITOR
916
384
966
433
MaxDist
max-distance
0
1

PLOT
663
382
913
566
Swarm Width
 
 
0.0
863.0
0.0
64.0
true
false

MONITOR
860
384
910
433
Width
swarm-width
0
1

MONITOR
366
288
449
337
World
screen-edge-x * 2
0
1

MONITOR
404
288
461
337
Size
screen-edge-y * 2
0
1

BUTTON
130
462
207
495
antColor
toggleAntColor
NIL
1
T
OBSERVER
T
NIL

@#$#@#$#@
Model instructions/description:

This model reproduces the models of army ant foraging developed by 
Deneubourg et al. (1989. The blind leading the blind: modeling 
chemically mediated army ant raid patterns. J. Insect Behav., 2, 719-725) 
and the extension of this model analyzed by Sole et al in 2000 
(Pattern formation and optimization in army ant raids. Artificial Life, 6(3), 219-226).  

The main structural differences between the Deneubourg et al model 
and the Sole et al model was that in the initial model, the world 
was diamond shaped, that is, at any given point an ant could go 
either to the “Northeast” or to the “Northwest” of its current 
position.  In the Sole et al model, the world was a grid so an 
ant chooses to either go straight ahead (“North”) of its current 
position or NE or NW.  

For further details of previous models and model parameters, see:
http://www.dandelion.org/ant/model/previous.htm

To make it Run, click the 'Setup' button, then 'makeFood' if you want

Sliders: 

maxPher-Out: Maximum amt of pheromone the ants will leave while
heading away from nest, i.e. if there is more than this on a spot they
won't add any themselves. (Sole et al value for E burchelli: 51) -- There 
is a table at the end of Sole with thie nuimbers for each species I'll 
include the e burchelli number for each of the sliders.

maxPher-In: same as above but for returning ants -- note they are leaving 
the same 'type' of pheromone going either way, it is jnust the threshold 
above which they choose not to leave pheromone that changes. (value: 540)

amtPherToDrop: amt of pheromone that gets dropped at each step, if amt of 
pheromone at current node is below threshold (47 in sole model for all 
species of ants). -- Note that 47 is about the same as the going-out 
threshold meaning that after 1 ants has passed a given point on the way 
out, no other ants reinforce the trail until the come back -- seems like 
a pretty bogus value to me.

totalTimeToLeave:  Allocates the timescale overwhich all ants leave the 
next, in seconds, i.e. a value of 120 means that it will take the ants 2 
minutes forall of them to leave (although some may not leave on their 
turn if the first point outside the nest is already full) -- as I 
mentioned in my previous email, to mimic the sole et al model, you can 
set this to 1000 and the numAnts to 10000, meaning that about 10 ants per 
second are released for 1000 timesteps which is how they ran their 
model.  I'm going to fix this so its more like their model, i.e. 10 ants 
are created per time step.

amtPherToRemove: amount of pheromone removed per timestep "(i.e. 
evaporation rate of pheromone) -- can't remember what the sole value was,
but I think it was .005. 

emptyNodeWeightOut/In: How attractive are nodes with no pheromone in them 
(this is the 'u' value in the sole model) - SOle et all use 24 for both 
in and out, but again this seem counter-intuitive.  A returning ant 
should never go somewhere without pheromone, particularly if they are 
leaving huge amouns of pheromone on the way back.  I they do, 1/4/ of the 
time or more they leave strong trails the lead off the world and never 
make it home.  I would think that a returning ant would do everything it 
could to stay on the strongest trail, and in real army ants, when there 
is no pheromone, they stop, freak out and return around until they find a 
trail again, so they are really almost never choosing to go to an empty 
node unless they are at the front of the swarm.

HomeY:  Y-position of the bivouac -- you can leave this where it is 
unless you decide to change the size of the grid -- to do this, you drag 
a little square around some part of the edge of the grid, at it gets 
highlighted.  double click on the edge and a dialog box comes up.  you 
can set the world size to anything you want.  when you hit OK it shrinks 
down.  Then you grab a corner of the world and drag it out to be big 
again.  The pizels scale up, so a small-size 20x20 world and a big 20x20 
world are the same size, but the pixels are bigger and easier to see in 
the big world.  I don't recommend going over 100x100 with a patch size of ~2.

Other buttons:
Don't worry about the "on/off" buttons.  The allowMove? one toggles 
whether or not Probability of Moving (P_m) is caluclated for each ant.  
If off, the ants always decide toe move at each time step.  When on, they 
do the P_m calculation from Sole et al before each step.

The allowPher? off/on switch decides if ants will ever choose a patch 
with no pheromone when they are returning home (I think it does anyway).

%Ants monitor: How many ants are left of the total created.
colFood monitor: amt of food the colony has.

CloneFood:  If you set patch quality to >1, the amt of food created is 
reduced by this amt and then the number of peaces of food at each patch 
are increased by this amt.  In the sole et all model runs, they kept the 
amount of food constant but varied % coverage and patch quality.
  When you hit the 'clone food' button, it increases the amt of food at each 
patch by a number propotional to the patch quality so the same amt of 
food is made as if it had een distributed accross the world at one piece 
of food per patch -- this button takes a long time to work, particularly 
if %food is high

The Procedures tab has the code for the model, the Information tab has notes


CREDITS AND REFERENCES
-----------------------
This model was created and developed by Tim Brown (timb@dandelion.org), with the 
help of Fred Adler (adler@math.utah.edu), as part of a PhD research examining
self-organization and emergent behaviors in army ant swarms.

For more in formation on army ants or on this research, please visit:
http://www.infiniteworld.org/

(c) tim brown, 2003
@#$#@#$#@
default
true
0
Polygon -7566196 true true 150 5 40 250 150 205 260 250

ant
true
0
Polygon -7566196 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7566196 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7566196 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7566196 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7566196 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7566196 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7566196 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7566196 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7566196 true true 249 107 211 147 168 147 168 150 213 150

arrow
true
0
Polygon -7566196 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee
true
0
Polygon -256 true false 151 152 137 77 105 67 89 67 66 74 48 85 36 100 24 116 14 134 0 151 15 167 22 182 40 206 58 220 82 226 105 226 134 222
Polygon -16777216 true false 151 150 149 128 149 114 155 98 178 80 197 80 217 81 233 95 242 117 246 141 247 151 245 177 234 195 218 207 206 211 184 211 161 204 151 189 148 171
Polygon -7566196 true true 246 151 241 119 240 96 250 81 261 78 275 87 282 103 277 115 287 121 299 150 286 180 277 189 283 197 281 210 270 222 256 222 243 212 242 192
Polygon -16777216 true false 115 70 129 74 128 223 114 224
Polygon -16777216 true false 89 67 74 71 74 224 89 225 89 67
Polygon -16777216 true false 43 91 31 106 31 195 45 211
Line -1 false 200 144 213 70
Line -1 false 213 70 213 45
Line -1 false 214 45 203 26
Line -1 false 204 26 185 22
Line -1 false 185 22 170 25
Line -1 false 169 26 159 37
Line -1 false 159 37 156 55
Line -1 false 157 55 199 143
Line -1 false 200 141 162 227
Line -1 false 162 227 163 241
Line -1 false 163 241 171 249
Line -1 false 171 249 190 254
Line -1 false 192 253 203 248
Line -1 false 205 249 218 235
Line -1 false 218 235 200 144

bird1
false
0
Polygon -7566196 true true 2 6 2 39 270 298 297 298 299 271 187 160 279 75 276 22 100 67 31 0

bird2
false
0
Polygon -7566196 true true 2 4 33 4 298 270 298 298 272 298 155 184 117 289 61 295 61 105 0 43

boat1
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6524078 true false 150 32 157 162
Polygon -16776961 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7566196 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7566196 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

boat2
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6524078 true false 150 32 157 162
Polygon -16776961 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7566196 true true 157 54 175 79 174 96 185 102 178 112 194 124 196 131 190 139 192 146 211 151 216 154 157 154
Polygon -7566196 true true 150 74 146 91 139 99 143 114 141 123 137 126 131 129 132 139 142 136 126 142 119 147 148 147

boat3
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6524078 true false 150 32 157 162
Polygon -16776961 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7566196 true true 158 37 172 45 188 59 202 79 217 109 220 130 218 147 204 156 158 156 161 142 170 123 170 102 169 88 165 62
Polygon -7566196 true true 149 66 142 78 139 96 141 111 146 139 148 147 110 147 113 131 118 106 126 71

box
true
0
Polygon -7566196 true true 45 255 255 255 255 45 45 45

butterfly1
true
0
Polygon -16777216 true false 151 76 138 91 138 284 150 296 162 286 162 91
Polygon -7566196 true true 164 106 184 79 205 61 236 48 259 53 279 86 287 119 289 158 278 177 256 182 164 181
Polygon -7566196 true true 136 110 119 82 110 71 85 61 59 48 36 56 17 88 6 115 2 147 15 178 134 178
Polygon -7566196 true true 46 181 28 227 50 255 77 273 112 283 135 274 135 180
Polygon -7566196 true true 165 185 254 184 272 224 255 251 236 267 191 283 164 276
Line -7566196 true 167 47 159 82
Line -7566196 true 136 47 145 81
Circle -7566196 true true 165 45 8
Circle -7566196 true true 134 45 6
Circle -7566196 true true 133 44 7
Circle -7566196 true true 133 43 8

circle
false
0
Circle -7566196 true true 34 34 230

person
false
0
Circle -7566196 true true 155 20 63
Rectangle -7566196 true true 158 79 217 164
Polygon -7566196 true true 158 81 110 129 131 143 158 109 165 110
Polygon -7566196 true true 216 83 267 123 248 143 215 107
Polygon -7566196 true true 167 163 145 234 183 234 183 163
Polygon -7566196 true true 195 163 195 233 227 233 206 159

spacecraft
true
0
Polygon -7566196 true true 150 0 180 135 255 255 225 240 150 180 75 240 45 255 120 135

thin-arrow
true
0
Polygon -7566196 true true 150 0 0 150 120 150 120 293 180 293 180 150 300 150

truck-down
false
0
Polygon -7566196 true true 225 30 225 270 120 270 105 210 60 180 45 30 105 60 105 30
Polygon -8716033 true false 195 75 195 120 240 120 240 75
Polygon -8716033 true false 195 225 195 180 240 180 240 225

truck-left
false
0
Polygon -7566196 true true 120 135 225 135 225 210 75 210 75 165 105 165
Polygon -8716033 true false 90 210 105 225 120 210
Polygon -8716033 true false 180 210 195 225 210 210

truck-right
false
0
Polygon -7566196 true true 180 135 75 135 75 210 225 210 225 165 195 165
Polygon -8716033 true false 210 210 195 225 180 210
Polygon -8716033 true false 120 210 105 225 90 210

turtle
true
0
Polygon -7566196 true true 138 75 162 75 165 105 225 105 225 142 195 135 195 187 225 195 225 225 195 217 195 202 105 202 105 217 75 225 75 195 105 187 105 135 75 142 75 105 135 105

wolf-left
false
3
Polygon -6524078 true true 117 97 91 74 66 74 60 85 36 85 38 92 44 97 62 97 81 117 84 134 92 147 109 152 136 144 174 144 174 103 143 103 134 97
Polygon -6524078 true true 87 80 79 55 76 79
Polygon -6524078 true true 81 75 70 58 73 82
Polygon -6524078 true true 99 131 76 152 76 163 96 182 104 182 109 173 102 167 99 173 87 159 104 140
Polygon -6524078 true true 107 138 107 186 98 190 99 196 112 196 115 190
Polygon -6524078 true true 116 140 114 189 105 137
Rectangle -6524078 true true 109 150 114 192
Rectangle -6524078 true true 111 143 116 191
Polygon -6524078 true true 168 106 184 98 205 98 218 115 218 137 186 164 196 176 195 194 178 195 178 183 188 183 169 164 173 144
Polygon -6524078 true true 207 140 200 163 206 175 207 192 193 189 192 177 198 176 185 150
Polygon -6524078 true true 214 134 203 168 192 148
Polygon -6524078 true true 204 151 203 176 193 148
Polygon -6524078 true true 207 103 221 98 236 101 243 115 243 128 256 142 239 143 233 133 225 115 214 114

wolf-right
false
3
Polygon -6524078 true true 170 127 200 93 231 93 237 103 262 103 261 113 253 119 231 119 215 143 213 160 208 173 189 187 169 190 154 190 126 180 106 171 72 171 73 126 122 126 144 123 159 123
Polygon -6524078 true true 201 99 214 69 215 99
Polygon -6524078 true true 207 98 223 71 220 101
Polygon -6524078 true true 184 172 189 234 203 238 203 246 187 247 180 239 171 180
Polygon -6524078 true true 197 174 204 220 218 224 219 234 201 232 195 225 179 179
Polygon -6524078 true true 78 167 95 187 95 208 79 220 92 234 98 235 100 249 81 246 76 241 61 212 65 195 52 170 45 150 44 128 55 121 69 121 81 135
Polygon -6524078 true true 48 143 58 141
Polygon -6524078 true true 46 136 68 137
Polygon -6524078 true true 45 129 35 142 37 159 53 192 47 210 62 238 80 237
Line -16777216 false 74 237 59 213
Line -16777216 false 59 213 59 212
Line -16777216 false 58 211 67 192
Polygon -6524078 true true 38 138 66 149
Polygon -6524078 true true 46 128 33 120 21 118 11 123 3 138 5 160 13 178 9 192 0 199 20 196 25 179 24 161 25 148 45 140
Polygon -6524078 true true 67 122 96 126 63 144

@#$#@#$#@
NetLogo 2.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
