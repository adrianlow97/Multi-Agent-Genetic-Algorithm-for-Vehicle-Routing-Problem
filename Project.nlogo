extensions[ rnd ]

patches-own [
  chromosome
  fitness
  num-vehicles-represented
]

globals [
  best-fitness
  worst-fitness
  best-chromosome
  best-generation
  locations
  changes-in-best
]

to setup
  clear-all
  reset-ticks
  ;;resizes the world into grid of sqrt-population x sqrt-population patches
  ;;ex: sqrt-population = 10 -> 10x10 grid of 100 patches
  resize-world (sqrt-population * -0.5) (sqrt-population - 1) / 2 (sqrt-population * -0.5) (sqrt-population - 1) / 2
  set locations create-locations
  set changes-in-best 0
  initialize-patches
end

to setup-fixed
  clear-all
  reset-ticks
  resize-world (sqrt-population * -0.5) (sqrt-population - 1) / 2 (sqrt-population * -0.5) (sqrt-population - 1) / 2
  set locations [[0 13 98 81 19 6 92 68 48 13 9] [13 0 66 46 84 41 69 8 23 92 66] [98 66 0 14 46 48 69 29 98 50 81] [81 46 14 0 67 45 46 96 93 65 47] [19 84 46 67 0 30 36 6 83 38 75] [6 41 48 45 30 0 73 9 36 65 79] [92 69 69 46 36 73 0 59 92 38 59] [68 8 29 96 6 9 59 0 48 14 17] [48 23 98 93 83 36 92 48 0 24 12] [13 92 50 65 38 65 38 14 24 0 67] [9 66 81 47 75 79 59 17 12 67 0]]
  set num-locations 10
  initialize-patches
end

to go
  tick
  ask patches [compete]
  ask patches [if random-float 1 < probability-mutation [ifelse use-traditional-mutation = false [mutate][traditional-mutation]]]
  update-globals
  ask patches [color-patches]
  if (ticks = n-generations) [stop]
end

to update-globals
  let temp [fitness] of max-one-of patches [fitness]
  if temp > best-fitness [
    set best-fitness temp
    set best-generation ticks
    set best-chromosome [chromosome] of max-one-of patches [fitness]
    set changes-in-best changes-in-best + 1
  ]
  set worst-fitness [fitness] of min-one-of patches [fitness]
end

;;This function is used in setup and creates the locations needed to travel to
to initialize-patches
  ask patches [
    set chromosome create-chromosome
    set fitness calculate-fitness chromosome
    set num-vehicles-represented count-num-vehicles-represented
  ]
  ;;update-globals
  set best-fitness [fitness] of max-one-of patches [fitness]
  set best-generation ticks
  set best-chromosome [chromosome] of max-one-of patches [fitness]
  set worst-fitness [fitness] of min-one-of patches [fitness]
  ;;color patches based on fitness
  ask patches [color-patches]
end

to color-patches
  ifelse worst-fitness != best-fitness [
    set pcolor scale-color green fitness worst-fitness best-fitness
    ifelse fitness = best-fitness [set plabel-color black set plabel "best"]
    [ifelse fitness = worst-fitness [set plabel-color white set plabel "worst"][set plabel ""]]
  ][ set pcolor blue set plabel "done"]
end

;;creates a list of location coordinates with the index corresponding to the chromosomal representation
to-report create-locations
  let matrix []
  let i 0

  while [i <= num-locations][
    let arr []
    let j 0
    while [j <= num-locations][
      let val 0
      ifelse i = j [ set val 0 ]
      [ ifelse i > j [set val (item i (item j matrix))]
        [ set val ((random 99) + 1) ] ] ;;values 1 to 100
      set arr lput val arr
      set j j + 1
    ]
    set matrix lput arr matrix
    set i i + 1
  ]
  report matrix
end

;;This function is called in setup and creates a chromosome for each strategy
to-report create-chromosome
  let temp shuffle (range 1 ( num-locations + 1 )) ;;randomly shuffle list of locations
  ;;The rest of the code is to insert the return trips into the chromosome
  let returns-to-home find-return-trips
  repeat returns-to-home [ set temp insert-zero temp ]
  report temp
end

to-report insert-zero [ genome ]
  let g genome
  let insert-success false
    let index-range (range 1 (length g))
    let random-index one-of index-range
    while [not insert-success] [
      ;;if no neighboring 0's insert, else look for new space
      ifelse item random-index g != 0 and item ( random-index - 1 ) g != 0 [
        set g insert-item random-index g 0
        set insert-success true
      ]
      [
        ;;if pointed at the end of the list, start from the beginning
        ;;else increment pointer 1 over
        ifelse random-index = ( length g - 1) [ set random-index 1] [ set random-index random-index + 1 ]
      ]
    ]
  report g
end

;;This function is used in create-chromosome
;;it randomly calculates return trips to home simulating multiple-vehicle routes
;;the number of extra vehicles is right-skewed so single vehicle routes are more frequent
to-report find-return-trips
  let returns n-values num-locations [i -> i] ;;ex 3 locations [0, 1, 2]
  let weights reverse n-values num-locations [ i -> i + 1 ] ;; [3, 2, 1]
  ;; https://ccl.northwestern.edu/netlogo/docs/rnd.html#rnd:weighted-one-of-list
  let pairs (map list returns weights)
  report first rnd:weighted-one-of-list pairs [ [p] -> last p ] ;; random number of return trips right-skewed so 0 appears more often than 1
end

to-report count-num-vehicles-represented
  let num 1
  foreach chromosome [ x -> if x = 0 [set num num + 1]]
  report num
end

to-report zero-indexes [ c ]
  let temp []
  let i 1
  while [i < length c] [
    if 0 = (item i c) [ set temp lput i temp ] ;;if finds 0 add index to end of temp
   set i i + 1
  ]
  report temp
end

to-report add-back-zeros [ c zero-locations ]
  let temp c
  foreach zero-locations [ x ->
    set temp insert-item x temp 0
  ]
  report temp
end

;;This function is run by each patch
;;Visits every location in the chromosome then returns home
;;and records total distance traveled as 'fitness'
to-report calculate-fitness [ c ]
  let total 0
  let prev-loc 0
   ;;for each gene in chromosome
  foreach c [ gene ->
      set total total - (item prev-loc (item gene locations))
      set prev-loc gene
    ]
  ;;return home
  set total total - (item prev-loc (item 0 locations))

  report total
end

to-report crossover [g1 g2]
  let o1 []
  let o2 []
  let bit2 0
  let pos1 0
  let pos2 0

  let continue true
  let start-index 0
  while [continue] [
    ;;Step 2 in algorithm
    set bit2 item start-index g2
    set o1 lput bit2 o1 ;;1st bit from parent2 is 1st bit of offspring1

    let continue2 true
    while [continue2] [
      ;;Step 3
      set pos1  position bit2 g1 ;;find position of bit2 in parent1
      set bit2 item pos1 g2 ;;find bit in parent2 at the same position of parent1
      set pos1 position bit2 g1 ;;find position of bit2 in parent1 again
      set bit2 item pos1 g2 ;;find bit in parent2 at same position of parent1
      set o2 lput bit2 o2

      ;;Step 4 in algorithm
      set pos1 position bit2 g1
      ifelse pos1 != start-index [
        set bit2 item pos1 g2
        set o1 lput bit2 o1
      ]
      [set continue2 false]
    ]

    let ciac check-if-algorithm-complete o2 g1
    ifelse ciac = true [set continue false] [set start-index ciac]
  ]

  report list o1 o2
end

;;reports value not in offspring or true if all values are in the offspring
to-report check-if-algorithm-complete [offspring parent]
  foreach parent [x ->
    if position x offspring = false [report position x parent] ;;report value missing
  ]
  report true
end

to mutate
  let insertions (random (num-locations - 2)) + 1 ;;1 to num-locations-1 insertions
  let i 0
  while [ i < insertions ][
    if num-vehicles-represented < num-locations [
      set chromosome insert-zero chromosome
      set num-vehicles-represented count-num-vehicles-represented ;; or just +1
    ]
    set i i + 1
  ]
  set fitness calculate-fitness chromosome
end

to traditional-mutation
  let zeros zero-indexes chromosome
  let c-zero-removed remove 0 chromosome
  let bit-one random length c-zero-removed
  let bit-two random length c-zero-removed
  while [bit-one != bit-two][
   set bit-two random length c-zero-removed
  ]
  let temp item bit-one c-zero-removed
  set c-zero-removed replace-item bit-one c-zero-removed (item bit-two c-zero-removed)
  set c-zero-removed replace-item bit-two c-zero-removed temp
  set chromosome add-back-zeros c-zero-removed zeros
  set fitness calculate-fitness chromosome
end

to compete
  let max-neighbor-fitness [fitness] of max-one-of neighbors4 [fitness]
  if fitness < max-neighbor-fitness [
    let max-neighbor-chromosome [chromosome] of max-one-of neighbors4 [fitness]
    let crossover-results 0
    let zeros zero-indexes max-neighbor-chromosome
    let c-zero-removed remove 0 chromosome
    let c2-zero-removed remove 0 max-neighbor-chromosome
    ifelse random-float 1 < probability-best-parent-first-cross [
      set crossover-results crossover c2-zero-removed c-zero-removed
    ]
    [set crossover-results crossover c-zero-removed c2-zero-removed]
    let os1 add-back-zeros (item 0 crossover-results) zeros
    let os2 add-back-zeros (item 1 crossover-results) zeros
    let os1-fitness calculate-fitness os1
    let os2-fitness calculate-fitness os2
    ifelse os1-fitness > os2-fitness [
      set chromosome os1
      set fitness os1-fitness
    ][
      set chromosome os2
      set fitness os2-fitness
    ]
    set num-vehicles-represented count-num-vehicles-represented
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
520
10
1128
619
-1
-1
30.0
1
10
1
1
1
0
1
1
1
-10
9
-10
9
0
0
1
ticks
30.0

BUTTON
7
16
103
49
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
171
216
204
num-locations
num-locations
2
20
10.0
1
1
NIL
HORIZONTAL

SLIDER
5
134
214
167
sqrt-population
sqrt-population
10
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
3
257
214
290
probability-best-parent-first-cross
probability-best-parent-first-cross
0
1
0.5
0.1
1
NIL
HORIZONTAL

TEXTBOX
7
222
182
254
Probability of using fittest parent as Parent-1 in Crossover
11
0.0
1

SLIDER
4
298
214
331
probability-mutation
probability-mutation
0
0.5
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
115
16
204
49
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
7
56
102
89
NIL
setup-fixed
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
3
336
95
381
Best Fitness
best-fitness
17
1
11

MONITOR
3
390
273
435
NIL
best-chromosome
17
1
11

BUTTON
112
56
204
89
go-forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
95
215
128
n-generations
n-generations
0
1000
500.0
10
1
NIL
HORIZONTAL

MONITOR
100
335
215
380
NIL
best-generation
17
1
11

PLOT
234
15
502
172
Vehicles Per Route
Number of Vehicles
Frequency
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-plot-x-range 1 num-locations + 1\nhistogram [num-vehicles-represented] of patches"

MONITOR
3
444
125
489
Agents w/ Best Fitness
count patches with [fitness = best-fitness]
17
1
11

PLOT
236
186
503
336
Best Fitness Over Time
Generations
Fitness
0.0
10.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 0 [ plot best-fitness ]"

MONITOR
130
445
274
490
New Best Fitness Count
changes-in-best
17
1
11

SWITCH
227
344
415
377
use-traditional-mutation
use-traditional-mutation
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Default" repetitions="100" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-fitness</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-mutation">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Default1000" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-fitness</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-mutation">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vary Best Parent First" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-fitness</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-best-parent-first-cross" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-mutation">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vary Best Parent First Changes" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>changes-in-best</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-best-parent-first-cross" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-mutation">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vary Mutation" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-fitness</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-mutation" first="0.05" step="0.05" last="0.5"/>
  </experiment>
  <experiment name="Vary Mutation Changes" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>changes-in-best</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-mutation" first="0.05" step="0.05" last="0.5"/>
  </experiment>
  <experiment name="Vary Mutation Best Generation" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-generation</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-mutation" first="0.05" step="0.05" last="0.5"/>
  </experiment>
  <experiment name="Find Worst Fitness" repetitions="100" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>worst-fitness</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-mutation">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vary Mutation Traditional" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-fitness</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-mutation" first="0.05" step="0.05" last="0.5"/>
    <enumeratedValueSet variable="use-traditional-mutation">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vary Mutation Changes Traditional" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>changes-in-best</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-mutation" first="0.05" step="0.05" last="0.5"/>
    <enumeratedValueSet variable="use-traditional-mutation">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vary Mutation Best Generation Traditional" repetitions="20" runMetricsEveryStep="true">
    <setup>setup-fixed</setup>
    <go>go</go>
    <metric>best-generation</metric>
    <enumeratedValueSet variable="num-locations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability-best-parent-first-cross">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqrt-population">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="probability-mutation" first="0.05" step="0.05" last="0.5"/>
    <enumeratedValueSet variable="use-traditional-mutation">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
