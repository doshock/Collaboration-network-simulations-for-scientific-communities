globals [
  active-agent
  retire-agent
  explored
  explored-x
  explored-y
  co-inclina-list
  learn-strategy-list
  discipline-list
  discipline-num
  discipline-num-list
  mp-time
  peak-ratio
  recruit-treshould
  peaks
  num-components
  component-count
  component-num
  link-list-45
  mean-co-scope-count
  mean-wei-credit-list
  credit-list
]

turtles-own [
  age
  credit
  coordinates
  d-credit
  co-prob
  co-scope
  co-inclina
  co-threshold-ratio
  learn-degree
  learn-strategy
  ycor-p
  max-links
  work-count
  component-id
  wei-credit
]

patches-own[
  value
]

links-own[
  weight
  max-weight-life
]

to setup
  clear-all
  reset-ticks
  mark-landscape
  start

  set mean-wei-credit-list []
;  create-crisis


end

to go

  set active-agent turtles with [color = red]
  set retire-agent turtles with [color = white]
  let mean-co-scope mean [co-scope] of turtles



  ask active-agent [

    mark-path
    set work-count 0]




  ask active-agent [

    if work-count = 0[
      work]

  ]

;  ask active-agent [
;
;        learn
;
;    if max-links < count my-links[
;    set max-links count my-links]
;  ]



retire


  ;  ask retire-agent [
  ;   set age age + 1
  ;   if age > 80[
  ;      die ]
  ;  ]



;  ask links [
;    if max-weight-life < weight [set max-weight-life weight]
;
;;    if weight > 0 [set weight weight - mean [weight] of links / 10]
;;    if weight <= 0 [die]
;
;
;  ]



  ;count-discipline

  ; 要增加新成员吗？
  ;  let total-credit sum [value] of patches
  ;  if sum [credit] of turtles - recruit-treshould > sum [value] of patches / count active-agent[
  ;
  ;  recruit
  ;    set recruit-treshould sum [credit] of turtles
  ;  ]


;  if ticks = 45 [
;    calculate-com
;    print-link-list
;    ;stop
;  ]



      ask retire-agent[
    die]


  set credit-list []
  foreach sort-on [credit] turtles
  [ the-turtle -> ask the-turtle [
    set credit-list lput credit credit-list ] ]




  if ticks mod climate-change = 0 [
    set mean-wei-credit-list lput precision (mean [co-scope] of turtles) 2 mean-wei-credit-list

    mark-landscape


  ]


;if count turtles > 0 [
;    print-mp-time]


  tick


  if  max [co-scope] of turtles - min [co-scope] of turtles <= 10 [stop]


;  ifelse mean [co-scope] of turtles = mean-co-scope[
;   set  mean-co-scope-count mean-co-scope-count + 1
;  ][
;    set mean-co-scope-count 0
;  ]
;
;  output-print mean-co-scope-count
;
;  if mean-co-scope-count > 200
;  [stop]


  if ticks = 20000
  ;  or peak-ratio = 1
  [ stop ]



end

to print-link-list

  set link-list-45 []
ask turtles [
let a count my-links
set link-list-45 lput a link-list-45
]


end

to mark-landscape

;  let x-cor-p n-values peak-num [ p ->
;    round ( (max-pxcor - (peak-num - 1) * mountain-distance ) / 2
;      + mountain-distance * p) ]
;
;  if max x-cor-p > 200
;  [stop]
;
;  set peaks patches with [member? pxcor x-cor-p and pycor = 100]

  ask patches [
   set pcolor black
   set value 0
  ]


  if distance-mountain = "choose" [

   set peaks nobody

  let new-patch one-of patches

  ask new-patch [
    set peaks (patch-set new-patch peaks) ]

  repeat peak-num - 1 [
  ask one-of patches with [distance new-patch = mountain-distance]
    [set new-patch self
      set peaks (patch-set new-patch peaks)
  ]]]

  if distance-mountain = "random" [

    set peaks n-of peak-num patches

  ]

  ask n-of (peak-value-adjust * peak-num) peaks [
      set value 1]
  ask peaks with [value != 1] [
    set value
      random-float 0.19 + 0.8]

;  ask patches [
;    if distance min-one-of peaks [distance myself] < mountain-radius [
;      set value precision([value] of min-one-of peaks [distance myself]
;        - distance min-one-of peaks [distance myself] / mountain-radius) 2 ] ]

  ask patches [
  let closest-peak min-one-of peaks [distance myself]  ; 预先计算最近的 peak
  let closest-distance distance closest-peak            ; 计算该最近峰的距离
  if closest-distance < mountain-radius [
    set value precision([value] of closest-peak - closest-distance / mountain-radius) 2
  ]
]


;   ; 放大value数值
;  ask patches [
;   let v value
;   set v  (2 * v) ^ 3
;   set value v ]


;  ask patches with [not member? self  peaks][
;    if distance one-of peaks with [value = 1] < mountain-radius [
;      set value precision([value] of one-of peaks with [value = 1]
;        - distance one-of peaks with [value = 1] / mountain-radius) 2 ]
;  if distance one-of peaks with [value < 1] < mountain-radius + 40 [
;      set value precision([value] of one-of peaks with [value < 1]
;        - (distance one-of peaks with [value < 1] ) / (mountain-radius + 40)) 2 ]]




;此处最大值需改为上述放大数值
  ask patches [
    if value > 0[
      set pcolor scale-color green value 0 1] ]

  ;    if landspace? [
  ;    ask patches [
  ;      set pcolor black]]



end


;let p-k peak-num
;let x-cor-p n-values peak-num [ p -> (p + 1) * max-pxcor / (peak-num + 2) ]
;  ask patch [ i -> i] x-cor-p 100 [
;  set pcolor green]






to start


  set-default-shape turtles "person"

  set explored []

  create-turtles initial-scientists [
    set credit 1
    setxy random-xcor random-ycor
    set age 20 ;(random 31) + 20
    set color red
    set coordinates []

    primal-strategy-distribute

  ]

  set mp-time [0 0 0]

  set recruit-treshould 0

end

to create-crisis

  ;  ask n-of (0.5 * count patches) patches   [
  ;   if random-float 1 < pycor /  max-pycor / 50
  ;    [ set pcolor white ]  ]

end

to mark-path

  set coordinates lput (list xcor ycor) coordinates
  ask patch-here[
      set pcolor yellow ]

;  set explored remove-duplicates (sentence explored coordinates)
;  set explored-x remove-duplicates ( map [i -> item 0 i ] explored )
;  set explored-y remove-duplicates (map [i -> item 1 i ] explored )


;  foreach explored [
;    [xy] -> ask patch (item 0 xy) (item 1 xy) [
;      set pcolor yellow ]]


end



to work

  let potential-pal-a other active-agent in-radius co-scope

  let turtle-t turtle-set self
  repeat 1 [
    set turtle-t (turtle-set turtle-t [link-neighbors] of turtle-t with [color = red])]
  let potential-pal-b other turtle-t

  let potential-pal-c turtle-set nobody
  if count my-links with [not hidden?] > 0[
    let max-link max-one-of my-links with [not hidden?] [weight]
    ask max-link [set potential-pal-c both-ends] ]
  set potential-pal-c other potential-pal-c

  set d-credit 0

  if co-inclina = "equal" [
    let potential-pal
    ;       potential-pal-a
    ( turtle-set potential-pal-a potential-pal-b)
    ifelse count potential-pal > 0
    [ let partner one-of potential-pal
      collaborate partner ]
    [no-collaborate] ]

  if co-inclina = "community" [
    ifelse random-float 1 < ((count potential-pal-b) / (count potential-pal-a + count potential-pal-b + 1))
      [let potential-pal ( turtle-set potential-pal-b)
        ifelse count potential-pal > 0
        [ let partner one-of potential-pal
          collaborate partner ]
        [no-collaborate] ]
    [let potential-pal ( turtle-set potential-pal-a potential-pal-b)
      ifelse count potential-pal > 0
      [ let partner one-of potential-pal
        collaborate partner ]
      [no-collaborate] ]  ]

  if co-inclina = "friend" [
    ifelse random-float 1 < ((count potential-pal-b) / (count potential-pal-a + count potential-pal-b + 1))
    [  let max-weight max [weight] of my-links
      let link-weight sum [weight] of my-links
      ifelse random-float 1 < (max-weight / (link-weight + 1))
      [  let potential-pal ( turtle-set potential-pal-c)
        ifelse count potential-pal > 0
        [ let partner one-of potential-pal
          collaborate partner ]
        [no-collaborate] ]
      [let potential-pal ( turtle-set potential-pal-b)
        ifelse count potential-pal > 0
        [ let partner one-of potential-pal
          collaborate partner ]
        [no-collaborate] ] ]
    [let potential-pal ( turtle-set potential-pal-a potential-pal-b)
      ifelse count potential-pal > 0
      [ let partner one-of potential-pal
        collaborate partner ]
      [no-collaborate] ]

  ]

end

to learn

  let potential-learn-a turtles in-radius co-scope

  let turtle-t turtle-set self
  repeat 2 [
    set turtle-t (turtle-set turtle-t [link-neighbors] of turtle-t)]
  let potential-learn-b other turtle-t

  let potential-learn ( turtle-set potential-learn-a potential-learn-b)

  if learn-strategy = "conservative"
  [
    ;    if learn-degree > 0.01 [
    ;    set learn-degree learn-degree - 0.01 ]
    ;   if co-scope > 1 [
    ;      set co-scope co-scope - 1 ]
    ;   if co-prob > 0.11[
    ;    set co-prob co-prob - 0.01]

    if random-float 1 < learn-degree and learn-degree < 0.66 and learn-degree > 0.33
        [let learn-strategy-counts map [i -> count turtles in-radius co-scope with [learn-strategy = i]] learn-strategy-list
          let max-count-learn-strategy max learn-strategy-counts
          let max-learn-strategy position max-count-learn-strategy learn-strategy-counts
          set learn-strategy item max-learn-strategy learn-strategy-list  ]

    if random-float 1 < learn-degree and learn-degree > 0.66
        [set learn-strategy [learn-strategy] of max-one-of potential-learn [credit]
    ]

  ]

  if learn-strategy = "paradigmical"
  [
    set learn-degree precision (learn-degree + learn-degree * (mean [learn-degree] of turtles in-radius co-scope - learn-degree)) 2
    set co-threshold-ratio precision (co-threshold-ratio + learn-degree * (mean [co-threshold-ratio] of turtles in-radius co-scope - co-threshold-ratio)) 2
    set co-prob precision (co-prob + learn-degree * (mean [co-prob] of turtles in-radius co-scope - co-prob)) 2

    if random-float 1 < learn-degree
    [  let co-inclina-counts map [i -> count turtles in-radius co-scope with [co-inclina = i]] co-inclina-list
      let max-count-co-inclina max co-inclina-counts
      let max-co-inclina position max-count-co-inclina co-inclina-counts
      set co-inclina item max-co-inclina co-inclina-list  ]

    if random-float 1 < learn-degree
    [  let learn-strategy-counts map [i -> count turtles in-radius co-scope with [learn-strategy = i]] learn-strategy-list
      let max-count-learn-strategy max learn-strategy-counts
      let max-learn-strategy position max-count-learn-strategy learn-strategy-counts
      set learn-strategy item max-learn-strategy learn-strategy-list  ]

    set co-scope round (co-scope + learn-degree * (mean [co-scope] of turtles in-radius co-scope - co-scope))

  ]

  if learn-strategy = "radical"
  [let target-learn max-one-of potential-learn [credit]
    set co-scope round (co-scope + learn-degree * ([co-scope] of target-learn - co-scope))
    set co-threshold-ratio precision (co-threshold-ratio + learn-degree * ([co-threshold-ratio] of target-learn - co-threshold-ratio)) 2
    set co-prob precision (co-prob + learn-degree * ([co-prob] of target-learn - co-prob)) 2
    if random-float 1 < learn-degree [set co-inclina [co-inclina] of target-learn ]
    if random-float 1 < learn-degree [set learn-strategy [learn-strategy] of target-learn ]
    set learn-degree precision (learn-degree + learn-degree * ([learn-degree] of target-learn - learn-degree)) 2
  ]

end

to recruit





  ask one-of max-n-of round (count active-agent / 3 ) active-agent [credit] [
    hatch 1 [
      set credit 1 + [value] of patch xcor ycor
      set age 0 ;(precision (random-float 30) 0) + 20
      set coordinates []

  ] ]

  ;  ask one-of active-agent [hatch random (count active-agent / 40 ) + 1 [
  ;    set credit 1 + [value] of patch xcor ycor
  ;    set age (precision (random-float 30) 0) + 20
  ;    set coordinates []
  ;    set xcor round (mean [xcor] of turtles in-radius co-scope)
  ;    set ycor round (mean [ycor] of turtles in-radius co-scope)
  ;
  ;    recruit-strategy-distribute
  ;  ]]


end

to recruit-strategy-distribute

  distribute-co-scope
  distribute-learn-degree
  distribute-co-prob
  distribute-co-threshold-ratio

  ifelse random-float 1 < 0.5
  [ distribute-co-inclina
    distribute-learn-strategy
  ][

    if random-float 1 < learn-degree
    [  let co-inclina-counts map [i -> count turtles in-radius co-scope with [co-inclina = i]] co-inclina-list
      let max-count-co-inclina max co-inclina-counts
      let max-co-inclina position max-count-co-inclina co-inclina-counts
      set co-inclina item max-co-inclina co-inclina-list  ]
    if random-float 1 < learn-degree
    [  let learn-strategy-counts map [i -> count turtles in-radius co-scope with [learn-strategy = i]] learn-strategy-list
      let max-count-learn-strategy max learn-strategy-counts
      let max-learn-strategy position max-count-learn-strategy learn-strategy-counts
      set learn-strategy item max-learn-strategy learn-strategy-list  ]]



end



to no-collaborate

  if roam-type = "roam" [
    roam]
  if roam-type = "roam-space"[
    roam-space]
  if roam-type = "mix" [
    ifelse random-float 1 < roam-pro [
      roam ][
      roam-space]]


end

to collaborate [partner]

  ifelse random-float 1 < co-prob

  [ ifelse random-float 1 < [co-prob] of partner
    [;let co-threshold (count active-agent in-radius co-scope + sum [weight] of my-links ) * co-threshold-ratio

      ifelse link-with partner = nobody
      ;or [weight] of link-with partner < co-threshold

      [;let distance-pal distance partner
        let co-prob-distance min (list ((abs (xcor - [xcor] of partner)) / (co-scope)) ((abs (ycor - [ycor] of partner)) / (co-scope) ))
        ifelse random-float 1 >= co-prob-distance *  sqrt 2
        [game partner]
        [collaborate-failed  ]]
      [game partner]]
    [collaborate-failed]

  ][
    no-collaborate
  ]

end

to game [partner]


  ifelse link-with partner != nobody

  [
    if roam-type = "roam" [
      p-roam partner]
    if roam-type = "roam-space"[
      p-roam-space partner]
    if roam-type = "mix" [
      ifelse random-float 1 < roam-pro [
        p-roam partner ][
        p-roam-space partner]]

    ifelse d-credit = 0 or [d-credit] of partner = 0[
      collaborate-failed][
      ask link-with partner [set weight weight + ( [d-credit] of myself + [d-credit] of partner ) ]  ]  ]


  [ if roam-type = "roam" [
    p-roam partner]
    if roam-type = "roam-space"[
      p-roam-space partner]
    if roam-type = "mix" [
      ifelse random-float 1 < roam-pro [
        p-roam partner ][
        p-roam-space partner]]

    ifelse d-credit = 0 or [d-credit] of partner = 0[
      collaborate-failed][
      create-link-with partner
      ask link-with partner [set weight ( [d-credit] of myself + [d-credit] of partner )
        set max-weight-life weight]  ]]



end

to roam

  ;  ifelse not landspace? [

  let credit0 credit


;  let solo-direction random 4
;  if solo-direction = 0 [
;    if not member? list xcor (ycor + 1) coordinates and [value] of patch xcor (ycor + 1) >= [value] of patch xcor ycor[
;      set heading 0
;      fd 1]]
;  if solo-direction = 1 [
;    if not member? list (xcor + 1) ycor coordinates and [value] of patch (xcor + 1) ycor >= [value] of patch xcor ycor[
;      set heading 90
;      fd 1]]
;  if solo-direction = 2 [
;    if not member? list xcor (ycor - 1) coordinates and [value] of patch xcor (ycor - 1) >= [value] of patch xcor ycor[
;      set heading 180
;      fd 1]]
;  if solo-direction = 3 [
;    if not member? list (xcor - 1) ycor coordinates and [value] of patch (xcor - 1) ycor  >= [value] of patch xcor ycor[
;      set heading 270
;      fd 1]]

  let move-target max-one-of neighbors4 [value]
  if [value] of move-target  >= [value] of patch xcor ycor[
    move-to move-target]


  set d-credit [value] of patch xcor ycor

  set credit credit + d-credit

  ;]  [
  ;
  ;    roam-space
  ;
  ;   calculate-credit
  ;  ]

  set work-count 1
end

to roam-space

  let p-here patch-here
  let move-target-1 nobody
  let move-target-2 nobody

  set move-target-1 one-of neighbors4 with [pcolor != yellow and value >= [value] of p-here and count turtles-here = 0] ;目标value更高且无人占据

  ifelse move-target-1 != nobody [
    move-to move-target-1][
    set move-target-2 one-of neighbors4 with [pcolor != yellow and count turtles-here = 0 ]
     if move-target-2 != nobody[
        move-to move-target-2]
    ]




;  set move-target-1 one-of neighbors4 with [pcolor != yellow and value >= [value] of p-here and count turtles-here = 0] ;目标value更高且无人占据
;
;  ifelse move-target-1 != nobody [
;   move-to move-target-1][
;
;  ifelse credit > 1 [
;    set move-target-2 max-one-of patches in-radius
;    ;    co-scope
;    (credit / max [credit] of turtles * co-scope)
;    with [free-neighbor > 0
;    and pcolor = yellow] [value]                             ;目标value是有空间可占据的中最大的
;  ][
;    set move-target-2 max-one-of patches in-radius 1 with [free-neighbor > 0] [value]  ]
;
;  if move-target-2 != nobody [
;    ask move-target-2 [ set move-tarfet-final max-one-of neighbors with [pcolor != yellow and count turtles-here = 0 ][value]]  ]
;
;  if move-tarfet-final != nobody [
;      move-to move-tarfet-final  ]]


  ;  ifelse move-target != nobody [
  ;    move-to move-target][
  ;    set move-target one-of patches in-radius co-scope with [free-neighbor > 0 and value > [value] of p-here]
  ;    ifelse move-target != nobody[
  ;      ask move-target [ set move-tarfet-final neighbors with [pcolor != yellow]]][
  ;      set move-target one-of patches in-radius co-scope with [free-neighbor > 0]
  ;      if move-target != nobody [
  ;        ask move-target [ set move-tarfet-final neighbors with [pcolor != yellow]]
  ;      ]]
  ;      if move-tarfet-final != nobody [
  ;      move-to one-of move-tarfet-final]]

  ;  let move-target max-one-of patches in-radius co-scope with [free-neighbor > 0] [ value]
  ;  if move-target != nobody and [value] of move-target > [value] of patch-here [
  ;    ifelse [pcolor] of move-target = yellow [
  ;      move-to one-of move-target
  ;    ][ask move-target [ set move-tarfet-final neighbors with [pcolor != yellow]]
  ;      if any?  move-tarfet-final [
  ;        move-to one-of move-tarfet-final]]]



  calculate-credit

  set work-count 1
end

to p-roam [partner]

  let my-original patch-here
  let pa-original nobody
  ask partner [
    set pa-original patch-here]

  ifelse credit = [credit] of partner [
    no-collaborate][


    ifelse [value] of my-original < [value] of pa-original [
      move-to  pa-original
      set d-credit [value] of patch xcor ycor
      set credit credit + d-credit
      set work-count 1

      ask partner [
        roam]

    ][ roam
      ask partner [
        move-to my-original
        set d-credit [value] of patch xcor ycor
        set credit credit + d-credit
        set work-count 1
    ]]




  ]




;  ; ifelse not landspace? [
;
;  let credit0 credit
;  let credit1 [credit] of partner
;
;
;  let new-xcor precision ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner ))) 0
;  let new-ycor precision ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner ))) 0
;
;  ifelse [value] of patch new-xcor new-ycor > [value] of patch xcor ycor[
;    set xcor new-xcor
;    set ycor new-ycor
;
;    set d-credit [value] of patch xcor ycor
;    set credit d-credit + credit0
;  ][
;    roam]
;
;  ask partner [
;    ifelse [value] of patch new-xcor new-ycor > [value] of patch xcor ycor[
;      set xcor new-xcor
;      set ycor new-ycor
;
;      set d-credit [value] of patch xcor ycor
;      set credit d-credit + credit1  ][
;      roam]]

  ;  ]
  ;  [
  ;
  ;    p-roam-space partner
  ;
  ;    calculate-credit
  ;  ]

end

to p-roam-space [partner]

  let my-original patch-here
  let pa-original nobody
  ask partner [
    set pa-original patch-here]

  ifelse credit = [credit] of partner [
    no-collaborate][

    ifelse [value] of my-original < [value] of pa-original [

      while [([pcolor] of patch-here = yellow or count turtles-here > 1 ) and distance partner > 1] [
        face partner
        fd 1]


      ifelse distance partner <= 1[
        move-to my-original
        roam-space
      ][
        move-to patch-here
      calculate-credit
      set work-count 1]

      ask partner [
        roam-space]

    ][

      ask partner [

        while [([pcolor] of patch-here = yellow or count turtles-here > 1 ) and distance myself > 1] [
        face myself
        fd 1]


      ifelse distance myself <= 1[
        move-to pa-original
        roam-space
      ][
        move-to patch-here
      calculate-credit
      set work-count 1]


      ]



       roam-space
      ]







  ]

;   let middel-set (patch-set patch ceiling ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner ))  )
;                                   floor ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner ))  )
;                             patch floor ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner ))  )
;                                   ceiling ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner ))  ) )
;
;  let middle-point one-of middel-set
;
;  let move-tarfet-final nobody
;  let move-target-1 nobody
;  let move-target-2 nobody
;
;  ifelse [pcolor] of middle-point != yellow [
;    ifelse credit > 1 [
;      if [value] of middle-point > 0 [
;      ask middle-point [ set move-tarfet-final self]]
;    ][
;      ask middle-point [ set move-tarfet-final self]]
;      ;max-one-of neighbors with [pcolor != yellow and count turtles-here = 0 ][value]]
;  ][
;
;  ifelse credit > 1 [
;
;   set move-target-2 max-one-of patches with  [free-neighbor > 0 and pcolor = yellow and distance middle-point <
;;      [co-scope] of myself ] [value]   ;研究视野不受credit影响
;    (([credit] of myself  + [credit] of partner) / (2 * max [credit] of turtles) * ([co-scope] of myself + [co-scope] of partner)) / 2  ] [value]        ;目标value是有空间可占据的中最大的
;  ][
;   set move-target-2 max-one-of patches with  [free-neighbor > 0 and pcolor = yellow and distance middle-point <= 1 ] [value]
;  ]
;
;  if move-target-2 != nobody [
;      ask move-target-2 [ set move-tarfet-final max-one-of neighbors with [pcolor != yellow and count turtles-here = 0 ][value]]  ]]
;
;
;  let move-x 0
;  let move-y 0
;
;  if move-tarfet-final != nobody [
;
;    ask move-tarfet-final [
;      set move-x [pxcor] of move-tarfet-final
;      set move-y [pycor] of move-tarfet-final]
;
;;    move-to move-tarfet-final
;    setxy move-x move-y
;
;
;    p-calculate-credit
;
;  ask partner [
;     setxy move-x move-y
;;      move-to move-tarfet-final
;      p-calculate-credit]]
;




  ;  let solo-direction random 4
  ;
  ;  if solo-direction = 0 [
  ;
  ;    let new-xcor precision ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner )) + 1 ) 0
  ;    let new-ycor precision ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner )) + 1 ) 0
  ;    let new-xcor-p new-xcor - 2
  ;    let new-ycor-p new-ycor - 2
  ;
  ;    while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
  ;      or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
  ;      or [pcolor] of patch new-xcor new-ycor = yellow
  ;    ] [
  ;      ifelse random-float 1 < 0.5 [
  ;        set new-xcor new-xcor + 1][
  ;        set new-ycor new-ycor + 1]]
  ;    set xcor new-xcor
  ;    set ycor new-ycor
  ;
  ;    ask partner [
  ;      while [any? other turtles with [xcor = new-xcor-p and ycor = new-ycor-p]
  ;        or [pcolor] of patches with [pxcor = new-xcor-p and pycor = new-ycor-p] = white
  ;        or [pcolor] of patch new-xcor-p new-ycor-p = yellow
  ;      ] [
  ;        ifelse random-float 1 < 0.5 [
  ;          set new-xcor-p new-xcor-p - 1][
  ;          set new-ycor-p new-ycor-p - 1]]
  ;      set xcor new-xcor-p
  ;      set ycor new-ycor-p  ]
  ;  ]
  ;
  ;  if solo-direction = 1 [
  ;    let new-xcor precision ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner )) - 1 ) 0
  ;    let new-ycor precision ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner )) - 1 ) 0
  ;    let new-xcor-p new-xcor + 2
  ;    let new-ycor-p new-ycor + 2
  ;
  ;    while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
  ;      or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
  ;      or [pcolor] of patch new-xcor new-ycor = yellow
  ;    ] [
  ;      ifelse random-float 1 < 0.5 [
  ;        set new-xcor new-xcor - 1][
  ;        set new-ycor new-ycor - 1]]
  ;    set xcor new-xcor
  ;    set ycor new-ycor
  ;
  ;    ask partner [
  ;      while [any? other turtles with [xcor = new-xcor-p and ycor = new-ycor-p]
  ;        or [pcolor] of patches with [pxcor = new-xcor-p and pycor = new-ycor-p] = white
  ;        or [pcolor] of patch new-xcor-p new-ycor-p = yellow
  ;      ] [
  ;        ifelse random-float 1 < 0.5 [
  ;          set new-xcor-p new-xcor-p + 1][
  ;          set new-ycor-p new-ycor-p + 1]]
  ;      set xcor new-xcor-p
  ;      set ycor new-ycor-p  ]
  ;  ]
  ;
  ;  if solo-direction = 2 [
  ;
  ;    let new-xcor precision ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner )) + 1 ) 0
  ;    let new-ycor precision ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner )) - 1 ) 0
  ;    let new-xcor-p new-xcor - 2
  ;    let new-ycor-p new-ycor + 2
  ;
  ;    while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
  ;      or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
  ;      or [pcolor] of patch new-xcor new-ycor = yellow
  ;    ] [
  ;      ifelse random-float 1 < 0.5 [
  ;        set new-xcor new-xcor + 1][
  ;        set new-ycor new-ycor - 1]]
  ;    set xcor new-xcor
  ;    set ycor new-ycor
  ;
  ;    ask partner [
  ;      while [any? other turtles with [xcor = new-xcor-p and ycor = new-ycor-p]
  ;        or [pcolor] of patches with [pxcor = new-xcor-p and pycor = new-ycor-p] = white
  ;        or [pcolor] of patch new-xcor-p new-ycor-p = yellow
  ;      ] [
  ;        ifelse random-float 1 < 0.5 [
  ;          set new-xcor-p new-xcor-p - 1][
  ;          set new-ycor-p new-ycor-p + 1]]
  ;      set xcor new-xcor-p
  ;      set ycor new-ycor-p  ]
  ;  ]
  ;
  ;  if solo-direction = 3 [
  ;
  ;    let new-xcor precision ((xcor * credit / ( credit + [credit] of partner ) +  [xcor] of partner * [credit] of partner / ( credit + [credit] of partner )) - 1 ) 0
  ;    let new-ycor precision ((ycor * credit / ( credit + [credit] of partner ) +  [ycor] of partner * [credit] of partner / ( credit + [credit] of partner )) + 1 ) 0
  ;    let new-xcor-p new-xcor + 2
  ;    let new-ycor-p new-ycor - 2
  ;
  ;    while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
  ;      or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
  ;      or [pcolor] of patch new-xcor new-ycor = yellow
  ;    ] [
  ;      ifelse random-float 1 < 0.5 [
  ;        set new-xcor new-xcor - 1][
  ;        set new-ycor new-ycor + 1]]
  ;    set xcor new-xcor
  ;    set ycor new-ycor
  ;
  ;    ask partner [
  ;      while [any? other turtles with [xcor = new-xcor-p and ycor = new-ycor-p]
  ;        or [pcolor] of patches with [pxcor = new-xcor-p and pycor = new-ycor-p] = white
  ;        or [pcolor] of patch new-xcor-p new-ycor-p = yellow
  ;      ] [
  ;        ifelse random-float 1 < 0.5 [
  ;          set new-xcor-p new-xcor-p + 1][
  ;          set new-ycor-p new-ycor-p - 1]]
  ;      set xcor new-xcor-p
  ;      set ycor new-ycor-p  ]
  ;  ]
  ;
  ;  calculate-credit
  ;
  ;  ask partner [
  ;    calculate-credit]

end

to calculate-credit





    set d-credit 0

    let listself (list xcor ycor)

    if member? listself explored [
      set credit credit + d-credit
    ]

    if not member? listself explored[
      set d-credit [value] of patch xcor ycor
      set credit credit + d-credit]

    ;    let sorted-list-x remove-duplicates ( sort-by < ( map  [[x] -> abs(x - xcor)] explored-x ) )
    ;    let d-credit-x item 0 sorted-list-x
    ;    let d-credit-y ycor - ycor-p
    ;    set d-credit d-credit-x + d-credit-y
    ;    set credit credit + d-credit



end

to p-calculate-credit



  set d-credit 0

  let listself (list xcor ycor)

  if member? listself explored [
    set d-credit 0
    set credit credit + d-credit
  ]

  if not member? listself explored[
    set d-credit ([value] of patch xcor ycor) / 2
    set credit credit + d-credit

    ;    let sorted-list-x remove-duplicates ( sort-by < ( map  [[x] -> abs(x - xcor)] explored-x ) )
    ;    let d-credit-x item 0 sorted-list-x
    ;    let d-credit-y ycor - ycor-p
    ;    set d-credit d-credit-x + d-credit-y
    ;    set credit credit + d-credit
  ]


end

to collaborate-failed

  set work-count 1

end


to primal-strategy-distribute
  distribute-co-prob
  distribute-co-scope
  distribute-co-inclina
  distribute-learn-degree
  distribute-learn-strategy
  distribute-co-threshold-ratio

end

to distribute-co-prob
  if co-prob-choice = "choose" [set co-prob co-prob-setting]
  if co-prob-choice = "random" [set co-prob precision (random-float 1) 2]
end

to distribute-co-scope
  if co-scope-choice = "input" [set co-scope co-scope-setting]
  if co-scope-choice = "random" [set co-scope (random max-scope)  + 1
  ]

  ; ( round (max-pxcor / count turtles) * 10 ) + 1        ( round ( random (max-pxcor / 10)  * max-pxcor /  (max-pxcor / 10 ) ) ) + 1    ( round (max-pxcor / 4 + max-pxcor / 16 )) + 1

end

to distribute-co-inclina
  if co-inclina-choice = "choose" [
    set co-inclina co-inclina-setting
    set co-inclina-list (list co-inclina-setting)]

  if co-inclina-choice = "random" [
    let co-inclina-figure random 3
    if co-inclina-figure = 0 [set co-inclina "equal" ]
    if co-inclina-figure = 1 [set co-inclina "community" ]
    if co-inclina-figure = 2 [set co-inclina "friend" ]
    set co-inclina-list ["equal" "community" "friend" ]  ]

end

to distribute-co-threshold-ratio
  if co-threshold-ratio-choice = "choose" [set co-threshold-ratio co-threshold-ratio-setting]
  if co-threshold-ratio-choice = "random" [set co-threshold-ratio precision (random-float 0.3) 2]

end

to distribute-learn-degree
  if learn-degree-choice = "choose" [set learn-degree learn-degree-setting]
  if learn-degree-choice = "random" [set learn-degree precision (random-float 1) 2]

end

to distribute-learn-strategy
  if learn-strategy-choice = "choose" [
    set learn-strategy learn-strategy-setting
    set learn-strategy-list (list learn-strategy-setting)]

  if learn-strategy-choice = "random" [
    let learn-strategy-figure random 3
    if learn-strategy-figure = 0 [set learn-strategy "conservative" ]
    if learn-strategy-figure = 1 [set learn-strategy "paradigmical" ]
    if learn-strategy-figure = 2 [set learn-strategy "radical" ]
    set learn-strategy-list ["conservative" "paradigmical" "radical"]  ]

end

to count-discipline

  let d1 count active-agent with [xcor >= 0 and xcor < 10]
  let d2 count active-agent with [xcor >= 10 and xcor < 20]
  let d3 count active-agent with [xcor >= 20 and xcor < 30]
  let d4 count active-agent with [xcor >= 30 and xcor < 40]
  let d5 count active-agent with [xcor >= 40 and xcor < 50]
  let d6 count active-agent with [xcor >= 50 and xcor < 60]
  let d7 count active-agent with [xcor >= 60 and xcor < 70]
  let d8 count active-agent with [xcor >= 70 and xcor < 80]
  let d9 count active-agent with [xcor >= 80 and xcor < 90]
  let d10 count active-agent with [xcor >= 90 and xcor < 100]
  let d11 count active-agent with [xcor >= 100 and xcor < 110]
  let d12 count active-agent with [xcor >= 110 and xcor < 120]
  let d13 count active-agent with [xcor >= 120 and xcor < 130]
  let d14 count active-agent with [xcor >= 130 and xcor < 140]
  let d15 count active-agent with [xcor >= 140 and xcor < 150]
  let d16 count active-agent with [xcor >= 150 and xcor < 160]
  let d17 count active-agent with [xcor >= 160 and xcor < 170]
  let d18 count active-agent with [xcor >= 170 and xcor < 180]
  let d19 count active-agent with [xcor >= 180 and xcor < 190]
  let d20 count active-agent with [xcor >= 190 and xcor < 200]

  set discipline-list (list d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20)

  ;  set discipline-list n-values 20 [i ->
  ;  let lower-bound i * 10
  ;  let upper-bound lower-bound + 10
  ;  count active-agent with [xcor >= lower-bound and xcor < upper-bound]]

  set discipline-num length (filter [i -> i > 0] discipline-list)

  set discipline-num-list (sentence discipline-num-list discipline-num)

end

to print-out
  clear-output

  ;  output-type "turtle properties \n"

  ;  foreach sort-on [who] turtles
  ;  [ each-turtle -> ask each-turtle [
  ;    output-type "[ " output-type [who] of self  output-type " " output-type [xcor] of self output-type " "  output-type [ycor] of self output-type " "
  ;    output-type[age] of self output-type " ]" output-type "\n"
  ;    output-print [coordinates] of self
  ;    ] ]

  output-print explored output-print length explored output-type precision ( (length explored) / (max-pxcor * max-pycor)) 4 * 100 output-type"%" output-type "\n"
  output-print sort-by < (explored-x) output-print length explored-x output-type precision ( (length explored-x) / max-pxcor) 2 * 100 output-type"%" output-type "\n"
  output-print sort-by < (explored-y) output-print max explored-y output-type precision ( (max explored-y) / max-pycor) 2 * 100 output-type"%" output-type "\n"






end

to good-network
  layout-spring turtles links 2 30 25
end

to color-links
  ask links [
    show-link

    if link-color-setting = "now" [
      let link-color [89.9 89 88 87 86 19 18 17 16 15]
      let weight-list-10devided n-values 10 [i -> floor (max [weight] of links * (i / 10))]
      let weight-section reduce [ [a b] -> ifelse-value (a <= weight and b > weight) [a] [b] ] weight-list-10devided
      let color-num position weight-section weight-list-10devided
      set color item color-num link-color    ]

    if link-color-setting = "all-time" [
      let link-color [89.9 89 88 87 86 19 18 17 16 15]
      let weight-list-10devided n-values 10 [i -> floor (max [max-weight-life] of links * (i / 10))]
      let weight-section reduce [ [a b] -> ifelse-value (a <= max-weight-life and b > max-weight-life) [a] [b] ] weight-list-10devided
      let color-num position weight-section weight-list-10devided
      set color item color-num link-color    ]]
end

to print-mp-time

  let lcth []
  ask patches with [value = 1] [
    let cth count turtles-here
    set lcth lput cth lcth]

    set peak-ratio precision ((sum lcth) / count turtles) 2



  let ratio-mount count turtles with [credit > 1] / count turtles

  if ratio-mount = 1 and item 0 mp-time = 0 [
    let all-mount-time ticks
    set mp-time replace-item 0 mp-time all-mount-time]

  if sum lcth > 0 and item 1 mp-time = 0 [
    let first-peak-time ticks
    set mp-time replace-item 1 mp-time first-peak-time
  ]


  if peak-ratio = 1 and item 2 mp-time = 0 [
    let all-peak-time ticks
    set mp-time replace-item 2 mp-time all-peak-time]





end

to retire



  ask active-agent [




    set age age + 1
    if age >= (20 + max-life)
    [


      ;将自己与neighbor的credit平均值作为自己的wei-credit
      let turtle-s turtle-set self
      repeat group-selection-degree [
        set turtle-s (turtle-set turtle-s [link-neighbors] of turtle-s with [color = red])]
      set wei-credit mean [credit] of turtle-s

;      ;将自己与CS上下5范围内的credit平均值作为自己的wei-credit
;            ask active-agent [
;              let turtle-t turtle-set self
;              let turtle-scope co-scope
;              ask active-agent [
;          if abs round (co-scope -  turtle-scope) <= 2.5[
;            set  turtle-t (turtle-set turtle-t self)]]
;;        output-print turtle-t
;        set wei-credit mean [wei-credit] of turtle-t]



  let min-cre min-one-of active-agent [wei-credit]
  let max-cre max-one-of active-agent [wei-credit]
  let a [wei-credit] of min-cre
  let b [wei-credit] of max-cre


  let c b - a

       ;    let d (credit - a) / c
    let d (wei-credit - a) / c
    set d ((2 * d) ^ 3 / (2 ^ 3))



      ifelse random-float 1 < d [
        hatch 1[
          set credit 1
          set age 20 ;(random 31) + 20
          set coordinates []
          set d-credit 0
          set work-count 0
          setxy random-xcor random-ycor
      ]][
        hatch 1[
          set credit 1
          set age 20;(random 31) + 20
          set coordinates []
          set d-credit 0
          set work-count 0
          setxy random-xcor random-ycor

          ifelse random-float 1 > 1[

            let turtle-t turtle-set myself
            let turtle-t-1 turtles in-radius co-scope
            let turtle-t-2 [link-neighbors] of turtle-t
            set turtle-t (turtle-set turtle-t turtle-t-1 turtle-t-2 )
            let q [credit] of myself
            let better-target one-of turtle-t with [credit >= q]
            ;          let better-target max-one-of turtle-t [credit]
            let x [co-scope] of better-target - co-scope
            set co-scope co-scope + random x][

            let min-coscop min-one-of active-agent [co-scope]
            let max-coscop max-one-of active-agent [co-scope]
            let x [co-scope] of min-coscop
            let y [co-scope] of max-coscop
;            let z round (mean [co-scope] of turtles)
;            let r min (list abs (x - z) abs (y - z))
            set co-scope x + random (y - x)
          ]

      ]]

      set color white]]


;  set color white
;  ask my-links [
;    die
;    set weight weight / 2
;  hide-link]

end


to   calculate-com
  ask turtles [
    set component-id -1
  ]

  set num-components 0
  let current-id 0
  set component-count []

  ask turtles [
    if component-id = -1 [

      ; 如果turtle没有被访问过，执行下面遍历自己网络命令

      let stack (list self)
      let stack-count 0
      while [not empty? stack] [
        ask one-of stack [
          set component-id current-id
          set stack remove self stack
          set stack-count stack-count + 1
          ask link-neighbors with [component-id = -1][
            set stack lput self stack
      ]]]


      set component-count lput (list current-id stack-count) component-count
      set current-id current-id + 1
      set num-components num-components + 1
    ]
  ]

  set component-num map[
    a -> item 1 a  ] component-count

  let result-list []
  let num-of-comps remove-duplicates component-num
  foreach num-of-comps [ i ->
    let counts 0
    foreach component-num [a ->
      if a = i [
        set counts counts + 1
    ]]
    set result-list lput (list i counts) result-list
  ]
  set component-num result-list
  set component-num sort-by [
    [a b] -> item 0 a > item 0 b ] component-num


;  output-print num-components
;  output-print component-count
;  output-print map[
;    a -> item 1 a  ] component-count
;  output-print component-num
end



;代码垃圾桶




;to roam-space
;
;  let solo-direction random 2
;
;  if solo-direction = 0 [
;    ifelse random-float 1 < 0.5
;
;    [let new-xcor xcor + 1
;      let new-ycor ycor
;      while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
;        or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
;        or [pcolor] of patch new-xcor new-ycor = yellow
;      ]
;      [set new-xcor new-xcor + 1]
;      set xcor new-xcor ]
;
;
;    [let new-xcor xcor - 1
;      let new-ycor ycor
;      while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
;        or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
;        or [pcolor] of patch new-xcor new-ycor = yellow
;      ]
;      [set new-xcor new-xcor - 1]
;      set xcor new-xcor ]]
;
;
;  if solo-direction = 1
;    [ifelse random-float 1 < 0.5
;
;      [let new-ycor ycor + 1
;        let new-xcor xcor
;        while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
;          or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
;          or [pcolor] of patch new-xcor new-ycor = yellow
;        ]
;        [set new-ycor new-ycor + 1]
;        set ycor new-ycor ]
;
;
;      [let new-ycor ycor - 1
;        let new-xcor xcor
;        while [any? other turtles with [xcor = new-xcor and ycor = new-ycor]
;          or [pcolor] of patches with [pxcor = new-xcor and pycor = new-ycor] = white
;          or [pcolor] of patch new-xcor new-ycor = yellow
;        ]
;        [set new-ycor new-ycor - 1]
;        set ycor new-ycor ] ]
;
;end
@#$#@#$#@
GRAPHICS-WINDOW
562
68
1041
548
-1
-1
2.3433
1
10
1
1
1
0
1
1
1
0
200
0
200
0
0
1
ticks
30.0

BUTTON
20
64
86
97
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

INPUTBOX
122
46
172
106
initial-scientists
50.0
1
0
Number

OUTPUT
220
534
506
819
12

BUTTON
0
661
94
694
NIL
print-out
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
17
124
80
157
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
110
124
173
157
NIL
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

INPUTBOX
374
27
529
87
co-scope-setting
50.0
1
0
Number

SLIDER
537
1123
695
1156
learn-degree-setting
learn-degree-setting
0
1
1.0
0.01
1
NIL
HORIZONTAL

CHOOSER
321
399
478
444
co-inclina-setting
co-inclina-setting
"equal" "community" "friend"
0

CHOOSER
368
184
528
229
learn-strategy-setting
learn-strategy-setting
"conservative" "paradigmical" "radical"
2

CHOOSER
180
189
359
234
learn-strategy-choice
learn-strategy-choice
"choose" "random"
0

BUTTON
1
462
121
495
NIL
good-network
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
352
1120
517
1165
learn-degree-choice
learn-degree-choice
"choose" "random"
0

CHOOSER
139
397
289
442
co-inclina-choice
co-inclina-choice
"choose" "random"
0

CHOOSER
193
36
331
81
co-scope-choice
co-scope-choice
"input" "random"
1

MONITOR
754
10
832
55
平均credit
precision (mean [credit] of turtles) 2
17
1
11

MONITOR
843
11
921
56
最高credit
round (max [credit] of turtles)
17
1
11

MONITOR
929
10
1007
55
最大链接数
max [ count my-links ] of turtles
17
1
11

PLOT
1057
543
1280
698
credit变化
NIL
NIL
0.0
0.0
0.0
0.0
true
true
"" ""
PENS
"最大" 1.0 0 -2674135 true "" "plot max [credit] of turtles"
"平均" 1.0 0 -13791810 true "" "plot mean [credit] of turtles"
"核心" 1.0 0 -13840069 true "" ";plot [credit] of max-one-of active-agent [count my-links]"

SLIDER
538
1069
701
1102
co-threshold-ratio-setting
co-threshold-ratio-setting
0
0.3
0.0
0.01
1
NIL
HORIZONTAL

CHOOSER
354
1060
513
1105
co-threshold-ratio-choice
co-threshold-ratio-choice
"choose" "random"
0

MONITOR
1073
48
1228
93
活跃科学家平均合作范围
precision (mean [co-scope] of active-agent) 2
17
1
11

MONITOR
1449
49
1511
94
活跃科学家平均学习程度
precision (mean [learn-degree] of active-agent) 2
17
1
11

MONITOR
1367
49
1442
94
活跃科学家平均合作门槛
precision (mean [co-threshold-ratio] of active-agent) 2
17
1
11

MONITOR
1366
290
1431
335
范式策略
count turtles with [learn-strategy = \"paradigmical\"]
17
1
11

MONITOR
1295
291
1360
336
保守策略
count turtles with [learn-strategy = \"conservative\"]
17
1
11

MONITOR
1431
290
1501
335
激进策略
count turtles with [learn-strategy = \"radical\"]
17
1
11

CHOOSER
164
1069
321
1114
link-color-setting
link-color-setting
"now" "all-time"
1

BUTTON
96
654
208
687
链接染色
color-links
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
547
10
599
55
总人数
count turtles
17
1
11

MONITOR
611
11
683
56
活跃人数
count active-agent
17
1
11

MONITOR
1067
292
1124
337
合作倾向-平等
count turtles with [co-inclina = \"equal\"]
17
1
11

MONITOR
1131
292
1188
337
社群
count turtles with [co-inclina = \"community\"]
17
1
11

MONITOR
1190
291
1247
336
熟人
count turtles with [co-inclina = \"friend\"]
17
1
11

PLOT
1059
703
1288
850
科学共同体规模
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"活跃人数" 1.0 0 -2674135 true "" "plot count active-agent"
"活人" 1.0 0 -13840069 true "" "plot count turtles"
"总人" 1.0 0 -14737633 true "" "plot [who] of max-one-of turtles [who] "

PLOT
1058
103
1259
263
合作范围，门槛与学习程度
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"合作范围" 1.0 0 -2674135 true "" "plot precision (mean [co-scope] of active-agent) 2"

PLOT
1061
357
1261
507
合作倾向
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"平等" 1.0 0 -16777216 true "" "plot (count turtles with [co-inclina = \"equal\"]) / count turtles"
"社群" 1.0 0 -7500403 true "" "plot (count turtles with [co-inclina = \"community\"]) / count turtles"
"熟人" 1.0 0 -2674135 true "" "plot (count turtles with [co-inclina = \"friend\"]) / count turtles"

PLOT
1309
355
1509
505
学习策略
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"保守" 1.0 0 -16777216 true "" "plot (count turtles with [learn-strategy = \"conservative\"]) / count turtles"
"范式" 1.0 0 -7500403 true "" "plot (count turtles with [learn-strategy = \"paradigmical\"]) / count turtles"
"激进" 1.0 0 -2674135 true "" "plot (count turtles with [learn-strategy = \"radical\"]) / count turtles"

PLOT
1270
103
1512
260
合作门槛与学习程度
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"合作门槛" 1.0 0 -13840069 true "" "plot precision (mean [co-threshold-ratio] of active-agent) 2"
"学习程度" 1.0 0 -13791810 true "" "plot precision (mean [learn-degree] of active-agent) 2"
"合作率" 1.0 0 -2674135 true "" "plot precision (mean [co-prob] of active-agent) 2"

SLIDER
359
112
531
145
co-prob-setting
co-prob-setting
0
1
1.0
0.01
1
NIL
HORIZONTAL

CHOOSER
188
106
327
151
co-prob-choice
co-prob-choice
"choose" "random"
0

MONITOR
1280
48
1338
93
合作率
precision (mean [co-scope] of turtles) 2
17
1
11

BUTTON
107
708
221
741
去除路径黄色
ask patches \n[set pcolor black]
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
0
547
135
580
隐藏权重为1链接
ask links with [weight = 1] \n[hide-link]
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
4
579
132
612
隐藏无链接主体
ask turtles with [count my-links with [hidden? != true] = 0]\n[hide-turtle]
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
31
193
136
226
co-scope 5
ask turtles [\nset co-scope 5 ]
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
686
11
752
56
登山人数
count turtles with [credit > 1]
17
1
11

PLOT
1307
517
1490
662
dcredit变化
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"平均dcredit变化" 1.0 0 -13791810 true "" "plot mean [d-credit] of turtles"
"最大dcredit变化" 1.0 0 -2674135 true "" "plot max [d-credit] of turtles"

PLOT
1289
683
1489
833
山上人占比
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"登山占比" 1.0 0 -11085214 true "" "plot count turtles with [credit > 1] / count turtles"
"登顶占比" 1.0 0 -2674135 true "" "plot peak-ratio"

MONITOR
518
632
624
677
NIL
mp-time
17
1
11

MONITOR
717
631
782
676
登顶率
peak-ratio
17
1
11

SWITCH
175
1124
308
1157
landspace?
landspace?
0
1
-1000

MONITOR
627
632
705
677
登山人比例
precision (count turtles with [credit > 1] / count turtles) 2
17
1
11

MONITOR
515
695
638
740
当前patch平均价值
precision (mean [value] of patches with [count turtles-here > 0]) 2
17
1
11

PLOT
1051
866
1251
1016
探索率
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot precision ( (length explored) / (max-pxcor * max-pycor)) 4 * 100"

MONITOR
543
759
600
804
探索率
precision ( (length explored) / (max-pxcor * max-pycor)) 4 * 100
17
1
11

CHOOSER
166
247
304
292
roam-type
roam-type
"roam" "roam-space" "mix"
1

SLIDER
327
253
499
286
roam-pro
roam-pro
0
1
0.0
0.1
1
NIL
HORIZONTAL

MONITOR
643
695
721
740
平均credit
precision (mean [credit] of turtles) 2
17
1
11

MONITOR
611
761
700
806
探索总价值
precision (sum [value] of patches with [pcolor = yellow]) 2
17
1
11

CHOOSER
158
302
308
347
distance-mountain
distance-mountain
"random" "choose"
0

MONITOR
1016
10
1076
55
NIL
precision (mean [count my-links] of turtles) 2
17
1
11

SLIDER
327
307
504
340
mountain-distance
mountain-distance
0
125
15.0
5
1
NIL
HORIZONTAL

PLOT
793
603
1007
764
节点CS分布直方图
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"co-scope" 1.0 1 -16777216 true "" "let co-scope-list []\nask turtles [\nlet a co-scope\nset co-scope-list lput a co-scope-list\n]\nset-plot-x-range min [co-scope] of turtles max [co-scope] of turtles\nset-histogram-num-bars max [co-scope] of turtles - min [co-scope] of turtles\nhistogram co-scope-list\n"
"credit" 1.0 1 -7500403 true "" ";let credit-list []\n;ask turtles [\n;let a credit\n;set credit-list lput a credit-list ]\n;set-plot-x-range min [credit] of turtles max [credit] of turtles\n;set-histogram-num-bars max [credit] of turtles - min [credit] of turtles\n;histogram credit-list\n"

SLIDER
316
352
493
385
mountain-radius
mountain-radius
10
160
70.0
10
1
NIL
HORIZONTAL

SLIDER
138
356
310
389
peak-num
peak-num
1
100
100.0
1
1
NIL
HORIZONTAL

PLOT
795
768
1000
924
credit分布
NIL
NIL
0.0
50.0
0.0
10.0
true
false
"" ""
PENS
"y坐标" 1.0 1 -16777216 false "" ";let y-list [ycor] of active-agent\n;set-histogram-num-bars (max-pxcor / 10)\n;set-plot-x-range 0 max-pycor\n;histogram y-list"
"credit分布" 1.0 0 -11085214 true "" "clear-plot\n\nset-plot-y-range round (min [credit] of turtles) round (max [credit] of turtles)\n\nforeach credit-list [i ->    plot i]\n"

MONITOR
716
757
773
802
挖掘率
precision (sum [value] of patches with [pcolor = yellow] / sum [value] of patches * 100) 2
17
1
11

MONITOR
723
691
783
736
已发现peak数
count peaks with [pcolor = yellow]
17
1
11

SLIDER
6
262
128
295
peak-value-adjust
peak-value-adjust
0
1
0.1
0.1
1
NIL
HORIZONTAL

MONITOR
551
583
607
628
发现global peak数
count patches with [value = 1 and count turtles-here != 0 ]
17
1
11

MONITOR
626
588
686
633
NIL
precision ( count patches with [value = 1 and count turtles-here != 0 ] / count patches with [value = 1] ) 4 * 100
17
1
11

MONITOR
702
586
764
631
NIL
precision ( count patches with [value = 1 and count turtles-here != 0 ] / count peaks with [pcolor = yellow] ) 4 * 100
17
1
11

MONITOR
1086
10
1136
55
NIL
num-components
17
1
11

MONITOR
1151
5
1382
50
NIL
mean-wei-credit-list
17
1
11

SLIDER
19
323
124
356
max-scope
max-scope
50
200
150.0
10
1
NIL
HORIZONTAL

INPUTBOX
172
461
270
521
climate-change
100.0
1
0
Number

INPUTBOX
304
458
457
518
max-life
100.0
1
0
Number

SLIDER
9
394
101
427
group-selection-degree
group-selection-degree
0
4
0.0
1
1
NIL
HORIZONTAL

MONITOR
580
890
660
935
NIL
credit-list
17
1
11

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test-1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <metric>mean-wei-credit-list</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test-2" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <metric>mean-wei-credit-list</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <metric>mean-wei-credit-list</metric>
    <steppedValueSet variable="max-scope" first="100" step="10" last="200"/>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <metric>mean-wei-credit-list</metric>
    <steppedValueSet variable="max-scope" first="100" step="10" last="200"/>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-3" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-4-1r" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-4-1rs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-4-2r" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-4-2rs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-5-1r" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-5-1rs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-5-2r" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-5-2rs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-6 test-life-climate （100-50）" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-6 test-life-climate （50-100）" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-6 test-life-climate （50-25）" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-6 test-life-climate （25-50）" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-7" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <metric>mean-wei-credit-list</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-8" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-4-1r-200" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-4-2r-200" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-5-1r-200" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-5-2r-200" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-9-2" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-9-1" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-6 r-200-1" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="V-6 rs-150-0" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>precision (mean [co-scope] of turtles) 2</metric>
    <enumeratedValueSet variable="max-scope">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-type">
      <value value="&quot;roam-space&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-selection-degree">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-life">
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-change">
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-num">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-setting">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-mountain">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peak-value-adjust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-setting">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mountain-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-scope-choice">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-inclina-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roam-pro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-threshold-ratio-setting">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-strategy-setting">
      <value value="&quot;radical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn-degree-setting">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-choice">
      <value value="&quot;choose&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="co-prob-setting">
      <value value="1"/>
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
