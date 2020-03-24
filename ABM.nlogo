globals[
  waiting-planes
  takeoff-time
  landing
  runway-free
  hourly-schedule-1
  hourly-schedule-2
  day
  stop-queue
]

patches-own [
  dist
  schedule
  next-plane
  next-day
]

turtles-own[
  speed
  waiting-time-runway
  waiting-time-taxiway
  heure-start
  heure-takeoff
]


to setup
  clear-all
  draw-road
  ask patches [setup-terminals]
  setup-variables
  file-open file-name
  file-write "Nbjours =" file-write Nbjours file-write "d1 = " file-write d1 file-write "d2 = " file-write d2 
  file-write "landing-time = " file-write landing-time file-write "p =" file-write p file-write "Cycle journalier =" file-write daily-cycle 
  file-write "Max queue length =" file-write queue-length file-print ""
  file-write "Indicatif" file-write "HeureStart" file-write "HeureStop" file-write "TempsAttentePiste" file-write "TempsArretTotal" file-print ""
  reset-ticks
end

to draw-road
  ask patches [
    ; the road is surrounded by green grass of varying shades
    set pcolor green - random-float 0.5
  ]
  ask patches with [pycor = -16 and pxcor >= -15 and pxcor <= 14] [
    ; the road itself is varying shades of grey
    set pcolor grey - 2.5 + random-float 0.25
  ]
  ask patches with [pycor = -14 and pxcor >= -15 and pxcor <= 14] [
    ; the road itself is varying shades of grey
    set pcolor grey - 2.5 + random-float 0.25
  ]
  ask patches with [pxcor = 16 and pycor >= -12 and pxcor <= 16] [
    ; the road itself is varying shades of grey
    set pcolor grey - 2.5 + random-float 0.25
  ]
  ask patches with [pxcor >= 15 and pycor >= -16 and pycor <= -13] [
    ; the road itself is varying shades of grey
    set pcolor yellow - 2.5 + random-float 0.25
  ]
end

to setup-terminals
  if pycor = -16 and pxcor = -16 [
    set pcolor red
    set dist d1
    set schedule 4 * n1
    set next-plane random schedule
    set next-day FALSE
  ]

  if pycor = -14 and pxcor = -16 [
    set pcolor blue
    set dist d2
    set schedule 4 * n2
    set next-plane random schedule
    set next-day FALSE
  ]
end

to setup-variables
  set waiting-planes []
  set takeoff-time 0
  set runway-free TRUE
  set hourly-schedule-1 [900.85524 1768.65672  440.86188  407.44986  476.14264  144.46815  123.20222  122.76612   77.77292   75.59608 85.72204   73.63107
    74.40157   95.58057   92.04181  110.58833  107.53583  110.50668  126.98129   81.61860 88.47961  199.50894  336.48841  534.38557]
  set hourly-schedule-2 [900.85524 1768.65672  440.86188  407.44986  476.14264  144.46815  123.20222  122.76612   77.77292   75.59608 85.72204   73.63107
    74.40157   95.58057   92.04181  110.58833  107.53583  110.50668  126.98129   81.61860 88.47961  199.50894  336.48841  534.38557]
  set day 1
  set stop-queue FALSE 
end


to go
  if durée_simu [if day = Nbjours + 1 [
    file-close
    stop]]
  if ticks = 86400 [
    set day day + 1
    output-print day
    ask patch -16 -16 [
      set next-day FALSE
    ]
    ask patch -16 -14 [
      set next-day FALSE
    ]
    reset-ticks]
  create-plane
  move-planes
  waiting-zone
  take-off
  change-schedule
  tick
end

to create-plane
  set-default-shape turtles "airplane"
  ;;let terminals-patches patches with [pxcor = -16 and member? pycor [-16 -14]]
  ;;ask terminals-patches [if next-plane <= ticks [sprout 1[
  ask patch -16 -16[if next-plane <= ticks and next-day = FALSE [sprout 1[
    set heading 90
    set speed (v * 30) / (dist * 3600) + random-exponential 0.001
    set color pcolor
    set waiting-time-runway 0
    set heure-start ticks
    ]
    set next-plane ticks + random-normal schedule 10
    if next-plane mod 86400 < next-plane [
      set next-day TRUE
      set next-plane next-plane mod 86400
    ]
  ]]
  ask patch -16 -14[if next-plane <= ticks and next-day = FALSE [sprout 1[
    set heading 90
    set speed (v * 30) / (dist * 3600) + random-exponential 0.001
    set color pcolor
    set waiting-time-runway 0
    set heure-start ticks
    ]
    set next-plane ticks + random-normal schedule 5
     if next-plane mod 86400 < next-plane [
      set next-day TRUE
      set next-plane next-plane mod 86400
    ]
  ]]
end

to move-planes
  ifelse stop-queue = FALSE [ask turtles-on patches with [pxcor < 15][
    fd speed
  ]][ask turtles-on patches with [pxcor < 15][
    set waiting-time-taxiway waiting-time-taxiway + 1
  ]]
end

to waiting-zone
  foreach waiting-planes [n -> ask turtle n [set waiting-time-runway waiting-time-runway + 1]]
  ask turtles-on patches with [pxcor = 15 and pycor <= -14] [
    set waiting-planes lput who  waiting-planes
    set heading 0
    move-to patch 16 -13
    if length(waiting-planes) >= queue-length [
      set stop-queue TRUE  
    ]
  ]
end

to take-off
  if length waiting-planes > 0[
    ifelse takeoff-time = 0 and runway-free [
    ask turtle item 0 waiting-planes [move-to patch 16 -12]
    set waiting-planes remove-item 0 waiting-planes
      if length(waiting-planes) < queue-length [ set stop-queue FALSE]
    set runway-free FALSE
    set landing random-float 1
      ifelse landing < p [set takeoff-time landing-time] [set takeoff-time 0]][if runway-free [set takeoff-time takeoff-time - 1]]]

  ask turtles-on patches with [pxcor = 16 and pycor >= -12][
    fd 28 / 121
  ]

  if any? turtles-on patch 16 16 [set runway-free TRUE]
  ask turtles-on patches with[pxcor = 16 and pycor  = 16][
    set heure-takeoff ticks
    file-write who file-write heure-start file-write heure-takeoff file-write waiting-time-runway file-write waiting-time-taxiway file-print ""
    die
  ]

end

to change-schedule
  if daily-cycle and ticks mod 3600 = 0[
    set n1 item (ticks / 3600  mod 24) hourly-schedule-1 + random-normal 0 15
    set n2 item (ticks / 3600  mod 24) hourly-schedule-2 + random-normal 0 15
    ask patch -16 -16 [
      set schedule 4 * n1
    ]
    ask patch -16 -14 [
      set schedule 4 * n2
    ]
  ]
end
