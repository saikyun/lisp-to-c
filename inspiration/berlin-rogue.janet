(use freja/flow)

# type: has :attack -> has :hp -> void
(defn attack
  [o1 o2]
  (update o2 :hp - (o1 :attack)))

# type: game object -> void
(defn move
  [self]
  (print "moving " (self :name)))

# vector2 -> maybe<game-object>
(defn adjacent-enemy
  [pos]
  # (assert (indexed? pos))
  # thing
)

# type: game object -> void
(defn move-attack
  [self]
  (print "moving " (self :name))

  ### type error! adjacent-enemy needs vector2 !
  ## self is wolf / game-object
  (when-let [target (adjacent-enemy self)]
    (attack self target)))

# type: game object -> void
(defn render-wolf
  [self]
  (draw-rectangle 1 1 ;(self :size) :gray))

# type: list<game object>
(def tiles @[{:hp 3
              :name "Wolf"
              :tick move-attack
              :render render-wolf
              :size [30 30]}
             {:hp 3
              :name "Wolf"
              :tick move
              :render render-wolf
              :size [30 30]}])

# type: void
(defn render
  [_]
  (rl-translatef 10 10 0)

  (loop [t :in tiles]
    (:tick t))

  (var x 0)
  (var y 0)
  (loop [t :in tiles]
    (rl-translatef x 0 0)
    (:render t)
    (+= x 32)))

(start-game {:render render})
