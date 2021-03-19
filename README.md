# trivial-gamekit-ui
### _ava fox_

trivial-gamekit module for creating common user interface widgets quickly

## Installing

```shell
$ git clone https://github.com/compufox/trivial-gamekit-ui ~/common-lisp/
$ ros run # (or run whichever lisp you use)
* ;; install the quicklisp distro that houses trivial-gamekit
* (ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
* ;; ready to go!
```

## Usage

Define your game with the `with-ui` superclass

```lisp
(gamekit:defgame your-game (with-ui) ()
  ...)
```

in your `gamekit:post-initialize`:

```lisp
(defmethod gamekit:post-initialize ((this your-game))
  ...
  (initalize-ui this
    (:label (make-label "Hello World!" (vec2 50 50) :color gamekit.colors:+black+)))
  ...)
```

then in your `gamekit:draw`

```lisp
(defmethod gamekit:draw ((this your-game))
  ...
  (draw-ui this)
  ...)
```


please see `example.lisp` for a more detailed example

## API

`(make-label (text &key (position (vec2 0 0)) (font :default) (size 12) (color gamekit.colors:+black+) (justification :left))`

creates and returns a label

---

`(make-button &key (position (vec2 0 0)) label (fill-color gamekit.colors:+black+) (hover-color gamekit.colors:+black+) (pressed-color gamekit.colors:+black+) (stroke-color gamekit.colors:+transparent+) (stroke-thickness 0) (rounding 0) image hover-image pressed-image on-click)`

creates and returns a button

---

`(make-panel &key (position (vec2 0 0)) width height children (fill-color gamekit.colors:+black+) (stroke-color gamekit.colors:+transparent+) (stroke-thickness 0) (rounding 0) image)`

creates and returns a panel

---

`(make-progress-bar width height &key (position (vec2 0 0)) (fill-color gamekit.colors:+white+) (stroke-color gamekit.colors:+transparent+) (stroke-thickness 0) (inner-color gamekit.colors:+black+) (rounding 0) (percent 0) show-percent (font :default) (font-size 12) (font-color gamekit.colors:+black+))`

creates and returns a progress-bar

---

`(draw-widget this)`

draws a UI widget on the screen

---

`(ui-element state-or-game element)`

returns a UI widget from STATE-OR-GAME identified by ELEMENT

ex: `(ui-element (gamekit.fistmachine:current-state) :score-label)`

---

`(setf (ui-element state-or-game element) widget)`

sets a UI widget in STATE-OR-GAME, identified by ELEMENT

---

`(draw-ui state-or-game)`

draws all ui elements of state-or-game

---

`(initialize-ui state-or-game &rest forms)`

macro that allows you to set multiple widgets in STATE-OR-GAME at once

each form in FORMS should be (IDENTIFIER WIDGET)

---

`(text-width text &optional (font gamekit::*font*))`

returns the width of TEXT using FONT

---

`(text-height text &optional (font gamekit::*font*))`

returns the height of TEXT using FONT

---

`(text-dimensions text &optional (font gamekit::*font*))`

returns a vec2 of the width and height of TEXT using FONT

---

accessors exported: `text` `size` `widget-position` `text-color` `image` `stroke` `hover-color` `pressed-color` `on-click` `stroke-thickness` `hover-image` `pressed-image` `stroke-color` `children` `width` `height` `label` `rounding` `justification` `render`

---

## License

MIT

