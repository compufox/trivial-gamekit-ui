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

Create a UI element (label, button, panel)

`(defvar *my-label* (make-label "Hello World!" (vec2 50 50) :color gamekit.colors:+black+))`

then in your `gamekit:draw`

```lisp
(defmethod gamekit:draw ((this your-game))
  ...
  (draw-widget *my-label*)
  ...)
```

please see `example.lisp` for more detailed example

## API

`(make-label (text &key (position (vec2 0 0)) (font :default) (size 12) (color gamekit.colors:+black+))`

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

`(text-width text &optional (font gamekit::*font*))`

returns the width of TEXT using FONT

---

`(text-height text &optional (font gamekit::*font*))`

returns the height of TEXT using FONT

---

`(text-dimensions text &optional (font gamekit::*font*))`

returns a vec2 of the width and height of TEXT using FONT

---

accessors exported: `text` `size` `widget-position` `text-color` `image` `stroke` `hover-color` `pressed-color` `on-click` `stroke-thickness` `hover-image` `pressed-image` `stroke-color` `children` `width` `height` `label` `rounding`

---

## License

MIT

